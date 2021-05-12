const sampler_t sampler =
    CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

#define DIM 2
#if DIM==3

#define IMG_T image3d_t
#define GID_T int3
#define INIT_GID(gid) GID_T gid; gid = (GID_T)(get_global_id(0), get_global_id(1), get_global_id(2));

#else
#define IMG_T image2d_t
#define GID_T int2
#define INIT_GID(gid) GID_T gid; gid = (GID_T)(get_global_id(0), get_global_id(1));
#endif


__kernel void intensity(__read_only IMG_T inputImage,
                        __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newf4 = (float4)(f4.x * 0.2126 + f4.y * 0.7152 + f4.z * 0.0722, 0, 0 , 0 );

  write_imagef(outImage, gid, newf4);
}

__kernel void getComponent(__read_only image2d_t inputImage,
                           __write_only image2d_t outputImage,
                           __global unsigned int *c) {

  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  uint4 ui4 = read_imageui(inputImage, sampler, gid);
  int i = ((0 == *c) * ui4.x) + ((1 == *c) * ui4.y) + ((2 == *c) * ui4.z) +
          ((3 == *c) * ui4.w);

  write_imageui(outputImage, gid, i);
}

__kernel void rgbComps(__read_only image2d_t inputImage1,
                       __read_only image2d_t inputImage2,
                       __read_only image2d_t inputImage3,
                       __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  uint4 pix1 = (uint4)read_imageui(inputImage1, gid);
  uint4 pix2 = (uint4)read_imageui(inputImage2, gid);
  uint4 pix3 = (uint4)read_imageui(inputImage3, gid);

  write_imageui(outImage, gid, (uint4)(pix1.x, pix2.y, pix3.z, 255));
}

__kernel void rgbaComps(__read_only image2d_t inputImage1,
                        __read_only image2d_t inputImage2,
                        __read_only image2d_t inputImage3,
                        __read_only image2d_t inputImage4,
                        __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  uint4 pix1 = (uint4)read_imageui(inputImage1, gid);
  uint4 pix2 = (uint4)read_imageui(inputImage2, gid);
  uint4 pix3 = (uint4)read_imageui(inputImage3, gid);
  uint4 pix4 = (uint4)read_imageui(inputImage4, gid);

  write_imageui(outImage, gid, (uint4)(pix1.x, pix2.y, pix3.z, pix4.w));
}

__kernel void border(__write_only image2d_t outputImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 dim = (int2)(get_global_size(0), get_global_size(1));

  int condition = (gid.x == 0 || gid.x == dim.x - 1) || (gid.y == 0 || gid.y == dim.y - 1);

  write_imageui(outputImage, gid, condition);
}

__kernel void dilate(__read_only image2d_t inputImage,
                     __write_only image2d_t outputImage) {
  int x = get_global_id(0);
  int y = get_global_id(1);
  int2 coord = (int2)(x, y);

  int found = 0;
  int2 newcoord;
  uint4 ui4;
  for (int a = -1; a <= 1; a++) {
    for (int b = -1; b <= 1; b++) {
      newcoord = (int2)(x + a, y + b);
      ui4 = read_imageui(inputImage, sampler, newcoord);
      found = found + (ui4.x > 0);
    }
  }

  write_imageui(outputImage, coord, found > 0);
}

__kernel void erode(__read_only image2d_t inputImage,
                    __write_only image2d_t outputImage) {
  int x = get_global_id(0);
  int y = get_global_id(1);
  int2 coord = (int2)(x, y);

  int found = 0;
  int2 newcoord;
  uint4 ui4;
  for (int a = -1; a <= 1; a++) {
    for (int b = -1; b <= 1; b++) {
      newcoord = (int2)(x + a, y + b);
      ui4 = read_imageui(inputImage, sampler, newcoord);
      found = found || (ui4.x == 0);
    }
  }

  write_imageui(outputImage, coord, !found);
}

/*********************** BOOLEAN OPERATORS ***********************/

__kernel void booleanImg(__write_only IMG_T outputImage, __constant float val) {
  INIT_GID(gid)

  write_imageui(outputImage, gid, convert_int(val));
}

__kernel void logand(__read_only IMG_T inputImage1,
                     __read_only IMG_T inputImage2,
                     __write_only IMG_T outImage) {

  INIT_GID(gid)

  int4 value1 = (int4)read_imageui(inputImage1, sampler, gid);
  int4 value2 = (int4)read_imageui(inputImage2, sampler, gid);

  write_imageui(outImage, gid, ((value1.x != 0.0) && (value2.x != 0.0)));
}

__kernel void logor(__read_only IMG_T inputImage1,
                     __read_only IMG_T inputImage2,
                     __write_only IMG_T outImage) {

  INIT_GID(gid)

  int4 value1 = (int4)read_imagef(inputImage1, sampler, gid);
  int4 value2 = (int4)read_imagef(inputImage2, sampler, gid);

  write_imageui(outImage, gid, ((value1.x != 0.0) || (value2.x != 0.0)));
}

__kernel void lognot(__read_only IMG_T inputImage,
                     __write_only IMG_T outImage) {

  INIT_GID(gid)

  int4 value1 = (int4)read_imagef(inputImage, sampler, gid);

  write_imageui(outImage, gid, !(value1.x != 0.0));
}

/******************** FLOAT OPERATORS ********************/

__kernel void constImg(__write_only image2d_t outImage, __constant float value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  write_imagef(outImage, gid, value);
}

__kernel void geq(__read_only image2d_t image, __write_only image2d_t outImage,
                  __constant float value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  float4 ui4 = (float4)read_imagef(image, sampler, gid);

  int condition = (ui4.x > value);

  write_imageui(outImage, gid, condition);
}

__kernel void leq(__read_only image2d_t inputImage,
                  __write_only image2d_t outImage, __constant float value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  float ui4 = (float4)read_imagef(inputImage, sampler, gid);

  int condition = (ui4.x < value);

  write_imageui(outImage, gid, condition);
}

__kernel void eq(__read_only image2d_t image, __write_only image2d_t outImage,
                 __constant float value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 ui4 = (float4)read_imagef(image, sampler, gid);

  int condition = (ui4.x == value);

  write_imageui(outImage, gid, condition);
}

__kernel void between(__read_only image2d_t image,
                      __write_only image2d_t outImage, __constant float value1,
                      __constant float value2) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 lid = (int2)(get_local_id(0), get_local_id(1));

  float4 ui4 = (float4)read_imagef(image, sampler, gid);

  int condition = (ui4.x >= value1) && (ui4.x <= value2);

  write_imageui(outImage, gid, condition);
}

__kernel void absImg(__read_only image2d_t inputImage,
                     __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 ui4 = (float4)read_imageui(inputImage, sampler, gid);
  float4 newui4 = (float4)(fabs(ui4.x), fabs(ui4.y), fabs(ui4.z), fabs(ui4.w));

  write_imagef(outImage, gid, newui4);
}

__kernel void add(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 + pix2)); //what about oerflows?
}

__kernel void addVS(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant float val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  uint4 pix1 = read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix1 + val));
}

__kernel void sub(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 - pix2));
}

__kernel void subSV(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant float val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (val - pix));
}

__kernel void subVS(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant float val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix - val));
}

__kernel void mul(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 * pix2));
}

__kernel void mulVS(__read_only image2d_t inputImage,
                    __write_only image2d_t outImage, __constant float val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix = (float4)read_imagef(inputImage, gid);

  write_imagef(outImage, gid, (pix * val));
}

__kernel void div(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 / pix2));
}

__kernel void divVS(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant float val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix / val));
}

__kernel void divSV(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant float val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (val / pix));
}

__kernel void mask(__read_only image2d_t inputImage1,
                   __read_only image2d_t inputImage2,
                   __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 pix = (float4)read_imagef(inputImage1, sampler, gid);
  float4 mask = (float4)read_imagef(inputImage2, sampler, gid);

  write_imagef(outImage, gid, pix.x * (mask.x > 0));
}

/******************** TEST KERNELS ********************/

__kernel void swapRG(__read_only image2d_t inputImage,
                     __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newf4 = (float4)(f4.y, f4.x, f4.z, f4.w);

  write_imagef(outImage, gid, newf4);
}

__kernel void test(__write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 newf4 = (float4)(255, 0, 0, 255);
  write_imagef(outImage, gid, newf4);
}

__kernel void slow(__read_only image2d_t inputImage,
                   __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newf4 = (float4)(f4.x, f4.y, f4.z, f4.w);

  for (int i = 0; i < 1000; i++) {
    for (int j = 0; j < 3; j++)
      newf4[j] = fmod(newf4[j] * newf4[j], 251.0f);
  }

  write_imagef(outImage, gid, newf4);
}

#endif