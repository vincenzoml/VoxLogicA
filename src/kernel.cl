#pragma OPENCL EXTENSION cl_khr_3d_image_writes : enable

const sampler_t sampler =
    CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

#define DIM 3
#if DIM==3

#define IMG_T image3d_t
#define GID_T int4
#define INIT_GID(gid) GID_T gid; gid = (GID_T)(get_global_id(0), get_global_id(1), get_global_id(2),0);

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

__kernel void getComponent(__read_only IMG_T inputImage,
                           __write_only IMG_T outputImage,
                           float c) {

  INIT_GID(gid)

  float4 ui4 = (float4)read_imagef(inputImage, sampler, gid);
  int i = ((0 == c) * ui4.x) + ((1 == c) * ui4.y) + ((2 == c) * ui4.z) +
          ((3 == c) * ui4.w);

  write_imagef(outputImage, gid, i);
}

__kernel void rgbComps(__read_only IMG_T inputImage1,
                       __read_only IMG_T inputImage2,
                       __read_only IMG_T inputImage3,
                       __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);
  float4 pix3 = (float4)read_imagef(inputImage3, gid);

  write_imagef(outImage, gid, (float4)(pix1.x, pix2.y, pix3.z, 255));
}

__kernel void rgbaComps(__read_only IMG_T inputImage1,
                        __read_only IMG_T inputImage2,
                        __read_only IMG_T inputImage3,
                        __read_only IMG_T inputImage4,
                        __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);
  float4 pix3 = (float4)read_imagef(inputImage3, gid);
  float4 pix4 = (float4)read_imagef(inputImage4, gid);

  write_imagef(outImage, gid, (float4)(pix1.x, pix2.y, pix3.z, pix4.w));
}

__kernel void border(__write_only IMG_T outputImage) {
  INIT_GID(gid)

  write_imageui(outputImage, gid, 1);
}

__kernel void dilate(__read_only IMG_T inputImage,
                     __write_only IMG_T outputImage) {
  INIT_GID(gid)

  write_imageui(outputImage, gid, 1);
}

__kernel void erode(__read_only IMG_T inputImage,
                    __write_only IMG_T outputImage) {
  INIT_GID(gid)

  write_imageui(outputImage, gid, 1);
}


__kernel void booleanImg(__write_only IMG_T outputImage, float val) {
  INIT_GID(gid)

  write_imageui(outputImage, gid, convert_int(val));
}

__kernel void logand(__read_only IMG_T inputImage1,
                     __read_only IMG_T inputImage2,
                     __write_only IMG_T outImage) {

  INIT_GID(gid)

  uint4 value1 = (uint4)read_imageui(inputImage1, sampler, gid);
  uint4 value2 = (uint4)read_imageui(inputImage2, sampler, gid);

  write_imageui(outImage, gid, ((value1.x != 0) && (value2.x != 0)));
}

__kernel void logor(__read_only IMG_T inputImage1,
                     __read_only IMG_T inputImage2,
                     __write_only IMG_T outImage) {

  INIT_GID(gid)

  uint4 value1 = (uint4)read_imageui(inputImage1, sampler, gid);
  uint4 value2 = (uint4)read_imageui(inputImage2, sampler, gid);

  write_imageui(outImage, gid, ((value1.x != 0) || (value2.x != 0)));
}

__kernel void lognot(__read_only IMG_T inputImage,
                     __write_only IMG_T outImage) {

  INIT_GID(gid)

  uint4 value1 = (uint4)read_imageui(inputImage, sampler, gid);

  write_imageui(outImage, gid, !(value1.x != 0));
}

__kernel void constImg(__write_only IMG_T outImage, float value) {
  INIT_GID(gid)

  write_imagef(outImage, gid, value);
}

__kernel void geq(__read_only IMG_T image, __write_only IMG_T outImage,
                  float value) {
  INIT_GID(gid)

  float4 ui4 = (float4)read_imagef(image, sampler, gid);

  int condition = (ui4.x > value);

  write_imageui(outImage, gid, condition);
}

__kernel void leq(__read_only IMG_T inputImage,
                  __write_only IMG_T outImage, float value) {
  INIT_GID(gid)

  float4 ui4 = (float4)read_imagef(inputImage, sampler, gid);

  int condition = (ui4.x < value);

  write_imageui(outImage, gid, condition);
}

__kernel void eq(__read_only IMG_T image, __write_only IMG_T outImage,
                 float value) {
  INIT_GID(gid)

  float4 ui4 = (float4)read_imagef(image, sampler, gid);

  int condition = (ui4.x == value);

  write_imageui(outImage, gid, condition);
}

__kernel void between(__read_only IMG_T image,
                      __write_only IMG_T outImage, float value1,
                      float value2) {
  INIT_GID(gid)

  float4 ui4 = (float4)read_imagef(image, sampler, gid);

  int condition = (ui4.x >= value1) && (ui4.x <= value2);

  write_imageui(outImage, gid, condition);
}

__kernel void absImg(__read_only IMG_T inputImage,
                     __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 ui4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newui4 = (float4)(fabs(ui4.x), fabs(ui4.y), fabs(ui4.z), fabs(ui4.w));

  write_imagef(outImage, gid, newui4);
}

__kernel void add(__read_only IMG_T inputImage1,
                  __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 + pix2)); //what about oerflows?
}

__kernel void addVS(__read_only IMG_T inputImage1,
                    __write_only IMG_T outImage, float val) {
  INIT_GID(gid)

  float4 pix1 = read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix1 + val));
}

__kernel void sub(__read_only IMG_T inputImage1,
                  __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 - pix2));
}

__kernel void subSV(__read_only IMG_T inputImage1,
                    __write_only IMG_T outImage, float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (val - pix));
}

__kernel void subVS(__read_only IMG_T inputImage1,
                    __write_only IMG_T outImage, float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix - val));
}

__kernel void mul(__read_only IMG_T inputImage1,
                  __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 * pix2));
}

__kernel void mulVS(__read_only IMG_T inputImage,
                    __write_only IMG_T outImage, float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage, gid);

  write_imagef(outImage, gid, (pix * val));
}

__kernel void div(__read_only IMG_T inputImage1,
                  __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 / pix2));
}

__kernel void divVS(__read_only IMG_T inputImage1,
                    __write_only IMG_T outImage, float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix / val));
}

__kernel void divSV(__read_only IMG_T inputImage1,
                    __write_only IMG_T outImage, float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (val / pix));
}

__kernel void mask(__read_only IMG_T inputImage1,
                   __read_only IMG_T inputImage2,
                   __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, sampler, gid);
  float4 mask = (float4)read_imagef(inputImage2, sampler, gid);

  write_imagef(outImage, gid, pix.x * (mask.x > 0));
}

/********************* CONNECTED COMPONENTS *********************/

__kernel void initCCL(__read_only image2d_t inputImage,
                      __write_only image2d_t outputImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  // int2 lid = (int2)(get_local_id(0), get_local_id(1));
  int x = gid.x;
  int y = gid.y;
  int2 size = get_image_dim(inputImage);

  float4 ui4 = (float4)read_imagef(inputImage, sampler, gid);
  int condition = (ui4.x > 0);

  write_imageui(outputImage, gid, condition * (y * size.x + x));
}

__kernel void initCCL3D(__read_only image3d_t inputImage,
                          __write_only image3d_t outputImage) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);
  int x = gid.x;
  int y = gid.y;
  int z = gid.z;
  int4 size = get_image_dim(inputImage);

  float4 ui4 = (float4)read_imagef(inputImage, sampler, gid);
  int condition = (ui4.x > 0);

  //Too large?
  write_imageui(outputImage, gid, condition * (z * (y * size.y) * ((size.x + x)*(size.x + x))));
}

__kernel void iterateCCL(__read_only image2d_t image,
                         __read_only image2d_t inputImage1,
                         __write_only image2d_t outImage1) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  int2 size = get_image_dim(inputImage1);

  uint4 base = read_imageui(image, sampler, gid);
  uint4 ui4a = read_imageui(inputImage1, sampler, gid);
  int2 t = (int2)(ui4a.x % size.x, ui4a.x / size.x);

  unsigned int m = ui4a.x;  

  if (base.x > 0) {
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        uint4 tmpa =
            read_imageui(inputImage1, sampler, (int2)(t.x + a, t.y + b));
        m = max(tmpa.x, m);
      }
  }

  write_imageui(outImage1, gid, (uint4)(m, 0, 0, 0));
}

__kernel void iterateCCL3D(__read_only image3d_t image,
                           __read_only image3d_t inputImage1,
                           __write_only image3d_t outImage1,
                           float guard) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);
  int x = gid.x;
  int y = gid.y;
  int z = gid.z;
  //int lock = 0;

  int4 size = get_image_dim(inputImage1);

  if(guard == 0)
    goto stop;

  guard = 1;
  uint4 base = read_imageui(image, sampler, gid);
  uint4 ui4a = read_imageui(inputImage1, sampler, gid);
  int4 t = (int4)(ui4a.x % size.x, ui4a.x / size.x, ui4a.y % size.y, ui4a.y / size.y);

  unsigned int m = ui4a.x;  

  if (base.x > 0) {
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        for (int c = -1; c <= 1; c++) {
          uint4 tmpa =
              read_imageui(inputImage1, sampler, (int4)(t.x + a, t.y + b, t.z + c, 0));
          m = max(tmpa.x, m);
        }
      }
  }

  stop: write_imageui(outImage1, gid, (uint4)(m, 0, 0, 0));
}

__kernel void reconnectCCL(__read_only image2d_t image,
                           __read_only image2d_t inputImage1,
                           __write_only image2d_t outImage1) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;
  int lock = 0;

  int2 size = get_image_dim(inputImage1);

  uint4 base = read_imageui(image, sampler, gid);
  uint4 ui4a = read_imageui(inputImage1, sampler, gid);
  int2 t = (int2)(ui4a.x % size.x, ui4a.x / size.x);

  uint4 tmpa = read_imageui(inputImage1, sampler, (int2)(t.x, t.y));
  unsigned int m = max(ui4a.x,tmpa.x);
  unsigned int n = ui4a.x;

  if (base.x > 0) {
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        
        //m = max(tmpa.x, m);
        uint4 tmpb = read_imageui(inputImage1, sampler, (int2)(x + a, y + b));
        n = max(tmpb.x, n);
      }
  }

  if(n > m) write_imageui(outImage1,t,(uint4)(n,0,0,0));
}

__kernel void reconnectCCL3D(__read_only image3d_t image,
                             __read_only image3d_t inputImage1,
                             __write_only image3d_t outImage1,
                             float guard) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);
  int x = gid.x;
  int y = gid.y;
  int z = gid.z;

  int4 size = get_image_dim(inputImage1);

  uint4 base = read_imageui(image, sampler, gid);
  uint4 ui4a = read_imageui(inputImage1, sampler, gid);
  int4 t = (int4)(ui4a.x % size.x, ui4a.x / size.x, ui4a.y % size.y, ui4a.y / size.y);

  uint4 tmpa = read_imageui(inputImage1, sampler, (int4)(t.x, t.y, t.z, 0));
  unsigned int m = max(ui4a.x,tmpa.x);
  unsigned int n = ui4a.x;

  if (base.x > 0) {
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        for (int c = -1; c <= 1; c++) {
          //m = max(tmpa.x, m);
          uint4 tmpb = read_imageui(inputImage1, sampler, (int4)(x + a, y + b, z + c, 0));
          n = max(tmpb.x, n);
        }
      }
  }

  if(n != base.x)
    guard = 1;

  if(n > m) write_imageui(outImage1,t,(n,0,0,0));
}