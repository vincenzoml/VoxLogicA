#pragma OPENCL EXTENSION cl_khr_3d_image_writes : enable

const sampler_t sampler =
    CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

#define DIM 3
#if DIM == 3

#define IMG_T image3d_t
#define GID_T int4
#define INIT_GID(gid)                                                          \
  GID_T gid;                                                                   \
  gid = (GID_T)(get_global_id(0), get_global_id(1), get_global_id(2), 0);

#else
#define IMG_T image2d_t
#define GID_T int2
#define INIT_GID(gid)                                                          \
  GID_T gid;                                                                   \
  gid = (GID_T)(get_global_id(0), get_global_id(1));
#endif

__kernel void intensity(__read_only IMG_T inputImage,
                        __write_only IMG_T outImage) {
  INIT_GID(gid)

  //float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  //float4 newf4 =
  //    (float4)(f4.x * 0.2126 + f4.y * 0.7152 + f4.z * 0.0722, 0, 0, 0);
//
  //write_imagef(outImage, gid, newf4);
  float4 f4 = (float4)read_imagef(inputImage, sampler, (int4)(-1, -1, -1, 0));
  write_imagef(outImage, gid, (float4)(f4.x, f4.y, f4.z, f4.w*65535));
}

__kernel void getComponent(__read_only IMG_T inputImage,
                           __write_only IMG_T outputImage, float c) {

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

  write_imagef(outImage, gid, (float4)(pix1.x, pix2.y, pix3.z, 255)); // TODO: why 255? What if the image is 16bit? Float32? This primitive needs a value for alpha
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

__kernel void border(__write_only image2d_t outputImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 dim = (int2)(get_global_size(0), get_global_size(1));

  int condition = (gid.x == 0 || gid.x == dim.x - 1) || (gid.y == 0 || gid.y == dim.y - 1);

  write_imageui(outputImage, gid, condition);
}

__kernel void border3D(__write_only image3d_t outputImage) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);
  int4 dim = (int4)(get_global_size(0), get_global_size(1), get_global_size(2), 0);

  int condition = (gid.x == 0 || gid.x == dim.x - 1) || (gid.y == 0 || gid.y == dim.y - 1)
                  || (gid.z == 0 || gid.z == dim.z -1);

  write_imageui(outputImage, gid, condition);
}

__kernel void dilate(__read_only image2d_t inputImage,
                     __write_only image2d_t outputImage) { //NEAR
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

__kernel void dilate3D(__read_only image3d_t inputImage,
                      __write_only image3d_t outputImage) { //NEAR
  int x = get_global_id(0);
  int y = get_global_id(1);
  int z = get_global_id(2);
  int4 coord = (int4)(x, y, z, 0);

  int found = 0;
  int4 newcoord;
  uint4 ui4;
  for (int a = -1; a <= 1; a++) {
    for (int b = -1; b <= 1; b++) {
      for (int c = -1; c <= 1; c++) {
        newcoord = (int4)(x + a, y + b, z + c, 0);
        ui4 = read_imageui(inputImage, sampler, newcoord);
        found = found + (ui4.x > 0);
      }
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

__kernel void erode3D(__read_only image3d_t inputImage,
                     __write_only image3d_t outputImage) {
  int x = get_global_id(0);
  int y = get_global_id(1);
  int z = get_global_id(2);
  int4 coord = (int4)(x, y, z, 0);

  int found = 0;
  int4 newcoord;
  uint4 ui4;
  for (int a = -1; a <= 1; a++) {
    for (int b = -1; b <= 1; b++) {
      for (int c = -1; c <= 1; c++) {
        newcoord = (int4)(x + a, y + b, z + c, 0);
        ui4 = read_imageui(inputImage, sampler, newcoord);
        found = found || (ui4.x == 0);
      }
    }
  }

  write_imageui(outputImage, coord, !found);
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

  write_imageui(outImage, gid, ((value1.x > 0) && (value2.x > 0)));
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

__kernel void copy4fto1f2D(__read_only image2d_t input,
                           __write_only image2d_t output) {
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const float4 v = read_imagef(input, sampler, gid);

  write_imagef(output, gid, v.x);
}

__kernel void leq(__read_only IMG_T inputImage, __write_only IMG_T outImage,
                  float value) {

  INIT_GID(gid)

  const float4 f4 = read_imagef(inputImage, sampler, gid);

  unsigned int condition = (unsigned int)(value <= f4.x);
  write_imageui(outImage, gid, condition);
}

__kernel void eq(__read_only IMG_T image, __write_only IMG_T outImage,
                 float value) {
  INIT_GID(gid)

  float4 ui4 = (float4)read_imagef(image, sampler, gid);

  int condition = (ui4.x == value);

  write_imageui(outImage, gid, condition);
}

__kernel void between(__read_only IMG_T image, __write_only IMG_T outImage,
                      float value1, float value2) {
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

__kernel void add(__read_only IMG_T inputImage1, __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 + pix2)); // what about oerflows?
}

__kernel void addVS(__read_only IMG_T inputImage1, __write_only IMG_T outImage,
                    float val) {
  INIT_GID(gid)

  float4 pix1 = read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix1 + val));
}

__kernel void sub(__read_only IMG_T inputImage1, __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 - pix2));
}

__kernel void subSV(__read_only IMG_T inputImage1, __write_only IMG_T outImage,
                    float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (val - pix));
}

__kernel void subVS(__read_only IMG_T inputImage1, __write_only IMG_T outImage,
                    float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix - val));
}

__kernel void mul(__read_only IMG_T inputImage1, __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 * pix2));
}

__kernel void mulVS(__read_only IMG_T inputImage, __write_only IMG_T outImage,
                    float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage, gid);

  write_imagef(outImage, gid, (pix * val));
}

__kernel void div(__read_only IMG_T inputImage1, __read_only IMG_T inputImage2,
                  __write_only IMG_T outImage) {
  INIT_GID(gid)

  float4 pix1 = (float4)read_imagef(inputImage1, gid);
  float4 pix2 = (float4)read_imagef(inputImage2, gid);

  write_imagef(outImage, gid, (pix1 / pix2));
}

__kernel void divVS(__read_only IMG_T inputImage1, __write_only IMG_T outImage,
                    float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (pix / val));
}

__kernel void divSV(__read_only IMG_T inputImage1, __write_only IMG_T outImage,
                    float val) {
  INIT_GID(gid)

  float4 pix = (float4)read_imagef(inputImage1, gid);

  write_imagef(outImage, gid, (val / pix));
}

__kernel void mask(__read_only IMG_T inputImage1, __read_only IMG_T inputImage2,
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

  uint4 ui4 = read_imageui(inputImage, sampler, gid);
  write_imagef(outputImage, gid,
               (float4)(ui4.x * x, ui4.x * y, 0, ui4.x));
}

__kernel void initCCL3D(__read_only image3d_t inputImage,
                        __write_only image3d_t outputImage) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);
  int x = gid.x;
  int y = gid.y;
  int z = gid.z;
  int4 size = get_image_dim(inputImage);

  uint4 ui4 = read_imageui(inputImage, sampler, gid);
  write_imagef(outputImage, gid, (float4)(ui4.x*x, ui4.x*y, ui4.x*z, ui4.x));
}

__kernel void iterateCCL(__read_only image2d_t inputImage1,
                         __write_only image2d_t outImage1) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 input1 = read_imagef(inputImage1, sampler, gid);

  float currentx = input1.x;
  float currenty = input1.y;
  float orig = input1.w; // original image (see the initialization kernel)

  float4 parent = read_imagef(inputImage1, sampler,
                              (int2)(currentx, currenty)); // pointer jumping
  float labelx = parent.x;
  float labely = parent.y;
  float4 tmpa;
  float maxx = labelx;
  float maxy = labely;
  for (int a = -1; a <= 1; a++) {
    for (int b = -1; b <= 1; b++) {
      tmpa = read_imagef(inputImage1, sampler, (int2)(labelx + a, labely + b));
      unsigned int condition =
          ((tmpa.x > maxx) || ((tmpa.x == maxx) && (tmpa.y > maxy))) &&
          (tmpa.w > 0) && (orig > 0);
      maxx = (condition * tmpa.x) + ((!condition) * maxx);
      maxy = (condition * tmpa.y) + ((!condition) * maxy);
    }
  }

  write_imagef(outImage1, gid, (float4)(maxx, maxy, 0, orig));
  // TODO: optimize by assuming input and output are copies of each other
  // (requires change in initialization?)
}

__kernel void iterateCCL3D( //__read_only image3d_t image,
    __read_only image3d_t inputImage1, __write_only image3d_t outImage1) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);

  float4 input1 = read_imagef(inputImage1, sampler, gid);

  float currentx = input1.x;
  float currenty = input1.y;
  float currentz = input1.z;
  float orig =
      input1.w; // original boolean image (see the initialization kernel)

  float4 parent =
      read_imagef(inputImage1, sampler,
                  (int4)(currentx, currenty, currentz, 0)); // pointer jumping
  float labelx = parent.x;
  float labely = parent.y;
  float labelz = parent.z;
  float4 tmpa;
  float maxx = labelx;
  float maxy = labely;
  float maxz = labelz;

  for (int a = -1; a <= 1; a++) {
    for (int b = -1; b <= 1; b++) {
      for (int c = -1; c <= 1; c++) {
        tmpa = read_imagef(inputImage1, sampler, (int4)(labelx + a, labely + b, labelz + c, 0));
        unsigned int conditiona =
            ((tmpa.x > maxx) || ((tmpa.x == maxx) && (tmpa.y > maxy)) || ((tmpa.x == maxx) && (tmpa.y == maxy) 
            && (tmpa.z > maxz))) &&
            (tmpa.w > 0) && (orig > 0);      
        maxx = (conditiona * tmpa.x) + ((!conditiona) * maxx);
        maxy = (conditiona * tmpa.y) + ((!conditiona) * maxy);
        maxz = (conditiona * tmpa.z) + ((!conditiona) * maxz);
      }
    }
  }
  write_imagef(outImage1, gid, (float4)(maxx, maxy, maxz, orig));
}

__kernel void resetFlag(__global char flag[1]) { flag[0] = 0; }

__kernel void reconnectCCL(__read_only image2d_t inputImage1,
                           __write_only image2d_t outImage1,
                           __global char flag[1]) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  float4 input1 = read_imagef(inputImage1, sampler, gid);

  float currentx = input1.x;
  float currenty = input1.y;
  float orig =
      input1.w; // original boolean image (see the initialization kernel)

  float4 max = (float4)(currentx, currenty, 0.0, orig);

  unsigned int toFlag = 0;

  if (orig > 0) {
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        float4 tmpb = read_imagef(inputImage1, sampler, (int2)(x + a, y + b));
        unsigned int tmpcondition =
            ((tmpb.x > max.x) || (tmpb.x == max.x && tmpb.y > max.y)) &&
            (tmpb.w > 0);
        max = (float4)(tmpcondition * tmpb.x + (!tmpcondition * max.x),
                       tmpcondition * tmpb.y + (!tmpcondition * max.y), 0.0,
                       orig);
        toFlag = toFlag || tmpcondition;
      }
  }

  if (toFlag) {
    flag[0] = 1;
    write_imagef(outImage1, (int2)(currentx, currenty), max);
  }  
}

__kernel void reconnectCCL3D(__read_only image3d_t inputImage1,
                           __write_only image3d_t outImage1,
                           __global char flag[1]) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1),get_global_id(2), 0);
  int x = gid.x;
  int y = gid.y;
  int z = gid.z;

  float4 input1 = read_imagef(inputImage1, sampler, gid);

  float currentx = input1.x;
  float currenty = input1.y;
  float currentz = input1.z;
  float orig =
      input1.w; // original boolean image (see the initialization kernel)

  float4 max = (float4)(currentx, currenty, currentz, orig);

  unsigned int toFlag = 0;

  if (orig > 0) {
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        for (int c = -1; c <= 1; c++) {
          float4 tmpb = read_imagef(inputImage1, sampler, (int4)(x + a, y + b, z + c, 0));
          unsigned int tmpcondition =
              ((tmpb.x > max.x) || (tmpb.x == max.x && tmpb.y > max.y) || 
              (tmpb.x == max.x && tmpb.y == max.y && tmpb.z > max.z)) &&
              (tmpb.w > 0);
          max = (float4)(tmpcondition * tmpb.x + (!tmpcondition * max.x),
                         tmpcondition * tmpb.y + (!tmpcondition * max.y), 
                         tmpcondition * tmpb.z + (!tmpcondition * max.z),
                         orig);
          toFlag = toFlag || tmpcondition;
        }
      }
  }

  if (toFlag) {
    flag[0] = 1;
    write_imagef(outImage1, (int4)(currentx, currenty, currentz, 0), max);
  }  
}

/********************* THROUGH *********************/

//kernel 1: prende output di LCC, img e immagine temporanea (output) 
//e scrive in [x, y, (z)] coord della componente connessa
// il valore di phi1 (img in Through)
__kernel void initThrough(__read_only image2d_t inputImage1, //primo parametro della primitiva Through
                          __read_only image2d_t inputImage2, //output di LCC(img2)
                          __write_only image2d_t tempOutput) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  float4 input1 = read_imagef(inputImage1, sampler, gid);
  float4 input2 = read_imagef(inputImage2, sampler, gid);

  if(input1.x > 0)
    write_imageui(tempOutput, (int2)(input2.x, input2.y), 1);
}

__kernel void initThrough3D(__read_only image3d_t inputImage1, //primo parametro della primitiva Through
                            __read_only image3d_t inputImage2, //output di LCC(img2)
                            __write_only image3d_t tempOutput) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);

  uint4 input1 = read_imageui(inputImage1, sampler, gid);
  float4 input2 = read_imagef(inputImage2, sampler, gid);

  if(input1.x > 0)
    write_imageui(tempOutput, (int4)(input2.x, input2.y, input2.z, 0), 1);
}

//kernel 2: prende output di LCC e immagine temporanea e scrive 
//in ogni pixel dove phi2 è vera il valore di tmp all'indice dato
//dalla label di output LCC in quel pixel
__kernel void finalizeThrough(__read_only image2d_t inputImage1, //immagine temporanea
                              __read_only image2d_t inputImage2, //output di LCC(img2)
                              __write_only image2d_t outputImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  float4 input2 = read_imagef(inputImage2, sampler, gid);
  uint4 input1 = read_imageui(inputImage1, sampler, (int2)(input2.x, input2.y));

  //l'output è una immagine booleana

  write_imagef(outputImage, gid, input1.x > 0);
}

__kernel void finalizeThrough3D(__read_only image3d_t inputImage1, //immagine temporanea
                                __read_only image3d_t inputImage2, //output di LCC(img2)
                                __write_only image3d_t tempOutput) {
  int4 gid = (int4)(get_global_id(0), get_global_id(1), get_global_id(2), 0);

  float4 input2 = read_imagef(inputImage2, sampler, gid);
  uint4 input1 = read_imageui(inputImage1, sampler, (int4)(input2.x, input2.y, input2.z, 0));

  write_imageui(tempOutput, gid, input1.x > 0);
}

/********************* WIP *********************/

__kernel void iterateCCLNew(__read_only image2d_t inputImage1,
                         __write_only image2d_t outImage1) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  float4 input1 = read_imagef(inputImage1, sampler, gid);

  float currentx = input1.x;
  float currenty = input1.y;
  float orig = input1.w; // original image (see the initialization kernel)

  float4 parent = read_imagef(inputImage1, sampler,
                              (int2)(currentx, currenty)); // pointer jumping
  float labelx = parent.x;
  float labely = parent.y;
  float4 tmpa;
  float4 tmpb;
  float maxx = labelx;
  float maxy = labely;
  for (int a = -1; a <= 1; a++) {
    for (int b = -1; b <= 1; b++) {
      tmpa = read_imagef(inputImage1, sampler, (int2)(labelx + a, labely + b));
      unsigned int conditiona =
          ((tmpa.x > maxx) || ((tmpa.x == maxx) && (tmpa.y > maxy))) &&
          (tmpa.w > 0) && (orig > 0);      
      maxx = (conditiona * tmpa.x) + ((!conditiona) * maxx);
      maxy = (conditiona * tmpa.y) + ((!conditiona) * maxy);
    }
  }
  // for (int a = -1; a <= 1; a++) {
  //   for (int b = -1; b <= 1; b++) {
  //     tmpa = read_imagef(inputImage1, sampler, (int2)(labelx + a, labely + b));
  //     tmpb = read_imagef(inputImage1, sampler, (int2)(x + a,y+b));
  //     unsigned int conditionb =
  //         ((tmpb.x > maxx) || ((tmpb.x == maxx) && (tmpb.y > maxy))) &&
  //         (tmpb.w > 0) && (orig > 0);
  //     maxx = conditionb * tmpb.x + !conditionb * maxx;
  //     maxy = conditionb * tmpb.y + !conditionb * maxy;   
  //   }
  // }

  write_imagef(outImage1, gid, (float4)(maxx, maxy, 0, orig));
  // TODO: optimize by assuming input and output are copies of each other
  // (requires change in initialization?)
}
