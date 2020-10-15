
// #define TILE_DIM 32

__kernel void getComponent(__read_only image2d_t inputImage,
                           __write_only image2d_t outputImage,
                           __global unsigned int *c) {

  int x = get_global_id(0);
  int y = get_global_id(1);

  int2 coord = (int2)(x, y);

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP_TO_EDGE |
      CLK_FILTER_NEAREST; // SET THE CLAMPING HERE / see
                          // https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/sampler_t.html

  uint4 ui4 = read_imageui(inputImage, sampler, coord);
  int i = ((0 == *c) * ui4.x) + ((1 == *c) * ui4.y) + ((2 == *c) * ui4.z) +
          ((3 == *c) * ui4.w);

  write_imageui(outputImage, coord, i);
}

__kernel void intensity(__read_only image2d_t inputImage,
                        __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = (uint4)read_imageui(inputImage, sampler, gid);
  uint4 newui4 = (uint4)(ui4.x * 0.2126, ui4.y * 0.7152, ui4.z * 0.0722, ui4.w);

  write_imageui(outImage, gid, newui4);
}

__kernel void copyImg(__read_only image2d_t inputImage,
                      __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = (uint4)read_imageui(inputImage, sampler, gid);

  write_imageui(outImage, gid, ui4);
}

__kernel void rgbComps(__read_only image2d_t inputImage1,
                       __read_only image2d_t inputImage2,
                       __read_only image2d_t inputImage3,
                       __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

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
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

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

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP |
      CLK_FILTER_NEAREST; // SET THE CLAMPING HERE / see
                          // https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/sampler_t.htm

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

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP |
      CLK_FILTER_NEAREST; // SET THE CLAMPING HERE / see
                          // https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/sampler_t.htm

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

__kernel void initCCL(__read_only image2d_t inputImage,
                          __write_only image2d_t outputImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  // int2 lid = (int2)(get_local_id(0), get_local_id(1));
  int x = gid.x;
  int y = gid.y;
  int2 size = get_image_dim(inputImage);
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(inputImage, sampler, gid);
  int condition = (ui4.x > 0);

  write_imageui(outputImage, gid, condition * (y * size.x + x));
}

__kernel void iterateCCL(/* base image */ __read_only image2d_t image,
                               /* uint32 */ __read_only image2d_t inputImage1,
                               __write_only image2d_t outImage1) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;
  int lock = 0;

  int2 size = get_image_dim(inputImage1);
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

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

__kernel void reconnectCCL(/* base image */ __read_only image2d_t image,
                               /* uint32 */ __read_only image2d_t inputImage1,
                               __write_only image2d_t outImage1) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;
  int lock = 0;

  int2 size = get_image_dim(inputImage1);
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

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

__kernel void preTermination( __read_only image2d_t image, __read_only image2d_t inputImage1, __write_only image2d_t outImage1) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;
  int lock = 0;

  int2 size = get_image_dim(inputImage1);
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 base = read_imageui(image, sampler, gid);
  uint4 ui4a = read_imageui(inputImage1, sampler, gid);

  unsigned int m = ui4a.x;  

  if (base.x > 0) {
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        uint4 tmpa =
            read_imageui(inputImage1, sampler, (int2)(x + a, y + b));
        m = max(tmpa.x, m);
      }
  }

  write_imageui(outImage1, gid, (uint4)(m, 0, 0, 0));
}

__kernel void termination(__read_only image2d_t inputImage1,
                          __read_only image2d_t inputImage2,
                          __global unsigned int *results,
                          __local unsigned int *tile) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 lid = (int2)(get_local_id(0), get_local_id(1));
  int2 gr_size = (int2)(get_local_size(0), get_local_size(1));
  int x = gid.x;
  int y = gid.y;

  int2 size = get_image_dim(inputImage1);
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 base = read_imageui(inputImage1, sampler, gid);
  uint4 comp = read_imageui(inputImage2, sampler, gid);
  tile[lid.x + lid.y * gr_size.x] = (base.x == comp.x);
  results[get_group_id(0) + get_group_id(1) * get_num_groups(0)] = 0;

  // Reduce rows
  for (uint stride = gr_size.x / 2 - 1; stride > 0; stride /= 2) {
    barrier(CLK_LOCAL_MEM_FENCE);
    if (lid.y < stride) {
      tile[lid.x + lid.y * gr_size.x] *=
          tile[(lid.x) + (lid.y + stride) * gr_size.x];
    }
  }

  // Reduce first column
  if (lid.x == 0 && lid.y == 0) {
    for (uint i = 0; i < gr_size.y; i++) {
      tile[lid.x + lid.y * gr_size.x] *= tile[i];
    }
    results[get_group_id(0) + get_group_id(1) * get_num_groups(0)] = tile[0];
  }
}

__kernel void mask(__read_only image2d_t inputImage1,
                   __read_only image2d_t inputImage2,
                   __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix = read_imageui(inputImage1, sampler, gid);
  uint4 mask = read_imageui(inputImage2, sampler, gid);

  write_imageui(outImage, gid, pix.x * (mask.x > 0));
}

__kernel void maskT(__read_only image2d_t inputImage1,
                    __read_only image2d_t inputImage2,
                    __write_only image2d_t outImage, __global int *colors) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix = read_imageui(inputImage1, sampler, gid);
  uint4 mask = read_imageui(inputImage2, sampler, gid);

  write_imageui(outImage, gid, pix.x * (mask.x > 0));

  if ((pix.x * mask.x) > 0)
    colors[pix.x] = 1;
}

__kernel void through(__read_only image2d_t inputImage1,
                      __read_only image2d_t inputImage2,
                      __write_only image2d_t outImage, __global int *colors) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, sampler, gid);
  uint4 pix2 = read_imageui(inputImage2, sampler, gid);

  write_imageui(outImage, gid, colors[pix2.x]);
}

__kernel void nick_thresholding(__read_only image2d_t image,
                                __write_only image2d_t outImage,
                                __local unsigned int *tile) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 lid = (int2)(get_local_id(0), get_local_id(1));
  int x = gid.x;
  int y = gid.y;

  int2 size = get_image_dim(image);

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(image, sampler, gid);
  tile[lid.x + lid.y * size.x] = ui4.x;

  int condition = (ui4.x == 0) && (ui4.y == 0) && (ui4.z == 0);
  int threshold = 255;

  if (!condition) {
    uint sum = 0;
    uint square_sum = 0;
    uint tmp = 0;
    for (int a = -1; a <= 1; a++)
      for (int b = -1; b <= 1; b++) {
        tmp = read_imageui(image, sampler, (int2)(x + a, y + b)).x;
        sum += tmp;
        square_sum += tmp * tmp;
      }
    uint mean = sum / 9;
    threshold = mean + (-0.2) * sqrt((float)(square_sum - mean * mean) / 9);
  }

  write_imageui(outImage, gid,
                !condition && threshold < tile[lid.x + lid.y * size.x]);
}

__kernel void global_thresholding(__read_only image2d_t image,
                                  __write_only image2d_t outImage,
                                  __constant unsigned int *threshold) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  int thresh = *threshold;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(image, sampler, gid);

  int condition = (ui4.x > thresh) && (ui4.y > thresh) && (ui4.z > thresh);

  write_imageui(outImage, gid, condition);
}

__kernel void geq(__read_only image2d_t image, __write_only image2d_t outImage,
                  __constant int *value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(image, sampler, gid);

  int condition = (ui4.x > *value);

  write_imageui(outImage, gid, condition);
}

__kernel void leq(__read_only image2d_t inputImage,
                  __write_only image2d_t outImage, __constant int *value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(inputImage, sampler, gid);

  int condition = (ui4.x < *value);

  write_imageui(outImage, gid, condition);
}

__kernel void eq(__read_only image2d_t image, __write_only image2d_t outImage,
                 __constant int *value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 lid = (int2)(get_local_id(0), get_local_id(1));
  int x = gid.x;
  int y = gid.y;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(image, sampler, gid);

  int condition = (ui4.x == *value);

  write_imageui(outImage, gid, condition);
}

__kernel void between(__read_only image2d_t image,
                      __write_only image2d_t outImage, __constant int *value1,
                      __constant int *value2) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 lid = (int2)(get_local_id(0), get_local_id(1));
  int x = gid.x;
  int y = gid.y;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(image, sampler, gid);

  int condition = (ui4.x >= *value1) && (ui4.x <= *value2);

  write_imageui(outImage, gid, condition);
}

__kernel void absImg(__read_only image2d_t inputImage,
                     __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(inputImage, sampler, gid);
  uint4 newui4 = (uint4)(abs(ui4.x), abs(ui4.y), abs(ui4.z), abs(ui4.w));

  write_imageui(outImage, gid, newui4);
}

__kernel void constImg(__write_only image2d_t outImage, __constant int *value) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  write_imageui(outImage, gid, *value);
}

__kernel void trueImg(__write_only image2d_t outputImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  write_imageui(outputImage, gid, 1);
}

__kernel void falseImg(__write_only image2d_t outputImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  write_imageui(outputImage, gid, 0);
}

__kernel void not(__read_only image2d_t image,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 ui4 = read_imageui(image, gid);

  int condition = !(ui4.x > 0);

  write_imageui(outImage, gid, condition);
}

__kernel void and
    (__read_only image2d_t inputImage1, __read_only image2d_t inputImage2,
     __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, gid);
  uint4 pix2 = read_imageui(inputImage2, gid);

  int cond1 = (pix1.x > 0);
  int cond2 = (pix2.x > 0);

  write_imageui(outImage, gid, (cond1 && cond2));
}

__kernel void or
    (__read_only image2d_t inputImage1, __read_only image2d_t inputImage2,
     __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int x = gid.x;
  int y = gid.y;

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, gid);
  uint4 pix2 = read_imageui(inputImage2, gid);

  int cond1 = (pix1.x > 0);
  int cond2 = (pix2.x > 0);

  write_imageui(outImage, gid, (cond1 || cond2));
}

__kernel void add(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, gid);
  uint4 pix2 = read_imageui(inputImage2, gid);

  write_imageui(outImage, gid, (pix1 + pix2) % 256);
}

__kernel void addVS(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant int *val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, gid);

  write_imageui(outImage, gid, (pix1 + *val) % 256);
}

__kernel void sub(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, gid);
  uint4 pix2 = read_imageui(inputImage2, gid);

  write_imageui(outImage, gid, (pix1 - pix2) % 256);
}

__kernel void subSV(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant int *val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix = read_imageui(inputImage1, gid);

  write_imageui(outImage, gid, (*val - pix) % 256);
}

__kernel void subVS(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant int *val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix = read_imageui(inputImage1, gid);

  write_imageui(outImage, gid, (pix - *val) % 256);
}

__kernel void mul(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, gid);
  uint4 pix2 = read_imageui(inputImage2, gid);

  write_imageui(outImage, gid, (pix1 * pix2) % 256);
}

__kernel void mulVS(__read_only image2d_t inputImage,
                    __write_only image2d_t outImage, __constant int *val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix = read_imageui(inputImage, gid);

  write_imageui(outImage, gid, (pix * (*val)) % 256);
}

__kernel void div(__read_only image2d_t inputImage1,
                  __read_only image2d_t inputImage2,
                  __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix1 = read_imageui(inputImage1, gid);
  uint4 pix2 = read_imageui(inputImage2, gid);

  write_imageui(outImage, gid, (pix1 / pix2) % 256);
}

__kernel void divVS(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant int *val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix = read_imageui(inputImage1, gid);

  write_imageui(outImage, gid, (pix / (*val)) % 256);
}

__kernel void divSV(__read_only image2d_t inputImage1,
                    __write_only image2d_t outImage, __constant int *val) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 pix = read_imageui(inputImage1, gid);

  write_imageui(outImage, gid, ((*val) / pix) % 256);
}

__kernel void volume(__read_only image2d_t inputImage, __global float *results,
                     __local float *tile) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  int2 lid = (int2)(get_local_id(0), get_local_id(1));
  int2 gr_size = (int2)(get_local_size(0), get_local_size(1));

  int2 size = get_image_dim(inputImage);
  const sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;

  uint4 base = read_imageui(inputImage, sampler, gid);
  tile[lid.x + lid.y * gr_size.x] = base.x;

  for (uint stride = gr_size.x / 2 - 1; stride > 0; stride /= 2) {
    barrier(CLK_LOCAL_MEM_FENCE);
    if (lid.y < stride) {
      tile[lid.x + lid.y * gr_size.x] +=
          tile[(lid.x) + (lid.y + stride) * gr_size.x];
    }
  }

  if (lid.x == 0 && lid.y == 0) {
    for (uint i = 0; i < gr_size.y; i++) {
      tile[0] += tile[i];
    }
    results[get_group_id(0) + get_group_id(1) * get_num_groups(0)] = tile[0];
  }
}
