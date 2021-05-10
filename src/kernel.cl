const sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;


__kernel void intensity(__read_only image2d_t inputImage,
                        __write_only image2d_t outImage, __global unsigned int *dim) {
  if(*dim == 2)
    int2 gid = (int2)(get_global_id(0), get_global_id(1));
  else
    int3 gid = (int3)(get_global_id(0), get_global_id(1), get_global_id(2));

  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newf4 = (float4)(f4.x * 0.2126, f4.y * 0.7152, f4.z * 0.0722, f4.w);

  write_imagef(outImage, gid, f4);
}

__kernel void getComponent(__read_only image2d_t inputImage,
                           __write_only image2d_t outputImage,
                           __global unsigned int *c, __global unsigned int *dim) {

  if(*dim == 2)
    int2 gid = (int2)(get_global_id(0), get_global_id(1));
  else
    int3 gid = (int3)(get_global_id(0), get_global_id(1), get_global_id(2));


  uint4 ui4 = read_imageui(inputImage, sampler, gid);
  int i = ((0 == *c) * ui4.x) + ((1 == *c) * ui4.y) + ((2 == *c) * ui4.z) +
          ((3 == *c) * ui4.w);

  write_imageui(outputImage, gid, i);
}

__kernel void rgbComps(__read_only image2d_t inputImage1,
                       __read_only image2d_t inputImage2,
                       __read_only image2d_t inputImage3,
                       __write_only image2d_t outImage,
                       __global unsigned int *dim) {
  if(*dim == 2)
    int2 gid = (int2)(get_global_id(0), get_global_id(1));
  else
    int3 gid = (int3)(get_global_id(0), get_global_id(1), get_global_id(2));

  uint4 pix1 = (uint4)read_imageui(inputImage1, gid);
  uint4 pix2 = (uint4)read_imageui(inputImage2, gid);
  uint4 pix3 = (uint4)read_imageui(inputImage3, gid);

  write_imageui(outImage, gid, (uint4)(pix1.x, pix2.y, pix3.z, 255));
}

__kernel void rgbaComps(__read_only image2d_t inputImage1,
                        __read_only image2d_t inputImage2,
                        __read_only image2d_t inputImage3,
                        __read_only image2d_t inputImage4,
                        __write_only image2d_t outImage,
                        __global unsigned int *dim) {
  if(*dim == 2)
    int2 gid = (int2)(get_global_id(0), get_global_id(1));
  else
    int3 gid = (int3)(get_global_id(0), get_global_id(1), get_global_id(2));

  uint4 pix1 = (uint4)read_imageui(inputImage1, gid);
  uint4 pix2 = (uint4)read_imageui(inputImage2, gid);
  uint4 pix3 = (uint4)read_imageui(inputImage3, gid);
  uint4 pix4 = (uint4)read_imageui(inputImage4, gid);

  write_imageui(outImage, gid, (uint4)(pix1.x, pix2.y, pix3.z, pix4.w));
}

__kernel void trueImg(__write_only image2d_t outputImage,
                      __global unsigned int *dim) {
  if(*dim == 2)
    int2 gid = (int2)(get_global_id(0), get_global_id(1));
  else
    int3 gid = (int3)(get_global_id(0), get_global_id(1), get_global_id(2));

  write_imageui(outputImage, gid, 1);
}

__kernel void falseImg(__write_only image2d_t outputImage,
                       __global unsigned int *dim) {
  if(*dim == 2)
    int2 gid = (int2)(get_global_id(0), get_global_id(1));
  else
    int3 gid = (int3)(get_global_id(0), get_global_id(1), get_global_id(2));

  write_imageui(outputImage, gid, 0);
}

/******************** TEST KERNELS ********************/

__kernel void swapRG(__read_only image2d_t inputImage, __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newf4 = (float4)(f4.y, f4.x, f4.z, f4.w);

  write_imagef(outImage, gid, newf4); 
}

__kernel void test(__write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 newf4 = (float4)(255, 0, 0, 255);
  write_imagef(outImage,gid,newf4);
}


__kernel void slow(__read_only image2d_t inputImage, __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));
  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);  
  float4 newf4 = (float4)(f4.x, f4.y, f4.z, f4.w);

  for(int i = 0;i < 1000; i++) {
    for (int j = 0; j < 3; j++)
    newf4[j] = fmod(newf4[j] * newf4[j],251.0f);
  } 

  write_imagef(outImage, gid, newf4); 
}