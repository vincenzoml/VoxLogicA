const sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;


__kernel void intensity(__read_only image2d_t inputImage,
                        __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newf4 = (float4)(f4.x * 0.2126, f4.y * 0.7152, f4.z * 0.0722, f4.w);

  write_imagef(outImage, gid, f4);
}

__kernel void swapRG(__read_only image2d_t inputImage, __write_only image2d_t outImage) {
  int2 gid = (int2)(get_global_id(0), get_global_id(1));

  float4 f4 = (float4)read_imagef(inputImage, sampler, gid);
  float4 newf4 = (float4)(f4.y, f4.x, f4.z, f4.w);

  write_imagef(outImage, gid, newf4); 
}
