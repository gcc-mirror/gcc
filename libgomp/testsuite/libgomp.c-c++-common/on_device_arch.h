#include <gomp-constants.h>

/* static */ int
device_arch_nvptx (void)
{
  return GOMP_DEVICE_NVIDIA_PTX;
}

#pragma omp declare variant (device_arch_nvptx) match(construct={target},device={arch(nvptx)})
/* static */ int
device_arch (void)
{
  return GOMP_DEVICE_DEFAULT;
}

static int
on_device_arch (int d)
{
  int d_cur;
  #pragma omp target map(from:d_cur)
  d_cur = device_arch ();

  return d_cur == d;
}

int
on_device_arch_nvptx ()
{
  return on_device_arch (GOMP_DEVICE_NVIDIA_PTX);
}
