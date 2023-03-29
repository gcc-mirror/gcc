#include <omp.h>
#include <gomp-constants.h>

/* static */ int
device_arch_nvptx (void)
{
  return GOMP_DEVICE_NVIDIA_PTX;
}

/* static */ int
device_arch_gcn (void)
{
  return GOMP_DEVICE_GCN;
}

#pragma omp declare variant (device_arch_nvptx) match(construct={target},device={arch(nvptx)})
#pragma omp declare variant (device_arch_gcn) match(construct={target},device={arch(gcn)})
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

int
on_device_arch_gcn ()
{
  return on_device_arch (GOMP_DEVICE_GCN);
}
