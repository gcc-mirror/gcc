#include <omp.h>
#include <gomp-constants.h>

/* static */ int
device_arch_nvptx (void)
{
  return GOMP_DEVICE_NVIDIA_PTX;
}

/* static */ int
device_arch_intel_mic (void)
{
  return GOMP_DEVICE_INTEL_MIC;
}

#pragma omp declare variant (device_arch_nvptx) match(construct={target},device={arch(nvptx)})
#pragma omp declare variant (device_arch_intel_mic) match(construct={target},device={arch(intel_mic)})
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
on_device_arch_intel_mic ()
{
  return on_device_arch (GOMP_DEVICE_INTEL_MIC);
}

static int
any_device_arch (int d)
{
  int nd = omp_get_num_devices ();
  for (int i = 0; i < nd; ++i)
    {
      int d_cur;
      #pragma omp target device(i) map(from:d_cur)
      d_cur = device_arch ();
      if (d_cur == d)
	return 1;
    }

  return 0;
}

int
any_device_arch_intel_mic ()
{
  return any_device_arch (GOMP_DEVICE_INTEL_MIC);
}
