#include <stdint.h>

double data [12];
#pragma omp requires self_maps

#pragma omp declare target enter(data)

void * __attribute__((noinline))
get_addr_from_function (void)
{
  return (void *) &data;
}

int main (void)
{
  void *devaddr;
  #pragma omp target map(from: devaddr)
  {
    devaddr = (void *) data;
  }

  if (devaddr != (void *) data)
    __builtin_abort ();

  #pragma omp target map(from: devaddr)
  {
    devaddr = get_addr_from_function ();
  }

  if (devaddr != (void *) data)
    __builtin_abort ();


  return 0;
}
