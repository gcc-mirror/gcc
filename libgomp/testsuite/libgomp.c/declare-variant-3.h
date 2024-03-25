#pragma omp declare target

__attribute__ ((noipa))
int
f30 (void)
{
  return 30;
}

__attribute__ ((noipa))
int
f35 (void)
{
  return 35;
}

__attribute__ ((noipa))
int
f53 (void)
{
  return 53;
}

__attribute__ ((noipa))
int
f70 (void)
{
  return 70;
}

__attribute__ ((noipa))
int
f75 (void)
{
  return 75;
}

__attribute__ ((noipa))
int
f80 (void)
{
  return 80;
}

#pragma omp declare variant (f30) match (device={isa("sm_30")})
#pragma omp declare variant (f35) match (device={isa("sm_35")})
#pragma omp declare variant (f53) match (device={isa("sm_53")})
#pragma omp declare variant (f70) match (device={isa("sm_70")})
#pragma omp declare variant (f75) match (device={isa("sm_75")})
#pragma omp declare variant (f80) match (device={isa("sm_80")})
__attribute__ ((noipa))
int
f (void)
{
  return 0;
}

#pragma omp end declare target

int
main (void)
{
  int v = 0;

  #pragma omp target map(from:v)
  v = f ();

#ifdef OFFLOAD_DEVICE_NVPTX
  if (v == 0)
    __builtin_abort ();

  __builtin_printf ("Nvptx accelerator: sm_%d\n", v);
#else
  if (v != 0)
    __builtin_abort ();
#endif

  return 0;
}
