#pragma omp declare target
int
f30 (void)
{
  return 30;
}

int
f35 (void)
{
  return 35;
}

int
f53 (void)
{
  return 53;
}

int
f70 (void)
{
  return 70;
}

int
f75 (void)
{
  return 75;
}

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

  if (v == 0)
    __builtin_abort ();

  __builtin_printf ("Nvptx accelerator: sm_%d\n", v);

  return 0;
}
