#pragma omp declare target
int
gfx803 (void)
{
  return 0x803;
}

int
gfx900 (void)
{
  return 0x900;
}

int
gfx906 (void)
{
  return 0x906;
}

int
gfx908 (void)
{
  return 0x908;
}

int
gfx90a (void)
{
  return 0x90a;
}

#ifdef USE_FIJI_FOR_GFX803
#pragma omp declare variant(gfx803) match(device = {isa("fiji")})
#else
#pragma omp declare variant(gfx803) match(device = {isa("gfx803")})
#endif
#pragma omp declare variant(gfx900) match(device = {isa("gfx900")})
#pragma omp declare variant(gfx906) match(device = {isa("gfx906")})
#pragma omp declare variant(gfx908) match(device = {isa("gfx908")})
#pragma omp declare variant(gfx90a) match(device = {isa("gfx90a")})
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

#pragma omp target map(from : v)
  v = f ();

  if (v == 0)
    __builtin_abort ();

  __builtin_printf ("AMDGCN accelerator: gfx%x\n", v);

  return 0;
}
