#pragma omp declare target

__attribute__ ((noipa))
int
gfx803 (void)
{
  return 0x803;
}

__attribute__ ((noipa))
int
gfx900 (void)
{
  return 0x900;
}

__attribute__ ((noipa))
int
gfx906 (void)
{
  return 0x906;
}

__attribute__ ((noipa))
int
gfx908 (void)
{
  return 0x908;
}

__attribute__ ((noipa))
int
gfx90a (void)
{
  return 0x90a;
}

__attribute__ ((noipa))
int
gfx90c (void)
{
  return 0x90c;
}

__attribute__ ((noipa))
int
gfx1030 (void)
{
  return 0x1030;
}

__attribute__ ((noipa))
int
gfx1036 (void)
{
  return 0x1036;
}

__attribute__ ((noipa))
int
gfx1100 (void)
{
  return 0x1100;
}

__attribute__ ((noipa))
int
gfx1103 (void)
{
  return 0x1103;
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
#pragma omp declare variant(gfx90c) match(device = {isa("gfx90c")})
#pragma omp declare variant(gfx1030) match(device = {isa("gfx1030")})
#pragma omp declare variant(gfx1036) match(device = {isa("gfx1036")})
#pragma omp declare variant(gfx1100) match(device = {isa("gfx1100")})
#pragma omp declare variant(gfx1103) match(device = {isa("gfx1103")})
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

#pragma omp target map(from : v)
  v = f ();

#ifdef OFFLOAD_DEVICE_GCN
  if (v == 0)
    __builtin_abort ();

  __builtin_printf ("AMDGCN accelerator: gfx%x\n", v);
#else
  if (v != 0)
    __builtin_abort ();
#endif

  return 0;
}
