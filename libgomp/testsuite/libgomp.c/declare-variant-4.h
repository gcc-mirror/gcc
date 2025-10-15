#pragma omp declare target

__attribute__ ((noipa))
int
gfx900 (void)
{
  return 0x900;
}

__attribute__ ((noipa))
int
gfx902 (void)
{
  return 0x902;
}

__attribute__ ((noipa))
int
gfx904 (void)
{
  return 0x904;
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
gfx909 (void)
{
  return 0x909;
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
gfx942 (void)
{
  return 0x942;
}

__attribute__ ((noipa))
int
gfx950 (void)
{
  return 0x950;
}

__attribute__ ((noipa))
int
gfx1030 (void)
{
  return 0x1030;
}

__attribute__ ((noipa))
int
gfx1031 (void)
{
  return 0x1031;
}

__attribute__ ((noipa))
int
gfx1032 (void)
{
  return 0x1032;
}

__attribute__ ((noipa))
int
gfx1033 (void)
{
  return 0x1033;
}

__attribute__ ((noipa))
int
gfx1034 (void)
{
  return 0x1034;
}

__attribute__ ((noipa))
int
gfx1035 (void)
{
  return 0x1035;
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
gfx1101 (void)
{
  return 0x1101;
}

__attribute__ ((noipa))
int
gfx1102 (void)
{
  return 0x1102;
}

__attribute__ ((noipa))
int
gfx1103 (void)
{
  return 0x1103;
}

__attribute__ ((noipa))
int
gfx1150 (void)
{
  return 0x1150;
}

__attribute__ ((noipa))
int
gfx1151 (void)
{
  return 0x1151;
}

__attribute__ ((noipa))
int
gfx1152 (void)
{
  return 0x1152;
}

__attribute__ ((noipa))
int
gfx1153 (void)
{
  return 0x1153;
}

__attribute__ ((noipa))
int
gfx9_generic (void)
{
  return 0x90ff;
}

__attribute__ ((noipa))
int
gfx9_4_generic (void)
{
  return 0x94ff;
}

__attribute__ ((noipa))
int
gfx10_3_generic (void)
{
  return 0x103ff;
}

__attribute__ ((noipa))
int
gfx11_generic (void)
{
  return 0x110ff;
}


#pragma omp declare variant(gfx900) match(device = {isa("gfx900")})
#pragma omp declare variant(gfx902) match(device = {isa("gfx902")})
#pragma omp declare variant(gfx904) match(device = {isa("gfx904")})
#pragma omp declare variant(gfx906) match(device = {isa("gfx906")})
#pragma omp declare variant(gfx908) match(device = {isa("gfx908")})
#pragma omp declare variant(gfx909) match(device = {isa("gfx909")})
#pragma omp declare variant(gfx90a) match(device = {isa("gfx90a")})
#pragma omp declare variant(gfx90c) match(device = {isa("gfx90c")})
#pragma omp declare variant(gfx942) match(device = {isa("gfx942")})
#pragma omp declare variant(gfx950) match(device = {isa("gfx950")})
#pragma omp declare variant(gfx1030) match(device = {isa("gfx1030")})
#pragma omp declare variant(gfx1031) match(device = {isa("gfx1031")})
#pragma omp declare variant(gfx1032) match(device = {isa("gfx1032")})
#pragma omp declare variant(gfx1033) match(device = {isa("gfx1033")})
#pragma omp declare variant(gfx1034) match(device = {isa("gfx1034")})
#pragma omp declare variant(gfx1035) match(device = {isa("gfx1035")})
#pragma omp declare variant(gfx1036) match(device = {isa("gfx1036")})
#pragma omp declare variant(gfx1100) match(device = {isa("gfx1100")})
#pragma omp declare variant(gfx1101) match(device = {isa("gfx1101")})
#pragma omp declare variant(gfx1102) match(device = {isa("gfx1102")})
#pragma omp declare variant(gfx1103) match(device = {isa("gfx1103")})
#pragma omp declare variant(gfx1150) match(device = {isa("gfx1150")})
#pragma omp declare variant(gfx1151) match(device = {isa("gfx1151")})
#pragma omp declare variant(gfx1152) match(device = {isa("gfx1152")})
#pragma omp declare variant(gfx1153) match(device = {isa("gfx1153")})
#pragma omp declare variant(gfx9_generic) match(device = {isa("gfx9-generic")})
#pragma omp declare variant(gfx9_4_generic) match(device = {isa("gfx9-4-generic")})
#pragma omp declare variant(gfx10_3_generic) match(device = {isa("gfx10-3-generic")})
#pragma omp declare variant(gfx11_generic) match(device = {isa("gfx11-generic")})
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
