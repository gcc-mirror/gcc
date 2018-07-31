// PR middle-end/86542

struct S { int s; S (); ~S (); S (const S &); };
S s;

S::S ()
{
}

S::~S ()
{
}

S::S (const S &x)
{
  s = x.s;
}

__attribute__((noipa)) void
foo (int i, int j, int k, S s)
{
  if (i != 0 || j != 0 || k != 0 || s.s != 12)
    __builtin_abort ();
}

int
main ()
{
  volatile int inc = 16, jnc = 16, knc = 16;
  s.s = 12;
  #pragma omp taskloop collapse (3) firstprivate (s)
  for (int i = 0; i < 16; i += inc)
    for (int j = 0; j < 16; j += jnc)
      for (int k = 0; k < 16; k += knc)
	foo (i, j, k, s);
  return 0;
}
