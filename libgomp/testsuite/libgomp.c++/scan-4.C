// { dg-require-effective-target size32plus }

extern "C" void abort ();

struct S {
  inline S ();
  inline ~S ();
  inline S (const S &);
  inline S & operator= (const S &);
  int s;
};

S::S () : s (0)
{
}

S::~S ()
{
}

S::S (const S &x)
{
  s = x.s;
}

S &
S::operator= (const S &x)
{
  s = x.s;
  return *this;
}

static inline void
ini (S &x)
{
  x.s = 0;
}

S r, a[1024], b[1024];

#pragma omp declare reduction (+: S: omp_out.s += omp_in.s)
#pragma omp declare reduction (plus: S: omp_out.s += omp_in.s) initializer (ini (omp_priv))

__attribute__((noipa)) void
foo (S *a, S *b, S &r)
{
  #pragma omp for reduction (inscan, +:r)
  for (int i = 0; i < 1024; i++)
    {
      r.s += a[i].s;
      #pragma omp scan inclusive(r)
      b[i] = r;
    }
}

__attribute__((noipa)) S
bar ()
{
  S s;
  #pragma omp parallel
  #pragma omp for reduction (inscan, plus:s)
  for (int i = 0; i < 1024; i++)
    {
      s.s += 2 * a[i].s;
      #pragma omp scan inclusive(s)
      b[i] = s;
    }
  return s;
}

__attribute__((noipa)) void
baz (S *a, S *b, S &r)
{
  #pragma omp parallel for reduction (inscan, +:r)
  for (int i = 0; i < 1024; i++)
    {
      r.s += a[i].s;
      #pragma omp scan inclusive(r)
      b[i] = r;
    }
}

__attribute__((noipa)) S
qux ()
{
  S s;
  #pragma omp parallel for reduction (inscan, plus:s)
  for (int i = 0; i < 1024; i++)
    {
      s.s += 2 * a[i].s;
      #pragma omp scan inclusive(s)
      b[i] = s;
    }
  return s;
}

int
main ()
{
  S s;
  for (int i = 0; i < 1024; ++i)
    {
      a[i].s = i;
      b[i].s = -1;
      asm ("" : "+g" (i));
    }
  #pragma omp parallel
  foo (a, b, r);
  if (r.s != 1024 * 1023 / 2)
    abort ();
  for (int i = 0; i < 1024; ++i)
    {
      s.s += i;
      if (b[i].s != s.s)
	abort ();
      else
	b[i].s = 25;
    }
  if (bar ().s != 1024 * 1023)
    abort ();
  s.s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      s.s += 2 * i;
      if (b[i].s != s.s)
	abort ();
    }
  r.s = 0;
  baz (a, b, r);
  if (r.s != 1024 * 1023 / 2)
    abort ();
  s.s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      s.s += i;
      if (b[i].s != s.s)
	abort ();
      else
	b[i].s = 25;
    }
  if (qux ().s != 1024 * 1023)
    abort ();
  s.s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      s.s += 2 * i;
      if (b[i].s != s.s)
	abort ();
    }
}
