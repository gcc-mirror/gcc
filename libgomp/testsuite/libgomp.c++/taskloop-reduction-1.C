extern "C" void abort ();

struct S { S (); S (unsigned long long int, int); ~S (); static int cnt1, cnt2, cnt3; unsigned long long int s; int t; };

int S::cnt1;
int S::cnt2;
int S::cnt3;

S::S ()
{
  #pragma omp atomic
  cnt1++;
}

S::S (unsigned long long int x, int y) : s (x), t (y)
{
  #pragma omp atomic update
  ++cnt2;
}

S::~S ()
{
  #pragma omp atomic
  cnt3 = cnt3 + 1;
  if (t < 3 || t > 9 || (t & 1) == 0)
    abort ();
}

void
rbar (S *p, S *o)
{
  p->s = 1;
  if (o->t != 5)
    abort ();
  p->t = 9;
}

static inline void
rbaz (S *o, S *i)
{
  if (o->t != 5 || i->t != 9)
    abort ();
  o->s *= i->s;
}

#pragma omp declare reduction (+: S : omp_out.s += omp_in.s) \
  initializer (omp_priv (0, 3))
#pragma omp declare reduction (*: S : rbaz (&omp_out, &omp_in)) \
  initializer (rbar (&omp_priv, &omp_orig))

S gs = { 0, 7 };
S &g = gs;
S hs (1, 5);
S &h = hs;

int
foo (int *a, int &b)
{
  int xs = 0;
  int &x = xs;
  #pragma omp taskloop reduction (+:x) in_reduction (+:b)
  for (int i = 0; i < 64; i++)
    {
      x += a[i];
      b += a[i] * 2;
    }
  return x;
}

unsigned long long int
bar (int *a, unsigned long long int &b)
{
  unsigned long long int xs = 1;
  unsigned long long int &x = xs;
  #pragma omp taskloop reduction (*:x) in_reduction (*:b)
  for (int i = 0; i < 64; i++)
    {
      #pragma omp task in_reduction (*:x)
      x *= a[i];
      #pragma omp task in_reduction (*:b)
      b *= (3 - a[i]);
    }
  return x;
}

void
baz (int i, int *a, int *c)
{
  #pragma omp task in_reduction (*:h) in_reduction (+:g)
  {
    g.s += 7 * a[i];
    h.s *= (3 - c[i]);
    if ((g.t != 7 && g.t != 3) || (h.t != 5 && h.t != 9))
      abort ();
  }
}

void
test ()
{
  int i, j, a[64], b = 0, c[64];
  unsigned long long int d = 1, e;
  S ms (0, 7);
  for (i = 0; i < 64; i++)
    {
      a[i] = 2 * i;
      c[i] = 1 + ((i % 3) != 1);
    }
  #pragma omp parallel
  #pragma omp master
  {
    S ns = { 1, 5 };
    S &m = ms;
    S &n = ns;
    #pragma omp taskgroup task_reduction (+:b)
      j = foo (a, b);
    #pragma omp taskgroup task_reduction (*:d)
      e = bar (c, d);
    #pragma omp taskloop reduction (+: g, m) reduction (*: h, n)
    for (i = 0; i < 64; ++i)
      {
	g.s += 3 * a[i];
	h.s *= (3 - c[i]);
	m.s += 4 * a[i];
	n.s *= c[i];
	if ((g.t != 7 && g.t != 3) || (h.t != 5 && h.t != 9)
	    || (m.t != 7 && m.t != 3) || (n.t != 5 && n.t != 9))
	  abort ();
	baz (i, a, c);
      }
    if (n.s != (1ULL << 43) || n.t != 5)
      abort ();
  }
  if (j != 63 * 64 || b != 63 * 64 * 2)
    abort ();
  if (e != (1ULL << 43) || d != (1ULL << 21))
    abort ();
  if (g.s != 63 * 64 * 10 || g.t != 7)
    abort ();
  if (h.s != (1ULL << 42) || h.t != 5)
    abort ();
  if (ms.s != 63 * 64 * 4 || ms.t != 7)
    abort ();
}

int
main ()
{
  int c1 = S::cnt1, c2 = S::cnt2, c3 = S::cnt3;
  test ();
  if (S::cnt1 + S::cnt2 - c1 - c2 != S::cnt3 - c3)
    abort ();
}
