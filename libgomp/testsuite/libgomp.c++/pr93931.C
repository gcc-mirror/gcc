// PR c++/93931
// { dg-do run }
// { dg-options "-O2 -std=c++14" }

extern "C" void abort ();

void
sink (int &x)
{
  int *volatile p;
  p = &x;
  (*p)++;
}

int
foo ()
{
  int r = 0;
  [&r] () {
#pragma omp parallel for reduction(+ : r)
    for (int i = 0; i < 1024; ++i)
      r += i;
  } ();
  return r;
}

int
bar ()
{
  int l = 0;
  [&l] () {
#pragma omp parallel for lastprivate (l)
    for (int i = 0; i < 1024; ++i)
      l = i;
  } ();
  return l;
}

void
baz ()
{
  int f = 18;
  [&f] () {
#pragma omp parallel for firstprivate (f)
    for (int i = 0; i < 1024; ++i)
      {
	sink (f);
	f += 3;
	sink (f);
	if (f != 23)
	  abort ();
	sink (f);
	f -= 7;
	sink (f);
      }
  } ();
  if (f != 18)
    abort ();
}

int
qux ()
{
  int r = 0;
  [&] () {
#pragma omp parallel for reduction(+ : r)
    for (int i = 0; i < 1024; ++i)
      r += i;
  } ();
  return r;
}

int
corge ()
{
  int l = 0;
  [&] () {
#pragma omp parallel for lastprivate (l)
    for (int i = 0; i < 1024; ++i)
      l = i;
  } ();
  return l;
}

void
garply ()
{
  int f = 18;
  [&] () {
#pragma omp parallel for firstprivate (f)
    for (int i = 0; i < 1024; ++i)
      {
	sink (f);
	f += 3;
	sink (f);
	if (f != 23)
	  abort ();
	sink (f);
	f -= 7;
	sink (f);
      }
  } ();
  if (f != 18)
    abort ();
}

int
main ()
{
  if (foo () != 1024 * 1023 / 2)
    abort ();
  if (bar () != 1023)
    abort ();
  baz ();
  if (qux () != 1024 * 1023 / 2)
    abort ();
  if (corge () != 1023)
    abort ();
  garply ();
}
