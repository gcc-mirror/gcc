// PR c++/49043
// { dg-options "-std=c++0x" }
// { dg-do run }

extern "C" void abort ();

int
main ()
{
  int r = 0;
  #pragma omp parallel for reduction (+:r)
    for (int a = 0; a < 10; ++a)
      {
	auto func = [=] () { return a; };
	r += func ();
      }
  if (r != 45)
    abort ();
}
