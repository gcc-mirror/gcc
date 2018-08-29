/* PR c/81687 */
/* { dg-do link } */
/* { dg-additional-options "-O2" } */

int
main ()
{
  __label__ lab4, lab5, lab6;
  volatile int l = 0;
  int m = l;
  void foo (int x) { if (x == 1) goto lab4; }
  void bar (int x) { if (x == 2) goto lab5; }
  void baz (int x) { if (x == 3) goto lab6; }
  #pragma omp parallel
  {
    foo (m + 1);
   lab4:;
  }
  #pragma omp task
  {
    bar (m + 2);
   lab5:;
  }
  baz (m + 3);
 lab6:;
  return 0;
}
