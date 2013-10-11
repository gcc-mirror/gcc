// { dg-do run }

extern "C" void abort (void);
int x = 6;

int
main ()
{
  int v, l = 2, s = 1;
  #pragma omp atomic seq_cst
    x = -3 + x;
  #pragma omp atomic read seq_cst
    v = x;
  if (v != 3)
    abort ();
  #pragma omp atomic update seq_cst
    x = 3 * 2 * 1 + x;
  #pragma omp atomic read seq_cst
    v = x;
  if (v != 9)
    abort ();
  #pragma omp atomic capture seq_cst
    v = x = x | 16;
  if (v != 25)
    abort ();
  #pragma omp atomic capture seq_cst
    v = x = x + 14 * 2 / 4;
  if (v != 32)
    abort ();
  #pragma omp atomic capture seq_cst
    v = x = 5 | x;
  if (v != 37)
    abort ();
  #pragma omp atomic capture seq_cst
    v = x = 40 + 12 - 2 - 7 - x;
  if (v != 6)
    abort ();
  #pragma omp atomic read seq_cst
    v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic capture seq_cst
    { v = x; x = 3 + x; }
  if (v != 6)
    abort ();
  #pragma omp atomic capture seq_cst
    { v = x; x = -1 * -1 * -1 * -1 - x; }
  if (v != 9)
    abort ();
  #pragma omp atomic read seq_cst
    v = x;
  if (v != -8)
    abort ();
  #pragma omp atomic capture seq_cst
    { x = 2 * 2 - x; v = x; }
  if (v != 12)
    abort ();
  #pragma omp atomic capture seq_cst
    { x = 7 & x; v = x; }
  if (v != 4)
    abort ();
  #pragma omp atomic capture seq_cst
    { v = x; x = 6; }
  if (v != 4)
    abort ();
  #pragma omp atomic read seq_cst
    v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic capture seq_cst
    { v = x; x = 7 * 8 + 23; }
  if (v != 6)
    abort ();
  #pragma omp atomic read seq_cst
    v = x;
  if (v != 79)
    abort ();
  #pragma omp atomic capture seq_cst
    { v = x; x = 23 + 6 * 4; }
  if (v != 79)
    abort ();
  #pragma omp atomic read seq_cst
    v = x;
  if (v != 47)
    abort ();
  #pragma omp atomic capture seq_cst
    { v = x; x = l ? 17 : 12; }
  if (v != 47)
    abort ();
  #pragma omp atomic capture seq_cst
    { v = x; x = l = s++ + 3; }
  if (v != 17 || l != 4 || s != 2)
    abort ();
  #pragma omp atomic read seq_cst
    v = x;
  if (v != 4)
    abort ();
  return 0;
}
