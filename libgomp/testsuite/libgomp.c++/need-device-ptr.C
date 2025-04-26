// Test the need_device_ptr and need_device_addr modifiers to the adjust_args clause

#include <omp.h>

void fptr_var (int *x1, int *x2, int *x3, int **x3a, int *x4, int *x5, int *x6, int **x6a)
{
  #pragma omp target is_device_ptr (x1)
  { if (*x1 != 1) __builtin_abort (); *x1 *= -1; }

  #pragma omp target is_device_ptr (x2)
  { if (*x2 != 2) __builtin_abort (); *x2 *= -1; }

  #pragma omp target is_device_ptr (x3)
  { if (*x3 != 3) __builtin_abort (); *x3 *= -1; }

  #pragma omp target is_device_ptr (x3a)
  { if (**x3a != 30) __builtin_abort (); **x3a *= -1; }

  #pragma omp target is_device_ptr (x4)
  { if (*x4 != 4) __builtin_abort (); *x4 *= -1; }

  #pragma omp target is_device_ptr (x5)
  { if (*x5 != 5) __builtin_abort (); *x5 *= -1; }

  #pragma omp target is_device_ptr (x6)
  { if (*x6 != 6) __builtin_abort (); *x6 *= -1; }

  #pragma omp target is_device_ptr (x6a)
  { if (**x6a != 60) __builtin_abort (); **x6a *= -1; }
}

#pragma omp declare variant(fptr_var) match(construct={dispatch}) adjust_args (need_device_ptr : 1:8)
void fptr (int *x1, int *x2, int *x3, int **x3a, int *x4, int *x5, int *x6, int **x6a);

void faddr_var (int &x1, int &x2, int &x3, int *&x3a, int &x4, int &x5, int &x6, int *&x6a)
{
  #pragma omp target has_device_addr (x1)
  { if (x1 != 1) __builtin_abort (); x1 *= -1; }

  #pragma omp target has_device_addr (x2)
  { if (x2 != 2) __builtin_abort (); x2 *= -1; }

  #pragma omp target has_device_addr (x3)
  { if (x3 != 3) __builtin_abort (); x3 *= -1; }

  #pragma omp target has_device_addr (x3a)
  { if (*x3a != 30) __builtin_abort (); *x3a *= -1; }

  #pragma omp target has_device_addr (x4)
  { if (x4 != 4) __builtin_abort (); x4 *= -1; }

  #pragma omp target has_device_addr (x5)
  { if (x5 != 5) __builtin_abort (); x5 *= -1; }

  #pragma omp target has_device_addr (x6)
  { if (x6 != 6) __builtin_abort (); x6 *= -1; }

  #pragma omp target has_device_addr (x6a)
  { if (*x6a != 60) __builtin_abort (); *x6a *= -1; }
}

#pragma omp declare variant(faddr_var) match(construct={dispatch}) adjust_args (need_device_addr : 1:8)
void faddr (int &x1, int &x2, int &x3, int *&, int &x4, int &x5, int &x6, int *&);

void caller_ptr(int x, int &y, int *z, int *zptr)
{
  int a = 4;
  int bval = 5;
  int &b = bval;
  int *c = (int*) __builtin_malloc (sizeof (int));
  int *cptr;
  *c = 6;

  zptr = (int *) omp_target_alloc (sizeof (int), omp_get_default_device ()); 
  cptr = (int *) omp_target_alloc (sizeof (int), omp_get_default_device ()); 

  #pragma omp target is_device_ptr(cptr, zptr)
  {
    *zptr = 30;
    *cptr = 60;
  }

  #pragma omp target enter data map(x, a, b, c[:1], cptr, zptr)

  #pragma omp dispatch
  fptr (&x, &y, z, &zptr, &a, &b, c, &cptr);

  #pragma omp target exit data map(x, a, b, c[:1], cptr, zptr)
  #pragma omp target update from(y, z[:1])

  if (x != -1) __builtin_abort ();
  if (y != -2) __builtin_abort ();
  if (*z != -3) __builtin_abort ();

  if (a != -4) __builtin_abort ();
  if (b != -5) __builtin_abort ();
  if (*c != -6) __builtin_abort ();

  #pragma omp target is_device_ptr(cptr, zptr)
  {
    if (*zptr != -30) __builtin_abort ();
    if (*cptr != -60) __builtin_abort ();
  }

  __builtin_free (c);
  omp_target_free (cptr, omp_get_default_device ());
  omp_target_free (zptr, omp_get_default_device ());
}

void caller_addr(int x, int &y, int *z, int *zptr)
{
  int a = 4;
  int bval = 5;
  int &b = bval;
  int *c = (int*) __builtin_malloc (sizeof (int));
  int *cptr;
  *c = 6;

  zptr = (int *) omp_target_alloc (sizeof (int), omp_get_default_device ()); 
  cptr = (int *) omp_target_alloc (sizeof (int), omp_get_default_device ()); 

  #pragma omp target is_device_ptr(cptr, zptr)
  {
    *zptr = 30;
    *cptr = 60;
  }

  #pragma omp target enter data map(x, a, b, c[:1], cptr, zptr)

  #pragma omp dispatch
  faddr (x, y, *z, zptr, a, b, *c, cptr);

  #pragma omp target exit data map(x, a, b, c[:1], cptr, zptr)
  #pragma omp target update from(y, z[:1])

  if (x != -1) __builtin_abort ();
  if (y != -2) __builtin_abort ();
  if (*z != -3) __builtin_abort ();

  if (a != -4) __builtin_abort ();
  if (b != -5) __builtin_abort ();
  if (*c != -6) __builtin_abort ();

  #pragma omp target is_device_ptr(cptr, zptr)
  {
    if (*zptr != -30) __builtin_abort ();
    if (*cptr != -60) __builtin_abort ();
  }


  __builtin_free (c);
}

int
main ()
{
  int x = 1;
  int yval = 2;
  int &y = yval;
  int *z = (int *) __builtin_malloc (sizeof (int));
  int *zptr;
  *z = 3;

  #pragma omp target data map(y, z[:1])
    caller_ptr (x, y, z, zptr);

  x = 1;
  y = 2;
  *z = 3;

  #pragma omp target data map(y, z[:1], zptr)
    caller_addr (x, y, z, zptr);

  __builtin_free (z);
}
