/* { dg-do run }  */
/* { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } } */
/* { dg-additional-sources reverse-offload-1-aux.c } */

/* Check that reverse offload works in particular:
   - no code is generated on the device side (i.e. no
     implicit declare target of called functions and no
     code gen for the target-region body)
     -> would otherwise fail due to 'add_3' symbol
   - Plus the usual (compiles, runs, produces correct result)

   Note: Running also the non-reverse-offload target regions
   on the host (host fallback) is valid and will pass.  */

#pragma omp requires reverse_offload

extern int add_3 (int);

static int global_var = 5;

void
check_offload (int *x, int *y)
{
  *x = add_3 (*x);
  *y = add_3 (*y);
}

#pragma omp declare target
void
tg_fn (int *x, int *y)
{
  int x2 = *x, y2 = *y;
  if (x2 != 2 || y2 != 3)
    __builtin_abort ();
  x2 = x2 + 2;
  y2 = y2 + 7;

  #pragma omp target device(ancestor : 1) map(tofrom: x2)
    check_offload(&x2, &y2);

  if (x2 != 2+2+3 || y2 != 3 + 7)
    __builtin_abort ();
  *x = x2, *y = y2;
}
#pragma omp end declare target

void
my_func (int *x, int *y)
{
  if (global_var != 5)
    __builtin_abort ();
  global_var = 242;
  *x = 2*add_3(*x);
  *y = 3*add_3(*y);
}

int
main ()
{
  #pragma omp target
  {
     int x = 2, y = 3;
     tg_fn (&x, &y);
  }

  #pragma omp target
  {
     int x = -2, y = -1;
     #pragma omp target device ( ancestor:1 ) firstprivate(y) map(tofrom:x)
     {
       if (x != -2 || y != -1)
         __builtin_abort ();
       my_func (&x, &y);
       if (x != 2*(3-2) || y != 3*(3-1))
         __builtin_abort ();
     }
     if (x != 2*(3-2) || y != -1)
       __builtin_abort ();
  }

  if (global_var != 242)
    __builtin_abort ();
  return 0;
}
