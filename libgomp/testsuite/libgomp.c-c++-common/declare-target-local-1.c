/* { dg-do run { target offload_device } } */

int local_var = 900;
#pragma omp declare target local(local_var)

#pragma omp begin declare target
int foo (int x)
{
  local_var += x;
  return local_var;
}
#pragma omp end declare target

int main (void)
{
  /* Variables declared in the 'local' clause should not be corresponding
     storage between host/device. Test if 'target update' has no effect
     and host-side 'local_var' retains original value.  */

  int e;
  #pragma omp target map(from : e)
  {
    e = foo (23);
  }

  #pragma omp target update from(local_var)

  if (local_var == e)
    __builtin_abort ();
  if (local_var != 900)
    __builtin_abort ();

  return 0;
}
