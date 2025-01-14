#include <stdio.h>

template <bool tasking>
int
fib (int n)
{
  int i, j;
  if (n < 2)
    return n;
  else if ( tasking && n < 8 )  // serial/taskless cutoff for n<8
    return fib<false> (n);
  else
    {
#pragma omp metadirective				\
  when (user = {condition (tasking)}: task shared(i))
      i = fib<tasking> (n - 1);
#pragma omp metadirective					\
  when (user = {condition (score(10): tasking)}: task shared(j))
      j = fib<tasking> (n - 2);
#pragma omp metadirective			\
  when (user = {condition (tasking)}: taskwait)
      return i + j;
    }
}

int
main ()
{
  int n = 15, o = 610;
#pragma omp parallel
#pragma omp single
  {
    if (fib<true> (n) != o)
      __builtin_abort ();
    if (fib<false> (n) != o)
      __builtin_abort ();
  }
  return 0;
}
