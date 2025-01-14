# include <stdio.h>

template <bool tasking>
int
fib (int n, bool flag)
{
  int i, j;
  if (n < 2)
    return n;
  else if ( tasking && flag && n < 8 )  // serial/taskless cutoff for n<8
    return fib<false> (n, false);
  else
    {
#pragma omp metadirective				\
  when (user = {condition (tasking && flag)}: task shared(i))
      i = fib<tasking> (n - 1, flag);
#pragma omp metadirective					\
  when (user = {condition (score(10): tasking && flag)}: task shared(j))
      j = fib<tasking> (n - 2, flag);
#pragma omp metadirective			\
  when (user = {condition (tasking && flag)}: taskwait)
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
    if (fib<true> (n, true) != o)
      __builtin_abort ();
    if (fib<true> (n, false) != o)
      __builtin_abort ();
    if (fib<false> (n, false) != o)
      __builtin_abort ();
    if (fib<false> (n, true) != o)
      __builtin_abort ();
  }
  return 0;
}
