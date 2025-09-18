/* { dg-do run } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* PR middle-end/121922  */

/* Failed to re-check the global flag due to tree sharing.  */

extern int flag;
int flag = 0;

int
test_with_flag ()
{
  return flag;
}

#pragma omp declare variant (test_with_flag) match (user={condition(score(10): flag > 1)})
int
test ()
{
  return 0;
}

void
doit ()
{
  flag = 0;
  if (test () != 0) __builtin_abort ();
  flag = 1;
  if (test () != 0) __builtin_abort ();
  flag = 42;
  if (test () != 42) __builtin_abort ();
}

int main ()
{
  doit ();
}

/* { dg-final { scan-tree-dump-times "flag\\.\[^=\]*= flag;\[\n\r\]+ *if \\(flag\\.\[^>\]*> 1\\)" 3 "gimple" } } */
