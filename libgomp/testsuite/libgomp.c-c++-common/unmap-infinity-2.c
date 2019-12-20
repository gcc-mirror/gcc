int foo[16];
#pragma omp declare target (foo)

__attribute__((used)) void bar (void)
{
  #pragma omp target parallel for
  for (int i = 0; i < 16; i++)
    foo[i] = i;
}

int
main (int argc, char *argv[])
{
  int *foo_copy = foo;
  /* Try to trigger the unmapping of a REFCOUNT_INFINITY target block.  This
     does nothing at the time of writing.  */
  #pragma omp target exit data map(delete: foo_copy[0:16])
  return 0;
}
