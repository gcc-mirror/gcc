/* { dg-do run } */
/* { dg-additional-options "-fno-tree-ch -fno-tree-dce -fno-tree-vrp" } */

int
main (void)
{
  int fa;

  #pragma acc kernels
  for (int rw = 0; rw < 1; ++rw)
    fa = 0;

  return 0;
}

