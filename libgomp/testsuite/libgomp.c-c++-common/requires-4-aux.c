/* { dg-skip-if "" { *-*-* } } */

#pragma omp requires reverse_offload

/* Note: The file does not have neither of:
   declare target directives, device constructs or device routines.  */

int x;

void foo (void)
{
  x = 1;
}
