/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>

int main ()
{
  struct {
    int *arr;
  } mystr;
  int localarr[16];
  mystr.arr = localarr;

  #pragma acc enter data copyin(mystr, localarr[0:16])

  #pragma acc data attach(mystr.arr)
  {
    #pragma acc exit data detach(mystr.arr)
    fprintf (stderr, "CheCKpOInT1\n");
    /* { dg-output ".*CheCKpOInT1(\n|\r\n|\r)" } */
  }
  /* { dg-shouldfail "" }
     { dg-output "(\n|\r\n|\r)libgomp: attach count underflow(\n|\r\n|\r)$" } */
  fprintf (stderr, "CheCKpOInT2\n");

  #pragma acc exit data copyout(mystr, localarr[0:16])

  return 0;
}
