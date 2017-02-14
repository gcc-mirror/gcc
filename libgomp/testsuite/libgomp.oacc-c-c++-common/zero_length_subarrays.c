/* Exercise zero-length sub-arrays.  */

const int n = 10;

void
subzero_present (int *a, int n)
{
#pragma acc data present (a[0:n])
  ;
#pragma acc data pcopy (a[0:n])
  ;
#pragma acc data pcopyin (a[0:n])
  ;
#pragma acc data pcopyout (a[0:n])
  ;

}

void
subzero (int *a, int n)
{
#pragma acc data create (a[0:n])
  ;
#pragma acc data copy (a[0:n])
  ;
#pragma acc data copyin (a[0:n])
  ;
#pragma acc data copyout (a[0:n])
  ;
}

int
main ()
{
  int a[n];

#pragma acc data copy (a[0:n])
  {
    subzero_present (a, 0);
  }

  subzero (a, 0);

  return 0;
}
