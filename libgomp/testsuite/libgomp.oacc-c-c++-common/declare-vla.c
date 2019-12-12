/* Verify OpenACC 'declare' with VLAs.  */

#include <assert.h>


void
f (void)
{
  int N = 1000;
  int i, A[N];
#pragma acc declare copy(A)

  for (i = 0; i < N; i++)
    A[i] = -i;

#pragma acc kernels
  for (i = 0; i < N; i++)
    A[i] = i;

#pragma acc update host(A)

  for (i = 0; i < N; i++)
    assert (A[i] == i);
}


/* The same as 'f' but everything contained in an OpenACC 'data' construct.  */

void
f_data (void)
{
#pragma acc data
  {
    int N = 1000;
    int i, A[N];
# pragma acc declare copy(A)

    for (i = 0; i < N; i++)
      A[i] = -i;

# pragma acc kernels
    for (i = 0; i < N; i++)
      A[i] = i;

# pragma acc update host(A)

    for (i = 0; i < N; i++)
      assert (A[i] == i);
  }
}


int
main ()
{
  f ();

  f_data ();

  return 0;
}


/* { dg-xfail-run-if "TODO PR90861" { *-*-* } { "-DACC_MEM_SHARED=0" } }
   This might XPASS if the compiler happens to put the two 'A' VLAs at the same
   address.  */
