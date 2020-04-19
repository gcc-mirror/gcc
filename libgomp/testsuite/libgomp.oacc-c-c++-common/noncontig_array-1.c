/* { dg-do run } */

#include <stdlib.h>
#include <assert.h>

#define n 100
#define m 100

int b[n][m];

void
test1 (void)
{
  int i, j, *a[100];

  /* Array of pointers form test.  */
  for (i = 0; i < n; i++)
    {
      a[i] = (int *)malloc (sizeof (int) * m);
      for (j = 0; j < m; j++)
	b[i][j] = j - i;
    }

  #pragma acc parallel loop copyout(a[0:n][0:m]) copyin(b)
  for (i = 0; i < n; i++)
    #pragma acc loop
    for (j = 0; j < m; j++)
      a[i][j] = b[i][j];

  for (i = 0; i < n; i++)
    {
      for (j = 0; j < m; j++)
	assert (a[i][j] == b[i][j]);
      /* Clean up.  */
      free (a[i]);
    }
}

void
test2 (void)
{
  int i, j, **a = (int **) malloc (sizeof (int *) * n);

  /* Separately allocated blocks.  */
  for (i = 0; i < n; i++)
    {
      a[i] = (int *)malloc (sizeof (int) * m);
      for (j = 0; j < m; j++)
	b[i][j] = j - i;
    }

  #pragma acc parallel loop copyout(a[0:n][0:m]) copyin(b)
  for (i = 0; i < n; i++)
    #pragma acc loop
    for (j = 0; j < m; j++)
      a[i][j] = b[i][j];

  for (i = 0; i < n; i++)
    {
      for (j = 0; j < m; j++)
	assert (a[i][j] == b[i][j]);
      /* Clean up.  */
      free (a[i]);
    }
  free (a);
}

void
test3 (void)
{
  int i, j, **a = (int **) malloc (sizeof (int *) * n);
  a[0] = (int *) malloc (sizeof (int) * n * m);

  /* Rows allocated in one contiguous block.  */
  for (i = 0; i < n; i++)
    {
      a[i] = *a + i * m;
      for (j = 0; j < m; j++)
	b[i][j] = j - i;
    }

  #pragma acc parallel loop copyout(a[0:n][0:m]) copyin(b)
  for (i = 0; i < n; i++)
    #pragma acc loop
    for (j = 0; j < m; j++)
      a[i][j] = b[i][j];

  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      assert (a[i][j] == b[i][j]);

  free (a[0]);
  free (a);
}

int
main (void)
{
  test1 ();
  test2 ();
  test3 ();
  return 0;
}
