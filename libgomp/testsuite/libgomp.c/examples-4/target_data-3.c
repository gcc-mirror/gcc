/* { dg-do run } */

#include <stdlib.h>

const int ROWS = 5;
const int COLS = 5;

void init (int Q[][COLS], const int rows, const int cols)
{
  int i, j;
  for (i = 0; i < rows; i++)
    for (j = 0; j < cols; j++)
      Q[i][j] = (i + 1) * 100 + (j + 1);
}

void check (int a[][COLS], int b[][COLS], const int rows, const int cols)
{
  int i, j;
  for (i = 0; i < rows; i++)
    for (j = 0; j < cols; j++)
      if (a[i][j] != b[i][j])
	abort ();
}

void gramSchmidt_ref (int Q[][COLS], const int rows, const int cols)
{
  int i, k;

  for (k = 0; k < cols; k++)
    {
      int tmp = 0;

      for (i = 0; i < rows; i++)
	tmp += (Q[i][k] * Q[i][k]);

      for (i = 0; i < rows; i++)
	Q[i][k] *= tmp;
    }
}

void gramSchmidt (int Q[][COLS], const int rows, const int cols)
{
  int i, k;

  #pragma omp target data map(Q[0:rows][0:cols]) map(to:COLS)
    for (k = 0; k < cols; k++)
      {
	int tmp = 0;

	#pragma omp target map(tofrom:tmp)
	  #pragma omp parallel for reduction(+:tmp)
	    for (i = 0; i < rows; i++)
	      tmp += (Q[i][k] * Q[i][k]);

	#pragma omp target
	  #pragma omp parallel for
	    for (i = 0; i < rows; i++)
	      Q[i][k] *= tmp;
      }
}

int main ()
{
  int (*Q1)[COLS] = (int(*)[COLS]) malloc (ROWS * COLS * sizeof (int));
  int (*Q2)[COLS] = (int(*)[COLS]) malloc (ROWS * COLS * sizeof (int));

  init (Q1, ROWS, COLS);
  init (Q2, ROWS, COLS);

  gramSchmidt_ref (Q1, ROWS, COLS);
  gramSchmidt (Q2, ROWS, COLS);

  check (Q1, Q2, ROWS, COLS);

  free (Q1);
  free (Q2);

  return 0;
}
