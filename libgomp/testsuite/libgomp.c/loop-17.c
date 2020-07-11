/* { dg-do run } */

extern void abort (void);

signed char v[5][7][9][21][4][42][3];
volatile int zero = 0, one = 1, two = 2, three = 3;
volatile int five = 5, seven = 7, nine = 9, eleven = 11;

int
main ()
{
  for (int i = 0; i < 5; i++)
  for (int j = 0; j < 7; j++)
  for (int k = 0; k < 9; k++)
  for (int l = 2 * j; l < 3 * j; l++)
  for (int m = 7; m < 11; m++)
  for (int n = l; n < 2 * l; n++)
  for (int o = 0; o < 3; o++)
    v[i][j][k][l][m - 7][n][o] = 1;

  int niters = 0;
  #pragma omp parallel
  #pragma omp for collapse(7) reduction(+:niters)
  for (int i = 0; i < 5; i++)
  for (int j = 0; j < 7; j++)
  for (int k = 0; k < 9; k++)
  for (int l = 2 * j; l < 3 * j; l++)
  for (int m = 7; m < 11; m++)
  for (int n = l; n < 2 * l; n++)
  for (int o = 0; o < 3; o++)
    {
      niters++;
      if (i < 0 || i >= 5
	  || j < 0 || j >= 7
	  || k < 0 || k >= 9
	  || l < 2 * j || l >= 3 * j
	  || m < 7 || m >= 11
	  || n < l || n >= 2 * l
	  || o < 0 || o >= 3)
	abort ();
      if (v[i][j][k][l][m - 7][n][o] != 1)
	abort ();
      v[i][j][k][l][m - 7][n][o]++;
    }

  if (niters != 117180)
    abort ();

  int niters2 = 0;
  #pragma omp parallel
  #pragma omp for collapse(7) reduction(+:niters2)
  for (int i = zero; i < five; i += one)
  for (int j = seven - one; j >= zero; j -= one)
  for (int k = nine - one; k >= zero; k += -one)
  for (int l = two * j + zero; l < three * j; l += one)
  for (int m = eleven - one; m >= seven; m -= one)
  for (int n = two * l - one; n > one * l - one; n -= one)
  for (int o = zero; o < three; o += one)
    {
      niters2++;
      if (i < 0 || i >= 5
	  || j < 0 || j >= 7
	  || k < 0 || k >= 9
	  || l < 2 * j || l >= 3 * j
	  || m < 7 || m >= 11
	  || n < l || n >= 2 * l
	  || o < 0 || o >= 3)
	abort ();
      if (v[i][j][k][l][m - 7][n][o] != 2)
	abort ();
      v[i][j][k][l][m - 7][n][o]++;
    }

  if (niters2 != 117180)
    abort ();

  for (int i = 0; i < 5; i++)
  for (int j = 0; j < 7; j++)
  for (int k = 0; k < 9; k++)
  for (int l = 2 * j; l < 3 * j; l++)
  for (int m = 7; m < 11; m++)
  for (int n = l; n < 2 * l; n++)
  for (int o = 0; o < 3; o++)
    if (v[i][j][k][l][m - 7][n][o] != 3)
      abort ();

  int niters3 = 0;
  #pragma omp parallel
  #pragma omp for collapse(5) reduction(+:niters3)
  for (int i = 4; i >= 0; i--)
  for (int j = 6; j >= 0; --j)
  for (int l = 3 * j - 1; l >= 2 * j; l--)
  for (int n = 2 * l + -1; n > l - 1; --n)
  for (int o = 2; o >= 0; o--)
    {
      niters3++;
      if (i < 0 || i >= 5
	  || j < 0 || j >= 7
	  || l < 2 * j || l >= 3 * j
	  || n < l || n >= 2 * l
	  || o < 0 || o >= 3)
	abort ();
      if (v[i][j][0][l][0][n][o] != 3)
	abort ();
      v[i][j][0][l][0][n][o]++;
    }

  if (niters3 != 3255)
    abort ();

  int niters4 = 0;
  #pragma omp parallel
  #pragma omp for collapse(5) reduction(+:niters4)
  for (int i = zero; i < five; i += one)
  for (int j = zero; j <= seven - one; j += one)
  for (int l = j * two; l < three * j + zero; l += one)
  for (int n = one * l; n <= l * two - one; n += one)
  for (int o = zero; o < three; o += one)
    {
      niters4++;
      if (i < 0 || i >= 5
	  || j < 0 || j >= 7
	  || l < 2 * j || l >= 3 * j
	  || n < l || n >= 2 * l
	  || o < 0 || o >= 3)
	abort ();
      if (v[i][j][0][l][0][n][o] != 4)
	abort ();
      v[i][j][0][l][0][n][o]++;
    }

  if (niters4 != 3255)
    abort ();

  for (int i = 0; i < 5; i++)
  for (int j = 0; j < 7; j++)
  for (int l = 2 * j; l < 3 * j; l++)
  for (int n = l; n < 2 * l; n++)
  for (int o = 0; o < 3; o++)
    if (v[i][j][0][l][0][n][o] != 5)
      abort ();

  int niters5 = 0;
  #pragma omp parallel
  #pragma omp for collapse(3) reduction(+:niters5)
  for (int j = 6; j >= 0; --j)
  for (int l = 2 * j; l <= 3 * j - 1; l++)
  for (int n = 2 * l + -1; n > l - 1; --n)
    {
      niters5++;
      if (j < 0 || j >= 7
	  || l < 2 * j || l >= 3 * j
	  || n < l || n >= 2 * l)
	abort ();
      if (v[0][j][0][l][0][n][0] != 5)
	abort ();
      v[0][j][0][l][0][n][0]++;
    }

  if (niters5 != 217)
    abort ();

  int niters6 = 0;
  #pragma omp parallel
  #pragma omp for collapse(3) reduction(+:niters6)
  for (int j = seven - one; j > - one; j -= one)
  for (int l = j * three - one; l >= j * two + zero; l += -one)
  for (int n = two * l - one; n > l - one; n -= one)
    {
      niters6++;
      if (j < 0 || j >= 7
	  || l < 2 * j || l >= 3 * j
	  || n < l || n >= 2 * l)
	abort ();
      if (v[0][j][0][l][0][n][0] != 6)
	abort ();
      v[0][j][0][l][0][n][0]++;
    }

  if (niters6 != 217)
    abort ();

  for (int j = 0; j < 7; j++)
  for (int l = 2 * j; l < 3 * j; l++)
  for (int n = l; n < 2 * l; n++)
    if (v[0][j][0][l][0][n][0] != 7)
      abort ();
  return 0;
}
