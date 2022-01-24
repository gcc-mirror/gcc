/* { dg-do run } */

extern void abort (void);

signed char v[5][7][9][21][4][42][3];
int a[84];
int *volatile zero = &a[42];
int *volatile two = &a[42 + 2];
int *volatile three = &a[42 + 3];
int *volatile five = &a[42 + 5];
int *volatile seven = &a[42 + 7];
int *volatile nine = &a[42 + 9];
int *volatile eleven = &a[42 + 11];
int *volatile minusone = &a[42 - 1];
volatile int zeroi = 0, onei = 1, twoi = 2, threei = 3, fivei = 5;

int
main ()
{
  for (int i = 0; i < 5; i++)
  for (int j = 0; j < 7; j++)
  for (int k = 0; k < 9; k++)
  for (int l = j; l < 5 + j; l++)
  for (int m = 7; m < 11; m++)
  for (int n = 0; n < l - 2; n++)
  for (int o = 0; o < 3; o++)
    v[i][j][k][l][m - 7][n][o] = 1;

  int niters = 0;
  #pragma omp parallel
  #pragma omp for collapse(7) reduction(+:niters)
  for (int i = 0; i < 5; i++)
  for (int *j = &a[42]; j < &a[42 + 7]; j++)
  for (int *k = &a[42]; k < &a[42 + 9]; k++)
  for (int *l = j; l < 5 + j; l++)
  for (int *m = &a[42 + 7]; m < &a[42 + 11]; m++)
  for (int *n = &a[42]; n < l - 2; n++)
  for (int *o = &a[42]; o < &a[42 + 3]; o++)
    {
      niters++;
      if (i < 0 || i >= 5
	  || j - &a[42] < 0 || j - &a[42] >= 7
	  || k - &a[42] < 0 || k - &a[42] >= 9
	  || l - &a[42] < 0 || l >= j + 5
	  || m - &a[42] < 7 || m - &a[42] >= 11
	  || n - &a[42] < 0 || n >= l - 2
	  || o - &a[42] < 0 || o - &a[42] >= 3)
	abort ();
      if (v[i][j - &a[42]][k - &a[42]][l - &a[42]][m - &a[42 + 7]][n - &a[42]][o - &a[42]] != 1)
	abort ();
      v[i][j - &a[42]][k - &a[42]][l - &a[42]][m - &a[42 + 7]][n - &a[42]][o - &a[42]]++;
    }

  if (niters != 58860)
    abort ();
  int niters2 = 0;
  #pragma omp parallel
  #pragma omp for collapse(7) reduction(+:niters2)
  for (int *i = zero; i < five; i += onei)
  for (int *j = seven - onei; j >= zero; j -= onei)
  for (int *k = nine - onei; k >= zero; k += -onei)
  for (int *l = j + zeroi; l < fivei + j; l += onei)
  for (int *m = eleven - onei; m >= seven; m -= onei)
  for (int *n = l - threei; n >= zero; n -= onei)
  for (int *o = zero; o < three; o += onei)
    {
      niters2++;
      if (i - &a[42] < 0 || i - &a[42] >= 5
	  || j - &a[42] < 0 || j - &a[42] >= 7
	  || k - &a[42] < 0 || k - &a[42] >= 9
	  || l < j || l >= j + 5
	  || m - &a[42] < 7 || m - &a[42] >= 11
	  || n - &a[42] < 0 || n >= l - 2
	  || o - &a[42] < 0 || o - &a[42] >= 3)
	abort ();
      if (v[i - &a[42]][j - &a[42]][k - &a[42]][l - &a[42]][m - &a[42 + 7]][n - &a[42]][o - &a[42]] != 2)
	abort ();
      v[i - &a[42]][j - &a[42]][k - &a[42]][l - &a[42]][m - &a[42 + 7]][n - &a[42]][o - &a[42]]++;
    }

  if (niters2 != 58860)
    abort ();

  for (int i = 0; i < 5; i++)
  for (int j = 0; j < 7; j++)
  for (int k = 0; k < 9; k++)
  for (int l = j; l < 5 + j; l++)
  for (int m = 7; m < 11; m++)
  for (int n = 0; n < l - 2; n++)
  for (int o = 0; o < 3; o++)
    if (v[i][j][k][l][m - 7][n][o] != 3)
      abort ();

  int niters3 = 0;
  #pragma omp parallel
  #pragma omp for collapse(5) reduction(+:niters3)
  for (int *i = &a[42 + 4]; i >= &a[42 + 0]; i--)
  for (int *j = &a[42 + 6]; j >= &a[42 + 0]; --j)
  for (int *l = j + 4; l >= j; l--)
  for (int *n = l - 3; n >= &a[42]; --n)
  for (int *o = &a[42 + 2]; o >= &a[42 + 0]; o--)
    {
      niters3++;
      if (i - &a[42] < 0 || i - &a[42] >= 5
	  || j - &a[42] < 0 || j - &a[42] >= 7
	  || l < j || l >= j + 5
	  || n - &a[42] < 0 || n >= l - 2
	  || o - &a[42] < 0 || o - &a[42] >= 3)
	abort ();
      if (v[i - &a[42]][j - &a[42]][0][l - &a[42]][0][n - &a[42]][o - &a[42]] != 3)
	abort ();
      v[i - &a[42]][j - &a[42]][0][l - &a[42]][0][n - &a[42]][o - &a[42]]++;
    }

  if (niters3 != 1635)
    abort ();

  int niters4 = 0;
  #pragma omp parallel
  #pragma omp for collapse(5) reduction(+:niters4)
  for (int *i = zero; i < five; i += onei)
  for (int *j = zero; j <= seven - onei; j += onei)
  for (int *l = zeroi + j; l < j + fivei; l += onei)
  for (int *n = zero; n <= l - threei; n += onei)
  for (int o = zeroi; o < threei; o += onei)
    {
      niters4++;
      if (i - &a[42] < 0 || i - &a[42] >= 5
	  || j - &a[42] < 0 || j - &a[42] >= 7
	  || l < j || l >= j + 5
	  || n - &a[42] < 0 || n >= l - 2
	  || o < 0 || o >= 3)
	abort ();
      if (v[i - &a[42]][j - &a[42]][0][l - &a[42]][0][n - &a[42]][o] != 4)
	abort ();
      v[i - &a[42]][j - &a[42]][0][l - &a[42]][0][n - &a[42]][o]++;
    }

  if (niters4 != 1635)
    abort ();

  for (int i = 0; i < 5; i++)
  for (int j = 0; j < 7; j++)
  for (int l = j; l < j + 5; l++)
  for (int n = 0; n < l - 2; n++)
  for (int o = 0; o < 3; o++)
    if (v[i][j][0][l][0][n][o] != 5)
      abort ();

  int niters5 = 0;
  #pragma omp parallel
  #pragma omp for collapse(3) reduction(+:niters5)
  for (int *j = &a[42 + 6]; j >= &a[42]; --j)
  for (int *l = j + 0; l <= j + 4; l++)
  for (int *n = l - 3; n > &a[42 - 1]; --n)
    {
      niters5++;
      if (j - &a[42] < 0 || j - &a[42] >= 7
	  || l < j || l >= j + 5
	  || n < &a[42] || n >= l - 2)
	abort ();
      if (v[0][j - &a[42]][0][l - &a[42]][0][n - &a[42]][0] != 5)
	abort ();
      v[0][j - &a[42]][0][l - &a[42]][0][n - &a[42]][0]++;
    }

  if (niters5 != 109)
    abort ();

  int niters6 = 0;
  #pragma omp parallel
  #pragma omp for collapse(3) reduction(+:niters6)
  for (int *j = seven - onei; j > minusone; j -= onei)
  for (int *l = j + threei + onei; l >= j; l += -onei)
  for (int *n = l - threei; n > minusone; n -= onei)
    {
      niters6++;
      if (j - &a[42] < 0 || j - &a[42] >= 7
	  || l < j || l >= j + 5
	  || n < &a[42] || n >= l - 2)
	abort ();
      if (v[0][j - &a[42]][0][l - &a[42]][0][n - &a[42]][0] != 6)
	abort ();
      v[0][j - &a[42]][0][l - &a[42]][0][n - &a[42]][0]++;
    }

  if (niters6 != 109)
    abort ();

  for (int j = 0; j < 7; j++)
  for (int l = j; l < j + 5; l++)
  for (int n = 0; n < l - 2; n++)
    if (v[0][j][0][l][0][n][0] != 7)
      abort ();
  return 0;
}
