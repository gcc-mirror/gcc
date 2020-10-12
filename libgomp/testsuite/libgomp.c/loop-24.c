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
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(7) in_reduction(+:niters)
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
  #pragma omp parallel master reduction(task, +:niters2)
  #pragma omp taskloop collapse(7) in_reduction(+:niters2)
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
  #pragma omp parallel master reduction(task, +:niters3)
  #pragma omp taskloop collapse(5) in_reduction(+:niters3)
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
  #pragma omp parallel master reduction(task, +:niters4)
  #pragma omp taskloop collapse(5) in_reduction(+:niters4)
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
  #pragma omp parallel master reduction(task, +:niters5)
  #pragma omp taskloop collapse(3) in_reduction(+:niters5)
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
  #pragma omp parallel master reduction(task, +:niters6)
  #pragma omp taskloop collapse(3) in_reduction(+:niters6)
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

  {
  static int i, j, x;
  static volatile int a, b, c, d, e, f, g, h;
  static int w[13][27];
  for (i = -4; i < 8; i++)
    for (j = 3 * i; j > 2 * i; j--)
      w[i + 5][j + 5] = 1;
  a = -4; b = 8; c = 1; d = 3; e = 0; f = 2; g = 0; h = -1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = -4; i < 8; i++)
    for (j = 3 * i; j > 2 * i; j--)
      {
	if (i < -4 || i >= 8 || j > 3 * i || j <= i * 2 || w[i + 5][j + 5] != 1)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 8 || j != 14 || x != 7183 || niters != 28)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i < -4 || i >= 8 || j > 3 * i || j <= i * 2 || w[i + 5][j + 5] != 2)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 8 || j != 14 || x != 7183 || niters != 28)
    abort ();
  for (int i = -4; i < 8; i++)
    for (int j = 3 * i; j > 2 * i; j--)
      if (w[i + 5][j + 5] == 3)
	w[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = -2; i < 4; i++)
    for (j = -2 * i + 3; j > -3; j -= 2)
      w[i + 5][j + 5] = 1;
  a = -2; b = 4; c = 1; d = -2; e = 3; f = 0; g = -3; h = -2;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = -2; i < 4; i++)
    for (j = -2 * i + 3; j > -3; j -= 2)
      {
	if (i < -2 || i >= 4 || j <= -3 || j > -2 * i + 3 || w[i + 5][j + 5] != 1)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/* i != 4 || j != -3 || */x != 3071 || niters != 15)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i < -2 || i >= 4 || j <= -3 || j > -2 * i + 3 || w[i + 5][j + 5] != 2)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != 4 || j != -3 || */x != 3071 || niters != 15)
    abort ();
  for (i = -2; i < 4; i++)
    for (j = -2 * i + 3; j > -3; j -= 2)
      if (w[i + 5][j + 5] == 3)
	w[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 3; i > -3; i--)
    for (j = -2 * i + 7; j > 2 * i + 1; j--)
      w[i + 5][j + 5] = 1;
  a = 3; b = -3; c = -1; d = -2; e = 7; f = 2; g = 1; h = -1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = 3; i > -3; i--)
    for (j = -2 * i + 7; j > 2 * i + 1; j--)
      {
	if (i <= -3 || i > 3 || j <= 2 * i + 1 || j > -2 * i + 7 || w[i + 5][j + 5] != 1)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -3 || j != -3 || x != -1026 || niters != 32)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i <= -3 || i > 3 || j <= 2 * i + 1 || j > -2 * i + 7 || w[i + 5][j + 5] != 2)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -3 || j != -3 || x != -1026 || niters != 32)
    abort ();
  for (i = 3; i > -3; i--)
    for (j = -2 * i + 7; j > 2 * i + 1; j--)
      if (w[i + 5][j + 5] == 3)
	w[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 3; i > -3; i--)
    for (j = 2 * i + 7; j > -2 * i + 1; j--)
      w[i + 5][j + 5] = 1;
  a = 3; b = -3; c = -1; d = 2; e = 7; f = -2; g = 1; h = -1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = 3; i > -3; i--)
    for (j = 2 * i + 7; j > -2 * i + 1; j--)
      {
	if (i <= -3 || i > 3 || j <= -2 * i + 1 || j > 2 * i + 7 || w[i + 5][j + 5] != 1)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -3 || j != 3 || */x != -1020 || niters != 50)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i <= -3 || i > 3 || j <= -2 * i + 1 || j > 2 * i + 7 || w[i + 5][j + 5] != 2)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -3 || j != 3 || */x != -1020 || niters != 50)
    abort ();
  for (i = 3; i > -3; i--)
    for (j = 2 * i + 7; j > -2 * i + 1; j--)
      if (w[i + 5][j + 5] == 3)
	w[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 6; i > -6; i--)
    for (j = 2 * i + 7; j <= -2 * i + 1; j++)
      w[i + 5][j + 5] = 1;
  a = 6; b = -6; c = -1; d = 2; e = 7; f = -2; g = 2; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = 6; i > -6; i--)
    for (j = 2 * i + 7; j <= -2 * i + 1; j++)
      {
	if (i <= -6 || i > 6 || j < 2 * i + 7 || j >= -2 * i + 2 || w[i + 5][j + 5] != 1)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -6 || j != 12 || x != -5109 || niters != 36)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i <= -6 || i > 6 || j < 2 * i + 7 || j >= -2 * i + 2 || w[i + 5][j + 5] != 2)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -6 || j != 12 || x != -5109 || niters != 36)
    abort ();
  for (i = 6; i > -6; i--)
    for (j = 2 * i + 7; j <= -2 * i + 1; j++)
      if (w[i + 5][j + 5] == 3)
	w[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 6; i > -6; i -= 2)
    for (j = -2 * i + 7; j <= 2 * i + 1; j++)
      w[i + 5][j + 5] = 1;
  a = 6; b = -6; c = -2; d = -2; e = 7; f = 2; g = 2; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = 6; i > -6; i -= 2)
    for (j = -2 * i + 7; j <= 2 * i + 1; j++)
      {
	if (i <= -6 || i > 6 || j < -2 * i + 7 || j >= 2 * i + 2 || w[i + 5][j + 5] != 1)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -6 || j != 15 || */x != 2053 || niters != 33)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel master reduction(task, +:niters)
  #pragma omp taskloop collapse(2) lastprivate (i, j, x) in_reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i <= -6 || i > 6 || j < -2 * i + 7 || j >= 2 * i + 2 || w[i + 5][j + 5] != 2)
	  abort ();
	w[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -6 || j != 15 || */x != 2053 || niters != 33)
    abort ();
  for (i = 6; i > -6; i -= 2)
    for (j = -2 * i + 7; j <= 2 * i + 1; j++)
      if (w[i + 5][j + 5] == 3)
	w[i + 5][j + 5] = 0;
      else
	abort ();
  }

  return 0;
}
