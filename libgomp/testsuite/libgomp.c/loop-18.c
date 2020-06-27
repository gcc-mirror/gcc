/* { dg-do run } */

extern void abort (void);

int x, i, j;
volatile int a, b, c, d, e, f, g, h;
int k[11][101];

int
main ()
{
  int niters;
  for (i = 1; i <= 10; i++)
    for (j = 1; j <= 10 * i; j++)
      k[i][j] = 1;
  a = 1; b = 11; c = 1; d = 0; e = 1; f = 10; g = 1; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 1; i <= 10; i++)
    for (j = 1; j <= 10 * i; j++)
      {
	if (i < 1 || i > 10 || j < 1 || j > 10 * i || k[i][j] != 1)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 11 || j != 101 || x != 10340 || niters != 550)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i < 1 || i > 10 || j < 1 || j > 10 * i || k[i][j] != 2)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 11 || j != 101 || x != 10340 || niters != 550)
    abort ();
  for (i = 1; i <= 10; i++)
    for (j = 1; j <= 10 * i; j++)
      if (k[i][j] == 3)
	k[i][j] = 0;
      else
	abort ();
  for (i = 0; i < 11; i++)
    for (j = 0; j < 101; j++)
      if (k[i][j] != 0)
	abort ();
  for (i = 0; i < 10; i++)
    for (j = 0; j < 10 * i; j++)
      k[i][j] = 1;
  a = 0; b = 10; c = 1; d = 0; e = 0; f = 10; g = 0; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 0; i < 10; i++)
    for (j = 0; j < 10 * i; j++)
      {
	if (i < 0 || i >= 10 || j < 0 || j >= 10 * i || k[i][j] != 1)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 10 || j != 90 || x != 9305 || niters != 450)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i < 0 || i >= 10 || j < 0 || j >= 10 * i || k[i][j] != 2)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 10 || j != 90 || x != 9305 || niters != 450)
    abort ();
  for (i = 0; i < 10; i++)
    for (j = 0; j < 10 * i; j++)
      if (k[i][j] == 3)
	k[i][j] = 0;
      else
	abort ();
  for (i = 0; i < 11; i++)
    for (j = 0; j < 101; j++)
      if (k[i][j] != 0)
	abort ();
  for (i = 4; i < 10; i++)
    for (j = -9 + 2 * i; j < i; j++)
      k[i][j + 1] = 1;
  a = 4; b = 10; c = 1; d = 2; e = -9; f = 1; g = 0; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 4; i < 10; i++)
    for (j = -9 + 2 * i; j < i; j++)
      {
	if (i < 4 || i >= 10 || j < -9 + 2 * i || j >= i || k[i][j + 1] != 1)
	  abort ();
	k[i][j + 1]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != 10 || j != 9 || */x != 8199 || niters != 15)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i < 4 || i >= 10 || j < -9 + 2 * i || j >= i || k[i][j + 1] != 2)
	  abort ();
	k[i][j + 1]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != 10 || j != 9 || */x != 8199 || niters != 15)
    abort ();
  for (i = 4; i < 10; i++)
    for (j = -9 + 2 * i; j < i; j++)
      if (k[i][j + 1] == 3)
	k[i][j + 1] = 0;
      else
	abort ();
  for (i = 0; i < 11; i++)
    for (j = 0; j < 101; j++)
      if (k[i][j] != 0)
	abort ();
  for (i = 1; i < 10; i += 2)
    for (j = 1; j < i + 1; j++)
      k[i][j] = 1;
  a = 1; b = 10; c = 2; d = 0; e = 1; f = 1; g = 1; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 1; i < 10; i += 2)
    for (j = 1; j < i + 1; j++)
      {
	if (i < 1 || i >= 10 || j < 1 || j >= i + 1 || k[i][j] != 1)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 11 || j != 10 || x != 9225 || niters != 25)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i < 1 || i >= 10 || j < 1 || j >= i + 1 || k[i][j] != 2)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 11 || j != 10 || x != 9225 || niters != 25)
    abort ();
  for (i = 1; i < 10; i += 2)
    for (j = 1; j < i + 1; j++)
      if (k[i][j] == 3)
	k[i][j] = 0;
      else
	abort ();
  for (i = 0; i < 11; i++)
    for (j = 0; j < 101; j++)
      if (k[i][j] != 0)
	abort ();
  for (j = -11; j >= -41; j -= 15)
    k[0][-j] = 1;
  a = 4; b = 8; c = 12; d = -8; e = -9; f = -3; g = 6; h = 15;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 4; i < 8; i += 12)
    for (j = -8 * i - 9; j < i * -3 + 6; j += 15)
      {
	if (i != 4 || j < -41 || j > -11 || k[0][-j] != 1)
	  abort ();
	k[0][-j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 16 || j != 4 || x != 5109 || niters != 3)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i != 4 || j < -41 || j > -11 || k[0][-j] != 2)
	  abort ();
	k[0][-j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 16 || j != 4 || x != 5109 || niters != 3)
    abort ();
  for (j = -11; j >= -41; j -= 15)
    if (k[0][-j] == 3)
      k[0][-j] = 0;
    else
      abort ();
  for (j = -11; j >= -41; j--)
    if (k[0][-j] != 0)
      abort ();
  for (j = -34; j <= -7; j++)
    k[0][-j] = 1;
  a = -13; b = 7; c = 12; d = 3; e = 5; f = 0; g = -6; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = -13; i < 7; i += 12)
    for (j = 3 * i + 5; j < -6; j++)
      {
	if (i != -13 || j < -34 || j > -7 || k[0][-j] != 1)
	  abort ();
	k[0][-j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != 11 || j != 2 || */x != -12295 || niters != 28)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i != -13 || j < -34 || j > -7 || k[0][-j] != 2)
	  abort ();
	k[0][-j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != 11 || j != 2 || */x != -12295 || niters != 28)
    abort ();
  for (j = -34; j <= -7; j++)
    if (k[0][-j] == 3)
      k[0][-j] = 0;
    else
      abort ();
  return 0;
}
