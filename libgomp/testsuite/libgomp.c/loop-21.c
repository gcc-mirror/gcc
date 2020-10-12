/* { dg-do run } */

extern void abort (void);

int x, i, j;
volatile int a, b, c, d, e, f, g, h;
int k[13][27];

int
main ()
{
  int niters;
  for (i = -4; i < 8; i++)
    for (j = 3 * i; j > 2 * i; j--)
      k[i + 5][j + 5] = 1;
  a = -4; b = 8; c = 1; d = 3; e = 0; f = 2; g = 0; h = -1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = -4; i < 8; i++)
    for (j = 3 * i; j > 2 * i; j--)
      {
	if (i < -4 || i >= 8 || j > 3 * i || j <= i * 2 || k[i + 5][j + 5] != 1)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 8 || j != 14 || x != 7183 || niters != 28)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i < -4 || i >= 8 || j > 3 * i || j <= i * 2 || k[i + 5][j + 5] != 2)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 8 || j != 14 || x != 7183 || niters != 28)
    abort ();
  for (int i = -4; i < 8; i++)
    for (int j = 3 * i; j > 2 * i; j--)
      if (k[i + 5][j + 5] == 3)
	k[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = -2; i < 4; i++)
    for (j = -2 * i + 3; j > -3; j -= 2)
      k[i + 5][j + 5] = 1;
  a = -2; b = 4; c = 1; d = -2; e = 3; f = 0; g = -3; h = -2;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = -2; i < 4; i++)
    for (j = -2 * i + 3; j > -3; j -= 2)
      {
	if (i < -2 || i >= 4 || j <= -3 || j > -2 * i + 3 || k[i + 5][j + 5] != 1)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/* i != 4 || j != -3 || */x != 3071 || niters != 15)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i < -2 || i >= 4 || j <= -3 || j > -2 * i + 3 || k[i + 5][j + 5] != 2)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != 4 || j != -3 || */x != 3071 || niters != 15)
    abort ();
  for (i = -2; i < 4; i++)
    for (j = -2 * i + 3; j > -3; j -= 2)
      if (k[i + 5][j + 5] == 3)
	k[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 3; i > -3; i--)
    for (j = -2 * i + 7; j > 2 * i + 1; j--)
      k[i + 5][j + 5] = 1;
  a = 3; b = -3; c = -1; d = -2; e = 7; f = 2; g = 1; h = -1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 3; i > -3; i--)
    for (j = -2 * i + 7; j > 2 * i + 1; j--)
      {
	if (i <= -3 || i > 3 || j <= 2 * i + 1 || j > -2 * i + 7 || k[i + 5][j + 5] != 1)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -3 || j != -3 || x != -1026 || niters != 32)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i <= -3 || i > 3 || j <= 2 * i + 1 || j > -2 * i + 7 || k[i + 5][j + 5] != 2)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -3 || j != -3 || x != -1026 || niters != 32)
    abort ();
  for (i = 3; i > -3; i--)
    for (j = -2 * i + 7; j > 2 * i + 1; j--)
      if (k[i + 5][j + 5] == 3)
	k[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 3; i > -3; i--)
    for (j = 2 * i + 7; j > -2 * i + 1; j--)
      k[i + 5][j + 5] = 1;
  a = 3; b = -3; c = -1; d = 2; e = 7; f = -2; g = 1; h = -1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 3; i > -3; i--)
    for (j = 2 * i + 7; j > -2 * i + 1; j--)
      {
	if (i <= -3 || i > 3 || j <= -2 * i + 1 || j > 2 * i + 7 || k[i + 5][j + 5] != 1)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -3 || j != 3 || */x != -1020 || niters != 50)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j > g + i * f; j += h)
      {
	if (i <= -3 || i > 3 || j <= -2 * i + 1 || j > 2 * i + 7 || k[i + 5][j + 5] != 2)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -3 || j != 3 || */x != -1020 || niters != 50)
    abort ();
  for (i = 3; i > -3; i--)
    for (j = 2 * i + 7; j > -2 * i + 1; j--)
      if (k[i + 5][j + 5] == 3)
	k[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 6; i > -6; i--)
    for (j = 2 * i + 7; j <= -2 * i + 1; j++)
      k[i + 5][j + 5] = 1;
  a = 6; b = -6; c = -1; d = 2; e = 7; f = -2; g = 2; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 6; i > -6; i--)
    for (j = 2 * i + 7; j <= -2 * i + 1; j++)
      {
	if (i <= -6 || i > 6 || j < 2 * i + 7 || j >= -2 * i + 2 || k[i + 5][j + 5] != 1)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -6 || j != 12 || x != -5109 || niters != 36)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i <= -6 || i > 6 || j < 2 * i + 7 || j >= -2 * i + 2 || k[i + 5][j + 5] != 2)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != -6 || j != 12 || x != -5109 || niters != 36)
    abort ();
  for (i = 6; i > -6; i--)
    for (j = 2 * i + 7; j <= -2 * i + 1; j++)
      if (k[i + 5][j + 5] == 3)
	k[i + 5][j + 5] = 0;
      else
	abort ();
  for (i = 6; i > -6; i -= 2)
    for (j = -2 * i + 7; j <= 2 * i + 1; j++)
      k[i + 5][j + 5] = 1;
  a = 6; b = -6; c = -2; d = -2; e = 7; f = 2; g = 2; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 6; i > -6; i -= 2)
    for (j = -2 * i + 7; j <= 2 * i + 1; j++)
      {
	if (i <= -6 || i > 6 || j < -2 * i + 7 || j >= 2 * i + 2 || k[i + 5][j + 5] != 1)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -6 || j != 15 || */x != 2053 || niters != 33)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i > b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i <= -6 || i > 6 || j < -2 * i + 7 || j >= 2 * i + 2 || k[i + 5][j + 5] != 2)
	  abort ();
	k[i + 5][j + 5]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (/*i != -6 || j != 15 || */x != 2053 || niters != 33)
    abort ();
  for (i = 6; i > -6; i -= 2)
    for (j = -2 * i + 7; j <= 2 * i + 1; j++)
      if (k[i + 5][j + 5] == 3)
	k[i + 5][j + 5] = 0;
      else
	abort ();
  return 0;
}
