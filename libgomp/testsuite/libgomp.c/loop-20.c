/* { dg-do run } */

extern void abort (void);

unsigned long long int x, i, j;
volatile unsigned long long int a, b, c, d, e, f, g, h;
int k[4][206];

int
main ()
{
  long long int niters;
  for (j = ~0ULL / 2 - 32; j < ((~0ULL / 2) + 6); j++)
    k[0][j - ~0ULL / 2 + 64] = 1;
  a = 1; b = 2; c = 1; d = 0; e = ~0ULL / 2 - 32; f = ((~0ULL / 2) + 6); g = 0; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 1; i < 2; i++)
    for (j = ~0ULL / 2 - 32; j < i * ((~0ULL / 2) + 6); j++)
      {
	if (i != 1 || j < ~0ULL / 2 - 32 || j >= ((~0ULL / 2) + 6) || k[0][j - ~0ULL / 2 + 64] != 1)
	  abort ();
	k[0][j - ~0ULL / 2 + 64]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 2 || j != ((~0ULL / 2) + 6) || x != 1028 || niters != 38)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i != 1 || j < ~0ULL / 2 - 32 || j >= ((~0ULL / 2) + 6) || k[0][j - ~0ULL / 2 + 64] != 2)
	  abort ();
	k[0][j - ~0ULL / 2 + 64]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 2 || j != ((~0ULL / 2) + 6) || x != 1028 || niters != 38)
    abort ();
  for (j = ~0ULL / 2 - 32; j < ((~0ULL / 2) + 6); j++)
    if (k[0][j - ~0ULL / 2 + 64] == 3)
      k[0][j - ~0ULL / 2 + 64] = 0;
    else
      abort ();
  for (i = 1; i < 4; i++)
    for (j = 64ULL * i; j < i * 32ULL + 110; j++)
      k[i][j] = 1;
  a = 1; b = 4; c = 1; d = 64ULL; e = 0; f = 32ULL; g = 110ULL; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 1; i < 4; i++)
    for (j = 64ULL * i; j < i * 32ULL + 110; j++)
      {
	if (i < 1 || i >= 4 || j < 64ULL * i || j >= i * 32ULL + 110 || k[i][j] != 1)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 4 || j != 206 || x != 3277 || niters != 138)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i < 1 || i >= 4 || j < 64ULL * i || j >= i * 32ULL + 110 || k[i][j] != 2)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 4 || j != 206 || x != 3277 || niters != 138)
    abort ();
  for (i = 1; i < 4; i++)
    for (j = 64ULL * i; j < i * 32ULL + 110; j++)
      if (k[i][j] == 3)
	k[i][j] = 0;
      else
	abort ();
  return 0;
}
