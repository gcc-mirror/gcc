/* { dg-do run } */

extern void abort (void);

int x, i, j;
volatile int a, b, c, d, e, f, g, h;
int k[16][67];

int
main ()
{
  int niters;
  for (i = 0; i < 16; i++)
    for (j = i * 2 + 1; j < 4 * i + 3; j++)
      k[i][j] = 1;
  a = 0; b = 16; c = 1; d = 2; e = 1; f = 4; g = 3; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 0; i < 16; i++)
    for (j = i * 2 + 1; j < 4 * i + 3; j++)
      {
	if (i < 0 || i >= 16 || j < 2 * i + 1 || j >= 3 + i * 4 || k[i][j] != 1)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 16 || j != 63 || x != 15422 || niters != 272)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i < 0 || i >= 16 || j < 2 * i + 1 || j >= 3 + i * 4 || k[i][j] != 2)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 16 || j != 63 || x != 15422 || niters != 272)
    abort ();
  for (i = 0; i < 16; i++)
    for (j = i * 2 + 1; j < 4 * i + 3; j++)
      if (k[i][j] == 3)
	k[i][j] = 0;
      else
	abort ();
  for (i = 0; i < 16; i++)
    for (j = i * 2 + 1; j < 2 * i + 7; j++)
      k[i][j] = 1;
  a = 0; b = 16; c = 1; d = 2; e = 1; f = 2; g = 7; h = 1;
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = 0; i < 16; i++)
    for (j = i * 2 + 1; j < 2 * i + 7; j++)
      {
	if (i < 0 || i >= 16 || j < 2 * i + 1 || j >= 7 + i * 2 || k[i][j] != 1)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 16 || j != 37 || x != 15396 || niters != 96)
    abort ();
  niters = 0; i = -100; j = -100; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d * i + e; j < g + i * f; j += h)
      {
	if (i < 0 || i >= 16 || j < 2 * i + 1 || j >= 7 + i * 2 || k[i][j] != 2)
	  abort ();
	k[i][j]++;
	x = i * 1024 + (j & 1023);
	niters++;
      }
  if (i != 16 || j != 37 || x != 15396 || niters != 96)
    abort ();
  for (i = 0; i < 16; i++)
    for (j = i * 2 + 1; j < 2 * i + 7; j++)
      if (k[i][j] == 3)
	k[i][j] = 0;
      else
	abort ();
  return 0;
}
