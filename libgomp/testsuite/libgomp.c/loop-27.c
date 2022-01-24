/* { dg-do run } */

extern void abort (void);

int x;
short *i, *j;
int ii, jj;
short *volatile a;
short *volatile b;
volatile int c, e, f, g;
short *volatile d;
int k[11][20];
short v[84];

int
main ()
{
  int niters;
  for (ii = 1; ii <= 10; ii++)
    for (jj = 1; jj <= ii + 5; jj++)
      k[ii][jj] = 1;
  a = &v[42 + 1]; b = &v[42 + 11]; c = 1; d = &v[42 + 1]; e = 6; f = 1;
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = &v[42 + 1]; i <= &v[42 + 10]; i++)
    for (j = &v[42 + 1]; j <= i + 5; j++)
      {
	if (i < &v[42 + 1] || i > &v[42 + 10] || j < &v[42 + 1] || j > i + 5 || k[i - &v[42]][j - &v[42]] != 1)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 11] || j != &v[42 + 16] || x != 10255 || niters != 105)
    abort ();
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d; j < e + i; j += f)
      {
	if (i < &v[42 + 1] || i > &v[42 + 10] || j < &v[42 + 1] || j > i + 5 || k[i - &v[42]][j - &v[42]] != 2)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 11] || j != &v[42 + 16] || x != 10255 || niters != 105)
    abort ();
  for (ii = 1; ii <= 10; ii++)
    for (jj = 1; jj <= ii + 5; jj++)
      if (k[ii][jj] == 3)
	k[ii][jj] = 0;
      else
	abort ();
  for (ii = 0; ii < 11; ii++)
    for (jj = 0; jj < 20; jj++)
      if (k[ii][jj] != 0)
	abort ();
  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < ii; jj++)
      k[ii][jj] = 1;
  a = &v[42]; b = &v[42 + 10]; c = 1; d = &v[42]; e = 0; f = 1;
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = &v[42]; i < &v[42 + 10]; i++)
    for (j = &v[42]; j < i; j++)
      {
	if (i < &v[42] || i >= &v[42 + 10] || j < &v[42] || j >= i || k[i - &v[42]][j - &v[42]] != 1)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 10] || j != &v[42 + 9] || x != 9224 || niters != 45)
    abort ();
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d; j < i - e; j += f)
      {
	if (i < &v[42] || i >= &v[42 + 10] || j < &v[42] || j >= i || k[i - &v[42]][j - &v[42]] != 2)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 10] || j != &v[42 + 9] || x != 9224 || niters != 45)
    abort ();
  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < ii; jj++)
      if (k[ii][jj] == 3)
	k[ii][jj] = 0;
      else
	abort ();
  for (ii = 0; ii < 11; ii++)
    for (jj = 0; jj < 20; jj++)
      if (k[ii][jj] != 0)
	abort ();
  for (ii = 0; ii < 10; ii++)
    for (jj = ii + 1; jj < ii + 4; jj++)
      k[ii][jj] = 1;
  a = &v[42]; b = &v[42 + 10]; c = 1; e = 1; f = 1; g = -4;
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = &v[42]; i < &v[42 + 10]; i++)
    for (j = i + 1; j < i + 4; j++)
      {
	if (i < &v[42] || i >= &v[42 + 10] || j < i + 1 || j >= i + 4 || k[i - &v[42]][j - &v[42]] != 1)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 10] || j != &v[42 + 13] || x != 9228 || niters != 30)
    abort ();
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = i + e; j < i - g; j += f)
      {
	if (i < &v[42] || i >= &v[42 + 10] || j < i + 1 || j >= i + 4 || k[i - &v[42]][j - &v[42]] != 2)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 10] || j != &v[42 + 13] || x != 9228 || niters != 30)
    abort ();
  for (ii = 0; ii < 10; ii++)
    for (jj = ii + 1; jj < ii + 4; jj++)
      if (k[ii][jj] == 3)
	k[ii][jj] = 0;
      else
	abort ();
  for (ii = 0; ii < 11; ii++)
    for (jj = 0; jj < 20; jj++)
      if (k[ii][jj] != 0)
	abort ();
  for (ii = 1; ii < 10; ii += 2)
    for (jj = 1; jj < ii + 1; jj++)
      k[ii][jj] = 1;
  a = &v[42 + 1]; b = &v[42 + 10]; c = 2; d = &v[42 + 1]; e = 1; f = 1;
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = &v[42 + 1]; i < &v[42 + 10]; i += 2)
    for (j = &v[42 + 1]; j < i + 1; j++)
      {
	if (i < &v[42 + 1] || i >= &v[42 + 10] || j < &v[42 + 1] || j >= i + 1 || k[i - &v[42]][j - &v[42]] != 1)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 11] || j != &v[42 + 10] || x != 9225 || niters != 25)
    abort ();
  niters = 0; i = &v[0]; j = &v[0]; x = -100;
  #pragma omp parallel for collapse(2) lastprivate (i, j, x) reduction(+:niters)
  for (i = a; i < b; i += c)
    for (j = d; j < i + e; j += f)
      {
	if (i < &v[42 + 1] || i >= &v[42 + 10] || j < &v[42 + 1] || j >= i + 1 || k[i - &v[42]][j - &v[42]] != 2)
	  abort ();
	k[i - &v[42]][j - &v[42]]++;
	x = (i - &v[42]) * 1024 + ((j - &v[42]) & 1023);
	niters++;
      }
  if (i != &v[42 + 11] || j != &v[42 + 10] || x != 9225 || niters != 25)
    abort ();
  for (ii = 1; ii < 10; ii += 2)
    for (jj = 1; jj < ii + 1; jj++)
      if (k[ii][jj] == 3)
	k[ii][jj] = 0;
      else
	abort ();
  for (ii = 0; ii < 11; ii++)
    for (jj = 0; jj < 20; jj++)
      if (k[ii][jj] != 0)
	abort ();
  return 0;
}
