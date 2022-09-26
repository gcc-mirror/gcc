extern void abort (void);

#define N 256
int a[N], b[N / 16][8][4], c[N / 32][8][8], g[N / 16][8][6];
volatile int d, e;
volatile unsigned long long f;

int
main ()
{
  unsigned long long i;
  int j, k, l, m;
  #pragma omp parallel private (l)
  {
    #pragma omp for schedule(static, 1) ordered nowait
    for (i = 1; i < N + f; i++)
      {
	#pragma omp atomic write
	a[i] = 1;
	#pragma omp ordered doacross(sink: i - 1)
	if (i > 1)
	  {
	    #pragma omp atomic read
	    l = a[i - 1];
	    if (l < 2)
	      abort ();
	  }
	#pragma omp atomic write
	a[i] = 2;
	if (i < N - 1)
	  {
	    #pragma omp atomic read
	    l = a[i + 1];
	    if (l == 3)
	      abort ();
	  }
	#pragma omp ordered doacross(source : omp_cur_iteration)
	#pragma omp atomic write
	a[i] = 3;
      }
    #pragma omp for schedule(static) ordered (3) nowait
    for (i = 3; i < N / 16 - 1 + f; i++)
      for (j = 0; j < 8; j += 2)
	for (k = 1; k <= 3; k++)
	  {
	    #pragma omp atomic write
	    b[i][j][k] = 1;
	    #pragma omp ordered doacross(sink: i, j - 2, k - 1) \
				doacross(sink: i - 2, j - 2, k + 1)
	    #pragma omp ordered doacross(sink: i - 3, j + 2, k - 2)
	    if (j >= 2 && k > 1)
	      {
		#pragma omp atomic read
		l = b[i][j - 2][k - 1];
		if (l < 2)
		  abort ();
	      }
	    #pragma omp atomic write
	    b[i][j][k] = 2;
	    if (i >= 5 && j >= 2 && k < 3)
	      {
		#pragma omp atomic read
		l = b[i - 2][j - 2][k + 1];
		if (l < 2)
		  abort ();
	      }
	    if (i >= 6 && j < N / 16 - 3 && k == 3)
	      {
		#pragma omp atomic read
		l = b[i - 3][j + 2][k - 2];
		if (l < 2)
		  abort ();
	      }
	    #pragma omp ordered doacross(source : )
	    #pragma omp atomic write
	    b[i][j][k] = 3;
	  }
#define A(n) int n;
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3)
#define C(n) B(n##0) B(n##1) B(n##2) B(n##3)
#define D(n) C(n##0) C(n##1) C(n##2) C(n##3)
    D(m)
#undef A
    #pragma omp for collapse (2) ordered(61) schedule(dynamic, 15)
    for (i = 2; i < N / 32 + f; i++)
      for (j = 7; j > 1; j--)
	for (k = 6; k >= 0; k -= 2)
#define A(n) for (n = 4; n < 5; n++)
	  D(m)
#undef A
	    {
	      #pragma omp atomic write
	      c[i][j][k] = 1;
#define A(n) ,n
#define E(n) C(n##0) C(n##1) C(n##2) B(n##30) B(n##31) A(n##320) A(n##321)
	      #pragma omp ordered doacross (sink: i, j, k + 2 E(m)) \
				  doacross (sink:i - 2, j + 1, k - 4 E(m)) \
				  doacross(sink: i - 1, j - 2, k - 2 E(m))
	      if (k <= 4)
		{
		  #pragma omp atomic read
		  l = c[i][j][k + 2];
		  if (l < 2)
		    abort ();
		}
	      #pragma omp atomic write
	      c[i][j][k] = 2;
	      if (i >= 4 && j < 7 && k >= 4)
		{
		  #pragma omp atomic read
		  l = c[i - 2][j + 1][k - 4];
		  if (l < 2)
		    abort ();
		}
	      if (i >= 3 && j >= 4 && k >= 2)
		{
		  #pragma omp atomic read
		  l = c[i - 1][j - 2][k - 2];
		  if (l < 2)
		    abort ();
		}
	      #pragma omp ordered doacross (source : omp_cur_iteration)
	      #pragma omp atomic write
	      c[i][j][k] = 3;
	    }
    #pragma omp for schedule(static) ordered (3) nowait
    for (j = 0; j < N / 16 - 1; j++)
      for (k = 0; k < 8; k += 2)
	for (i = 3; i <= 5 + f; i++)
	  {
	    #pragma omp atomic write
	    g[j][k][i] = 1;
	    #pragma omp ordered doacross(sink: j, k - 2, i - 1) \
				doacross(sink: j - 2, k - 2, i + 1)
	    #pragma omp ordered doacross(sink: j - 3, k + 2, i - 2)
	    if (k >= 2 && i > 3)
	      {
		#pragma omp atomic read
		l = g[j][k - 2][i - 1];
		if (l < 2)
		  abort ();
	      }
	    #pragma omp atomic write
	    g[j][k][i] = 2;
	    if (j >= 2 && k >= 2 && i < 5)
	      {
		#pragma omp atomic read
		l = g[j - 2][k - 2][i + 1];
		if (l < 2)
		  abort ();
	      }
	    if (j >= 3 && k < N / 16 - 3 && i == 5)
	      {
		#pragma omp atomic read
		l = g[j - 3][k + 2][i - 2];
		if (l < 2)
		  abort ();
	      }
	    #pragma omp ordered doacross(source :)
	    #pragma omp atomic write
	    g[j][k][i] = 3;
	  }
    #pragma omp for collapse(2) ordered(4) lastprivate (i, j, k)
    for (i = 2; i < f + 3; i++)
      for (j = d + 1; j >= 0; j--)
	for (k = 0; k < d; k++)
	  for (l = 0; l < d + 2; l++)
	    {
	      #pragma omp ordered doacross (source : omp_cur_iteration)
	      #pragma omp ordered doacross (sink:i - 2, j + 2, k - 2, l)
	      if (!e)
		abort ();
	    }
    #pragma omp single
    {
      if (i != 3 || j != -1 || k != 0)
	abort ();
      i = 8; j = 9; k = 10;
    }
    #pragma omp for collapse(2) ordered(4) lastprivate (i, j, k, m)
    for (i = 2; i < f + 3; i++)
      for (j = d + 1; j >= 0; j--)
	for (k = 0; k < d + 2; k++)
	  for (m = 0; m < d; m++)
	    {
	      #pragma omp ordered doacross (source:)
	      #pragma omp ordered doacross (sink:i - 2, j + 2, k - 2, m)
	      abort ();
	    }
    #pragma omp single
    if (i != 3 || j != -1 || k != 2 || m != 0)
      abort ();
    #pragma omp for collapse(2) ordered(4) nowait
    for (i = 2; i < f + 3; i++)
      for (j = d; j > 0; j--)
	for (k = 0; k < d + 2; k++)
	  for (l = 0; l < d + 4; l++)
	    {
	      #pragma omp ordered doacross (source : omp_cur_iteration)
	      #pragma omp ordered doacross (sink:i - 2, j + 2, k - 2, l)
	      if (!e)
		abort ();
	    }
    #pragma omp for nowait
    for (i = 0; i < N; i++)
      if (a[i] != 3 * (i >= 1))
	abort ();
    #pragma omp for collapse(2) private(k) nowait
    for (i = 0; i < N / 16; i++)
      for (j = 0; j < 8; j++)
	for (k = 0; k < 4; k++)
	  if (b[i][j][k] != 3 * (i >= 3 && i < N / 16 - 1 && (j & 1) == 0 && k >= 1))
	    abort ();
    #pragma omp for collapse(3) nowait
    for (i = 0; i < N / 32; i++)
      for (j = 0; j < 8; j++)
	for (k = 0; k < 8; k++)
	  if (c[i][j][k] != 3 * (i >= 2 && j >= 2 && (k & 1) == 0))
	    abort ();
    #pragma omp for collapse(2) private(k) nowait
    for (i = 0; i < N / 16; i++)
      for (j = 0; j < 8; j++)
	for (k = 0; k < 6; k++)
	  if (g[i][j][k] != 3 * (i < N / 16 - 1 && (j & 1) == 0 && k >= 3))
	    abort ();
  }
  return 0;
}
