#define N 30
#define M 3

int a[N][M], b[N][M], c[N][M];

int
main()
{
  int x, y, shift;
  int j = 0;
  for (int i = 0; i < N; i++)
    {
      a[i][0] = (i+1)*32;
      a[i][1] = (i+1)*17;
      a[i][2] = (i+1)*11;
      b[i][0] = (i+1)*7;
      b[i][1] = (i+1)*5;
      b[i][2] = (i+1)*3;
    }

  x = 0;
  #pragma omp parallel for simd collapse(2) reduction(inscan,+: x) private(shift)
  for (int i = 0; i < N; i++)
    for (int j = 0; j < M; j++)
      {
	x += a[i][j];
	x += b[i][j];
        #pragma omp scan inclusive(x)
        shift = i + 29*j;
        c[i][j] = x + shift;
      }

  y = 0;
  for (int i = 0; i < N; i++)
    for (int j = 0; j < M; j++)
      {
	y += a[i][j] + b[i][j];
	if (c[i][j] != y + i + 29*j)
	  __builtin_abort ();
      }
  if (x != y)
    __builtin_abort ();

  x = 0;
  #pragma omp parallel for simd collapse(2) reduction(inscan,+: x) private(shift)
  for (int i = 0; i < N; i++)
    for (int j = 0; j < M; j++)
      {
	shift = i + 29*j;
	c[i][j] = x + shift;
	#pragma omp scan exclusive(x)
	x += a[i][j];
	x += b[i][j];
      }

  y = 0;
  for (int i = 0; i < N; i++)
    for (int j = 0; j < M; j++)
      {
	if (c[i][j] != y + i + 29*j)
	  __builtin_abort ();
	y += a[i][j] + b[i][j];
      }
  if (x != y)
    __builtin_abort ();

  return 0;
}
