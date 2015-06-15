/* PR debug/36617 */
/* { dg-do run } */
/* { dg-options "-g -O0" } */

int
f1 (void)
{
  int v1i, v1j, v1k, v1l = 0;
  v1i = 6;
  v1j = 8;
  #pragma omp parallel private (v1k) firstprivate (v1j) shared (v1i) reduction (+:v1l)
  {
    v1k = v1i + v1j;
    {
      int v1m = 1;
      v1l = v1m;
    }
  }
  return v1l;
}

int v2k = 9;

int
f2 (void)
{
  int v2i = 6, v2j = 7;
  #pragma omp single private (v2i) firstprivate (v2k)
  {
    int v2l = v2j + v2k;
    v2i = 8;
    v2k = 10;
    v2j = v2l + v2i;
  }
  return v2i + v2j;
}

int
f3 (void)
{
  int v3i = 6, v3j = 7, v3k = 9;
  #pragma omp parallel
  {
    #pragma omp master
      v3i++;
    #pragma omp single private (v3i) firstprivate (v3k)
    {
      int v3l = v3j + v3k;
      v3i = 8;
      v3k = 10;
      v3j = v3l + v3i;
    }
    #pragma omp atomic
      v3k++;
  }
  return v3i + v3j;
}

int v4k = 9, v4l = 0;

int
f4 (void)
{
  int v4i = 6, v4j = 7, v4n = 0;
  #pragma omp sections private (v4i) firstprivate (v4k) reduction (+:v4l)
  {
    #pragma omp section
    {
      int v4m = v4j + v4k;
      v4i = 8;
      v4k = 10;
      v4l++;
      v4n = v4m + v4i;
    }
    #pragma omp section
    {
      int v4o = v4j + v4k;
      v4i = 10;
      v4k = 11;
      v4l++;
    }
  }
  return v4i + v4j + v4l + v4n;
}

int
f5 (void)
{
  int v5i = 6, v5j = 7, v5k = 9, v5l = 0, v5n = 0, v5p = 0;
  #pragma omp parallel
  {
    #pragma omp master
      v5p++;
    #pragma omp sections private (v5i) firstprivate (v5k) reduction (+:v5l)
    {
      #pragma omp section
      {
	int v5m = v5j + v5k;
	v5i = 8;
	v5k = 10;
	v5l++;
	v5n = v5m + v5i;
      }
      #pragma omp section
      {
	int v5o = v5j + v5k;
	v5i = 10;
	v5k = 11;
	v5l++;
      }
    }
  }
  return v5i + v5j + v5l + v5n + v5p;
}

int v6k = 9, v6l = 0;

int
f6 (void)
{
  int v6i = 6, v6j = 7, v6n = 0;
  #pragma omp for private (v6i) firstprivate (v6k) reduction (+:v6l)
  for (v6n = 0; v6n < 3; v6n++)
    {
      int v6m = v6j + v6k;
      v6i = 8;
      v6l++;
    }
  return v6i + v6j + v6k + v6l + v6n;
}

int
f7 (void)
{
  int v7i = 6, v7j = 7, v7k = 9, v7l = 0, v7n = 0, v7o = 1;
  #pragma omp parallel
  {
    #pragma omp master
      v7o++;
    #pragma omp for private (v7i) firstprivate (v7k) reduction (+:v7l)
    for (v7n = 0; v7n < 3; v7n++)
      {
	int v7m = v7j + v7k;
	v7i = 8;
	v7l++;
      }
  }
  return v7i + v7j + v7k + v7l + v7n;
}

int
main (void)
{
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  f6 ();
  f7 ();
  return 0;
}
