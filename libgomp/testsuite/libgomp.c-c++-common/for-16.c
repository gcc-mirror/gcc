extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

unsigned long long int k = 16;
#pragma omp declare target to (k)

int
main ()
{
  unsigned char a[144], b[144], c[144];
  int l;
  #pragma omp target map(from:a, b, c)
  {
    int i;
    unsigned long long int j;
    #pragma omp parallel for schedule (runtime)
    for (i = 0; i < 16; i++)
      a[i] = i;
    #pragma omp parallel for schedule (monotonic: runtime)
    for (i = 0; i < 16; i++)
      a[i + 16] = i + 16;
    #pragma omp parallel for schedule (nonmonotonic: runtime)
    for (i = 0; i < 16; i++)
      a[i + 32] = i + 32;
    #pragma omp parallel
    {
      #pragma omp for schedule (runtime)
      for (i = 0; i < 16; i++)
        a[i + 48] = i + 48;
      #pragma omp for schedule (monotonic: runtime)
      for (i = 0; i < 16; i++)
        a[i + 64] = i + 64;
      #pragma omp for schedule (nonmonotonic: runtime)
      for (i = 0; i < 16; i++)
        a[i + 80] = i + 80;
      #pragma omp for schedule (runtime)
      for (j = 0; j < k; j++)
        a[j + 96] = j + 96;
      #pragma omp for schedule (monotonic: runtime)
      for (j = 0; j < k; j++)
        a[j + 112] = j + 112;
      #pragma omp for schedule (nonmonotonic: runtime)
      for (j = 0; j < k; j++)
        a[j + 128] = j + 128;
    }
    #pragma omp parallel for schedule (dynamic)
    for (i = 0; i < 16; i++)
      b[i] = i;
    #pragma omp parallel for schedule (monotonic: dynamic)
    for (i = 0; i < 16; i++)
      b[i + 16] = i + 16;
    #pragma omp parallel for schedule (nonmonotonic: dynamic)
    for (i = 0; i < 16; i++)
      b[i + 32] = i + 32;
    #pragma omp parallel
    {
      #pragma omp for schedule (dynamic)
      for (i = 0; i < 16; i++)
        b[i + 48] = i + 48;
      #pragma omp for schedule (monotonic: dynamic)
      for (i = 0; i < 16; i++)
        b[i + 64] = i + 64;
      #pragma omp for schedule (nonmonotonic: dynamic)
      for (i = 0; i < 16; i++)
        b[i + 80] = i + 80;
      #pragma omp for schedule (dynamic)
      for (j = 0; j < k; j++)
        b[j + 96] = j + 96;
      #pragma omp for schedule (monotonic: dynamic)
      for (j = 0; j < k; j++)
        b[j + 112] = j + 112;
      #pragma omp for schedule (nonmonotonic: dynamic)
      for (j = 0; j < k; j++)
        b[j + 128] = j + 128;
    }
    #pragma omp parallel for schedule (guided)
    for (i = 0; i < 16; i++)
      c[i] = i;
    #pragma omp parallel for schedule (monotonic: guided)
    for (i = 0; i < 16; i++)
      c[i + 16] = i + 16;
    #pragma omp parallel for schedule (nonmonotonic: guided)
    for (i = 0; i < 16; i++)
      c[i + 32] = i + 32;
    #pragma omp parallel
    {
      #pragma omp for schedule (guided)
      for (i = 0; i < 16; i++)
        c[i + 48] = i + 48;
      #pragma omp for schedule (monotonic: guided)
      for (i = 0; i < 16; i++)
        c[i + 64] = i + 64;
      #pragma omp for schedule (nonmonotonic: guided)
      for (i = 0; i < 16; i++)
        c[i + 80] = i + 80;
      #pragma omp for schedule (guided)
      for (j = 0; j < k; j++)
        c[j + 96] = j + 96;
      #pragma omp for schedule (monotonic: guided)
      for (j = 0; j < k; j++)
        c[j + 112] = j + 112;
      #pragma omp for schedule (nonmonotonic: guided)
      for (j = 0; j < k; j++)
        c[j + 128] = j + 128;
    }
  }
  for (l = 0; l < 144; ++l)
    if (a[l] != l || b[l] != l || c[l] != l)
      abort ();
  return 0;
}
