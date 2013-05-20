/* { dg-do run } */

volatile int ji = 100, ki = 2;
volatile unsigned int ju = 100, ku = 2;
volatile long long int jll = 100, kll = 2;
volatile unsigned long long int jull = 100, kull = 2;
unsigned long long l;

void
f0 (void)
{
  int i, j, k;
  unsigned int j2, k2;
  #pragma omp for reduction(+: l) schedule(static, 2)
  for (i = ji; i < ki; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) schedule(static, 2)
  for (i = ji; i < ki; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = ji; i < ki; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = ji; i < ki; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = ji; i < ki; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = ji; i < ki; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = ji; i < ki; i++)
      for (k = ki + 10; k < ji - 10; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = ki + 10; j < ji - 10; j++)
    for (i = ji; i < ki; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
}

void
f1 (void)
{
  unsigned int i, j, k;
  int j2, k2;
  #pragma omp for reduction(+: l) schedule(static, 2)
  for (i = ju; i < ku; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) schedule(static, 2)
  for (i = ju; i < ku; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = ju; i < ku; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = ju; i < ku; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = ju; i < ku; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = ju; i < ku; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = ju; i < ku; i++)
      for (k = ku; k < ju; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = ku; j < ju; j++)
    for (i = ju; i < ku; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
}

void
f2 (void)
{
  long long int i, j, k;
  unsigned long long int j2, k2;
  #pragma omp for reduction(+: l) schedule(static, 2)
  for (i = jll; i < kll; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) schedule(static, 2)
  for (i = jll; i < kll; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = jll; i < kll; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = jll; i < kll; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = jll; i < kll; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = jll; i < kll; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = jll; i < kll; i++)
      for (k = kll; k < jll; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = kll; j < jll; j++)
    for (i = jll; i < kll; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
}

void
f3 (void)
{
  unsigned long long int i, j, k;
  long long int j2, k2;
  #pragma omp for reduction(+: l) schedule(static, 2)
  for (i = jull; i < kull; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) schedule(static, 2)
  for (i = jull; i < kull; i++)
    l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = jull; i < kull; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = jull; i < kull; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = jull; i < kull; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j2 = 0; j2 < 4; j2++)
    for (i = jull; i < kull; i++)
      for (k2 = 0; k2 < 5; k2 += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = 0; j < 4; j++)
    for (i = jull; i < kull; i++)
      for (k = kull; k < jull; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
  #pragma omp parallel for reduction(+: l) collapse(3) schedule(static, 2)
  for (j = kull; j < jull; j++)
    for (i = jull; i < kull; i++)
      for (k = 0; k < 5; k += 2)
	l++;
  if (l != 0)
    __builtin_abort ();
}

int
main ()
{
  f0 ();
  f1 ();
  f2 ();
  f3 ();
  return 0;
}
