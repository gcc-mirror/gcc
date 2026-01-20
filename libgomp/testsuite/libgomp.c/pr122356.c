#include <omp.h>

void abort ();

#define NUM_THREADS 8
unsigned full_data[NUM_THREADS] = {0};
#pragma omp declare target enter(full_data)

void
test ()
{
#pragma omp parallel num_threads(8)
  {
#pragma omp for
    for (int i = 0; i < 10; i++)
#pragma omp task
      {
	full_data[omp_get_thread_num ()] += 1;
      }
#pragma omp barrier

    unsigned total = 0;
    for (int i = 0; i < NUM_THREADS; i++)
      total += full_data[i];

    if (total != 10)
      abort ();
  }
}
#pragma omp declare target enter(test)


int
main ()
{
  test ();

  #pragma omp target
    test ();
}
