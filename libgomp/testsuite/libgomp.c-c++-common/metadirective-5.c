/* { dg-do run } */

#define N 100

#include <stdio.h>
#include <omp.h>

int
f (int a[], int num)
{
  int on_device = 0;
  int i;

  #pragma omp metadirective \
      when (target_device={device_num(num), kind("gpu")}: \
	target parallel for map(to: a[0:N]), map(from: on_device)) \
      default (parallel for private (on_device))
    for (i = 0; i < N; i++)
      {
	a[i] += i;
	on_device = 1;
      }

  return on_device;
}

int
main (void)
{
  int a[N];
  int on_device_count = 0;
  int i;

  for (i = 0; i < N; i++)
    a[i] = i;

  for (i = 0; i <= omp_get_num_devices (); i++)
    on_device_count += f (a, i);

  if (on_device_count != omp_get_num_devices ())
    return 1;

  for (i = 0; i < N; i++)
    if (a[i] != 2 * i)
      return 2;

  return 0;
}
