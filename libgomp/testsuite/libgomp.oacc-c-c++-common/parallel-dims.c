/* { dg-do run { target openacc_nvidia_accel_selected } } */

/* Worker and vector size checks.  Picked an outrageously large
   value. */

int main ()
{
  int dummy[10];

#pragma acc parallel num_workers (2<<20) /* { dg-error "using num_workers" } */
  {
#pragma acc loop worker
    for (int  i = 0; i < 10; i++)
      dummy[i] = i;
  }

#pragma acc parallel vector_length (2<<20) /* { dg-error "using vector_length" } */
  {
#pragma acc loop vector
    for (int  i = 0; i < 10; i++)
      dummy[i] = i;
  }

  return 0;
}
