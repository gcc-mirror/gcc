/* { dg-do run { target { openacc_nvidia_accel_selected && lto } } } */
/* { dg-additional-options "-flto -fno-use-linker-plugin" } */

/* Worker and vector size checks.  Picked an outrageously large
   value.  */

int main ()
{
#pragma acc parallel num_workers (2<<20) /* { dg-error "using num_workers" } */
  {
  }

#pragma acc parallel vector_length (2<<20) /* { dg-error "using vector_length" } */
  {
  }

  return 0;
}

