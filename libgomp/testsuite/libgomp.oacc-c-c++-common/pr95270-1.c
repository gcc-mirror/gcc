/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <openacc.h>
#include <stdint.h>

int
main ()
{
  int data;
  int *data_p_dev = (int *) acc_create (&data, sizeof data);
  int *data_p = &data;
  uintptr_t ptrbits;

  acc_copyin (&data_p, sizeof data_p);

  /* Test attach/detach directives.  */
#pragma acc enter data attach(data_p)
#pragma acc serial copyout(ptrbits) /* { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } } */
  {
    ptrbits = (uintptr_t) data_p;
  }
#pragma acc exit data detach(data_p)
  assert ((void *) ptrbits == data_p_dev);

  acc_update_self (&data_p, sizeof data_p);
  assert (data_p == &data);

  /* Test attach/detach API call.  */
  acc_attach ((void **) &data_p);
#pragma acc serial copyout(ptrbits) /* { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } } */
  {
    ptrbits = (uintptr_t) data_p;
  }
  acc_detach ((void **) &data_p);

  assert ((void *) ptrbits == data_p_dev);
  acc_update_self (&data_p, sizeof data_p);
  assert (data_p == &data);

  acc_delete (&data_p, sizeof data_p);
  acc_delete (&data, sizeof data);

  return 0;
}

