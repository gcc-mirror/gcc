/* PR middle-end/110270 */

/* Ensure that defaultmap(default : pointer) uses correct OpenMP 5.2
   semantic, i.e. keeping the pointer value even if not mapped;
   before OpenMP 5.0/5.1 required that it is NULL, causing issues
   especially with unified-shared memory but also the code below
   shows why that's not a good idea.  */

#include <stdio.h>
#include <stdint.h>
#include <omp.h>

/* 'unified_address' is required by the OpenMP spec as only then
   'is_device_ptr' can be left out.  All our devices support this
   while remote offloading would not.  However, in practice it is
   sufficient that the host and device pointer size is the same
   (or the device pointer is smaller) - and then a device pointer is
   representable and omp_target_alloc can return a bare device pointer.

   We here assume that this weaker condition holds and do not
   require:   #pragma omp requires unified_address  */

void
test_device (int dev)
{
  int *p1 = (int*) 0x12345;
  int *p1a = (int*) 0x67890;
  int *p2 = (int*) omp_target_alloc (sizeof (int) * 5, dev);
  int *p2a = (int*) omp_target_alloc (sizeof (int) * 10, dev);
  intptr_t ip = (intptr_t) p2;
  intptr_t ipa = (intptr_t) p2a;

  int A[3] = {1,2,3};
  int B[5] = {4,5,6,7,8};
  int *p3 = &A[0];
  int *p3a = &B[0];

  #pragma omp target enter data map(to:A) device(dev)

  #pragma omp target device(dev) /* defaultmap(default:pointer) */
  {
    /* The pointees aren't mapped. */
    /* OpenMP 5.2 -> same value as before the target region. */
    if ((intptr_t) p1 != 0x12345) __builtin_abort ();
    if ((intptr_t) p2 != ip) __builtin_abort ();
    for (int i = 0; i < 5; i++)
      p2[i] = 13*i;

    for (int i = 0; i < 10; i++)
      ((int *)ipa)[i] = 7*i;

    /* OpenMP: Mapped => must point to the corresponding device storage of 'A' */
    if (p3[0] != 1 || p3[1] != 2 || p3[2] != 3)
      __builtin_abort ();
    p3[0] = -11; p3[1] = -22; p3[2] = -33;
  }
  #pragma omp target exit data map(from:A) device(dev)

  if (p3[0] != -11 || p3[1] != -22 || p3[2] != -33)
    __builtin_abort ();

  // With defaultmap:

  #pragma omp target enter data map(to:B) device(dev)

  #pragma omp target device(dev) defaultmap(default:pointer)
  {
    /* The pointees aren't mapped. */
    /* OpenMP 5.2 -> same value as before the target region. */
    if ((intptr_t) p1a != 0x67890) __builtin_abort ();
    if ((intptr_t) p2a != ipa) __builtin_abort ();

    for (int i = 0; i < 5; i++)
      ((int *)ip)[i] = 13*i;

    for (int i = 0; i < 10; i++)
      p2a[i] = 7*i;

    /* OpenMP: Mapped => must point to the corresponding device storage of 'B' */
    if (p3a[0] != 4 || p3a[1] != 5 || p3a[2] != 6 || p3a[3] != 7 || p3a[4] != 8)
      __builtin_abort ();
    p3a[0] = -44; p3a[1] = -55; p3a[2] = -66; p3a[3] = -77; p3a[4] = -88;
  }
  #pragma omp target exit data map(from:B) device(dev)

  if (p3a[0] != -44 || p3a[1] != -55 || p3a[2] != -66 || p3a[3] != -77 || p3a[4] != -88)
    __builtin_abort ();

  omp_target_free (p2, dev);
  omp_target_free (p2a, dev);
}

int
main()
{
  int ntgts = omp_get_num_devices();
  if (ntgts)
    fprintf (stderr, "Offloading devices exist\n");  /* { dg-output "Offloading devices exist(\n|\r\n|\r)" { target offload_device } } */
  else
    fprintf (stderr, "Only host fallback\n");        /* { dg-output "Only host fallback(\n|\r\n|\r)" { target { ! offload_device } } } */

  for (int i = 0; i <= ntgts; i++)
    test_device (i);
  return 0;
}
