/* Verify device memory allocation/deallocation
   { dg-additional-options "-DOPENACC_RUNTIME" } using OpenACC Runtime Library routines,
   { dg-additional-options "-DPOINTERS" } using pointers.  */

/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#if OPENACC_RUNTIME
#elif OPENACC_DIRECTIVES
#else
# error
#endif

#if POINTERS
#elif ARRAYS
#else
# error
#endif


#include <openacc.h>
#include <acc_prof.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>


static bool cb_ev_alloc_expected;
static size_t cb_ev_alloc_bytes;
static const void *cb_ev_alloc_device_ptr;
static void
cb_ev_alloc (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  assert (cb_ev_alloc_expected);
  cb_ev_alloc_expected = false;

  cb_ev_alloc_bytes = event_info->data_event.bytes;
  cb_ev_alloc_device_ptr = event_info->data_event.device_ptr;
}

static bool cb_ev_free_expected;
static const void *cb_ev_free_device_ptr;
static void
cb_ev_free (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  assert (cb_ev_free_expected);
  cb_ev_free_expected = false;

  cb_ev_free_device_ptr = event_info->data_event.device_ptr;
}


/* Match the alignment processing that
   'libgomp/target.c:gomp_map_vars_internal' is doing; simplified, not
   considering special alignment requirements of certain data types.  */

static size_t
aligned_size (size_t tgt_size)
{
  size_t tgt_align = sizeof (void *);
  return tgt_size + tgt_align - 1;
}

static const void *
aligned_address (const void *tgt_start)
{
  size_t tgt_align = sizeof (void *);
  return (void *) (((uintptr_t) tgt_start + tgt_align - 1) & ~(tgt_align - 1));
}


#define SIZE 1024
#define SUBSET 32


/* A "create", [...], "delete" sequence.  */

static void
f1 (void)
{
  cb_ev_alloc_expected = false;
  cb_ev_free_expected = false;
  acc_prof_register (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_register (acc_ev_free, cb_ev_free, acc_reg);

#if POINTERS
  char *h = (char *) malloc (SIZE);
#else
  char h[SIZE];
#endif

  void *d;
  cb_ev_alloc_expected = true;
#if OPENACC_RUNTIME
  d = acc_create (h, SIZE);
#else
# if POINTERS
#  pragma acc enter data create (h[0:SIZE])
# else
#  pragma acc enter data create (h)
# endif
  d = acc_deviceptr (h);
#endif
  assert (d);
  assert (!cb_ev_alloc_expected);
  assert (cb_ev_alloc_bytes == aligned_size (SIZE));
  assert (aligned_address (cb_ev_alloc_device_ptr) == d);
  assert (acc_is_present (h, SIZE));

#if OPENACC_RUNTIME
  acc_create (h, SIZE);
#else
# if POINTERS
#  pragma acc enter data create (h[0:SIZE])
# else
#  pragma acc enter data create (h)
# endif
#endif

#if POINTERS
# pragma acc data create (h[0:SIZE])
  ;
#else
# pragma acc data create (h)
  ;
#endif
  assert (acc_is_present (h, SIZE));

#if OPENACC_RUNTIME
  acc_delete (h, SIZE);
#else
# if POINTERS
#  pragma acc exit data delete (h[0:SIZE])
# else
#  pragma acc exit data delete (h)
# endif
#endif
  assert (acc_is_present (h, SIZE));

  cb_ev_free_expected = true;
#if OPENACC_RUNTIME
  acc_delete (h, SIZE);
#else
# if POINTERS
#  pragma acc exit data delete (h[0:SIZE])
# else
#  pragma acc exit data delete (h)
# endif
#endif
  assert (!cb_ev_free_expected);
  assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);
  assert (!acc_is_present (h, SIZE));

#if POINTERS
  free (h);
#endif

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);
}


/* A "map", [...] "unmap" sequence.  */

static void
f2 (void)
{
  cb_ev_alloc_expected = false;
  cb_ev_free_expected = false;
  acc_prof_register (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_register (acc_ev_free, cb_ev_free, acc_reg);

#if POINTERS
  char *h = (char *) malloc (SIZE);
#else
  char h[SIZE];
#endif

  void *d;
  cb_ev_alloc_expected = true;
  d = acc_malloc (SIZE);
  assert (d);
  assert (!cb_ev_alloc_expected);
  assert (cb_ev_alloc_bytes == SIZE);
  assert (cb_ev_alloc_device_ptr == d);

  acc_map_data (h, d, SIZE);
  assert (acc_is_present (h, SIZE));

#if OPENACC_RUNTIME
  acc_create (h, SIZE);
#else
# if POINTERS
#  pragma acc enter data create (h[0:SIZE])
# else
#  pragma acc enter data create (h)
# endif
#endif

#if POINTERS
# pragma acc data create (h[0:SIZE])
  ;
#else
# pragma acc data create (h)
  ;
#endif
  assert (acc_is_present (h, SIZE));

#if OPENACC_RUNTIME
  acc_delete (h, SIZE);
#else
# if POINTERS
#  pragma acc exit data delete (h[0:SIZE])
# else
#  pragma acc exit data delete (h)
# endif
#endif
  assert (acc_is_present (h, SIZE));

  acc_unmap_data (h);
  assert (!acc_is_present (h, SIZE));

  cb_ev_free_expected = true;
  acc_free (d);
  assert (!cb_ev_free_expected);
  assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);

#if POINTERS
  free (h);
#endif

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);
}


/* A structured 'data' construct.  */

static void
f3 (void)
{
  cb_ev_alloc_expected = false;
  cb_ev_free_expected = false;
  acc_prof_register (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_register (acc_ev_free, cb_ev_free, acc_reg);

#if POINTERS
  char *h = (char *) malloc (SIZE);
#else
  char h[SIZE];
#endif

  cb_ev_alloc_expected = true;
#if POINTERS
# pragma acc data create (h[0:SIZE])
#else
# pragma acc data create (h)
#endif
  {
    void *d = acc_deviceptr (h);
    assert (d);
    assert (!cb_ev_alloc_expected);
    assert (cb_ev_alloc_bytes == aligned_size (SIZE));
    assert (aligned_address (cb_ev_alloc_device_ptr) == d);
    assert (acc_is_present (h, SIZE));

#if OPENACC_RUNTIME
    acc_create (h, SIZE);
#else
# if POINTERS
#  pragma acc enter data create (h[0:SIZE])
# else
#  pragma acc enter data create (h)
# endif
#endif

#if POINTERS
# pragma acc data create (h[0:SIZE])
    ;
#else
# pragma acc data create (h)
    ;
#endif
    assert (acc_is_present (h, SIZE));

#if OPENACC_RUNTIME
    acc_delete (h, SIZE);
#else
# if POINTERS
#  pragma acc exit data delete (h[0:SIZE])
# else
#  pragma acc exit data delete (h)
# endif
#endif
    assert (acc_is_present (h, SIZE));

    cb_ev_free_expected = true;
  }
  assert (!cb_ev_free_expected);
  assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);
  assert (!acc_is_present (h, SIZE));

#if POINTERS
  free (h);
#endif

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);
}


int
main ()
{
  f1 ();
  f2 ();
  f3 ();

  return 0;
}
