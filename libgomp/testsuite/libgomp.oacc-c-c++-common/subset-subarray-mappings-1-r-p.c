/* Test "subset" subarray mappings
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


static void
f1 (void)
{
  cb_ev_alloc_expected = false;
  cb_ev_free_expected = false;
  acc_prof_register (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_register (acc_ev_free, cb_ev_free, acc_reg);

#if POINTERS
  char* myblock = (char *) malloc (SIZE);
#else
  char myblock[SIZE];
#endif
  int i;
  void *dst;
  for (i = 0; i < SIZE; i++)
    myblock[i] = i;

  cb_ev_alloc_expected = true;
#if OPENACC_RUNTIME
  dst = acc_copyin (myblock, SIZE);
#else
# if POINTERS
#  pragma acc enter data copyin (myblock[0:SIZE])
# else
#  pragma acc enter data copyin (myblock)
# endif
  dst = acc_deviceptr (myblock);
#endif
  assert (dst);
  assert (!cb_ev_alloc_expected);
  assert (cb_ev_alloc_bytes == aligned_size (SIZE));
  assert (aligned_address (cb_ev_alloc_device_ptr) == dst);
  for (i = 0; i < SIZE; i += SUBSET)
    {
      void *partdst = acc_deviceptr (&myblock[i]);
      assert ((uintptr_t) partdst == (uintptr_t) dst + i);
      assert (acc_hostptr (partdst) == &myblock[i]);
    }
  for (i = 0; i < SIZE; i += SUBSET)
    {
      void *partdst;
#if OPENACC_RUNTIME
      partdst = acc_pcopyin (&myblock[i], SUBSET);
#else
# pragma acc enter data pcopyin (myblock[i:SUBSET])
      partdst = acc_deviceptr (&myblock[i]);
#endif
      assert ((uintptr_t) partdst == (uintptr_t) dst + i);
    }
  /* Dereference first half.  */
  for (i = 0; i < 512; i += SUBSET)
    {
      assert (acc_is_present (&myblock[i], SUBSET));
      assert (acc_is_present (myblock, SIZE));
#if OPENACC_RUNTIME
      acc_delete (&myblock[i], SUBSET);
#else
# pragma acc exit data delete (myblock[i:SUBSET])
#endif
      assert (acc_is_present (&myblock[i], SUBSET));
      assert (acc_is_present (myblock, SIZE));
    }
  /* Dereference all.  */
#if OPENACC_RUNTIME
  acc_delete (myblock, SIZE);
#else
# if POINTERS
#  pragma acc exit data delete (myblock[0:SIZE])
# else
#  pragma acc exit data delete (myblock)
# endif
#endif
  /* Expect it's still present.  */
  assert (acc_is_present (myblock, SIZE));
  /* Dereference second half.  */
  for (i = 512; i < SIZE; i += SUBSET)
    {
      bool last = i >= SIZE - SUBSET;

      assert (acc_is_present (&myblock[i], SUBSET));
      assert (acc_is_present (myblock, SIZE));
      if (last)
	cb_ev_free_expected = true;
#if OPENACC_RUNTIME
      acc_delete (&myblock[i], SUBSET);
#else
# pragma acc exit data delete (myblock[i:SUBSET])
#endif
      assert (!cb_ev_free_expected);
      if (last)
	assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);
      assert (acc_is_present (&myblock[i], SUBSET) != last);
      assert (acc_is_present (myblock, SIZE) != last);
    }
  /* Expect it's all gone now.  */
  for (i = 512; i < SIZE; i += SUBSET)
    assert (!acc_is_present (&myblock[i], SUBSET));
  assert (!acc_is_present (myblock, SIZE));
  assert (!acc_is_present (myblock, 1));

#if POINTERS
  free (myblock);
#endif

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);
}


static void
f2 (void)
{
  cb_ev_alloc_expected = false;
  cb_ev_free_expected = false;
  acc_prof_register (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_register (acc_ev_free, cb_ev_free, acc_reg);

#if POINTERS
  char *block1 = (char *) malloc (SIZE);
  char *block2 = (char *) malloc (SIZE);
  char *block3 = (char *) malloc (SIZE);
#else
  char block1[SIZE];
  char block2[SIZE];
  char block3[SIZE];
#endif
  int i;
  for (i = 0; i < SIZE; i++)
    block1[i] = block2[i] = block3[i] = i;

  cb_ev_alloc_expected = true;
#if POINTERS
# pragma acc data copyin(block1[0:SIZE], block2[0:SIZE], block3[0:SIZE])
#else
# pragma acc data copyin(block1, block2, block3)
#endif
  {
    void *block1_d = acc_deviceptr (block1);
    void *block2_d = acc_deviceptr (block2);
    void *block3_d = acc_deviceptr (block3);
    assert (!cb_ev_alloc_expected);
    /* 'block1', 'block2', 'block3' get mapped in one device memory object, in
       reverse order.  */
    assert (cb_ev_alloc_bytes == aligned_size (3 * SIZE));
    assert ((void *) ((uintptr_t) aligned_address (cb_ev_alloc_device_ptr) + 2 * SIZE) == block1_d);
    assert ((void *) ((uintptr_t) aligned_address (cb_ev_alloc_device_ptr) + 1 * SIZE) == block2_d);
    assert ((void *) ((uintptr_t) aligned_address (cb_ev_alloc_device_ptr) + 0 * SIZE) == block3_d);

    for (i = 0; i < SIZE; i += SUBSET)
      {
	void *block2_part_d;
#if OPENACC_RUNTIME
	block2_part_d = acc_pcopyin (&block2[i], SUBSET);
#else
# pragma acc enter data pcopyin (block2[i:SUBSET])
	block2_part_d = acc_deviceptr (&block2[i]);
#endif
	assert ((uintptr_t) block2_part_d == (uintptr_t) block2_d + i);
      }
  }
  /* The mappings have been removed, but the device memory object has not yet
     been 'free'd.  */
  assert (!acc_is_present (block1, SIZE));
  assert (acc_is_present (block2, SIZE));
  assert (!acc_is_present (block3, SIZE));
  for (i = 0; i < SIZE; i += SUBSET)
    {
      bool last = i >= SIZE - SUBSET;

      assert (acc_is_present (block2, SIZE));
      if (last)
	cb_ev_free_expected = true;
#if OPENACC_RUNTIME
      acc_delete (&block2[i], SUBSET);
#else
# pragma acc exit data delete (block2[i:SUBSET])
#endif
      assert (!cb_ev_free_expected);
      if (last)
	assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);
    }
  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));
  assert (!acc_is_present (block3, SIZE));

#if POINTERS
  free (block1);
  free (block2);
  free (block3);
#endif

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);
}


static void
f3 ()
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

  char *d1;
  cb_ev_alloc_expected = true;
#if OPENACC_RUNTIME
  d1 = (char *) acc_present_or_create (h, SIZE);
#else
# if POINTERS
#  pragma acc enter data present_or_create (h[0:SIZE])
# else
#  pragma acc enter data present_or_create (h)
# endif
  d1 = (char *) acc_deviceptr (h);
#endif
  assert (d1);
  assert (!cb_ev_alloc_expected);
  assert (cb_ev_alloc_bytes == aligned_size (SIZE));
  assert (aligned_address (cb_ev_alloc_device_ptr) == d1);
  assert (acc_is_present (h, SIZE));
  assert (acc_is_present (&h[2], SIZE - 2));

  char *d2;
#if OPENACC_RUNTIME
  d2 = (char *) acc_present_or_create (&h[2], SIZE - 2);
#else
# pragma acc enter data present_or_create (h[2:SIZE - 2])
  d2 = (char *) acc_deviceptr (&h[2]);
#endif
  assert (d2);
  assert (d1 == d2 - 2);
  assert (acc_is_present (h, SIZE));
  assert (acc_is_present (&h[2], SIZE - 2));

  d2 = (char *) acc_deviceptr (&h[2]);
  assert (d1 == d2 - 2);

#if OPENACC_RUNTIME
  acc_delete (&h[2], SIZE - 2);
#else
# pragma acc exit data delete (h[2:SIZE - 2])
#endif
  assert (acc_is_present (h, SIZE));
  assert (acc_is_present (&h[2], SIZE - 2));

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
  assert (!acc_is_present (&h[2], SIZE - 2));
  assert (!acc_is_present (h, 1));

# if POINTERS
  free (h);
#endif

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);
}


/* Based on what used to be 'libgomp.oacc-c-c++-common/lib-22.c'.  */

static void
f_lib_22 (void)
{
  cb_ev_alloc_expected = false;
  cb_ev_free_expected = false;
  acc_prof_register (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_register (acc_ev_free, cb_ev_free, acc_reg);

  const int c0 = 0;
  const int c1 = 1;

#if POINTERS
  char *h = (char *) malloc (SIZE);
#else
  char h[SIZE];
#endif

  memset (h, c0, SIZE);
  void *d;
  cb_ev_alloc_expected = true;
#if OPENACC_RUNTIME
  d = acc_copyin (h, SIZE);
#else
# if POINTERS
#  pragma acc enter data copyin (h[0:SIZE])
# else
#  pragma acc enter data copyin (h)
# endif
  d = acc_deviceptr (h);
#endif
  assert (d);
  assert (!cb_ev_alloc_expected);
  assert (cb_ev_alloc_bytes == aligned_size (SIZE));
  assert (aligned_address (cb_ev_alloc_device_ptr) == d);
  /* Overwrite the local memory.  */
  memset (h, c1, SIZE);
  /* Now 'copyout' not the whole but only a "subset" subarray, missing one
     SUBSET at the beginning, and half a SUBSET at the end...  */
  cb_ev_free_expected = true;
#if OPENACC_RUNTIME
  acc_copyout (h + SUBSET, SIZE - SUBSET - SUBSET / 2);
#else
# pragma acc exit data copyout (h[SUBSET:SIZE - SUBSET - SUBSET / 2])
#endif
  /* ..., yet, expect the device memory object to be 'free'd...  */
  assert (!cb_ev_free_expected);
  assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);
  /* ..., and the mapping to be removed...  */
  assert (!acc_is_present (h, SIZE));
  assert (!acc_is_present (&h[SUBSET], SIZE - SUBSET - SUBSET / 2));
  assert (!acc_is_present (h, 1));
  /* ..., but the 'copyout'ed device memory to correspond to just the "subset"
     subarray.  */
  for (size_t i = 0; i < SIZE; ++i)
    {
      if (i < SUBSET)
	assert (h[i] == c1);
      else if (i < SIZE - SUBSET / 2)
	assert (h[i] == c0);
      else if (i < SIZE)
	assert (h[i] == c1);
    }

#if POINTERS
  free (h);
#endif

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);
}


/* Based on what used to be 'libgomp.oacc-c-c++-common/lib-30.c'.  */

static void
f_lib_30 (void)
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
  memset (h, 0, SIZE);

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

  /* We 'delete' not the whole but only a "subset" subarray...  */
  cb_ev_free_expected = true;
#if OPENACC_RUNTIME
  acc_delete (h, SIZE - SUBSET);
#else
# pragma acc exit data delete (h[0:SIZE - SUBSET])
#endif
  /* ..., yet, expect the device memory object to be 'free'd...  */
  assert (!cb_ev_free_expected);
  assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);
  /* ..., and the mapping to be removed.  */
  assert (!acc_is_present (h, SIZE));
  assert (!acc_is_present (h, SIZE - SUBSET));
  assert (!acc_is_present (h, 1));

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
  f_lib_22 ();
  f_lib_30 ();

  return 0;
}
