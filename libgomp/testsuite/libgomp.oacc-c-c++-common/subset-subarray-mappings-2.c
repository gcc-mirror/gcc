/* Test "subset" subarray mappings.  */

/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <openacc.h>
#include <acc_prof.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>


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


int
main ()
{
  cb_ev_alloc_expected = false;
  cb_ev_free_expected = false;
  acc_prof_register (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_register (acc_ev_free, cb_ev_free, acc_reg);

  char *block1 = (char *) malloc (SIZE);
  char *block2 = (char *) malloc (SIZE);
  char *block3 = (char *) malloc (SIZE);
  cb_ev_alloc_expected = true;
#pragma acc data create (block1[0:SIZE], block2[0:SIZE], block3[0:SIZE])
  {
    void *s_block1_d = acc_deviceptr (&block1[1]);
    void *s_block2_d = acc_deviceptr (&block2[20]);
    void *s_block3_d = acc_deviceptr (&block3[300]);
    assert (!cb_ev_alloc_expected);
    /* 'block1', 'block2', 'block3' get mapped in one device memory object, in
       reverse order.  */
    assert (cb_ev_alloc_bytes == aligned_size (3 * SIZE));
    assert ((void *) ((uintptr_t) aligned_address (cb_ev_alloc_device_ptr) + 2 * SIZE + 1) == s_block1_d);
    assert ((void *) ((uintptr_t) aligned_address (cb_ev_alloc_device_ptr) + 1 * SIZE + 20) == s_block2_d);
    assert ((void *) ((uintptr_t) aligned_address (cb_ev_alloc_device_ptr) + 0 * SIZE + 300) == s_block3_d);

    void *s_block1_p_d = acc_pcopyin (&block1[1], SIZE - 3);
    void *s_block2_p_d = acc_pcopyin (&block2[20], SIZE - 33);
    void *s_block3_p_d = acc_pcopyin (&block3[300], SIZE - 333);
    assert (s_block1_p_d == s_block1_d);
    assert (s_block2_p_d == s_block2_d);
    assert (s_block3_p_d == s_block3_d);

    acc_delete (block1, SIZE);
    acc_delete (block2, SIZE);
    acc_delete (block3, SIZE);
    assert (acc_is_present (block1, SIZE));
    assert (acc_is_present (block2, SIZE));
    assert (acc_is_present (block3, SIZE));

    cb_ev_free_expected = true;
  }
  assert (!cb_ev_free_expected);
  assert (cb_ev_free_device_ptr == cb_ev_alloc_device_ptr);
  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));
  assert (!acc_is_present (block3, SIZE));

  free (block1);
  free (block2);
  free (block3);

  acc_prof_unregister (acc_ev_alloc, cb_ev_alloc, acc_reg);
  acc_prof_unregister (acc_ev_free, cb_ev_free, acc_reg);

  return 0;
}
