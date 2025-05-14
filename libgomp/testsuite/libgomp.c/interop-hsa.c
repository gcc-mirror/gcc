/* { dg-additional-options "-ldl" } */
/* { dg-require-effective-target offload_device_gcn }
   The 'asm' insert is valid for GCN only:
   { dg-additional-options -foffload=amdgcn-amdhsa } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include <assert.h>
#include <dlfcn.h>
#include "../../../include/hsa.h"
#include "../../config/gcn/libgomp-gcn.h"

#define STACKSIZE (100 * 1024)
#define HEAPSIZE (10 * 1024 * 1024)
#define ARENASIZE HEAPSIZE

/* This code fragment must be optimized or else the host-fallback kernel has
 * invalid ASM inserts.  The rest of the file can be compiled safely at -O0.  */
#pragma omp declare target
uintptr_t __attribute__((optimize("O1")))
get_kernel_ptr ()
{
  uintptr_t val;
  if (!omp_is_initial_device ())
    /* "main._omp_fn.0" is the name GCC gives the first OpenMP target
     * region in the "main" function.
     * The ".kd" suffix is added by the LLVM assembler when it creates the
     * kernel meta-data, and this is what we need to launch a kernel.  */
    asm ("s_getpc_b64 %0\n\t"
	 "s_add_u32 %L0, %L0, main._omp_fn.0.kd@rel32@lo+4\n\t"
	 "s_addc_u32 %H0, %H0, main._omp_fn.0.kd@rel32@hi+4"
	 : "=Sg"(val));
  return val;
}
#pragma omp end declare target

int
main(int argc, char** argv)
{

  /* Load the HSA runtime DLL.  */
  void *hsalib = dlopen ("libhsa-runtime64.so.1", RTLD_LAZY);
  assert (hsalib);

  hsa_status_t (*hsa_signal_create) (hsa_signal_value_t initial_value,
				     uint32_t num_consumers,
				     const hsa_agent_t *consumers,
				     hsa_signal_t *signal)
    = dlsym (hsalib, "hsa_signal_create");
  assert (hsa_signal_create);

  uint64_t (*hsa_queue_load_write_index_relaxed) (const hsa_queue_t *queue)
    = dlsym (hsalib, "hsa_queue_load_write_index_relaxed");
  assert (hsa_queue_load_write_index_relaxed);

  void (*hsa_signal_store_relaxed) (hsa_signal_t signal,
				    hsa_signal_value_t value)
    = dlsym (hsalib, "hsa_signal_store_relaxed");
  assert (hsa_signal_store_relaxed);

  hsa_signal_value_t (*hsa_signal_wait_relaxed) (hsa_signal_t signal,
						 hsa_signal_condition_t condition,
						 hsa_signal_value_t compare_value,
						 uint64_t timeout_hint,
						 hsa_wait_state_t wait_state_hint)
    = dlsym (hsalib, "hsa_signal_wait_relaxed");
  assert (hsa_signal_wait_relaxed);

  void (*hsa_queue_store_write_index_relaxed) (const hsa_queue_t *queue,
					       uint64_t value)
    = dlsym (hsalib, "hsa_queue_store_write_index_relaxed");
  assert (hsa_queue_store_write_index_relaxed);

  hsa_status_t (*hsa_signal_destroy) (hsa_signal_t signal)
    = dlsym (hsalib, "hsa_signal_destroy");
  assert (hsa_signal_destroy);

  /* Set up the device data environment.  */
  int test_data_value = 0;
#pragma omp target enter data map(test_data_value)

  /* Get the interop details.  */
  int device_num = omp_get_default_device();
  hsa_agent_t *gpu_agent;
  hsa_queue_t *hsa_queue = NULL;

  omp_interop_t interop = omp_interop_none;
#pragma omp interop init(target, targetsync, prefer_type("hsa"): interop) device(device_num)
  assert (interop != omp_interop_none);

  omp_interop_rc_t retcode;
  omp_interop_fr_t fr = omp_get_interop_int (interop, omp_ipr_fr_id, &retcode);
  assert (retcode == omp_irc_success);
  assert (fr == omp_ifr_hsa);

  gpu_agent = omp_get_interop_ptr(interop, omp_ipr_device, &retcode);
  assert (retcode == omp_irc_success);

  hsa_queue = omp_get_interop_ptr(interop, omp_ipr_targetsync, &retcode);
  assert (retcode == omp_irc_success);
  assert (hsa_queue);

  /* Call an offload kernel via OpenMP/libgomp.
   *
   * This kernel serves two purposes:
   *   1) Lookup the device-side load-address of itself (thus avoiding the
   *   need to access the libgomp internals).
   *   2) Count how many times it is called.
   * We then call it once using OpenMP, and once manually, and check
   * the counter reads "2".  */
  uint64_t kernel_object = 0;
#pragma omp target map(from:kernel_object) map(present,alloc:test_data_value)
  {
    kernel_object = get_kernel_ptr ();
    ++test_data_value;
  }

  assert (kernel_object != 0);

  /* Configure the same kernel to run again, using HSA manually this time.  */
  hsa_status_t status;
  hsa_signal_t signal;
  status = hsa_signal_create(1, 0, NULL, &signal);
  assert (status == HSA_STATUS_SUCCESS);

  /* The kernel is built by GCC for OpenMP, so we need to pass the same
   * data pointers that libgomp would pass in.  */
  struct {
    uintptr_t test_data_value;
    uintptr_t kernel_object;
  } tgtaddrs;

#pragma omp target data use_device_addr(test_data_value)
  {
    tgtaddrs.test_data_value = (uintptr_t)&test_data_value;
    tgtaddrs.kernel_object = (uintptr_t)omp_target_alloc (8, device_num);
  }

  /* We also need to duplicate the launch ABI used by plugin-gcn.c.  */
  struct kernargs_abi args;    /* From libgomp-gcn.h.  */
  args.dummy1 = (int64_t)&tgtaddrs;
  args.out_ptr = (int64_t)malloc (sizeof (struct output)); /* Host side.  */
  args.heap_ptr = (int64_t)omp_target_alloc (HEAPSIZE, device_num);
  args.arena_ptr = (int64_t)omp_target_alloc (ARENASIZE, device_num);
  args.stack_ptr = (int64_t)omp_target_alloc (STACKSIZE, device_num);
  args.arena_size_per_team = ARENASIZE;
  args.stack_size_per_thread = STACKSIZE;

  /* Build the HSA dispatch packet, and insert it into the queue.  */
  uint64_t packet_id = hsa_queue_load_write_index_relaxed (hsa_queue);
  const uint32_t queueMask = hsa_queue->size - 1;
  hsa_kernel_dispatch_packet_t *dispatch_packet =
    &(((hsa_kernel_dispatch_packet_t *)
	  (hsa_queue->base_address))[packet_id & queueMask]);

  dispatch_packet->setup = 3 << HSA_KERNEL_DISPATCH_PACKET_SETUP_DIMENSIONS;
  dispatch_packet->workgroup_size_x = 1;
  dispatch_packet->workgroup_size_y = 64;
  dispatch_packet->workgroup_size_z = 1;
  dispatch_packet->grid_size_x = 1;
  dispatch_packet->grid_size_y = 64;
  dispatch_packet->grid_size_z = 1;
  dispatch_packet->completion_signal = signal;
  dispatch_packet->kernel_object = kernel_object;
  dispatch_packet->kernarg_address = &args;
  dispatch_packet->private_segment_size = 0;
  dispatch_packet->group_segment_size = 1536;

  uint16_t header = 0;
  header |= HSA_PACKET_TYPE_KERNEL_DISPATCH << HSA_PACKET_HEADER_TYPE;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_ACQUIRE_FENCE_SCOPE;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_RELEASE_FENCE_SCOPE;

  /* Finish writing the packet header with an atomic release.  */
  __atomic_store_n((uint16_t*)dispatch_packet, header, __ATOMIC_RELEASE);

  hsa_queue_store_write_index_relaxed (hsa_queue, packet_id + 1);
  
  ;/* Run the kernel and wait for it to complete.  */
  hsa_signal_store_relaxed(hsa_queue->doorbell_signal, packet_id);
  while (hsa_signal_wait_relaxed(signal, HSA_SIGNAL_CONDITION_LT, 1,
	UINT64_MAX, HSA_WAIT_STATE_ACTIVE) != 0)
    ;

  /* Clean up HSA.  */
  hsa_signal_destroy(signal);
  free ((void*)args.out_ptr);
  omp_target_free ((void*)args.heap_ptr, device_num);
  omp_target_free ((void*)args.arena_ptr, device_num);
  omp_target_free ((void*)args.stack_ptr, device_num);
  omp_target_free ((void*)tgtaddrs.kernel_object, device_num);

  /* Clean up OpenMP.  */
  #pragma omp interop destroy(interop)

  /* Bring the data back from the device.  */
#pragma omp target exit data map(test_data_value)

  /* Ensure the kernel was called twice.  Once by OpenMP, once by HSA.  */
  assert (test_data_value == 2);

  return 0;
}
