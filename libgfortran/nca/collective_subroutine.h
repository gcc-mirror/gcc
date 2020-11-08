
#ifndef COLLECTIVE_SUBROUTINE_HDR
#define COLLECTIVE_SUBROUTINE_HDR

#include "shared_memory.h"

typedef struct collsub_iface_shared 
{
  size_t curr_size;
  shared_mem_ptr collsub_buf;
  counter_barrier barrier;
  pthread_mutex_t mutex;
} collsub_iface_shared;

typedef struct collsub_iface
{
  collsub_iface_shared *s;
  allocator *a;
  shared_memory *sm;
} collsub_iface;

void collsub_broadcast_scalar (collsub_iface *, void *, index_type, int);
internal_proto (collsub_broadcast_scalar);

void collsub_broadcast_array (collsub_iface *, gfc_array_char *, int);
internal_proto (collsub_broadcast_array);

void collsub_reduce_array (collsub_iface *, gfc_array_char *, int *,
			   void (*) (void *, void *));
internal_proto (collsub_reduce_array);

void collsub_reduce_scalar (collsub_iface *, void *, index_type, int *,
			    void (*) (void *, void *));
internal_proto (collsub_reduce_scalar);

void collsub_sync (collsub_iface *);
internal_proto (collsub_sync);

void collsub_iface_init (collsub_iface *, alloc_iface *, shared_memory *);
internal_proto (collsub_iface_init);

void * get_collsub_buf (collsub_iface *ci, size_t size);
internal_proto (get_collsub_buf);


/* Needed to prevent one image starting the next collective subroutine before
 * everyone has finished the current one. At the moment, this  is just an alias
 * for collsub_sync, but there might be more work to do later.  */

static inline void
finish_collective_subroutine(collsub_iface *ci) {
  collsub_sync(ci);
}

#endif
