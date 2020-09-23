#ifndef SHARED_ALLOCATOR_HDR
#define SHARED_ALLOCATOR_HDR

#include "util.h"
#include "shared_memory.h"

typedef struct {
  shared_mem_ptr free_bucket_head[PTR_BITS];
} allocator_shared;

typedef struct {
  allocator_shared *s;
  shared_memory *shm;
} allocator;

void allocator_init (allocator *, allocator_shared *, shared_memory *);

shared_mem_ptr shared_malloc (allocator *, size_t size);
void shared_free (allocator *, shared_mem_ptr, size_t size);

#endif
