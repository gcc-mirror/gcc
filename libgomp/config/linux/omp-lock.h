/* This header is used during the build process to find the size and 
   alignment of the public OpenMP locks, so that we can export data
   structures without polluting the namespace.

   When using the Linux futex primitive, non-recursive locks require
   only one int.  Recursive locks require we identify the owning task
   and so require one int and a pointer.  */

typedef int omp_lock_t;
typedef struct { int lock, count; void *owner; } omp_nest_lock_t;
typedef int omp_lock_25_t;
typedef struct { int owner, count; } omp_nest_lock_25_t;
