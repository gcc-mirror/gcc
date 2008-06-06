/* This header is used during the build process to find the size and 
   alignment of the public OpenMP locks, so that we can export data
   structures without polluting the namespace.

   In this default POSIX implementation, we used to map the two locks to the
   same PTHREADS primitive, but for OpenMP 3.0 sem_t needs to be used
   instead, as pthread_mutex_unlock should not be called by different
   thread than the one that called pthread_mutex_lock.  */

#include <pthread.h>
#include <semaphore.h>

typedef pthread_mutex_t omp_lock_25_t;
typedef struct { pthread_mutex_t lock; int count; } omp_nest_lock_25_t;
#ifdef HAVE_BROKEN_POSIX_SEMAPHORES
/* If we don't have working semaphores, we'll make all explicit tasks
   tied to the creating thread.  */
typedef pthread_mutex_t omp_lock_t;
typedef struct { pthread_mutex_t lock; int count; void *owner; } omp_nest_lock_t;
#else
typedef sem_t omp_lock_t;
typedef struct { sem_t lock; int count; void *owner; } omp_nest_lock_t;
#endif
