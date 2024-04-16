/* Copyright (C) 2005-2024 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains data types and function declarations that are not
   part of the official OpenACC or OpenMP user interfaces.  There are
   declarations in here that are part of the GNU Offloading and Multi
   Processing ABI, in that the compiler is required to know about them
   and use them.

   The convention is that the all caps prefix "GOMP" is used group items
   that are part of the external ABI, and the lower case prefix "gomp"
   is used group items that are completely private to the library.  */

#ifndef LIBGOMP_H 
#define LIBGOMP_H 1

#ifndef _LIBGOMP_CHECKING_
/* Define to 1 to perform internal sanity checks.  */
#define _LIBGOMP_CHECKING_ 0
#endif

#include "config.h"
#include <stdint.h>
#include "libgomp-plugin.h"
#include "gomp-constants.h"

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>

/* Needed for memset in priority_queue.c.  */
#if _LIBGOMP_CHECKING_
# ifdef STRING_WITH_STRINGS
#  include <string.h>
#  include <strings.h>
# else
#  ifdef HAVE_STRING_H
#   include <string.h>
#  else
#   ifdef HAVE_STRINGS_H
#    include <strings.h>
#   endif
#  endif
# endif
#endif

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility push(hidden)
#endif

/* If we were a C++ library, we'd get this from <std/atomic>.  */
enum memmodel
{
  MEMMODEL_RELAXED = 0,
  MEMMODEL_CONSUME = 1,
  MEMMODEL_ACQUIRE = 2,
  MEMMODEL_RELEASE = 3,
  MEMMODEL_ACQ_REL = 4,
  MEMMODEL_SEQ_CST = 5
};

/* alloc.c */

#if defined(HAVE_ALIGNED_ALLOC) \
    || defined(HAVE_POSIX_MEMALIGN) \
    || defined(HAVE_MEMALIGN)
/* Defined if gomp_aligned_alloc doesn't use fallback version
   and free can be used instead of gomp_aligned_free.  */
#define GOMP_HAVE_EFFICIENT_ALIGNED_ALLOC 1
#endif

#if defined(GOMP_HAVE_EFFICIENT_ALIGNED_ALLOC) && !defined(__AMDGCN__)
#define GOMP_USE_ALIGNED_WORK_SHARES 1
#endif

extern void *gomp_malloc (size_t) __attribute__((malloc));
extern void *gomp_malloc_cleared (size_t) __attribute__((malloc));
extern void *gomp_realloc (void *, size_t);
extern void *gomp_aligned_alloc (size_t, size_t)
  __attribute__((malloc, alloc_size (2)));
extern void gomp_aligned_free (void *);

/* Avoid conflicting prototypes of alloca() in system headers by using
   GCC's builtin alloca().  */
#define gomp_alloca(x)  __builtin_alloca(x)

/* Optimized allocators for team-specific data that will die with the team.  */

#ifdef __AMDGCN__
#include "libgomp-gcn.h"
/* The arena is initialized in config/gcn/team.c.  */

static inline void * __attribute__((malloc))
team_malloc (size_t size)
{
  /* 4-byte align the size.  */
  size = (size + 3) & ~3;

  /* Allocate directly from the arena.
     The compiler does not support DS atomics, yet. */
  void *result;
  asm ("ds_add_rtn_u64 %0, %1, %2\n\ts_waitcnt 0"
       : "=v"(result) : "v"(TEAM_ARENA_FREE), "v"(size), "e"(1L) : "memory");

  /* Handle OOM.  */
  if (result + size > *(void * __lds *)TEAM_ARENA_END)
    {
      /* While this is experimental, let's make sure we know when OOM
	 happens.  */
      const char msg[] = "GCN team arena exhausted;"
			 " configure with GCN_TEAM_ARENA_SIZE=bytes\n";
      write (2, msg, sizeof(msg)-1);

      /* Fall back to using the heap (slowly).  */
      result = gomp_malloc (size);
    }
  return result;
}

static inline void * __attribute__((malloc))
team_malloc_cleared (size_t size)
{
  char *result = team_malloc (size);

  /* Clear the allocated memory.  */
  __builtin_memset (result, 0, size);

  return result;
}

static inline void
team_free (void *ptr)
{
  /* The whole arena is freed when the kernel exits.
     However, if we fell back to using heap then we should free it.
     It would be better if this function could be a no-op, but at least
     LDS loads are cheap.  */
  if (ptr < *(void * __lds *)TEAM_ARENA_START
      || ptr >= *(void * __lds *)TEAM_ARENA_END)
    free (ptr);
}
#else
#define team_malloc(...) gomp_malloc (__VA_ARGS__)
#define team_malloc_cleared(...) gomp_malloc_cleared (__VA_ARGS__)
#define team_free(...) free (__VA_ARGS__)
#endif

/* error.c */

extern void gomp_vdebug (int, const char *, va_list);
extern void gomp_debug (int, const char *, ...)
	__attribute__ ((format (printf, 2, 3)));
#define gomp_vdebug(KIND, FMT, VALIST) \
  do { \
    if (__builtin_expect (gomp_debug_var, 0)) \
      (gomp_vdebug) ((KIND), (FMT), (VALIST)); \
  } while (0)
#define gomp_debug(KIND, ...) \
  do { \
    if (__builtin_expect (gomp_debug_var, 0)) \
      (gomp_debug) ((KIND), __VA_ARGS__); \
  } while (0)
extern void gomp_verror (const char *, va_list);
extern void gomp_error (const char *, ...)
	__attribute__ ((format (printf, 1, 2)));
extern void gomp_vfatal (const char *, va_list)
	__attribute__ ((noreturn));
extern void gomp_fatal (const char *, ...)
	__attribute__ ((noreturn, format (printf, 1, 2)));

struct gomp_task;
struct gomp_taskgroup;
struct htab;

#include "priority_queue.h"
#include "sem.h"
#include "mutex.h"
#include "bar.h"
#include "simple-bar.h"
#include "ptrlock.h"


/* This structure contains the data to control one work-sharing construct,
   either a LOOP (FOR/DO) or a SECTIONS.  */

enum gomp_schedule_type
{
  GFS_RUNTIME,
  GFS_STATIC,
  GFS_DYNAMIC,
  GFS_GUIDED,
  GFS_AUTO,
  GFS_MONOTONIC = 0x80000000U
};

struct gomp_doacross_work_share
{
  union {
    /* chunk_size copy, as ws->chunk_size is multiplied by incr for
       GFS_DYNAMIC.  */
    long chunk_size;
    /* Likewise, but for ull implementation.  */
    unsigned long long chunk_size_ull;
    /* For schedule(static,0) this is the number
       of iterations assigned to the last thread, i.e. number of
       iterations / number of threads.  */
    long q;
    /* Likewise, but for ull implementation.  */
    unsigned long long q_ull;
  };
  /* Size of each array entry (padded to cache line size).  */
  unsigned long elt_sz;
  /* Number of dimensions in sink vectors.  */
  unsigned int ncounts;
  /* True if the iterations can be flattened.  */
  bool flattened;
  /* Actual array (of elt_sz sized units), aligned to cache line size.
     This is indexed by team_id for GFS_STATIC and outermost iteration
     / chunk_size for other schedules.  */
  unsigned char *array;
  /* These two are only used for schedule(static,0).  */
  /* This one is number of iterations % number of threads.  */
  long t;
  union {
    /* And this one is cached t * (q + 1).  */
    long boundary;
    /* Likewise, but for the ull implementation.  */
    unsigned long long boundary_ull;
  };
  /* Pointer to extra memory if needed for lastprivate(conditional).  */
  void *extra;
  /* Array of shift counts for each dimension if they can be flattened.  */
  unsigned int shift_counts[];
};

/* Like struct gomp_work_share, but only the 1st cacheline of it plus
   flexible array at the end.
   Keep in sync with struct gomp_work_share.  */
struct gomp_work_share_1st_cacheline
{
  enum gomp_schedule_type sched;
  int mode;
  union {
    struct {
      long chunk_size, end, incr;
    };
    struct {
      unsigned long long chunk_size_ull, end_ull, incr_ull;
    };
  };
  union {
    unsigned *ordered_team_ids;
    struct gomp_doacross_work_share *doacross;
  };
  unsigned ordered_num_used, ordered_owner, ordered_cur;
  struct gomp_work_share *next_alloc;
  char pad[];
};

struct gomp_work_share
{
  /* This member records the SCHEDULE clause to be used for this construct.
     The user specification of "runtime" will already have been resolved.
     If this is a SECTIONS construct, this value will always be DYNAMIC.  */
  enum gomp_schedule_type sched;

  int mode;

  union {
    struct {
      /* This is the chunk_size argument to the SCHEDULE clause.  */
      long chunk_size;

      /* This is the iteration end point.  If this is a SECTIONS construct,
	 this is the number of contained sections.  */
      long end;

      /* This is the iteration step.  If this is a SECTIONS construct, this
	 is always 1.  */
      long incr;
    };

    struct {
      /* The same as above, but for the unsigned long long loop variants.  */
      unsigned long long chunk_size_ull;
      unsigned long long end_ull;
      unsigned long long incr_ull;
    };
  };

  union {
    /* This is a circular queue that details which threads will be allowed
       into the ordered region and in which order.  When a thread allocates
       iterations on which it is going to work, it also registers itself at
       the end of the array.  When a thread reaches the ordered region, it
       checks to see if it is the one at the head of the queue.  If not, it
       blocks on its RELEASE semaphore.  */
    unsigned *ordered_team_ids;

    /* This is a pointer to DOACROSS work share data.  */
    struct gomp_doacross_work_share *doacross;
  };

  /* This is the number of threads that have registered themselves in
     the circular queue ordered_team_ids.  */
  unsigned ordered_num_used;

  /* This is the team_id of the currently acknowledged owner of the ordered
     section, or -1u if the ordered section has not been acknowledged by
     any thread.  This is distinguished from the thread that is *allowed*
     to take the section next.  */
  unsigned ordered_owner;

  /* This is the index into the circular queue ordered_team_ids of the
     current thread that's allowed into the ordered reason.  */
  unsigned ordered_cur;

  /* This is a chain of allocated gomp_work_share blocks, valid only
     in the first gomp_work_share struct in the block.  */
  struct gomp_work_share *next_alloc;

  /* The above fields are written once during workshare initialization,
     or related to ordered worksharing.  Make sure the following fields
     are in a different cache line.  */

  /* This lock protects the update of the following members.  */
#ifdef GOMP_USE_ALIGNED_WORK_SHARES
  gomp_mutex_t lock __attribute__((aligned (64)));
#else
  char pad[64 - offsetof (struct gomp_work_share_1st_cacheline, pad)];
  gomp_mutex_t lock;
#endif

  /* This is the count of the number of threads that have exited the work
     share construct.  If the construct was marked nowait, they have moved on
     to other work; otherwise they're blocked on a barrier.  The last member
     of the team to exit the work share construct must deallocate it.  */
  unsigned threads_completed;

  union {
    /* This is the next iteration value to be allocated.  In the case of
       GFS_STATIC loops, this the iteration start point and never changes.  */
    long next;

    /* The same, but with unsigned long long type.  */
    unsigned long long next_ull;

    /* This is the returned data structure for SINGLE COPYPRIVATE.  */
    void *copyprivate;
  };

  union {
    /* Link to gomp_work_share struct for next work sharing construct
       encountered after this one.  */
    gomp_ptrlock_t next_ws;

    /* gomp_work_share structs are chained in the free work share cache
       through this.  */
    struct gomp_work_share *next_free;
  };

  /* Task reductions for this work-sharing construct.  */
  uintptr_t *task_reductions;

  /* If only few threads are in the team, ordered_team_ids can point
     to this array which fills the padding at the end of this struct.  */
  unsigned inline_ordered_team_ids[0];
};

extern char gomp_workshare_struct_check1
  [offsetof (struct gomp_work_share_1st_cacheline, next_alloc)
   == offsetof (struct gomp_work_share, next_alloc) ? 1 : -1];
extern char gomp_workshare_struct_check2
  [offsetof (struct gomp_work_share, lock) == 64 ? 1 : -1];

/* This structure contains all of the thread-local data associated with 
   a thread team.  This is the data that must be saved when a thread
   encounters a nested PARALLEL construct.  */

struct gomp_team_state
{
  /* This is the team of which the thread is currently a member.  */
  struct gomp_team *team;

  /* This is the work share construct which this thread is currently
     processing.  Recall that with NOWAIT, not all threads may be 
     processing the same construct.  */
  struct gomp_work_share *work_share;

  /* This is the previous work share construct or NULL if there wasn't any.
     When all threads are done with the current work sharing construct,
     the previous one can be freed.  The current one can't, as its
     next_ws field is used.  */
  struct gomp_work_share *last_work_share;

  /* This is the ID of this thread within the team.  This value is
     guaranteed to be between 0 and N-1, where N is the number of
     threads in the team.  */
  unsigned team_id;

  /* Nesting level.  */
  unsigned level;

  /* Active nesting level.  Only active parallel regions are counted.  */
  unsigned active_level;

  /* Place-partition-var, offset and length into gomp_places_list array.  */
  unsigned place_partition_off;
  unsigned place_partition_len;

  /* Def-allocator-var ICV.  */
  uintptr_t def_allocator;

#ifdef HAVE_SYNC_BUILTINS
  /* Number of single stmts encountered.  */
  unsigned long single_count;
#endif

  /* For GFS_RUNTIME loops that resolved to GFS_STATIC, this is the
     trip number through the loop.  So first time a particular loop
     is encountered this number is 0, the second time through the loop
     is 1, etc.  This is unused when the compiler knows in advance that
     the loop is statically scheduled.  */
  unsigned long static_trip;
};

struct target_mem_desc;

enum gomp_icvs
{
   GOMP_ICV_NTEAMS = 1,
   GOMP_ICV_SCHEDULE = 2,
   GOMP_ICV_SCHEDULE_CHUNK_SIZE = 3,
   GOMP_ICV_DYNAMIC = 4,
   GOMP_ICV_TEAMS_THREAD_LIMIT = 5,
   GOMP_ICV_THREAD_LIMIT = 6,
   GOMP_ICV_NTHREADS = 7,
   GOMP_ICV_NTHREADS_LIST = 8,
   GOMP_ICV_NTHREADS_LIST_LEN = 9,
   GOMP_ICV_BIND = 10,
   GOMP_ICV_BIND_LIST = 11,
   GOMP_ICV_BIND_LIST_LEN = 12,
   GOMP_ICV_MAX_ACTIVE_LEVELS = 13,
   GOMP_ICV_WAIT_POLICY = 14,
   GOMP_ICV_STACKSIZE = 15,
   GOMP_ICV_DEFAULT_DEVICE = 16,
   GOMP_ICV_CANCELLATION = 17,
   GOMP_ICV_DISPLAY_AFFINITY = 18,
   GOMP_ICV_TARGET_OFFLOAD = 19,
   GOMP_ICV_MAX_TASK_PRIORITY = 20,
   GOMP_ICV_ALLOCATOR = 21
};

enum gomp_device_num
{
  GOMP_DEVICE_NUM_FOR_DEV = -1,
  GOMP_DEVICE_NUM_FOR_ALL = -2,
  GOMP_DEVICE_NUM_FOR_NO_SUFFIX = -3
};

/* These are the OpenMP 4.0 Internal Control Variables described in
   section 2.3.1.  Those described as having one copy per task are
   stored within the structure; those described as having one copy
   for the whole program are (naturally) global variables.  */
   
struct gomp_task_icv
{
  unsigned long nthreads_var;
  enum gomp_schedule_type run_sched_var;
  int run_sched_chunk_size;
  int default_device_var;
  unsigned int thread_limit_var;
  bool dyn_var;
  unsigned char max_active_levels_var;
  char bind_var;
  /* Internal ICV.  */
  struct target_mem_desc *target_data;
};

enum gomp_env_suffix
{
  GOMP_ENV_SUFFIX_UNKNOWN = 0,
  GOMP_ENV_SUFFIX_NONE = 1,
  GOMP_ENV_SUFFIX_DEV = 2,
  GOMP_ENV_SUFFIX_ALL = 4,
  GOMP_ENV_SUFFIX_DEV_X = 8
};

/* Struct that contains all ICVs for which we need to store initial values.
   Keeping the initial values is needed for omp_display_env.  Moreover initial
   _DEV and _ALL variants of environment variables are also used to determine
   actually used values for devices and for the host.  */
struct gomp_initial_icvs
{
  unsigned long *nthreads_var_list;
  char *bind_var_list;
  unsigned long nthreads_var;
  unsigned long nthreads_var_list_len;
  unsigned long bind_var_list_len;
  unsigned long stacksize;
  int run_sched_chunk_size;
  int default_device_var;
  int nteams_var;
  int teams_thread_limit_var;
  int wait_policy;
  unsigned int thread_limit_var;
  enum gomp_schedule_type run_sched_var;
  bool dyn_var;
  unsigned char max_active_levels_var;
  char bind_var;
};

struct gomp_default_icv
{
  unsigned long nthreads_var;
  enum gomp_schedule_type run_sched_var;
  int run_sched_chunk_size;
  int default_device_var;
  unsigned int thread_limit_var;
  int nteams_var;
  int teams_thread_limit_var;
  bool dyn_var;
  unsigned char max_active_levels_var;
  char bind_var;
};

/*  DEVICE_NUM "-1" is reserved for "_DEV" icvs.
    DEVICE_NUM "-2" is reserved for "_ALL" icvs.
    DEVICE_NUM "-3" is reserved for ICVs without suffix.
    Non-negative DEVICE_NUM is for "_DEV_X" icvs.  */
struct gomp_icv_list
{
  int device_num;
  uint32_t flags;
  struct gomp_initial_icvs icvs;
  struct gomp_icv_list *next;
};

struct gomp_offload_icvs
{
  int device_num;
  int default_device;
  int nteams;
  int teams_thread_limit;
};

struct gomp_offload_icv_list
{
  int device_num;
  struct gomp_offload_icvs icvs;
  struct gomp_offload_icv_list *next;
};

enum gomp_target_offload_t
{
  GOMP_TARGET_OFFLOAD_DEFAULT,
  GOMP_TARGET_OFFLOAD_MANDATORY,
  GOMP_TARGET_OFFLOAD_DISABLED
};

#define gomp_supported_active_levels UCHAR_MAX

extern struct gomp_task_icv gomp_global_icv;
#ifndef HAVE_SYNC_BUILTINS
extern gomp_mutex_t gomp_managed_threads_lock;
#endif
extern bool gomp_cancel_var;
extern enum gomp_target_offload_t gomp_target_offload_var;
extern int gomp_max_task_priority_var;
extern unsigned long long gomp_spin_count_var, gomp_throttled_spin_count_var;
extern unsigned long gomp_available_cpus, gomp_managed_threads;
extern unsigned long *gomp_nthreads_var_list, gomp_nthreads_var_list_len;
extern char *gomp_bind_var_list;
extern unsigned long gomp_bind_var_list_len;
extern void **gomp_places_list;
extern unsigned long gomp_places_list_len;
extern unsigned int gomp_num_teams_var;
extern int gomp_nteams_var;
extern int gomp_teams_thread_limit_var;
extern int gomp_debug_var;
extern bool gomp_display_affinity_var;
extern char *gomp_affinity_format_var;
extern size_t gomp_affinity_format_len;
extern uintptr_t gomp_def_allocator;
extern const struct gomp_default_icv gomp_default_icv_values;
extern struct gomp_icv_list *gomp_initial_icv_list;
extern struct gomp_offload_icv_list *gomp_offload_icv_list;
extern int goacc_device_num;
extern char *goacc_device_type;
extern int goacc_default_dims[GOMP_DIM_MAX];

enum gomp_task_kind
{
  /* Implicit task.  */
  GOMP_TASK_IMPLICIT,
  /* Undeferred task.  */
  GOMP_TASK_UNDEFERRED,
  /* Task created by GOMP_task and waiting to be run.  */
  GOMP_TASK_WAITING,
  /* Task currently executing or scheduled and about to execute.  */
  GOMP_TASK_TIED,
  /* Used for target tasks that have vars mapped and async run started,
     but not yet completed.  Once that completes, they will be readded
     into the queues as GOMP_TASK_WAITING in order to perform the var
     unmapping.  */
  GOMP_TASK_ASYNC_RUNNING,
  /* Task that has finished executing but is waiting for its
     completion event to be fulfilled.  */
  GOMP_TASK_DETACHED
};

struct gomp_task_depend_entry
{
  /* Address of dependency.  */
  void *addr;
  struct gomp_task_depend_entry *next;
  struct gomp_task_depend_entry *prev;
  /* Task that provides the dependency in ADDR.  */
  struct gomp_task *task;
  /* Depend entry is of type "IN" (1) or "INOUTSET" (2).  */
  unsigned char is_in;
  bool redundant;
  bool redundant_out;
};

struct gomp_dependers_vec
{
  size_t n_elem;
  size_t allocated;
  struct gomp_task *elem[];
};

/* Used when in GOMP_taskwait or in gomp_task_maybe_wait_for_dependencies.  */

struct gomp_taskwait
{
  bool in_taskwait;
  bool in_depend_wait;
  /* Number of tasks we are waiting for.  */
  size_t n_depend;
  gomp_sem_t taskwait_sem;
};

/* This structure describes a "task" to be run by a thread.  */

struct gomp_task
{
  /* Parent of this task.  */
  struct gomp_task *parent;
  /* Children of this task.  */
  struct priority_queue children_queue;
  /* Taskgroup this task belongs in.  */
  struct gomp_taskgroup *taskgroup;
  /* Tasks that depend on this task.  */
  struct gomp_dependers_vec *dependers;
  struct htab *depend_hash;
  struct gomp_taskwait *taskwait;
  /* Last depend({,in}out:omp_all_memory) child if any.  */
  struct gomp_task *depend_all_memory;
  /* Number of items in DEPEND.  */
  size_t depend_count;
  /* Number of tasks this task depends on.  Once this counter reaches
     0, we have no unsatisfied dependencies, and this task can be put
     into the various queues to be scheduled.  */
  size_t num_dependees;

  union {
      /* Valid only if deferred_p is false.  */
      gomp_sem_t *completion_sem;
      /* Valid only if deferred_p is true.  Set to the team that executes the
	 task if the task is detached and the completion event has yet to be
	 fulfilled.  */
      struct gomp_team *detach_team;
    };
  bool deferred_p;

  /* Priority of this task.  */
  int priority;
  /* The priority node for this task in each of the different queues.
     We put this here to avoid allocating space for each priority
     node.  Then we play offsetof() games to convert between pnode[]
     entries and the gomp_task in which they reside.  */
  struct priority_node pnode[3];

  struct gomp_task_icv icv;
  void (*fn) (void *);
  void *fn_data;
  enum gomp_task_kind kind;
  bool in_tied_task;
  bool final_task;
  bool copy_ctors_done;
  /* Set for undeferred tasks with unsatisfied dependencies which
     block further execution of their parent until the dependencies
     are satisfied.  */
  bool parent_depends_on;
  /* Dependencies provided and/or needed for this task.  DEPEND_COUNT
     is the number of items available.  */
  struct gomp_task_depend_entry depend[];
};

/* This structure describes a single #pragma omp taskgroup.  */

struct gomp_taskgroup
{
  struct gomp_taskgroup *prev;
  /* Queue of tasks that belong in this taskgroup.  */
  struct priority_queue taskgroup_queue;
  uintptr_t *reductions;
  bool in_taskgroup_wait;
  bool cancelled;
  bool workshare;
  gomp_sem_t taskgroup_sem;
  size_t num_children;
};

/* Various state of OpenMP async offloading tasks.  */
enum gomp_target_task_state
{
  GOMP_TARGET_TASK_DATA,
  GOMP_TARGET_TASK_BEFORE_MAP,
  GOMP_TARGET_TASK_FALLBACK,
  GOMP_TARGET_TASK_READY_TO_RUN,
  GOMP_TARGET_TASK_RUNNING,
  GOMP_TARGET_TASK_FINISHED
};

/* This structure describes a target task.  */

struct gomp_target_task
{
  struct gomp_device_descr *devicep;
  void (*fn) (void *);
  size_t mapnum;
  size_t *sizes;
  unsigned short *kinds;
  unsigned int flags;
  enum gomp_target_task_state state;
  struct target_mem_desc *tgt;
  struct gomp_task *task;
  struct gomp_team *team;
  /* Device-specific target arguments.  */
  void **args;
  void *hostaddrs[];
};

/* This structure describes a "team" of threads.  These are the threads
   that are spawned by a PARALLEL constructs, as well as the work sharing
   constructs that the team encounters.  */

struct gomp_team
{
  /* This is the number of threads in the current team.  */
  unsigned nthreads;

  /* This is number of gomp_work_share structs that have been allocated
     as a block last time.  */
  unsigned work_share_chunk;

  /* This is the saved team state that applied to a master thread before
     the current thread was created.  */
  struct gomp_team_state prev_ts;

  /* This semaphore should be used by the master thread instead of its
     "native" semaphore in the thread structure.  Required for nested
     parallels, as the master is a member of two teams.  */
  gomp_sem_t master_release;

  /* This points to an array with pointers to the release semaphore
     of the threads in the team.  */
  gomp_sem_t **ordered_release;

  /* List of work shares on which gomp_fini_work_share hasn't been
     called yet.  If the team hasn't been cancelled, this should be
     equal to each thr->ts.work_share, but otherwise it can be a possibly
     long list of workshares.  */
  struct gomp_work_share *work_shares_to_free;

  /* List of gomp_work_share structs chained through next_free fields.
     This is populated and taken off only by the first thread in the
     team encountering a new work sharing construct, in a critical
     section.  */
  struct gomp_work_share *work_share_list_alloc;

  /* List of gomp_work_share structs freed by free_work_share.  New
     entries are atomically added to the start of the list, and
     alloc_work_share can safely only move all but the first entry
     to work_share_list alloc, as free_work_share can happen concurrently
     with alloc_work_share.  */
  struct gomp_work_share *work_share_list_free;

#ifdef HAVE_SYNC_BUILTINS
  /* Number of simple single regions encountered by threads in this
     team.  */
  unsigned long single_count;
#else
  /* Mutex protecting addition of workshares to work_share_list_free.  */
  gomp_mutex_t work_share_list_free_lock;
#endif

  /* This barrier is used for most synchronization of the team.  */
  gomp_barrier_t barrier;

  /* Initial work shares, to avoid allocating any gomp_work_share
     structs in the common case.  */
  struct gomp_work_share work_shares[8];

  gomp_mutex_t task_lock;
  /* Scheduled tasks.  */
  struct priority_queue task_queue;
  /* Number of all GOMP_TASK_{WAITING,TIED} tasks in the team.  */
  unsigned int task_count;
  /* Number of GOMP_TASK_WAITING tasks currently waiting to be scheduled.  */
  unsigned int task_queued_count;
  /* Number of GOMP_TASK_{WAITING,TIED} tasks currently running
     directly in gomp_barrier_handle_tasks; tasks spawned
     from e.g. GOMP_taskwait or GOMP_taskgroup_end don't count, even when
     that is called from a task run from gomp_barrier_handle_tasks.
     task_running_count should be always <= team->nthreads,
     and if current task isn't in_tied_task, then it will be
     even < team->nthreads.  */
  unsigned int task_running_count;
  int work_share_cancelled;
  int team_cancelled;

  /* Number of tasks waiting for their completion event to be fulfilled.  */
  unsigned int task_detach_count;

  /* This array contains structures for implicit tasks.  */
  struct gomp_task implicit_task[];
};

/* This structure contains all data that is private to libgomp and is
   allocated per thread.  */

struct gomp_thread
{
  /* This is the function that the thread should run upon launch.  */
  void (*fn) (void *data);
  void *data;

  /* This is the current team state for this thread.  The ts.team member
     is NULL only if the thread is idle.  */
  struct gomp_team_state ts;

  /* This is the task that the thread is currently executing.  */
  struct gomp_task *task;

  /* This semaphore is used for ordered loops.  */
  gomp_sem_t release;

  /* Place this thread is bound to plus one, or zero if not bound
     to any place.  */
  unsigned int place;

  /* User pthread thread pool */
  struct gomp_thread_pool *thread_pool;

#ifdef LIBGOMP_USE_PTHREADS
  /* omp_get_num_teams () - 1.  */
  unsigned int num_teams;

  /* omp_get_team_num ().  */
  unsigned int team_num;
#endif

#if defined(LIBGOMP_USE_PTHREADS) \
    && (!defined(HAVE_TLS) \
	|| !defined(__GLIBC__) \
	|| !defined(USING_INITIAL_EXEC_TLS))
  /* pthread_t of the thread containing this gomp_thread.
     On Linux when using initial-exec TLS,
     (typeof (pthread_t)) gomp_thread () - pthread_self ()
     is constant in all threads, so we can optimize and not
     store it.  */
#define GOMP_NEEDS_THREAD_HANDLE 1
  pthread_t handle;
#endif
};


struct gomp_thread_pool
{
  /* This array manages threads spawned from the top level, which will
     return to the idle loop once the current PARALLEL construct ends.  */
  struct gomp_thread **threads;
  unsigned threads_size;
  unsigned threads_used;
  /* The last team is used for non-nested teams to delay their destruction to
     make sure all the threads in the team move on to the pool's barrier before
     the team's barrier is destroyed.  */
  struct gomp_team *last_team;
  /* Number of threads running in this contention group.  */
  unsigned long threads_busy;

  /* This barrier holds and releases threads waiting in thread pools.  */
  gomp_simple_barrier_t threads_dock;
};

enum gomp_cancel_kind
{
  GOMP_CANCEL_PARALLEL = 1,
  GOMP_CANCEL_LOOP = 2,
  GOMP_CANCEL_FOR = GOMP_CANCEL_LOOP,
  GOMP_CANCEL_DO = GOMP_CANCEL_LOOP,
  GOMP_CANCEL_SECTIONS = 4,
  GOMP_CANCEL_TASKGROUP = 8
};

/* ... and here is that TLS data.  */

#if defined __nvptx__
extern struct gomp_thread *nvptx_thrs __attribute__((shared));
static inline struct gomp_thread *gomp_thread (void)
{
  int tid;
  asm ("mov.u32 %0, %%tid.y;" : "=r" (tid));
  return nvptx_thrs + tid;
}
#elif defined __AMDGCN__
static inline struct gomp_thread *gcn_thrs (void)
{
  /* The value is at the bottom of LDS.  */
  struct gomp_thread * __lds *thrs = (struct gomp_thread * __lds *)4;
  return *thrs;
}
static inline void set_gcn_thrs (struct gomp_thread *val)
{
  /* The value is at the bottom of LDS.  */
  struct gomp_thread * __lds *thrs = (struct gomp_thread * __lds *)4;
  *thrs = val;
}
static inline struct gomp_thread *gomp_thread (void)
{
  int tid = __builtin_gcn_dim_pos(1);
  return gcn_thrs () + tid;
}
#elif defined HAVE_TLS || defined USE_EMUTLS
extern __thread struct gomp_thread gomp_tls_data;
static inline struct gomp_thread *gomp_thread (void)
{
  return &gomp_tls_data;
}
#else
extern pthread_key_t gomp_tls_key;
static inline struct gomp_thread *gomp_thread (void)
{
  return pthread_getspecific (gomp_tls_key);
}
#endif

extern struct gomp_task_icv *gomp_new_icv (void);

/* Here's how to access the current copy of the ICVs.  */

static inline struct gomp_task_icv *gomp_icv (bool write)
{
  struct gomp_task *task = gomp_thread ()->task;
  if (task)
    return &task->icv;
  else if (write)
    return gomp_new_icv ();
  else
    return &gomp_global_icv;
}

#ifdef LIBGOMP_USE_PTHREADS
/* The attributes to be used during thread creation.  */
extern pthread_attr_t gomp_thread_attr;

extern pthread_key_t gomp_thread_destructor;
#endif

/* Function prototypes.  */

/* affinity.c */

extern void gomp_init_affinity (void);
#ifdef LIBGOMP_USE_PTHREADS
extern void gomp_init_thread_affinity (pthread_attr_t *, unsigned int);
#endif
extern void **gomp_affinity_alloc (unsigned long, bool);
extern void gomp_affinity_init_place (void *);
extern bool gomp_affinity_add_cpus (void *, unsigned long, unsigned long,
				    long, bool);
extern bool gomp_affinity_remove_cpu (void *, unsigned long);
extern bool gomp_affinity_copy_place (void *, void *, long);
extern bool gomp_affinity_same_place (void *, void *);
extern bool gomp_affinity_finalize_place_list (bool);
extern bool gomp_affinity_init_level (int, unsigned long, bool);
extern void gomp_affinity_print_place (void *);
extern void gomp_get_place_proc_ids_8 (int, int64_t *);
extern void gomp_display_affinity_place (char *, size_t, size_t *, int);

/* affinity-fmt.c */

extern bool gomp_print_string (const char *str, size_t len);
extern void gomp_set_affinity_format (const char *, size_t);
extern void gomp_display_string (char *, size_t, size_t *, const char *,
				 size_t);
#ifdef LIBGOMP_USE_PTHREADS
typedef pthread_t gomp_thread_handle;
#else
typedef struct {} gomp_thread_handle;
#endif
extern size_t gomp_display_affinity (char *, size_t, const char *,
				     gomp_thread_handle,
				     struct gomp_team_state *, unsigned int);
extern void gomp_display_affinity_thread (gomp_thread_handle,
					  struct gomp_team_state *,
					  unsigned int) __attribute__((cold));

/* env.c */

extern struct gomp_icv_list *gomp_get_initial_icv_item (int dev_num);
extern bool gomp_get_icv_flag (uint32_t value, enum gomp_icvs icv);

/* iter.c */

extern int gomp_iter_static_next (long *, long *);
extern bool gomp_iter_dynamic_next_locked (long *, long *);
extern bool gomp_iter_guided_next_locked (long *, long *);

#ifdef HAVE_SYNC_BUILTINS
extern bool gomp_iter_dynamic_next (long *, long *);
extern bool gomp_iter_guided_next (long *, long *);
#endif

/* iter_ull.c */

extern int gomp_iter_ull_static_next (unsigned long long *,
				      unsigned long long *);
extern bool gomp_iter_ull_dynamic_next_locked (unsigned long long *,
					       unsigned long long *);
extern bool gomp_iter_ull_guided_next_locked (unsigned long long *,
					      unsigned long long *);

#if defined HAVE_SYNC_BUILTINS && defined __LP64__
extern bool gomp_iter_ull_dynamic_next (unsigned long long *,
					unsigned long long *);
extern bool gomp_iter_ull_guided_next (unsigned long long *,
				       unsigned long long *);
#endif

/* ordered.c */

extern void gomp_ordered_first (void);
extern void gomp_ordered_last (void);
extern void gomp_ordered_next (void);
extern void gomp_ordered_static_init (void);
extern void gomp_ordered_static_next (void);
extern void gomp_ordered_sync (void);
extern void gomp_doacross_init (unsigned, long *, long, size_t);
extern void gomp_doacross_ull_init (unsigned, unsigned long long *,
				    unsigned long long, size_t);

/* parallel.c */

extern unsigned gomp_resolve_num_threads (unsigned, unsigned);

/* proc.c (in config/) */

extern void gomp_init_num_threads (void);
extern unsigned gomp_dynamic_max_threads (void);

/* task.c */

extern void gomp_init_task (struct gomp_task *, struct gomp_task *,
			    struct gomp_task_icv *);
extern void gomp_end_task (void);
extern void gomp_barrier_handle_tasks (gomp_barrier_state_t);
extern void gomp_task_maybe_wait_for_dependencies (void **);
extern bool gomp_create_target_task (struct gomp_device_descr *,
				     void (*) (void *), size_t, void **,
				     size_t *, unsigned short *, unsigned int,
				     void **, void **,
				     enum gomp_target_task_state);
extern struct gomp_taskgroup *gomp_parallel_reduction_register (uintptr_t *,
								unsigned);
extern void gomp_workshare_taskgroup_start (void);
extern void gomp_workshare_task_reduction_register (uintptr_t *, uintptr_t *);

static void inline
gomp_finish_task (struct gomp_task *task)
{
  if (__builtin_expect (task->depend_hash != NULL, 0))
    free (task->depend_hash);
}

/* team.c */

extern struct gomp_team *gomp_new_team (unsigned);
extern void gomp_team_start (void (*) (void *), void *, unsigned,
			     unsigned, struct gomp_team *,
			     struct gomp_taskgroup *);
extern void gomp_team_end (void);
extern void gomp_free_thread (void *);
extern int gomp_pause_host (void);

/* target.c */

extern void gomp_init_targets_once (void);
extern int gomp_get_num_devices (void);
extern bool gomp_target_task_fn (void *);
extern void gomp_target_rev (uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
			     int, struct goacc_asyncqueue *);

/* Splay tree definitions.  */
typedef struct splay_tree_node_s *splay_tree_node;
typedef struct splay_tree_s *splay_tree;
typedef struct splay_tree_key_s *splay_tree_key;

struct target_var_desc {
  /* Splay key.  */
  splay_tree_key key;
  /* True if data should be copied from device to host at the end.  */
  bool copy_from;
  /* True if data always should be copied from device to host at the end.  */
  bool always_copy_from;
  /* True if this is for OpenACC 'attach'.  */
  bool is_attach;
  /* If GOMP_MAP_TO_PSET had a NULL pointer; used for Fortran descriptors,
     which were initially unallocated.  */
  bool has_null_ptr_assoc;
  /* Relative offset against key host_start.  */
  uintptr_t offset;
  /* Actual length.  */
  uintptr_t length;
};

struct target_mem_desc;

/* Special value for refcount - mask to indicate existence of special
   values. Right now we allocate 3 bits.  */
#define REFCOUNT_SPECIAL (~(uintptr_t) 0x7)

/* Special value for refcount - infinity.  */
#define REFCOUNT_INFINITY (REFCOUNT_SPECIAL | 0)
/* Special value for refcount - tgt_offset contains target address of the
   artificial pointer to "omp declare target link" object.  */
#define REFCOUNT_LINK     (REFCOUNT_SPECIAL | 1)
/* Special value for refcount - created through acc_map_data.  */
#define REFCOUNT_ACC_MAP_DATA (REFCOUNT_SPECIAL | 2)

/* Special value for refcount - structure element sibling list items.
   All such key refounts have REFCOUNT_STRUCTELEM bits set, with _FLAG_FIRST
   and _FLAG_LAST indicating first and last in the created sibling sequence.  */
#define REFCOUNT_STRUCTELEM (REFCOUNT_SPECIAL | 4)
#define REFCOUNT_STRUCTELEM_P(V)			\
  (((V) & REFCOUNT_STRUCTELEM) == REFCOUNT_STRUCTELEM)
/* The first leading key with _FLAG_FIRST set houses the actual reference count
   in the structelem_refcount field. Other siblings point to this counter value
   through its structelem_refcount_ptr field.  */
#define REFCOUNT_STRUCTELEM_FLAG_FIRST (1)
/* The last key in the sibling sequence has this set. This is required to
   indicate the sequence boundary, when we remove the structure sibling list
   from the map.  */
#define REFCOUNT_STRUCTELEM_FLAG_LAST  (2)

#define REFCOUNT_STRUCTELEM_FIRST_P(V)					\
  (REFCOUNT_STRUCTELEM_P (V) && ((V) & REFCOUNT_STRUCTELEM_FLAG_FIRST))
#define REFCOUNT_STRUCTELEM_LAST_P(V)					\
  (REFCOUNT_STRUCTELEM_P (V) && ((V) & REFCOUNT_STRUCTELEM_FLAG_LAST))

/* Special offset values.  */
#define OFFSET_INLINED (~(uintptr_t) 0)
#define OFFSET_POINTER (~(uintptr_t) 1)
#define OFFSET_STRUCT (~(uintptr_t) 2)

/* Auxiliary structure for infrequently-used or API-specific data.  */

struct splay_tree_aux {
  /* Pointer to the original mapping of "omp declare target link" object.  */
  splay_tree_key link_key;
  /* For a block with attached pointers, the attachment counters for each.
     Only used for OpenACC.  */
  uintptr_t *attach_count;
};

struct splay_tree_key_s {
  /* Address of the host object.  */
  uintptr_t host_start;
  /* Address immediately after the host object.  */
  uintptr_t host_end;
  /* Descriptor of the target memory.  */
  struct target_mem_desc *tgt;
  /* Offset from tgt->tgt_start to the start of the target object.  */
  uintptr_t tgt_offset;
  /* Reference count.  */
  uintptr_t refcount;
  union {
    /* Dynamic reference count.  */
    uintptr_t dynamic_refcount;

    /* Unified reference count for structure element siblings, this is used
       when REFCOUNT_STRUCTELEM_FIRST_P(k->refcount) == true, the first sibling
       in a structure element sibling list item sequence.  */
    uintptr_t structelem_refcount;

    /* When REFCOUNT_STRUCTELEM_P (k->refcount) == true, this field points
       into the (above) structelem_refcount field of the _FIRST splay_tree_key,
       the first key in the created sequence. All structure element siblings
       share a single refcount in this manner. Since these two fields won't be
       used at the same time, they are stashed in a union.  */
    uintptr_t *structelem_refcount_ptr;
  };
  struct splay_tree_aux *aux;
};

/* The comparison function.  */

static inline int
splay_compare (splay_tree_key x, splay_tree_key y)
{
  if (x->host_start == x->host_end
      && y->host_start == y->host_end)
    return 0;
  if (x->host_end <= y->host_start)
    return -1;
  if (x->host_start >= y->host_end)
    return 1;
  return 0;
}

#include "splay-tree.h"

/* Reverse offload splay-tree handling (functions only). */

struct reverse_splay_tree_key_s {
  /* Address of the device object.  */
  uint64_t dev;
  splay_tree_key k;
};

typedef struct reverse_splay_tree_node_s *reverse_splay_tree_node;
typedef struct reverse_splay_tree_s *reverse_splay_tree;
typedef struct reverse_splay_tree_key_s *reverse_splay_tree_key;

static inline int
reverse_splay_compare (reverse_splay_tree_key x, reverse_splay_tree_key y)
{
  if (x->dev < y->dev)
    return -1;
  if (x->dev > y->dev)
    return 1;
  return 0;
}

#define splay_tree_prefix reverse
#define splay_tree_static
#include "splay-tree.h"

/* Indirect target function splay-tree handling.  */

struct indirect_splay_tree_key_s {
  uint64_t host_addr, target_addr;
};

typedef struct indirect_splay_tree_node_s *indirect_splay_tree_node;
typedef struct indirect_splay_tree_s *indirect_splay_tree;
typedef struct indirect_splay_tree_key_s *indirect_splay_tree_key;

static inline int
indirect_splay_compare (indirect_splay_tree_key x, indirect_splay_tree_key y)
{
  if (x->host_addr < y->host_addr)
    return -1;
  if (x->host_addr > y->host_addr)
    return 1;
  return 0;
}

#define splay_tree_prefix indirect
#include "splay-tree.h"

struct target_mem_desc {
  /* Reference count.  */
  uintptr_t refcount;
  /* All the splay nodes allocated together.  */
  splay_tree_node array;
  /* Likewise for the reverse lookup device->host for reverse offload. */
  reverse_splay_tree_node rev_array;
  /* Start of the target region.  */
  uintptr_t tgt_start;
  /* End of the targer region.  */
  uintptr_t tgt_end;
  /* Handle to free.  */
  void *to_free;
  /* Previous target_mem_desc.  */
  struct target_mem_desc *prev;
  /* Number of items in following list.  */
  size_t list_count;

  /* Corresponding target device descriptor.  */
  struct gomp_device_descr *device_descr;

  /* List of target items to remove (or decrease refcount)
     at the end of region.  */
  struct target_var_desc list[];
};


typedef struct acc_dispatch_t
{
  /* Execute.  */
  __typeof (GOMP_OFFLOAD_openacc_exec) *exec_func;

  /* Create/destroy TLS data.  */
  __typeof (GOMP_OFFLOAD_openacc_create_thread_data) *create_thread_data_func;
  __typeof (GOMP_OFFLOAD_openacc_destroy_thread_data)
    *destroy_thread_data_func;
  
  struct {
    /* Once created and put into the "active" list, asyncqueues are then never
       destructed and removed from the "active" list, other than if the TODO
       device is shut down.  */
    gomp_mutex_t lock;
    int nasyncqueue;
    struct goacc_asyncqueue **asyncqueue;
    struct goacc_asyncqueue_list *active;

    __typeof (GOMP_OFFLOAD_openacc_async_construct) *construct_func;
    __typeof (GOMP_OFFLOAD_openacc_async_destruct) *destruct_func;
    __typeof (GOMP_OFFLOAD_openacc_async_test) *test_func;
    __typeof (GOMP_OFFLOAD_openacc_async_synchronize) *synchronize_func;
    __typeof (GOMP_OFFLOAD_openacc_async_serialize) *serialize_func;
    __typeof (GOMP_OFFLOAD_openacc_async_queue_callback) *queue_callback_func;

    __typeof (GOMP_OFFLOAD_openacc_async_exec) *exec_func;
    __typeof (GOMP_OFFLOAD_openacc_async_dev2host) *dev2host_func;
    __typeof (GOMP_OFFLOAD_openacc_async_host2dev) *host2dev_func;
  } async;

  __typeof (GOMP_OFFLOAD_openacc_get_property) *get_property_func;

  /* NVIDIA target specific routines.  */
  struct {
    __typeof (GOMP_OFFLOAD_openacc_cuda_get_current_device)
      *get_current_device_func;
    __typeof (GOMP_OFFLOAD_openacc_cuda_get_current_context)
      *get_current_context_func;
    __typeof (GOMP_OFFLOAD_openacc_cuda_get_stream) *get_stream_func;
    __typeof (GOMP_OFFLOAD_openacc_cuda_set_stream) *set_stream_func;
  } cuda;
} acc_dispatch_t;

/* Various state of the accelerator device.  */
enum gomp_device_state
{
  GOMP_DEVICE_UNINITIALIZED,
  GOMP_DEVICE_INITIALIZED,
  GOMP_DEVICE_FINALIZED
};

/* This structure describes accelerator device.
   It contains name of the corresponding libgomp plugin, function handlers for
   interaction with the device, ID-number of the device, and information about
   mapped memory.  */
struct gomp_device_descr
{
  /* Immutable data, which is only set during initialization, and which is not
     guarded by the lock.  */

  /* The name of the device.  */
  const char *name;

  /* Capabilities of device (supports OpenACC, OpenMP).  */
  unsigned int capabilities;

  /* This is the ID number of device among devices of the same type.  */
  int target_id;

  /* This is the TYPE of device.  */
  enum offload_target_type type;

  /* Function handlers.  */
  __typeof (GOMP_OFFLOAD_get_name) *get_name_func;
  __typeof (GOMP_OFFLOAD_get_caps) *get_caps_func;
  __typeof (GOMP_OFFLOAD_get_type) *get_type_func;
  __typeof (GOMP_OFFLOAD_get_num_devices) *get_num_devices_func;
  __typeof (GOMP_OFFLOAD_init_device) *init_device_func;
  __typeof (GOMP_OFFLOAD_fini_device) *fini_device_func;
  __typeof (GOMP_OFFLOAD_version) *version_func;
  __typeof (GOMP_OFFLOAD_load_image) *load_image_func;
  __typeof (GOMP_OFFLOAD_unload_image) *unload_image_func;
  __typeof (GOMP_OFFLOAD_alloc) *alloc_func;
  __typeof (GOMP_OFFLOAD_free) *free_func;
  __typeof (GOMP_OFFLOAD_dev2host) *dev2host_func;
  __typeof (GOMP_OFFLOAD_host2dev) *host2dev_func;
  __typeof (GOMP_OFFLOAD_memcpy2d) *memcpy2d_func;
  __typeof (GOMP_OFFLOAD_memcpy3d) *memcpy3d_func;
  __typeof (GOMP_OFFLOAD_dev2dev) *dev2dev_func;
  __typeof (GOMP_OFFLOAD_can_run) *can_run_func;
  __typeof (GOMP_OFFLOAD_run) *run_func;
  __typeof (GOMP_OFFLOAD_async_run) *async_run_func;

  /* Splay tree containing information about mapped memory regions.  */
  struct splay_tree_s mem_map;
  struct reverse_splay_tree_s mem_map_rev;

  /* Mutex for the mutable data.  */
  gomp_mutex_t lock;

  /* Current state of the device.  OpenACC allows to move from INITIALIZED state
     back to UNINITIALIZED state.  OpenMP allows only to move from INITIALIZED
     to FINALIZED state (at program shutdown).  */
  enum gomp_device_state state;

  /* OpenACC-specific data and functions.  */
  /* This is mutable because of its mutable target_data member.  */
  acc_dispatch_t openacc;
};

/* Kind of the pragma, for which gomp_map_vars () is called.  */
enum gomp_map_vars_kind
{
  GOMP_MAP_VARS_OPENACC    = 1,
  GOMP_MAP_VARS_TARGET     = 2,
  GOMP_MAP_VARS_DATA       = 4,
  GOMP_MAP_VARS_ENTER_DATA = 8
};

extern void gomp_acc_declare_allocate (bool, size_t, void **, size_t *,
				       unsigned short *);
struct gomp_coalesce_buf;
extern void gomp_copy_host2dev (struct gomp_device_descr *,
				struct goacc_asyncqueue *, void *, const void *,
				size_t, bool, struct gomp_coalesce_buf *);
extern void gomp_copy_dev2host (struct gomp_device_descr *,
				struct goacc_asyncqueue *, void *, const void *,
				size_t);
extern uintptr_t gomp_map_val (struct target_mem_desc *, void **, size_t);
extern void gomp_attach_pointer (struct gomp_device_descr *,
				 struct goacc_asyncqueue *, splay_tree,
				 splay_tree_key, uintptr_t, size_t,
				 struct gomp_coalesce_buf *, bool);
extern void gomp_detach_pointer (struct gomp_device_descr *,
				 struct goacc_asyncqueue *, splay_tree_key,
				 uintptr_t, bool, struct gomp_coalesce_buf *);
extern struct target_mem_desc *goacc_map_vars (struct gomp_device_descr *,
					       struct goacc_asyncqueue *,
					       size_t, void **, void **,
					       size_t *, void *, bool,
					       enum gomp_map_vars_kind);
extern void goacc_unmap_vars (struct target_mem_desc *, bool,
			      struct goacc_asyncqueue *);
extern void gomp_init_device (struct gomp_device_descr *);
extern bool gomp_fini_device (struct gomp_device_descr *);
extern void gomp_unload_device (struct gomp_device_descr *);
extern bool gomp_remove_var (struct gomp_device_descr *, splay_tree_key);
extern void gomp_remove_var_async (struct gomp_device_descr *, splay_tree_key,
				   struct goacc_asyncqueue *);

/* work.c */

extern void gomp_init_work_share (struct gomp_work_share *, size_t, unsigned);
extern void gomp_fini_work_share (struct gomp_work_share *);
extern bool gomp_work_share_start (size_t);
extern void gomp_work_share_end (void);
extern bool gomp_work_share_end_cancel (void);
extern void gomp_work_share_end_nowait (void);

static inline void
gomp_work_share_init_done (void)
{
  struct gomp_thread *thr = gomp_thread ();
  if (__builtin_expect (thr->ts.last_work_share != NULL, 1))
    gomp_ptrlock_set (&thr->ts.last_work_share->next_ws, thr->ts.work_share);
}

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility pop
#endif

/* Now that we're back to default visibility, include the globals.  */
#include "libgomp_g.h"

/* Include omp.h by parts.  */
#include "omp-lock.h"
#define _LIBGOMP_OMP_LOCK_DEFINED 1
#include "omp.h.in"

#if !defined (HAVE_ATTRIBUTE_VISIBILITY) \
    || !defined (HAVE_ATTRIBUTE_ALIAS) \
    || !defined (HAVE_AS_SYMVER_DIRECTIVE) \
    || !defined (PIC) \
    || !defined (HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT)
# undef LIBGOMP_GNU_SYMBOL_VERSIONING
#endif

#ifdef LIBGOMP_GNU_SYMBOL_VERSIONING
extern void gomp_init_lock_30 (omp_lock_t *) __GOMP_NOTHROW;
extern void gomp_destroy_lock_30 (omp_lock_t *) __GOMP_NOTHROW;
extern void gomp_set_lock_30 (omp_lock_t *) __GOMP_NOTHROW;
extern void gomp_unset_lock_30 (omp_lock_t *) __GOMP_NOTHROW;
extern int gomp_test_lock_30 (omp_lock_t *) __GOMP_NOTHROW;
extern void gomp_init_nest_lock_30 (omp_nest_lock_t *) __GOMP_NOTHROW;
extern void gomp_destroy_nest_lock_30 (omp_nest_lock_t *) __GOMP_NOTHROW;
extern void gomp_set_nest_lock_30 (omp_nest_lock_t *) __GOMP_NOTHROW;
extern void gomp_unset_nest_lock_30 (omp_nest_lock_t *) __GOMP_NOTHROW;
extern int gomp_test_nest_lock_30 (omp_nest_lock_t *) __GOMP_NOTHROW;

extern void gomp_init_lock_25 (omp_lock_25_t *) __GOMP_NOTHROW;
extern void gomp_destroy_lock_25 (omp_lock_25_t *) __GOMP_NOTHROW;
extern void gomp_set_lock_25 (omp_lock_25_t *) __GOMP_NOTHROW;
extern void gomp_unset_lock_25 (omp_lock_25_t *) __GOMP_NOTHROW;
extern int gomp_test_lock_25 (omp_lock_25_t *) __GOMP_NOTHROW;
extern void gomp_init_nest_lock_25 (omp_nest_lock_25_t *) __GOMP_NOTHROW;
extern void gomp_destroy_nest_lock_25 (omp_nest_lock_25_t *) __GOMP_NOTHROW;
extern void gomp_set_nest_lock_25 (omp_nest_lock_25_t *) __GOMP_NOTHROW;
extern void gomp_unset_nest_lock_25 (omp_nest_lock_25_t *) __GOMP_NOTHROW;
extern int gomp_test_nest_lock_25 (omp_nest_lock_25_t *) __GOMP_NOTHROW;

# define omp_lock_symver(fn) \
  __asm (".symver g" #fn "_30, " #fn "@@OMP_3.0"); \
  __asm (".symver g" #fn "_25, " #fn "@OMP_1.0");
#else
# define gomp_init_lock_30 omp_init_lock
# define gomp_destroy_lock_30 omp_destroy_lock
# define gomp_set_lock_30 omp_set_lock
# define gomp_unset_lock_30 omp_unset_lock
# define gomp_test_lock_30 omp_test_lock
# define gomp_init_nest_lock_30 omp_init_nest_lock
# define gomp_destroy_nest_lock_30 omp_destroy_nest_lock
# define gomp_set_nest_lock_30 omp_set_nest_lock
# define gomp_unset_nest_lock_30 omp_unset_nest_lock
# define gomp_test_nest_lock_30 omp_test_nest_lock
#endif

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif

#if __GNUC__ >= 9
#  define HAVE_ATTRIBUTE_COPY
#endif

#ifdef HAVE_ATTRIBUTE_COPY
# define attribute_copy(arg) __attribute__ ((copy (arg)))
#else
# define attribute_copy(arg)
#endif

#ifdef HAVE_ATTRIBUTE_ALIAS
# define strong_alias(fn, al) \
  extern __typeof (fn) al __attribute__ ((alias (#fn))) attribute_copy (fn);

# define ialias_ulp	ialias_str1(__USER_LABEL_PREFIX__)
# define ialias_str1(x)	ialias_str2(x)
# define ialias_str2(x)	#x
# define ialias(fn) \
  extern __typeof (fn) gomp_ialias_##fn \
    __attribute__ ((alias (#fn))) attribute_hidden attribute_copy (fn);
# define ialias_redirect(fn) \
  extern __typeof (fn) fn __asm__ (ialias_ulp "gomp_ialias_" #fn) attribute_hidden;
# define ialias_call(fn) gomp_ialias_ ## fn
#else
# define ialias(fn)
# define ialias_redirect(fn)
# define ialias_call(fn) fn
#endif

/* Helper function for priority_node_to_task() and
   task_to_priority_node().

   Return the offset from a task to its priority_node entry.  The
   priority_node entry is has a type of TYPE.  */

static inline size_t
priority_queue_offset (enum priority_queue_type type)
{
  return offsetof (struct gomp_task, pnode[(int) type]);
}

/* Return the task associated with a priority NODE of type TYPE.  */

static inline struct gomp_task *
priority_node_to_task (enum priority_queue_type type,
		       struct priority_node *node)
{
  return (struct gomp_task *) ((char *) node - priority_queue_offset (type));
}

/* Return the priority node of type TYPE for a given TASK.  */

static inline struct priority_node *
task_to_priority_node (enum priority_queue_type type,
		       struct gomp_task *task)
{
  return (struct priority_node *) ((char *) task
				   + priority_queue_offset (type));
}

#ifdef LIBGOMP_USE_PTHREADS
static inline gomp_thread_handle
gomp_thread_self (void)
{
  return pthread_self ();
}

static inline gomp_thread_handle
gomp_thread_to_pthread_t (struct gomp_thread *thr)
{
  struct gomp_thread *this_thr = gomp_thread ();
  if (thr == this_thr)
    return pthread_self ();
#ifdef GOMP_NEEDS_THREAD_HANDLE
  return thr->handle;
#else
  /* On Linux with initial-exec TLS, the pthread_t of the thread containing
     thr can be computed from thr, this_thr and pthread_self (),
     as the distance between this_thr and pthread_self () is constant.  */
  return pthread_self () + ((uintptr_t) thr - (uintptr_t) this_thr);
#endif
}
#else
static inline gomp_thread_handle
gomp_thread_self (void)
{
  return (gomp_thread_handle) {};
}

static inline gomp_thread_handle
gomp_thread_to_pthread_t (struct gomp_thread *thr)
{
  (void) thr;
  return gomp_thread_self ();
}
#endif

#endif /* LIBGOMP_H */
