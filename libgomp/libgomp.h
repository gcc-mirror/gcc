/* Copyright (C) 2005-2016 Free Software Foundation, Inc.
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
#include "gstdint.h"
#include "libgomp-plugin.h"

#include <pthread.h>
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

extern void *gomp_malloc (size_t) __attribute__((malloc));
extern void *gomp_malloc_cleared (size_t) __attribute__((malloc));
extern void *gomp_realloc (void *, size_t);

/* Avoid conflicting prototypes of alloca() in system headers by using
   GCC's builtin alloca().  */
#define gomp_alloca(x)  __builtin_alloca(x)

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
#include "ptrlock.h"


/* This structure contains the data to control one work-sharing construct,
   either a LOOP (FOR/DO) or a SECTIONS.  */

enum gomp_schedule_type
{
  GFS_RUNTIME,
  GFS_STATIC,
  GFS_DYNAMIC,
  GFS_GUIDED,
  GFS_AUTO
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
  /* Array of shift counts for each dimension if they can be flattened.  */
  unsigned int shift_counts[];
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
  gomp_mutex_t lock __attribute__((aligned (64)));

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

  /* If only few threads are in the team, ordered_team_ids can point
     to this array which fills the padding at the end of this struct.  */
  unsigned inline_ordered_team_ids[0];
};

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
  bool nest_var;
  char bind_var;
  /* Internal ICV.  */
  struct target_mem_desc *target_data;
};

extern struct gomp_task_icv gomp_global_icv;
#ifndef HAVE_SYNC_BUILTINS
extern gomp_mutex_t gomp_managed_threads_lock;
#endif
extern unsigned long gomp_max_active_levels_var;
extern bool gomp_cancel_var;
extern int gomp_max_task_priority_var;
extern unsigned long long gomp_spin_count_var, gomp_throttled_spin_count_var;
extern unsigned long gomp_available_cpus, gomp_managed_threads;
extern unsigned long *gomp_nthreads_var_list, gomp_nthreads_var_list_len;
extern char *gomp_bind_var_list;
extern unsigned long gomp_bind_var_list_len;
extern void **gomp_places_list;
extern unsigned long gomp_places_list_len;
extern int gomp_debug_var;
extern int goacc_device_num;
extern char *goacc_device_type;

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
  GOMP_TASK_ASYNC_RUNNING
};

struct gomp_task_depend_entry
{
  /* Address of dependency.  */
  void *addr;
  struct gomp_task_depend_entry *next;
  struct gomp_task_depend_entry *prev;
  /* Task that provides the dependency in ADDR.  */
  struct gomp_task *task;
  /* Depend entry is of type "IN".  */
  bool is_in;
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
  /* Number of items in DEPEND.  */
  size_t depend_count;
  /* Number of tasks this task depends on.  Once this counter reaches
     0, we have no unsatisfied dependencies, and this task can be put
     into the various queues to be scheduled.  */
  size_t num_dependees;

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
  bool in_taskgroup_wait;
  bool cancelled;
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

  /* This barrier holds and releases threads waiting in threads.  */
  gomp_barrier_t threads_dock;
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

#if defined HAVE_TLS || defined USE_EMUTLS
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

/* The attributes to be used during thread creation.  */
extern pthread_attr_t gomp_thread_attr;

extern pthread_key_t gomp_thread_destructor;

/* Function prototypes.  */

/* affinity.c */

extern void gomp_init_affinity (void);
extern void gomp_init_thread_affinity (pthread_attr_t *, unsigned int);
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
extern void gomp_doacross_init (unsigned, long *, long);
extern void gomp_doacross_ull_init (unsigned, unsigned long long *,
				    unsigned long long);

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

static void inline
gomp_finish_task (struct gomp_task *task)
{
  if (__builtin_expect (task->depend_hash != NULL, 0))
    free (task->depend_hash);
}

/* team.c */

extern struct gomp_team *gomp_new_team (unsigned);
extern void gomp_team_start (void (*) (void *), void *, unsigned,
			     unsigned, struct gomp_team *);
extern void gomp_team_end (void);
extern void gomp_free_thread (void *);

/* target.c */

extern void gomp_init_targets_once (void);
extern int gomp_get_num_devices (void);
extern bool gomp_target_task_fn (void *);

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
  /* Relative offset against key host_start.  */
  uintptr_t offset;
  /* Actual length.  */
  uintptr_t length;
};

struct target_mem_desc {
  /* Reference count.  */
  uintptr_t refcount;
  /* All the splay nodes allocated together.  */
  splay_tree_node array;
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

/* Special value for refcount - infinity.  */
#define REFCOUNT_INFINITY (~(uintptr_t) 0)
/* Special value for refcount - tgt_offset contains target address of the
   artificial pointer to "omp declare target link" object.  */
#define REFCOUNT_LINK (~(uintptr_t) 1)

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
  /* Asynchronous reference count.  */
  uintptr_t async_refcount;
  /* Pointer to the original mapping of "omp declare target link" object.  */
  splay_tree_key link_key;
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

typedef struct acc_dispatch_t
{
  /* This is a linked list of data mapped using the
     acc_map_data/acc_unmap_data or "acc enter data"/"acc exit data" pragmas.
     Unlike mapped_data in the goacc_thread struct, unmapping can
     happen out-of-order with respect to mapping.  */
  /* This is guarded by the lock in the "outer" struct gomp_device_descr.  */
  struct target_mem_desc *data_environ;

  /* Execute.  */
  void (*exec_func) (void (*) (void *), size_t, void **, void **, int,
		     unsigned *, void *);

  /* Async cleanup callback registration.  */
  void (*register_async_cleanup_func) (void *);

  /* Asynchronous routines.  */
  int (*async_test_func) (int);
  int (*async_test_all_func) (void);
  void (*async_wait_func) (int);
  void (*async_wait_async_func) (int, int);
  void (*async_wait_all_func) (void);
  void (*async_wait_all_async_func) (int);
  void (*async_set_async_func) (int);

  /* Create/destroy TLS data.  */
  void *(*create_thread_data_func) (int);
  void (*destroy_thread_data_func) (void *);

  /* NVIDIA target specific routines.  */
  struct {
    void *(*get_current_device_func) (void);
    void *(*get_current_context_func) (void);
    void *(*get_stream_func) (int);
    int (*set_stream_func) (int, void *);
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
  const char *(*get_name_func) (void);
  unsigned int (*get_caps_func) (void);
  int (*get_type_func) (void);
  int (*get_num_devices_func) (void);
  void (*init_device_func) (int);
  void (*fini_device_func) (int);
  unsigned (*version_func) (void);
  int (*load_image_func) (int, unsigned, const void *, struct addr_pair **);
  void (*unload_image_func) (int, unsigned, const void *);
  void *(*alloc_func) (int, size_t);
  void (*free_func) (int, void *);
  void *(*dev2host_func) (int, void *, const void *, size_t);
  void *(*host2dev_func) (int, void *, const void *, size_t);
  void *(*dev2dev_func) (int, void *, const void *, size_t);
  bool (*can_run_func) (void *);
  void (*run_func) (int, void *, void *, void **);
  void (*async_run_func) (int, void *, void *, void **, void *);

  /* Splay tree containing information about mapped memory regions.  */
  struct splay_tree_s mem_map;

  /* Mutex for the mutable data.  */
  gomp_mutex_t lock;

  /* Current state of the device.  OpenACC allows to move from INITIALIZED state
     back to UNINITIALIZED state.  OpenMP allows only to move from INITIALIZED
     to FINALIZED state (at program shutdown).  */
  enum gomp_device_state state;

  /* OpenACC-specific data and functions.  */
  /* This is mutable because of its mutable data_environ and target_data
     members.  */
  acc_dispatch_t openacc;
};

/* Kind of the pragma, for which gomp_map_vars () is called.  */
enum gomp_map_vars_kind
{
  GOMP_MAP_VARS_OPENACC,
  GOMP_MAP_VARS_TARGET,
  GOMP_MAP_VARS_DATA,
  GOMP_MAP_VARS_ENTER_DATA
};

extern void gomp_acc_insert_pointer (size_t, void **, size_t *, void *);
extern void gomp_acc_remove_pointer (void *, bool, int, int);

extern struct target_mem_desc *gomp_map_vars (struct gomp_device_descr *,
					      size_t, void **, void **,
					      size_t *, void *, bool,
					      enum gomp_map_vars_kind);
extern void gomp_copy_from_async (struct target_mem_desc *);
extern void gomp_unmap_vars (struct target_mem_desc *, bool);
extern void gomp_init_device (struct gomp_device_descr *);
extern void gomp_free_memmap (struct splay_tree_s *);
extern void gomp_unload_device (struct gomp_device_descr *);

/* work.c */

extern void gomp_init_work_share (struct gomp_work_share *, bool, unsigned);
extern void gomp_fini_work_share (struct gomp_work_share *);
extern bool gomp_work_share_start (bool);
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

# define strong_alias(fn, al) \
  extern __typeof (fn) al __attribute__ ((alias (#fn)));
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

#ifdef HAVE_ATTRIBUTE_ALIAS
# define ialias_ulp	ialias_str1(__USER_LABEL_PREFIX__)
# define ialias_str1(x)	ialias_str2(x)
# define ialias_str2(x)	#x
# define ialias(fn) \
  extern __typeof (fn) gomp_ialias_##fn \
    __attribute__ ((alias (#fn))) attribute_hidden;
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
#endif /* LIBGOMP_H */
