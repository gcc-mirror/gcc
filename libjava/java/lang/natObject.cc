// natObject.cc - Implementation of the Object class.

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <string.h>

#pragma implementation "Object.h"

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Object.h>
#include <java-threads.h>
#include <java-signal.h>
#include <java/lang/CloneNotSupportedException.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/IllegalMonitorStateException.h>
#include <java/lang/InterruptedException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/Class.h>
#include <java/lang/Cloneable.h>
#include <java/lang/Thread.h>

#ifdef LOCK_DEBUG
#  include <stdio.h>
#endif



// This is used to represent synchronization information.
struct _Jv_SyncInfo
{
#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
  // We only need to keep track of initialization state if we can
  // possibly finalize this object.
  bool init;
#endif
  _Jv_ConditionVariable_t condition;
  _Jv_Mutex_t mutex;
};



jclass
java::lang::Object::getClass (void)
{
  _Jv_VTable **dt = (_Jv_VTable **) this;
  return (*dt)->clas;
}

jint
java::lang::Object::hashCode (void)
{
  return _Jv_HashCode (this);
}

jobject
java::lang::Object::clone (void)
{
  jclass klass = getClass ();
  jobject r;
  jint size;

  // We also clone arrays here.  If we put the array code into
  // __JArray, then we'd have to figure out a way to find the array
  // vtbl when creating a new array class.  This is easier, if uglier.
  if (klass->isArray())
    {
      __JArray *array = (__JArray *) this;
      jclass comp = getClass()->getComponentType();
      jint eltsize;
      if (comp->isPrimitive())
	{
	  r = _Jv_NewPrimArray (comp, array->length);
	  eltsize = comp->size();
	}
      else
	{
	  r = _Jv_NewObjectArray (array->length, comp, NULL);
	  eltsize = sizeof (jobject);
	}
      // We can't use sizeof on __JArray because we must account for
      // alignment of the element type.
      size = (_Jv_GetArrayElementFromElementType (array, comp) - (char *) array
	      + array->length * eltsize);
    }
  else
    {
      if (! java::lang::Cloneable::class$.isAssignableFrom(klass))
	throw new CloneNotSupportedException;

      size = klass->size();
      r = JvAllocObject (klass, size);
    }

  memcpy ((void *) r, (void *) this, size);
  return r;
}

void
_Jv_FinalizeObject (jobject obj)
{
  // Ignore exceptions.  From section 12.6 of the Java Language Spec.
  try
    {
      obj->finalize ();
    }
  catch (java::lang::Throwable *t)
    {
      // Ignore.
    }
}


//
// Synchronization code.
//

#ifndef JV_HASH_SYNCHRONIZATION
// This global is used to make sure that only one thread sets an
// object's `sync_info' field.
static _Jv_Mutex_t sync_mutex;

// This macro is used to see if synchronization initialization is
// needed.
#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
#  define INIT_NEEDED(Obj) (! (Obj)->sync_info \
			    || ! ((_Jv_SyncInfo *) ((Obj)->sync_info))->init)
#else
#  define INIT_NEEDED(Obj) (! (Obj)->sync_info)
#endif

#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
// If we have to run a destructor for a sync_info member, then this
// function is registered as a finalizer for the sync_info.
static void
finalize_sync_info (jobject obj)
{
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) obj;
#if defined (_Jv_HaveCondDestroy)
  _Jv_CondDestroy (&si->condition);
#endif
#if defined (_Jv_HaveMutexDestroy)
  _Jv_MutexDestroy (&si->mutex);
#endif
  si->init = false;
}
#endif

// This is called to initialize the sync_info element of an object.
void
java::lang::Object::sync_init (void)
{
  _Jv_MutexLock (&sync_mutex);
  // Check again to see if initialization is needed now that we have
  // the lock.
  if (INIT_NEEDED (this))
    {
      // We assume there are no pointers in the sync_info
      // representation.
      _Jv_SyncInfo *si;
      // We always create a new sync_info, even if there is already
      // one available.  Any given object can only be finalized once.
      // If we get here and sync_info is not null, then it has already
      // been finalized.  So if we just reinitialize the old one,
      // we'll never be able to (re-)destroy the mutex and/or
      // condition variable.
      si = (_Jv_SyncInfo *) _Jv_AllocBytes (sizeof (_Jv_SyncInfo));
      _Jv_MutexInit (&si->mutex);
      _Jv_CondInit (&si->condition);
#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
      // Register a finalizer.
      si->init = true;
      _Jv_RegisterFinalizer (si, finalize_sync_info);
#endif
      sync_info = (jobject) si;
    }
  _Jv_MutexUnlock (&sync_mutex);
}

void
java::lang::Object::notify (void)
{
  if (__builtin_expect (INIT_NEEDED (this), false))
    sync_init ();
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) sync_info;
  if (__builtin_expect (_Jv_CondNotify (&si->condition, &si->mutex), false))
    throw new IllegalMonitorStateException(JvNewStringLatin1 
					   ("current thread not owner"));
}

void
java::lang::Object::notifyAll (void)
{
  if (__builtin_expect (INIT_NEEDED (this), false))
    sync_init ();
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) sync_info;
  if (__builtin_expect (_Jv_CondNotifyAll (&si->condition, &si->mutex), false))
    throw new IllegalMonitorStateException(JvNewStringLatin1 
					   ("current thread not owner"));
}

void
java::lang::Object::wait (jlong timeout, jint nanos)
{
  if (__builtin_expect (INIT_NEEDED (this), false))
    sync_init ();
  if (__builtin_expect (timeout < 0 || nanos < 0 || nanos > 999999, false))
    throw new IllegalArgumentException;
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) sync_info;
  switch (_Jv_CondWait (&si->condition, &si->mutex, timeout, nanos))
    {
      case _JV_NOT_OWNER:
	throw new IllegalMonitorStateException (JvNewStringLatin1 
						("current thread not owner"));
      case _JV_INTERRUPTED:
	if (Thread::interrupted ())
	  throw new InterruptedException;
    }
}

//
// Some runtime code.
//

// This function is called at system startup to initialize the
// `sync_mutex'.
void
_Jv_InitializeSyncMutex (void)
{
  _Jv_MutexInit (&sync_mutex);
}

void
_Jv_MonitorEnter (jobject obj)
{
#ifndef HANDLE_SEGV
  if (__builtin_expect (! obj, false))
    throw new java::lang::NullPointerException;
#endif
  if (__builtin_expect (INIT_NEEDED (obj), false))
    obj->sync_init ();
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) obj->sync_info;
  _Jv_MutexLock (&si->mutex);
  // FIXME: In the Windows case, this can return a nonzero error code.
  // We should turn that into some exception ...
}

void
_Jv_MonitorExit (jobject obj)
{
  JvAssert (obj);
  JvAssert (! INIT_NEEDED (obj));
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) obj->sync_info;
  if (__builtin_expect (_Jv_MutexUnlock (&si->mutex), false))
    throw new java::lang::IllegalMonitorStateException;
}

#else /* JV_HASH_SYNCHRONIZATION */

// FIXME: We shouldn't be calling GC_register_finalizer directly.
#ifndef HAVE_BOEHM_GC
# error Hash synchronization currently requires boehm-gc
// That's actually a bit of a lie: It should also work with the null GC,
// probably even better than the alternative.
// To really support alternate GCs here, we would need to widen the
// interface to finalization, since we sometimes have to register a
// second finalizer for an object that already has one.
// We might also want to move the GC interface to a .h file, since
// the number of procedure call levels involved in some of these
// operations is already ridiculous, and would become worse if we
// went through the proper intermediaries.
#else
# include "gc.h"
#endif

// What follows currenly assumes a Linux-like platform.
// Some of it specifically assumes X86 or IA64 Linux, though that
// should be easily fixable.

// A Java monitor implemention based on a table of locks.
// Each entry in the table describes
// locks held for objects that hash to that location.
// This started out as a reimplementation of the technique used in SGIs JVM,
// for which we obtained permission from SGI.
// But in fact, this ended up quite different, though some ideas are
// still shared with the original.
// It was also influenced by some of the published IBM work,
// though it also differs in many ways from that.
// We could speed this up if we had a way to atomically update
// an entire cache entry, i.e. 2 contiguous words of memory.
// That would usually be the case with a 32 bit ABI on a 64 bit processor.
// But we don't currently go out of our way to target those.
// I don't know how to do much better with a N bit ABI on a processor
// that can atomically update only N bits at a time.
// Author: Hans-J. Boehm  (Hans_Boehm@hp.com, boehm@acm.org)

#include <assert.h>
#include <limits.h>
#include <unistd.h>	// for usleep, sysconf.
#include <sched.h>	// for sched_yield.
#include <gcj/javaprims.h>

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/

// The following should move to some standard place. Linux-threads
// already defines roughly these, as do more recent versions of boehm-gc.
// The problem is that neither exports them.

#if defined(__GNUC__) && defined(__i386__)
  // Atomically replace *addr by new_val if it was initially equal to old.
  // Return true if the comparison succeeded.
  // Assumed to have acquire semantics, i.e. later memory operations
  // cannot execute before the compare_and_swap finishes.
  inline static bool
  compare_and_swap(volatile obj_addr_t *addr,
		  				obj_addr_t old,
						obj_addr_t new_val) 
  {
    char result;
    __asm__ __volatile__("lock; cmpxchgl %2, %0; setz %1"
	    	: "=m"(*(addr)), "=q"(result)
		: "r" (new_val), "0"(*(addr)), "a"(old) : "memory");
    return (bool) result;
  }

  // Set *addr to new_val with release semantics, i.e. making sure
  // that prior loads and stores complete before this
  // assignment.
  // On X86, the hardware shouldn't reorder reads and writes,
  // so we just have to convince gcc not to do it either.
  inline static void
  release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
  {
    __asm__ __volatile__(" " : : : "memory");
    *(addr) = new_val;
  }

  // Compare_and_swap with release semantics instead of acquire semantics.
  // On many architecture, the operation makes both guarantees, so the
  // implementation can be the same.
  inline static bool
  compare_and_swap_release(volatile obj_addr_t *addr,
		  				       obj_addr_t old,
						       obj_addr_t new_val)
  {
    return compare_and_swap(addr, old, new_val);
  }
#endif

#if defined(__GNUC__) && defined(__ia64__) && SIZEOF_VOID_P == 8
  inline static bool
  compare_and_swap(volatile obj_addr_t *addr,
	 				        obj_addr_t old,
						obj_addr_t new_val) 
  {
    unsigned long oldval;
    __asm__ __volatile__("mov ar.ccv=%4 ;; cmpxchg8.acq %0=%1,%2,ar.ccv"
		: "=r"(oldval), "=m"(*addr)
		: "r"(new_val), "1"(*addr), "r"(old) : "memory");
    return (oldval == old);
  }

  // The fact that *addr is volatile should cause the compiler to
  // automatically generate an st8.rel.
  inline static void
  release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
  {
    __asm__ __volatile__(" " : : : "memory");
    *(addr) = new_val;
  }

  inline static bool
  compare_and_swap_release(volatile obj_addr_t *addr,
	 				               obj_addr_t old,
						       obj_addr_t new_val) 
  {
    unsigned long oldval;
    __asm__ __volatile__("mov ar.ccv=%4 ;; cmpxchg8.rel %0=%1,%2,ar.ccv"
		: "=r"(oldval), "=m"(*addr)
		: "r"(new_val), "1"(*addr), "r"(old) : "memory");
    return (oldval == old);
  }
#endif

// Try to determine whether we are on a multiprocessor, i.e. whether
// spinning may be profitable.
// This should really use a suitable autoconf macro.
// False is the conservative answer, though the right one is much better.
static bool
is_mp()
{
  long nprocs = sysconf(_SC_NPROCESSORS_ONLN);
  return (nprocs > 1);
}

// A call to keep_live(p) forces p to be accessible to the GC
// at this point.
inline static void
keep_live(obj_addr_t p)
{
    __asm__ __volatile__("" : : "rm"(p) : "memory");
}


// Each hash table entry holds a single preallocated "lightweight" lock.
// In addition, it holds a chain of "heavyweight" locks.  Lightweight
// locks do not support Object.wait(), and are converted to heavyweight
// status in response to contention.  Unlike the SGI scheme, both
// ligtweight and heavyweight locks in one hash entry can be simultaneously
// in use.  (The SGI scheme requires that we be able to acquire a heavyweight
// lock on behalf of another thread, and can thus convert a lock we don't
// hold to heavyweight status.  Here we don't insist on that, and thus
// let the original holder of the lighweight lock keep it.)

struct heavy_lock {
  void * reserved_for_gc;
  struct heavy_lock *next;	// Hash chain link.
				// The only field traced by GC.
  obj_addr_t address;		// Object to which this lock corresponds.
				// Should not be traced by GC.
  _Jv_SyncInfo si;
  // The remaining fields save prior finalization info for
  // the object, which we needed to replace in order to arrange
  // for cleanup of the lock structure.
  GC_finalization_proc old_finalization_proc;
  void * old_client_data;
};

#ifdef LOCK_DEBUG
void
print_hl_list(heavy_lock *hl)
{
    heavy_lock *p = hl;
    for (; 0 != p; p = p->next)
      fprintf (stderr, "(hl = %p, addr = %p)", p, (void *)(p -> address));
}
#endif /* LOCK_DEBUG */

#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
// If we have to run a destructor for a sync_info member, then this
// function is registered as a finalizer for the sync_info.
static void
heavy_lock_finalization_proc (jobject obj)
{
  heavy_lock *hl = (heavy_lock *) obj;
#if defined (_Jv_HaveCondDestroy)
  _Jv_CondDestroy (&hl->si.condition);
#endif
#if defined (_Jv_HaveMutexDestroy)
  _Jv_MutexDestroy (&hl->si.mutex);
#endif
  hl->si.init = false;
}
#endif /* defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy) */

// We convert the lock back to lightweight status when
// we exit, so that a single contention episode doesn't doom the lock
// forever.  But we also need to make sure that lock structures for dead
// objects are eventually reclaimed.  We do that in a an additional
// finalizer on the underlying object.
// Note that if the corresponding object is dead, it is safe to drop
// the heavy_lock structure from its list.  It is not necessarily
// safe to deallocate it, since the unlock code could still be running.

struct hash_entry {
  volatile obj_addr_t address;	// Address of object for which lightweight
  				// k is held.
				// We assume the 3 low order bits are zero.
				// With the Boehm collector and bitmap
				// allocation, objects of size 4 bytes are
				// broken anyway.  Thus this is primarily
				// a constraint on statically allocated
				// objects used for synchronization.
				// This allows us to use the low order
  				// bits as follows:
#   define LOCKED 	1 	// This hash entry is locked, and its
  				// state may be invalid.
  				// The lock protects both the hash_entry
  				// itself (except for the light_count
  				// and light_thr_id fields, which
  				// are protected by the lightweight
  				// lock itself), and any heavy_monitor
  				// structures attached to it.
#   define HEAVY	2	// There may be heavyweight locks
				// associated with this cache entry.
				// The lightweight entry is still valid,
  				// if the leading bits of the address
  				// field are nonzero.
 				// Set if heavy_count is > 0 .
  				// Stored redundantly so a single
  				// compare-and-swap works in the easy case.
#   define REQUEST_CONVERSION 4 // The lightweight lock is held.  But
  				// one or more other threads have tried
  				// to acquire the lock, and hence request
  				// conversion to heavyweight status.
#   define FLAGS (LOCKED | HEAVY | REQUEST_CONVERSION)
  volatile _Jv_ThreadId_t light_thr_id;
				// Thr_id of holder of lightweight lock.
  				// Only updated by lightweight lock holder.
				// Must be recognizably invalid if the
				// lightweight lock is not held.
#   define INVALID_THREAD_ID 0  // Works for Linux?
				// If zero doesn't work, we have to
				// initialize lock table.
  volatile unsigned short light_count;
				// Number of times the lightweight lock
  				// is held minus one.  Zero if lightweight
  				// lock is not held.
  unsigned short heavy_count; 	// Total number of times heavyweight locks
  				// associated with this hash entry are held
  				// or waiting to be acquired.
  				// Threads in wait() are included eventhough
  				// they have temporarily released the lock.
  struct heavy_lock * heavy_locks;
  				// Chain of heavy locks.  Protected
  				// by lockbit for he.  Locks may
  				// remain allocated here even if HEAVY
  				// is not set and heavy_count is 0.
  				// If a lightweight and hevyweight lock
  				// correspond to the same address, the
  				// lightweight lock is the right one.
};

#ifndef JV_SYNC_TABLE_SZ
# define JV_SYNC_TABLE_SZ 1024
#endif

hash_entry light_locks[JV_SYNC_TABLE_SZ];

#define JV_SYNC_HASH(p) (((long)p ^ ((long)p >> 10)) % JV_SYNC_TABLE_SZ)

#ifdef LOCK_DEBUG
  void print_he(hash_entry *he)
  {
     fprintf(stderr, "lock hash entry = %p, index = %d, address = 0x%lx\n"
		     "\tlight_thr_id = 0x%lx, light_count = %d, "
		     "heavy_count = %d\n\theavy_locks:", he,
		     he - light_locks, he -> address, he -> light_thr_id,
		     he -> light_count, he -> heavy_count);
     print_hl_list(he -> heavy_locks);
     fprintf(stderr, "\n");
  }
#endif /* LOCK_DEBUG */

// Wait for roughly 2^n units, touching as little memory as possible.
static void
spin(unsigned n)
{
  const unsigned MP_SPINS = 10;
  const unsigned YIELDS = 4;
  const unsigned SPINS_PER_UNIT = 30;
  const unsigned MIN_SLEEP_USECS = 2001; // Shorter times spin under Linux.
  const unsigned MAX_SLEEP_USECS = 200000;
  static unsigned spin_limit = 0;
  static unsigned yield_limit = YIELDS;
  static bool mp = false;
  static bool spin_initialized = false;

  if (!spin_initialized)
    {
      mp = is_mp();
      if (mp)
	{
	  spin_limit = MP_SPINS;
	  yield_limit = MP_SPINS + YIELDS;
	}
      spin_initialized = true;
    }
  if (n < spin_limit)
    {
      unsigned i = SPINS_PER_UNIT << n;
      for (; i > 0; --i)
        __asm__ __volatile__("");
    }
  else if (n < yield_limit)
    {
      sched_yield();
    }
  else
    {
      unsigned duration = MIN_SLEEP_USECS << (n - yield_limit);
      if (n >= 15 + yield_limit || duration > MAX_SLEEP_USECS)
	duration = MAX_SLEEP_USECS;
      usleep(duration);
    }
}

// Wait for a hash entry to become unlocked.
static void
wait_unlocked (hash_entry *he)
{
  unsigned i = 0;
  while (he -> address & LOCKED)
    spin (i++);
}

// Return the heavy lock for addr if it was already allocated.
// The client passes in the appropriate hash_entry.
// We hold the lock for he.
static inline heavy_lock *
find_heavy (obj_addr_t addr, hash_entry *he)
{
  heavy_lock *hl = he -> heavy_locks;
  while (hl != 0 && hl -> address != addr) hl = hl -> next;
  return hl;
}

// Unlink the heavy lock for the given address from its hash table chain.
// Dies miserably and conspicuously if it's not there, since that should
// be impossible.
static inline void
unlink_heavy (obj_addr_t addr, hash_entry *he)
{
  heavy_lock **currentp = &(he -> heavy_locks);
  while ((*currentp) -> address != addr)
    currentp = &((*currentp) -> next);
  *currentp = (*currentp) -> next;
}

// Finalization procedure for objects that have associated heavy-weight
// locks.  This may replace the real finalization procedure.
static void
heavy_lock_obj_finalization_proc (void *obj, void *cd)
{
  heavy_lock *hl = (heavy_lock *)cd;
  obj_addr_t addr = (obj_addr_t)obj;
  GC_finalization_proc old_finalization_proc = hl -> old_finalization_proc;
  void * old_client_data = hl -> old_client_data;

  if (old_finalization_proc != 0)
    {
      // We still need to run a real finalizer.  In an idealized
      // world, in which people write thread-safe finalizers, that is
      // likely to require synchronization.  Thus we reregister
      // ourselves as the only finalizer, and simply run the real one.
      // Thus we don't clean up the lock yet, but we're likely to do so
      // on the next GC cycle.
      hl -> old_finalization_proc = 0;
      hl -> old_client_data = 0;
#     ifdef HAVE_BOEHM_GC
        GC_REGISTER_FINALIZER_NO_ORDER(obj, heavy_lock_obj_finalization_proc, cd, 0, 0);
#     endif
      old_finalization_proc(obj, old_client_data);
    }
  else
    {
      // The object is really dead, although it's conceivable that
      // some thread may still be in the process of releasing the
      // heavy lock.  Unlink it and, if necessary, register a finalizer
      // to distroy sync_info.
      hash_entry *he = light_locks + JV_SYNC_HASH(addr);
      obj_addr_t address = (he -> address & ~LOCKED);
      while (!compare_and_swap(&(he -> address), address, address | LOCKED ))
	{
	  // Hash table entry is currently locked.  We can't safely touch
	  // touch the list of heavy locks.  
	  wait_unlocked(he);
	  address = (he -> address & ~LOCKED);
	}
      unlink_heavy(addr, light_locks + JV_SYNC_HASH(addr));
      release_set(&(he -> address), address);
#     if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
        // Register a finalizer, yet again.
          hl->si.init = true;
          _Jv_RegisterFinalizer (hl, heavy_lock_finalization_proc);
#     endif
    }
}

// Allocate a new heavy lock for addr, returning its address.
// Assumes we already have the hash_entry locked, and there
// is currently no lightweight or allocated lock for addr.
// We register a finalizer for addr, which is responsible for
// removing the heavy lock when addr goes away, in addition
// to the responsibilities of any prior finalizer.
static heavy_lock *
alloc_heavy(obj_addr_t addr, hash_entry *he)
{
  heavy_lock * hl = (heavy_lock *) _Jv_AllocTraceOne(sizeof (heavy_lock));
  
  hl -> address = addr;
  _Jv_MutexInit (&(hl -> si.mutex));
  _Jv_CondInit (&(hl -> si.condition));
# if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
    hl->si.init = true;  // needed ?
# endif
  hl -> next = he -> heavy_locks;
  he -> heavy_locks = hl;
  // FIXME: The only call that cheats and goes directly to the GC interface.
# ifdef HAVE_BOEHM_GC
    GC_REGISTER_FINALIZER_NO_ORDER(
		    	  (void *)addr, heavy_lock_obj_finalization_proc,
			  hl, &hl->old_finalization_proc,
			  &hl->old_client_data);
# endif /* HAVE_BOEHM_GC */
  return hl;
}

// Return the heavy lock for addr, allocating if necessary.
// Assumes we have the cache entry locked, and there is no lightweight
// lock for addr.
static heavy_lock *
get_heavy(obj_addr_t addr, hash_entry *he)
{
  heavy_lock *hl = find_heavy(addr, he);
  if (0 == hl)
    hl = alloc_heavy(addr, he);
  return hl;
}

void
_Jv_MonitorEnter (jobject obj)
{
  obj_addr_t addr = (obj_addr_t)obj;
  obj_addr_t address;
  unsigned hash = JV_SYNC_HASH(addr);
  hash_entry * he = light_locks + hash;
  _Jv_ThreadId_t self = _Jv_ThreadSelf();
  unsigned count;
  const unsigned N_SPINS = 18;

  assert(!(addr & FLAGS));
retry:
  if (__builtin_expect(compare_and_swap(&(he -> address),
					0, addr),true))
    {
      assert(he -> light_thr_id == INVALID_THREAD_ID);
      assert(he -> light_count == 0);
      he -> light_thr_id = self;
      // Count fields are set correctly.  Heavy_count was also zero,
      // but can change asynchronously.
      // This path is hopefully both fast and the most common.
      return;
    }
  address = he -> address;
  if ((address & ~(HEAVY | REQUEST_CONVERSION)) == addr)
    {
      if (he -> light_thr_id == self)
	{
	  // We hold the lightweight lock, and it's for the right
	  // address.
	  count = he -> light_count;
	  if (count == USHRT_MAX)
	    {
	      // I think most JVMs don't check for this.
	      // But I'm not convinced I couldn't turn this into a security
	      // hole, even with a 32 bit counter.
	      throw new java::lang::IllegalMonitorStateException(
		JvNewStringLatin1("maximum monitor nesting level exceeded")); 
	    }
	  he -> light_count = count + 1;
	  return;
	}
      else
	{
	  // Lightweight lock is held, but by somone else.
          // Spin a few times.  This avoids turning this into a heavyweight
    	  // lock if the current holder is about to release it.
          for (unsigned int i = 0; i < N_SPINS; ++i)
	    {
	      if ((he -> address & ~LOCKED) != (address & ~LOCKED)) goto retry;
	      spin(i);
            }
	  address &= ~LOCKED;
	  if (!compare_and_swap(&(he -> address), address, address | LOCKED ))
	    {
	      wait_unlocked(he);      
	      goto retry;
	    }
	  heavy_lock *hl = get_heavy(addr, he);
	  ++ (he -> heavy_count);
	  // The hl lock acquisition can't block for long, since it can
	  // only be held by other threads waiting for conversion, and
	  // they, like us, drop it quickly without blocking.
	  _Jv_MutexLock(&(hl->si.mutex));
	  assert(he -> address == address | LOCKED );
	  release_set(&(he -> address), (address | REQUEST_CONVERSION | HEAVY));
				// release lock on he
	  while ((he -> address & ~FLAGS) == (address & ~FLAGS))
	    {
	      // Once converted, the lock has to retain heavyweight
	      // status, since heavy_count > 0 . 
	      _Jv_CondWait (&(hl->si.condition), &(hl->si.mutex), 0, 0);
	    }
	  keep_live(addr);
		// Guarantee that hl doesn't get unlinked by finalizer.
		// This is only an issue if the client fails to release
		// the lock, which is unlikely.
	  assert(he -> address & HEAVY);
	  // Lock has been converted, we hold the heavyweight lock,
	  // heavy_count has been incremented.
	  return;
        }
    }
  obj_addr_t was_heavy = (address & HEAVY);
  address &= ~LOCKED;
  if (!compare_and_swap(&(he -> address), address, (address | LOCKED )))
    {
      wait_unlocked(he);
      goto retry;
    }
  if ((address & ~(HEAVY | REQUEST_CONVERSION)) == 0)
    {
      // Either was_heavy is true, or something changed out from under us,
      // since the initial test for 0 failed.
      assert(!(address & REQUEST_CONVERSION));
	// Can't convert a nonexistent lightweight lock.
      heavy_lock *hl;
      hl = (was_heavy? find_heavy(addr, he) : 0);
      if (0 == hl)
        {
	  // It is OK to use the lighweight lock, since either the
	  // heavyweight lock does not exist, or none of the
	  // heavyweight locks currently exist.  Future threads
	  // trying to acquire the lock will see the lightweight
	  // one first and use that.
	  he -> light_thr_id = self;  // OK, since nobody else can hold
				      // light lock or do this at the same time.
	  assert(he -> light_count == 0);
	  assert(was_heavy == (he -> address & HEAVY));
	  release_set(&(he -> address), (addr | was_heavy));
        }
      else
	{
	  // Must use heavy lock.
	  ++ (he -> heavy_count);
	  assert(0 == (address & ~HEAVY));
          release_set(&(he -> address), HEAVY);
          _Jv_MutexLock(&(hl->si.mutex));
	  keep_live(addr);
        }
      return;
    }
  // Lightweight lock is held, but does not correspond to this object.
  // We hold the lock on the hash entry, and he -> address can't
  // change from under us.  Neither can the chain of heavy locks.
    {
      assert(0 == he -> heavy_count || (address & HEAVY));
      heavy_lock *hl = get_heavy(addr, he);
      ++ (he -> heavy_count);
      release_set(&(he -> address), address | HEAVY);
      _Jv_MutexLock(&(hl->si.mutex));
      keep_live(addr);
    }
}


void
_Jv_MonitorExit (jobject obj)
{
  obj_addr_t addr = (obj_addr_t)obj;
  _Jv_ThreadId_t self = _Jv_ThreadSelf();
  unsigned hash = JV_SYNC_HASH(addr);
  hash_entry * he = light_locks + hash;
  _Jv_ThreadId_t light_thr_id;
  unsigned count;
  obj_addr_t address;

retry:
  light_thr_id = he -> light_thr_id;
  // Unfortunately, it turns out we always need to read the address
  // first.  Even if we are going to update it with compare_and_swap,
  // we need to reset light_thr_id, and that's not safe unless we know
  // know that we hold the lock.
  address = he -> address;
  // First the (relatively) fast cases:
  if (__builtin_expect(light_thr_id == self, true))
    {
      count = he -> light_count;
      if (__builtin_expect((address & ~HEAVY) == addr, true))
	{
          if (count != 0)
            {
	      // We held the lightweight lock all along.  Thus the values
	      // we saw for light_thr_id and light_count must have been valid. 
	      he -> light_count = count - 1;
	      return;
            }
	  else
	    {
	      // We hold the lightweight lock once.
	      he -> light_thr_id = INVALID_THREAD_ID;
              if (compare_and_swap_release(&(he -> address), address,
					   address & HEAVY))
	        return;
	      else
		{
	          he -> light_thr_id = light_thr_id; // Undo prior damage.
	          goto retry;
	        }
            }
        }
      // else lock is not for this address, conversion is requested,
      // or the lock bit in the address field is set.
    }
  else
    {
      if ((address & ~(HEAVY | REQUEST_CONVERSION)) == addr)
	{
#	  ifdef LOCK_DEBUG
	    fprintf(stderr, "Lightweight lock held by other thread\n\t"
			    "light_thr_id = 0x%lx, self = 0x%lx, "
			    "address = 0x%lx, pid = %d\n",
			    light_thr_id, self, address, getpid());
	    print_he(he);
	    for(;;) {}
#	  endif
	  // Someone holds the lightweight lock for this object, and
	  // it can't be us.
	  throw new java::lang::IllegalMonitorStateException(
			JvNewStringLatin1("current thread not owner"));
        }
      else
	count = he -> light_count;
    }
  if (address & LOCKED)
    {
      wait_unlocked(he);
      goto retry;
    }
  // Now the unlikely cases.
  // We do know that:
  // - Address is set, and doesn't contain the LOCKED bit.
  // - If address refers to the same object as addr, then he -> light_thr_id
  //   refers to this thread, and count is valid.
  // - The case in which we held the lightweight lock has been
  //   completely handled, except for the REQUEST_CONVERSION case.
  //   
  if ((address & ~FLAGS) == addr)
    {
      // The lightweight lock is assigned to this object.
      // Thus we must be in the REQUEST_CONVERSION case.
      if (0 != count)
        {
	  // Defer conversion until we exit completely.
	  he -> light_count = count - 1;
	  return;
        }
      assert(he -> light_thr_id == self);
      assert(address & REQUEST_CONVERSION);
      // Conversion requested
      // Convert now.
      if (!compare_and_swap(&(he -> address), address, address | LOCKED))
	goto retry;
      heavy_lock *hl = find_heavy(addr, he);
      assert (0 != hl);
		// Requestor created it.
      he -> light_count = 0;
      assert(he -> heavy_count > 0);
	  	// was incremented by requestor.
      _Jv_MutexLock(&(hl->si.mutex));
	// Release the he lock after acquiring the mutex.
	// Otherwise we can accidentally
	// notify a thread that has already seen a heavyweight
	// lock.
      he -> light_thr_id = INVALID_THREAD_ID;
      release_set(&(he -> address), HEAVY);
	  	// lightweight lock now unused.
      _Jv_CondNotifyAll(&(hl->si.condition), &(hl->si.mutex));
      _Jv_MutexUnlock(&(hl->si.mutex));
      // heavy_count was already incremented by original requestor.
      keep_live(addr);
      return;
    }
  // lightweight lock not for this object.
  assert(!(address & LOCKED));
  assert((address & ~FLAGS) != addr);
  if (!compare_and_swap(&(he -> address), address, address | LOCKED))
	goto retry;
  heavy_lock *hl = find_heavy(addr, he);
  if (NULL == hl)
    {
#     ifdef LOCK_DEBUG
	fprintf(stderr, "Failed to find heavyweight lock for addr 0x%lx"
			" pid = %d\n", addr, getpid());
	print_he(he);
	for(;;) {}
#     endif
      throw new java::lang::IllegalMonitorStateException(
			JvNewStringLatin1("current thread not owner"));
    }
  assert(address & HEAVY);
  count = he -> heavy_count;
  assert(count > 0);
  --count;
  if (0 == count) address &= ~HEAVY;
  he -> heavy_count = count;
  release_set(&(he -> address), address);
    				// release lock bit, preserving
				// REQUEST_CONVERSION
    				// and object address.
  _Jv_MutexUnlock(&(hl->si.mutex));
  			// Unlock after releasing the lock bit, so that
  			// we don't switch to another thread prematurely.
  keep_live(addr);
}     

// The rest of these are moderately thin veneers on _Jv_Cond ops.
// The current version of Notify might be able to make the pthread
// call AFTER releasing the lock, thus saving some context switches??

void
java::lang::Object::wait (jlong timeout, jint nanos)
{
  obj_addr_t addr = (obj_addr_t)this;
  _Jv_ThreadId_t self = _Jv_ThreadSelf();
  unsigned hash = JV_SYNC_HASH(addr);
  hash_entry * he = light_locks + hash;
  unsigned count;
  obj_addr_t address;
  heavy_lock *hl;
    
  if (__builtin_expect (timeout < 0 || nanos < 0 || nanos > 999999, false))
    throw new IllegalArgumentException;
retry:
  address = he -> address;
  address &= ~LOCKED;
  if (!compare_and_swap(&(he -> address), address, address | LOCKED))
    {
      wait_unlocked(he);
      goto retry;
    }
  // address does not have the lock bit set.  We hold the lock on he.
  if ((address & ~FLAGS) == addr)
    {
      // Convert to heavyweight.
	if (he -> light_thr_id != self)
	  {
#	    ifdef LOCK_DEBUG
	      fprintf(stderr, "Found wrong lightweight lock owner in wait "
			      "address = 0x%lx pid = %d\n", address, getpid());
	      print_he(he);
	      for(;;) {}
#	    endif
	    release_set(&(he -> address), address);
	    throw new IllegalMonitorStateException (JvNewStringLatin1 
                          ("current thread not owner"));
	  }
	count = he -> light_count;
	hl = get_heavy(addr, he);
	he -> light_count = 0;
	he -> heavy_count += count + 1;
	for (unsigned i = 0; i <= count; ++i)
	  _Jv_MutexLock(&(hl->si.mutex));
	// Again release the he lock after acquiring the mutex.
        he -> light_thr_id = INVALID_THREAD_ID;
	release_set(&(he -> address), HEAVY);  // lightweight lock now unused.
	if (address & REQUEST_CONVERSION)
	  _Jv_CondNotify (&(hl->si.condition), &(hl->si.mutex));
    }
  else /* We should hold the heavyweight lock. */
    {
      hl = find_heavy(addr, he);
      release_set(&(he -> address), address);
      if (0 == hl)
	{
#	  ifdef LOCK_DEBUG
	    fprintf(stderr, "Couldn't find heavy lock in wait "
		 	    "addr = 0x%lx pid = %d\n", addr, getpid());
	    print_he(he);
	    for(;;) {}
#	  endif
	  throw new IllegalMonitorStateException (JvNewStringLatin1 
                          ("current thread not owner"));
	}
      assert(address & HEAVY);
    }
  switch (_Jv_CondWait (&(hl->si.condition), &(hl->si.mutex), timeout, nanos))
    {
      case _JV_NOT_OWNER:
	throw new IllegalMonitorStateException (JvNewStringLatin1 
                          ("current thread not owner"));        
      case _JV_INTERRUPTED:
	if (Thread::interrupted ())
	  throw new InterruptedException;        
    }
}

void
java::lang::Object::notify (void)
{
  obj_addr_t addr = (obj_addr_t)this;
  _Jv_ThreadId_t self = _Jv_ThreadSelf();
  unsigned hash = JV_SYNC_HASH(addr);
  hash_entry * he = light_locks + hash;
  heavy_lock *hl;
  obj_addr_t address;
  int result;

retry:
  address = ((he -> address) & ~LOCKED);
  if (!compare_and_swap(&(he -> address), address, address | LOCKED))
    {
      wait_unlocked(he);
      goto retry;
    }
  if ((address & ~FLAGS) == addr && he -> light_thr_id == self)
    {
      // We hold lightweight lock.  Since it has not
      // been inflated, there are no waiters.
      release_set(&(he -> address), address);	// unlock
      return;
    }
  hl = find_heavy(addr, he);
  // Hl can't disappear since we point to the underlying object.
  // It's important that we release the lock bit before the notify, since
  // otherwise we will try to wake up thee target while we still hold the
  // bit.  This results in lock bit contention, which we don't handle
  // terribly well.
  release_set(&(he -> address), address); // unlock
  if (0 == hl)
    {
      throw new IllegalMonitorStateException(JvNewStringLatin1 
                                              ("current thread not owner"));
      return;
    }
  result = _Jv_CondNotify(&(hl->si.condition), &(hl->si.mutex));
  keep_live(addr);
  if (__builtin_expect (result, 0))
    throw new IllegalMonitorStateException(JvNewStringLatin1 
                                              ("current thread not owner"));
}

void
java::lang::Object::notifyAll (void)
{
  obj_addr_t addr = (obj_addr_t)this;
  _Jv_ThreadId_t self = _Jv_ThreadSelf();
  unsigned hash = JV_SYNC_HASH(addr);
  hash_entry * he = light_locks + hash;
  heavy_lock *hl;
  obj_addr_t address;
  int result;

retry:
  address = (he -> address) & ~LOCKED;
  if (!compare_and_swap(&(he -> address), address, address | LOCKED))
    {
      wait_unlocked(he);
      goto retry;
    }
  hl = find_heavy(addr, he);
  if ((address & ~FLAGS) == addr && he -> light_thr_id == self)
    {
      // We hold lightweight lock.  Since it has not
      // been inflated, there are no waiters.
      release_set(&(he -> address), address);	// unlock
      return;
    }
  release_set(&(he -> address), address); // unlock
  if (0 == hl)
    {
      throw new IllegalMonitorStateException(JvNewStringLatin1 
                                              ("current thread not owner"));
    }
  result = _Jv_CondNotifyAll(&(hl->si.condition), &(hl->si.mutex));
  if (__builtin_expect (result, 0))
    throw new IllegalMonitorStateException(JvNewStringLatin1 
                                              ("current thread not owner"));
}

// This is declared in Java code and in Object.h.
// It should never be called with JV_HASH_SYNCHRONIZATION
void
java::lang::Object::sync_init (void)
{
  throw new IllegalMonitorStateException(JvNewStringLatin1 
                                              ("internal error: sync_init"));
}

// This is called on startup and declared in Object.h.
// For now we just make it a no-op.
void
_Jv_InitializeSyncMutex (void)
{
}

#endif /* JV_HASH_SYNCHRONIZATION */

