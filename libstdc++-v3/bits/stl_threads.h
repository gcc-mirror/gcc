/*
 * Copyright (c) 1997-1999
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

// WARNING: This is an internal header file, included by other C++
// standard library headers.  You should not attempt to use this header
// file directly.
// Stl_config.h should be included before this file.

#ifndef __SGI_STL_INTERNAL_THREADS_H
#define __SGI_STL_INTERNAL_THREADS_H

// Supported threading models are native SGI, pthreads, uithreads
// (similar to pthreads, but based on an earlier draft of the Posix
// threads standard), and Win32 threads.  Uithread support by Jochen
// Schlick, 1999.

#if defined(__STL_SGI_THREADS)
#include <mutex.h>
#include <time.h>
#elif defined(__STL_PTHREADS)
#include <pthread.h>
#elif defined(__STL_UITHREADS)
#include <thread.h>
#include <synch.h>
#elif defined(__STL_WIN32THREADS)
#include <windows.h>
#endif

__STL_BEGIN_NAMESPACE


// Class _Refcount_Base provides a type, _RC_t, a data member,
// _M_ref_count, and member functions _M_incr and _M_decr, which perform
// atomic preincrement/predecrement.  The constructor initializes 
// _M_ref_count.

// Hack for SGI o32 compilers.
#if defined(__STL_SGI_THREADS) && !defined(__add_and_fetch) && \
    (__mips < 3 || !(defined (_ABIN32) || defined(_ABI64)))
#  define __add_and_fetch(__l,__v) add_then_test((unsigned long*)__l,__v)  
#  define __test_and_set(__l,__v)  test_and_set(__l,__v)
#endif /* o32 */

struct _Refcount_Base
{
  // The type _RC_t
# ifdef __STL_WIN32THREADS
  typedef long _RC_t;
# else
  typedef size_t _RC_t;
#endif
  
  // The data member _M_ref_count
   volatile _RC_t _M_ref_count;

  // Constructor
# ifdef __STL_PTHREADS
  pthread_mutex_t _M_ref_count_lock;
  _Refcount_Base(_RC_t __n) : _M_ref_count(__n)
    { pthread_mutex_init(&_M_ref_count_lock, 0); }
# elif defined(__STL_UITHREADS)
  mutex_t         _M_ref_count_lock;
  _Refcount_Base(_RC_t __n) : _M_ref_count(__n)
    { mutex_init(&_M_ref_count_lock, USYNC_THREAD, 0); }
# else
  _Refcount_Base(_RC_t __n) : _M_ref_count(__n) {}
# endif

  // _M_incr and _M_decr
# ifdef __STL_SGI_THREADS
  void _M_incr() {  __add_and_fetch(&_M_ref_count, 1); }
  _RC_t _M_decr() { return __add_and_fetch(&_M_ref_count, (size_t) -1); }
# elif defined (__STL_WIN32THREADS)
   void _M_incr() { InterlockedIncrement((_RC_t*)&_M_ref_count); }
  _RC_t _M_decr() { return InterlockedDecrement((_RC_t*)&_M_ref_count); }
# elif defined(__STL_PTHREADS)
  void _M_incr() {
    pthread_mutex_lock(&_M_ref_count_lock);
    ++_M_ref_count;
    pthread_mutex_unlock(&_M_ref_count_lock);
  }
  _RC_t _M_decr() {
    pthread_mutex_lock(&_M_ref_count_lock);
    volatile _RC_t __tmp = --_M_ref_count;
    pthread_mutex_unlock(&_M_ref_count_lock);
    return __tmp;
  }
# elif defined(__STL_UITHREADS)
  void _M_incr() {
    mutex_lock(&_M_ref_count_lock);
    ++_M_ref_count;
    mutex_unlock(&_M_ref_count_lock);
  }
  _RC_t _M_decr() {
    mutex_lock(&_M_ref_count_lock);
    /*volatile*/ _RC_t __tmp = --_M_ref_count;
    mutex_unlock(&_M_ref_count_lock);
    return __tmp;
  }
# else  /* No threads */
  void _M_incr() { ++_M_ref_count; }
  _RC_t _M_decr() { return --_M_ref_count; }
# endif
};

// Atomic swap on unsigned long
// This is guaranteed to behave as though it were atomic only if all
// possibly concurrent updates use _Atomic_swap.
// In some cases the operation is emulated with a lock.
# ifdef __STL_SGI_THREADS
    inline unsigned long _Atomic_swap(unsigned long * __p, unsigned long __q) {
#   	if __mips < 3 || !(defined (_ABIN32) || defined(_ABI64))
            return test_and_set(__p, __q);
#       else
            return __test_and_set(__p, (unsigned long)__q);
#       endif
    }
# elif defined(__STL_WIN32THREADS)
    inline unsigned long _Atomic_swap(unsigned long * __p, unsigned long __q) {
        return (unsigned long) InterlockedExchange((LPLONG)__p, (LONG)__q);
    }
# elif defined(__STL_PTHREADS)
    // We use a template here only to get a unique initialized instance.
    template<int __dummy>
    struct _Swap_lock_struct {
	static pthread_mutex_t _S_swap_lock;
    };

    template<int __dummy>
    pthread_mutex_t
    _Swap_lock_struct<__dummy>::_S_swap_lock = PTHREAD_MUTEX_INITIALIZER;

    // This should be portable, but performance is expected
    // to be quite awful.  This really needs platform specific
    // code.
    inline unsigned long _Atomic_swap(unsigned long * __p, unsigned long __q) {
        pthread_mutex_lock(&_Swap_lock_struct<0>::_S_swap_lock);
        unsigned long __result = *__p;
        *__p = __q;
        pthread_mutex_unlock(&_Swap_lock_struct<0>::_S_swap_lock);
        return __result;
    }
# elif defined(__STL_UITHREADS)
    // We use a template here only to get a unique initialized instance.
    template<int __dummy>
    struct _Swap_lock_struct {
	static mutex_t _S_swap_lock;
    };

    template<int __dummy>
    mutex_t
    _Swap_lock_struct<__dummy>::_S_swap_lock = DEFAULTMUTEX;

    // This should be portable, but performance is expected
    // to be quite awful.  This really needs platform specific
    // code.
    inline unsigned long _Atomic_swap(unsigned long * __p, unsigned long __q) {
        mutex_lock(&_Swap_lock_struct<0>::_S_swap_lock);
        unsigned long __result = *__p;
        *__p = __q;
        mutex_unlock(&_Swap_lock_struct<0>::_S_swap_lock);
        return __result;
    }
# elif defined (__STL_SOLARIS_THREADS)
    // any better solutions ?
    // We use a template here only to get a unique initialized instance.
    template<int __dummy>
    struct _Swap_lock_struct {
	static mutex_t _S_swap_lock;
    };

# if ( __STL_STATIC_TEMPLATE_DATA > 0 )
    template<int __dummy>
    mutex_t
    _Swap_lock_struct<__dummy>::_S_swap_lock = DEFAULTMUTEX;
#  else
    __DECLARE_INSTANCE(mutex_t, _Swap_lock_struct<__dummy>::_S_swap_lock, 
		       =DEFAULTMUTEX);
# endif /* ( __STL_STATIC_TEMPLATE_DATA > 0 ) */

    // This should be portable, but performance is expected
    // to be quite awful.  This really needs platform specific
    // code.
    inline unsigned long _Atomic_swap(unsigned long * __p, unsigned long __q) {
        mutex_lock(&_Swap_lock_struct<0>::_S_swap_lock);
        unsigned long __result = *__p;
        *__p = __q;
        mutex_unlock(&_Swap_lock_struct<0>::_S_swap_lock);
        return __result;
    }
# else
    static inline unsigned long _Atomic_swap(unsigned long * __p, unsigned long __q) {
        unsigned long __result = *__p;
        *__p = __q;
        return __result;
    }
# endif

// Locking class.  Note that this class *does not have a constructor*.
// It must be initialized either statically, with __STL_MUTEX_INITIALIZER,
// or dynamically, by explicitly calling the _M_initialize member function.
// (This is similar to the ways that a pthreads mutex can be initialized.)
// There are explicit member functions for acquiring and releasing the lock.

// There is no constructor because static initialization is essential for
// some uses, and only a class aggregate (see section 8.5.1 of the C++
// standard) can be initialized that way.  That means we must have no
// constructors, no base classes, no virtual functions, and no private or
// protected members.

struct _STL_mutex_lock
{
#if defined(__STL_SGI_THREADS) || defined(__STL_WIN32THREADS)
  // It should be relatively easy to get this to work on any modern Unix.
  volatile unsigned long _M_lock;
  void _M_initialize() { _M_lock = 0; }
  static void _S_nsec_sleep(int __log_nsec) {
#     ifdef __STL_SGI_THREADS
          struct timespec __ts;
          /* Max sleep is 2**27nsec ~ 60msec      */
          __ts.tv_sec = 0;
          __ts.tv_nsec = 1 << __log_nsec;
          nanosleep(&__ts, 0);
#     elif defined(__STL_WIN32THREADS)
	  if (__log_nsec <= 20) {
	      Sleep(0);
	  } else {
	      Sleep(1 << (__log_nsec - 20));
	  }
#     else
#	error unimplemented
#     endif
  }
  void _M_acquire_lock() {
    const unsigned __low_spin_max = 30;  // spins if we suspect uniprocessor
    const unsigned __high_spin_max = 1000; // spins for multiprocessor
    static unsigned __spin_max = __low_spin_max;
    unsigned __my_spin_max;
    static unsigned __last_spins = 0;
    unsigned __my_last_spins;
    volatile unsigned __junk;
    int __i;
    volatile unsigned long* __lock = &this->_M_lock;

    if (!_Atomic_swap((unsigned long*)__lock, 1)) {
      return;
    }
    __my_spin_max = __spin_max;
    __my_last_spins = __last_spins;
    __junk = 17;	// Value doesn't matter.
    for (__i = 0; __i < __my_spin_max; __i++) {
      if (__i < __my_last_spins/2 || *__lock) {
        __junk *= __junk; __junk *= __junk;
        __junk *= __junk; __junk *= __junk;
        continue;
      }
      if (!_Atomic_swap((unsigned long*)__lock, 1)) {
        // got it!
        // Spinning worked.  Thus we're probably not being scheduled
        // against the other process with which we were contending.
        // Thus it makes sense to spin longer the next time.
        __last_spins = __i;
        __spin_max = __high_spin_max;
        return;
      }
    }
    // We are probably being scheduled against the other process.  Sleep.
    __spin_max = __low_spin_max;
    for (__i = 0 ;; ++__i) {
      int __log_nsec = __i + 6;

      if (__log_nsec > 27) __log_nsec = 27;
      if (!_Atomic_swap((unsigned long *)__lock, 1)) {
        return;
      }
      _S_nsec_sleep(__log_nsec);
    }
  }
  void _M_release_lock() {
    volatile unsigned long* __lock = &_M_lock;
#   if defined(__STL_SGI_THREADS) && defined(__GNUC__) && __mips >= 3
        asm("sync");
        *__lock = 0;
#   elif defined(__STL_SGI_THREADS) && __mips >= 3 \
	 && (defined (_ABIN32) || defined(_ABI64))
        __lock_release(__lock);
#   else 
        *__lock = 0;
        // This is not sufficient on many multiprocessors, since
        // writes to protected variables and the lock may be reordered.
#   endif
  }

// We no longer use win32 critical sections.
// They appear to be slower in the contention-free case,
// and they appear difficult to initialize without introducing a race.

#elif defined(__STL_PTHREADS)
  pthread_mutex_t _M_lock;
  void _M_initialize()   { pthread_mutex_init(&_M_lock, NULL); }
  void _M_acquire_lock() { pthread_mutex_lock(&_M_lock); }
  void _M_release_lock() { pthread_mutex_unlock(&_M_lock); }
#elif defined(__STL_UITHREADS)
  mutex_t _M_lock;
  void _M_initialize()   { mutex_init(&_M_lock, USYNC_THREAD, 0); }
  void _M_acquire_lock() { mutex_lock(&_M_lock); }
  void _M_release_lock() { mutex_unlock(&_M_lock); }
#else /* No threads */
  void _M_initialize()   {}
  void _M_acquire_lock() {}
  void _M_release_lock() {}
#endif
};

#ifdef __STL_PTHREADS
// Pthreads locks must be statically initialized to something other than
// the default value of zero.
#   define __STL_MUTEX_INITIALIZER = { PTHREAD_MUTEX_INITIALIZER }
#elif defined(__STL_UITHREADS)
// UIthreads locks must be statically initialized to something other than
// the default value of zero.
#   define __STL_MUTEX_INITIALIZER = { DEFAULTMUTEX }
#elif defined(__STL_SGI_THREADS) || defined(__STL_WIN32THREADS)
#   define __STL_MUTEX_INITIALIZER = { 0 }
#else
#   define __STL_MUTEX_INITIALIZER
#endif


// A locking class that uses _STL_mutex_lock.  The constructor takes a
// reference to an _STL_mutex_lock, and acquires a lock.  The
// destructor releases the lock.  It's not clear that this is exactly
// the right functionality.  It will probably change in the future.

struct _STL_auto_lock
{
  _STL_mutex_lock& _M_lock;
  
  _STL_auto_lock(_STL_mutex_lock& __lock) : _M_lock(__lock)
    { _M_lock._M_acquire_lock(); }
  ~_STL_auto_lock() { _M_lock._M_release_lock(); }

private:
  void operator=(const _STL_auto_lock&);
  _STL_auto_lock(const _STL_auto_lock&);
};

__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_THREADS_H */

// Local Variables:
// mode:C++
// End:





