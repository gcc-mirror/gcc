/*
    Copyright (c) 2014 Intel Corporation.  All Rights Reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of Intel Corporation nor the names of its
        contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*
 * Include file for Offload API.
 */

#ifndef OFFLOAD_H_INCLUDED
#define OFFLOAD_H_INCLUDED

#if defined(LINUX) || defined(FREEBSD)
#include <bits/functexcept.h>
#endif

#include <stddef.h>
#include <omp.h>

#ifdef __cplusplus
extern "C" {
#endif

#define TARGET_ATTRIBUTE __declspec(target(mic))

/*
 *  The target architecture.
 */
typedef enum TARGET_TYPE {
    TARGET_NONE,    /* Undefine target */
    TARGET_HOST,    /* Host used as target */
    TARGET_MIC      /* MIC target */
} TARGET_TYPE;

/*
 *  The default target type.
 */
#define DEFAULT_TARGET_TYPE TARGET_MIC

/*
 *  The default target number.
 */
#define DEFAULT_TARGET_NUMBER 0

/*
 *  Offload status.
 */
typedef enum {
    OFFLOAD_SUCCESS = 0,
    OFFLOAD_DISABLED,               /* offload is disabled */
    OFFLOAD_UNAVAILABLE,            /* card is not available */
    OFFLOAD_OUT_OF_MEMORY,          /* not enough memory on device */
    OFFLOAD_PROCESS_DIED,           /* target process has died */
    OFFLOAD_ERROR                   /* unspecified error */
} _Offload_result;

typedef struct {
    _Offload_result result;         /* result, see above */
    int             device_number;  /* device number */
    size_t          data_sent;      /* number of bytes sent to the target */
    size_t          data_received;  /* number of bytes received by host */
} _Offload_status;

#define OFFLOAD_STATUS_INIT(x) \
    ((x).result = OFFLOAD_DISABLED)

#define OFFLOAD_STATUS_INITIALIZER \
    { OFFLOAD_DISABLED, -1, 0, 0 }

/* Offload runtime interfaces */

extern int _Offload_number_of_devices(void);
extern int _Offload_get_device_number(void);
extern int _Offload_get_physical_device_number(void);

extern void* _Offload_shared_malloc(size_t size);
extern void  _Offload_shared_free(void *ptr);

extern void* _Offload_shared_aligned_malloc(size_t size, size_t align);
extern void  _Offload_shared_aligned_free(void *ptr);

extern int _Offload_signaled(int index, void *signal);
extern void _Offload_report(int val);

/* OpenMP API */

extern void omp_set_default_device(int num) __GOMP_NOTHROW;
extern int  omp_get_default_device(void) __GOMP_NOTHROW;
extern int  omp_get_num_devices(void) __GOMP_NOTHROW;

/* OpenMP API wrappers */

/* Set num_threads on target */
extern void omp_set_num_threads_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
);

/* Get max_threads from target */
extern int omp_get_max_threads_target(
    TARGET_TYPE target_type,
    int target_number
);

/* Get num_procs from target */
extern int omp_get_num_procs_target(
    TARGET_TYPE target_type,
    int target_number
);

/* Set dynamic on target */
extern void omp_set_dynamic_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
);

/* Get dynamic from target */
extern int omp_get_dynamic_target(
    TARGET_TYPE target_type,
    int target_number
);

/* Set nested on target */
extern void omp_set_nested_target(
    TARGET_TYPE target_type,
    int target_number,
    int nested
);

/* Get nested from target */
extern int omp_get_nested_target(
    TARGET_TYPE target_type,
    int target_number
);

extern void omp_set_num_threads_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
);

extern int omp_get_max_threads_target(
    TARGET_TYPE target_type,
    int target_number
);

extern int omp_get_num_procs_target(
    TARGET_TYPE target_type,
    int target_number
);

extern void omp_set_dynamic_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
);

extern int omp_get_dynamic_target(
    TARGET_TYPE target_type,
    int target_number
);

extern void omp_set_nested_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
);

extern int omp_get_nested_target(
    TARGET_TYPE target_type,
    int target_number
);

extern void omp_set_schedule_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_sched_t kind,
    int modifier
);

extern void omp_get_schedule_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_sched_t *kind,
    int *modifier
);

/* lock API functions */

typedef struct {
    omp_lock_t lock;
} omp_lock_target_t;

extern void omp_init_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
);

extern void omp_destroy_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
);

extern void omp_set_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
);

extern void omp_unset_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
);

extern int omp_test_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
);

/* nested lock API functions */

typedef struct {
    omp_nest_lock_t lock;
} omp_nest_lock_target_t;

extern void omp_init_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
);

extern void omp_destroy_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
);

extern void omp_set_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
);

extern void omp_unset_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
);

extern int omp_test_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
);

#ifdef __cplusplus
} /* extern "C" */

/* Namespace for the shared_allocator. */
namespace __offload {
  /* This follows the specification for std::allocator. */
  /* Forward declaration of the class template. */
  template <typename T>
  class shared_allocator;

  /* Specialization for shared_allocator<void>. */
  template <>
  class shared_allocator<void> {
  public:
    typedef void       *pointer;
    typedef const void *const_pointer;
    typedef void        value_type;
    template <class U> struct rebind { typedef shared_allocator<U> other; };
  };

  /* Definition of shared_allocator<T>. */
  template <class T>
  class shared_allocator {
  public:
    typedef size_t     size_type;
    typedef ptrdiff_t  difference_type;
    typedef T         *pointer;
    typedef const T   *const_pointer;
    typedef T         &reference;
    typedef const T   &const_reference;
    typedef T          value_type;
    template <class U> struct rebind { typedef shared_allocator<U> other; };
    shared_allocator() throw() { }
    shared_allocator(const shared_allocator&) throw() { }
    template <class U> shared_allocator(const shared_allocator<U>&) throw() { }
    ~shared_allocator() throw() { }
    pointer address(reference x) const { return &x; }
    const_pointer address(const_reference x) const { return &x; }
    pointer allocate(
      size_type, shared_allocator<void>::const_pointer hint = 0);
    void deallocate(pointer p, size_type n);
    size_type max_size() const throw() {
      return size_type(-1)/sizeof(T);
    } /* max_size */
    void construct(pointer p, const T& arg) {
      ::new (p) T(arg);
    } /* construct */
    void destroy(pointer p) {
      p->~T();
    } /* destroy */
  };

  /* Definition for allocate. */
  template <class T>
  typename shared_allocator<T>::pointer
  shared_allocator<T>::allocate(shared_allocator<T>::size_type s,
                                shared_allocator<void>::const_pointer) {
    /* Allocate from shared memory. */
    void *ptr = _Offload_shared_malloc(s*sizeof(T));
    if (ptr == 0) std::__throw_bad_alloc();
    return static_cast<pointer>(ptr);
  } /* allocate */

  template <class T>
  void shared_allocator<T>::deallocate(pointer p,
                                       shared_allocator<T>::size_type) {
    /* Free the shared memory. */
    _Offload_shared_free(p);
  } /* deallocate */

  template <typename _T1, typename _T2>
  inline bool operator==(const shared_allocator<_T1> &, 
                         const shared_allocator<_T2> &) throw() {
    return true;
  }  /* operator== */

  template <typename _T1, typename _T2>
  inline bool operator!=(const shared_allocator<_T1> &, 
                         const shared_allocator<_T2> &) throw() {
    return false;
  }  /* operator!= */
}  /* __offload */
#endif /* __cplusplus */

#endif /* OFFLOAD_H_INCLUDED */
