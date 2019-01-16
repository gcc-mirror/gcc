/*
    Copyright (c) 2014-2016 Intel Corporation.  All Rights Reserved.

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

#ifdef __cplusplus
#if defined(LINUX) || defined(FREEBSD)
#include <bits/functexcept.h>
#endif
#endif

#include <stddef.h>
#include <omp.h>

#ifdef TARGET_WINNT
// <stdint.h> is incompatible on Windows.
typedef unsigned long long int  uint64_t;
typedef   signed long long int   int64_t;
#else
#include <stdint.h>
#endif  // TARGET_WINNT

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

typedef int64_t _Offload_stream;

#define OFFLOAD_STATUS_INIT(x) \
    ((x).result = OFFLOAD_DISABLED)

#define OFFLOAD_STATUS_INITIALIZER \
    { OFFLOAD_DISABLED, -1, 0, 0 }

/* Offload runtime interfaces */

extern int _Offload_number_of_devices(void);
extern int _Offload_get_device_number(void);
extern int _Offload_get_physical_device_number(void);

/* Offload stream runtime interfaces */

extern _Offload_stream _Offload_stream_create(
    int device,           // MIC device number
    int number_of_cpus    // Cores allocated to the stream
);

extern int _Offload_stream_destroy(
    int device,             // MIC device number
    _Offload_stream stream  // stream handle
);

extern int _Offload_stream_delete(
    _Offload_stream handle  // stream handle
);

extern int _Offload_stream_completed(
    int device,             // MIC device number
    _Offload_stream handle  // stream handle
);

extern int _Offload_device_streams_completed(
    int device             // MIC device number
);

extern int _Offload_stream_is_empty(
    _Offload_stream handle  // stream handle
);

/*
 * _Offload_shared_malloc/free are only supported when offload is enabled
 * else they are defined to malloc and free
*/
#ifdef __INTEL_OFFLOAD
extern void* _Offload_shared_malloc(size_t size);
extern void  _Offload_shared_free(void *ptr);
extern void* _Offload_shared_aligned_malloc(size_t size, size_t align);
extern void  _Offload_shared_aligned_free(void *ptr);
#else
#include <malloc.h>
#define _Offload_shared_malloc(size)                 malloc(size)
#define _Offload_shared_free(ptr)                    free(ptr);
#if defined(_WIN32)
#define _Offload_shared_aligned_malloc(size, align)  _aligned_malloc(size, align)
#define _Offload_shared_aligned_free(ptr)            _aligned_free(ptr);
#else
#define _Offload_shared_aligned_malloc(size, align)  memalign(align, size)
#define _Offload_shared_aligned_free(ptr)            free(ptr);
#endif
#endif


extern int _Offload_signaled(int index, void *signal);
extern void _Offload_report(int val);
extern int _Offload_find_associated_mic_memory(
   int           target,
   const void*   cpu_addr,
   void**        cpu_base_addr,
   uint64_t*     buf_length,
   void**        mic_addr,
   uint64_t*     mic_buf_start_offset,
   int*          is_static
);

/* OpenMP API */

extern void omp_set_default_device(int num) __GOMP_NOTHROW;
extern int  omp_get_default_device(void) __GOMP_NOTHROW;
extern int  omp_get_num_devices(void) __GOMP_NOTHROW;

// OpenMP 4.5 APIs

/*! \fn omp_get_initial_device
    \brief Return the device id of the initial device.
    \return Returns the device id of the initial device.
*/
extern int omp_get_initial_device(
    void
) __GOMP_NOTHROW;

/*! \fn omp_target_alloc
    \brief Allocate memory in the device data environment.
    \param size        Number of bytes to allocate.
    \param device_num  The device number on which to allocate.
    \return            Returns a pointer to the allocated memory.  
*/
extern void* omp_target_alloc(
    size_t size, 
    int    device_num
) __GOMP_NOTHROW;

/*! \fn omp_target_free
    \brief Free memory in the device data environment.
    \param device_ptr  Address of allocated device memory.
    \param device_num  The device number on which to free.
*/
extern void omp_target_free(
    void *device_ptr, 
    int   device_num
) __GOMP_NOTHROW;

/*! \fn omp_target_is_present
    \brief Test whether a host pointer has corresponding storage on a device.
    \param device_ptr  Address of allocated device memory.
    \param device_num  The device number on which to test..
    \return            true if storage is found, false otherwise.
*/
extern int omp_target_is_present(
    const void *ptr,
    int device_num
) __GOMP_NOTHROW;

/*! \fn omp_target_memcpy
    \brief Copy memory between host/device pointers.
    \param dst         Address of destination memory.
    \param src         Address of source memory.
    \param length      Number of bytes to copy.
    \param dst_offset  Destination offset in bytes.
    \param src_offset  Source offset in bytes.
    \param dst_device  Destination device number.
    \param src_device  Source device number.
    \return            0 on success, 1 otherwise.
*/
extern int omp_target_memcpy(
    void   *dst, 
    const void *src,
    size_t  length, 
    size_t  dst_offset, 
    size_t  src_offset, 
    int     dst_device,
    int     src_device
) __GOMP_NOTHROW;

/*! \fn omp_target_memcpy_rect
    \brief Copy a rectangular subsection from 
    \brief one multi-dimensional array to another.
    \param dst           Address of destination array.
    \param src           Address of source array.
    \param element_size  Number of bytes in each array element.
    \param num_dims      Number of dimensions.
    \param volume        Array of element counts to copy in each dimension.
    \param dst_offsets   Destination offsets array.
    \param src_offsets   Source offsets array.
    \param dst_dims      Destination array dimensions array.
    \param src_dims      Source array dimensions array.
    \param dst_device    Destination device number.
    \param src_device    Source device number.
    \return              0 on success, 1 otherwise.
*/
extern int omp_target_memcpy_rect(
    void         *dst,
    const void   *src,
    size_t        element_size,
    int           num_dims,
    const size_t *volume,
    const size_t *dst_offsets,
    const size_t *src_offsets,
    const size_t *dst_dimensions,
    const size_t *src_dimensions,
    int           dst_device,
    int           src_device
) __GOMP_NOTHROW;

/*! \fn omp_target_associate_ptr
    \brief Map a device pointer to a host pointer.
    \param host_ptr       The host pointer.
    \param device_ptr     The device pointer.
    \param size           Number of bytes to map.
    \param device_offset  Offset on device of mapped memory.
    \param device_num     Device number.
    \return               0 on success, 1 otherwise.
*/
extern int omp_target_associate_ptr(
    const void *host_ptr,
    const void *device_ptr,
    size_t  size,
    size_t  device_offset,
    int     device_num
) __GOMP_NOTHROW;

/*! \fn omp_target_disassociate_ptr
    \brief Remove a host pointer to device pointer association.
    \param ptr         The host pointer to disassociate.
    \param device_num  Device number.
    \return            0 on success, 1 otherwise.
*/
extern int omp_target_disassociate_ptr(
    const void *host_ptr,
    int     device_num
) __GOMP_NOTHROW;

// End of OpenMP 4.5 APIs

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
#if (defined(_WIN32) || defined(_WIN64))   /* Windows */
        if (ptr == 0) throw std::bad_alloc();
#else
        if (ptr == 0) std::__throw_bad_alloc();
#endif
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
