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


#include <omp.h>
#include "offload.h"
#include "compiler_if_target.h"

// OpenMP API

void omp_set_default_device(int num) __GOMP_NOTHROW
{
}

int omp_get_default_device(void) __GOMP_NOTHROW
{
    return mic_index;
}

int omp_get_num_devices() __GOMP_NOTHROW
{
    return mic_engines_total;
}

// OpenMP 4.5 APIs

// COI supports 3-dim multiD transfers
#define MAX_ARRAY_RANK 3

DLL_LOCAL void omp_target_alloc_target(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[2] = {0};
    size_t size;
    void* memory;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &size;

    vars[1].type.src = c_data;
    vars[1].type.dst = c_data;
    vars[1].direction.bits = c_parameter_out;
    vars[1].ptr = &memory;

    OFFLOAD_TARGET_ENTER(ofld, 2, vars, NULL);
    OFFLOAD_DEBUG_TRACE(2, "omp_target_alloc(%lld)\n", size);
    // We do not check for malloc returning NULL because the 
    // specification of this API includes the possibility of failure.
    // The user will check the returned result
    memory = malloc(size);
    OFFLOAD_DEBUG_TRACE(2, "omp_target_alloc allocated at %p\n", memory);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_target_free_target(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    void* memory;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &memory;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    OFFLOAD_DEBUG_TRACE(2, "omp_target_free(%p)\n", memory);
    free(memory);
    OFFLOAD_DEBUG_TRACE(2, "omp_target_free freed\n");
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_target_memcpy_target(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[3] = {0};
    void* dst;
    void* src;
    size_t length;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &dst;
    
    vars[1].type.src = c_data;
    vars[1].type.dst = c_data;
    vars[1].direction.bits = c_parameter_in;
    vars[1].ptr = &src;
    
    vars[2].type.src = c_data;
    vars[2].type.dst = c_data;
    vars[2].direction.bits = c_parameter_in;
    vars[2].ptr = &length;

    OFFLOAD_TARGET_ENTER(ofld, 3, vars, NULL);
    OFFLOAD_DEBUG_TRACE(2, "omp_target_memcpy(%p, %p, %lld)\n",
                        dst, src, length);
    memcpy(dst, src, length);

    OFFLOAD_DEBUG_TRACE(2, "omp_target_memcpy done\n");
    OFFLOAD_TARGET_LEAVE(ofld);
}

static size_t bytesize_at_this_dimension(
    size_t element_size,
    int num_dims,
    const size_t* dimensions
)
{
    if (num_dims > 1) {
        return dimensions[1] * 
               bytesize_at_this_dimension(
                   element_size, num_dims-1, dimensions+1);
    } else {
        return element_size;
    }
}

static void memcpy_rect(
    char         *dst,
    char         *src,
    size_t        element_size,
    int           num_dims,
    const size_t *volume,
    const size_t *dst_offsets,
    const size_t *src_offsets,
    const size_t *dst_dimensions,
    const size_t *src_dimensions
)
{
    if (num_dims > 1) {
        int count = volume[0];
        int dst_index = dst_offsets[0];
        int src_index = src_offsets[0];
        size_t dst_element_size =
            bytesize_at_this_dimension(element_size, num_dims, dst_dimensions);
        size_t src_element_size =
            bytesize_at_this_dimension(element_size, num_dims, src_dimensions);
        for (; count>0; dst_index++, src_index++, count--) {
            memcpy_rect(dst+dst_element_size*dst_index,
                        src+src_element_size*src_index,
                        element_size, num_dims-1, volume+1,
                        dst_offsets+1, src_offsets+1,
                        dst_dimensions+1, src_dimensions+1);
        }
    } else {
        memcpy(dst+dst_offsets[0]*element_size,
               src+src_offsets[0]*element_size,
               element_size * volume[0]);
    }
}

DLL_LOCAL void omp_target_memcpy_rect_target(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    struct parameters {
        void   *dst;
        void   *src;
        size_t  element_size;
        int     num_dims;
        size_t array_info[MAX_ARRAY_RANK*5];
    } parameters;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &parameters;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    OFFLOAD_DEBUG_TRACE(2, "omp_target_memcpy_rect(%p, %p, %lld, %d)\n",
                        parameters.dst, parameters.src,
                        parameters.element_size, parameters.num_dims);
    memcpy_rect(
          (char*)parameters.dst, (char*)parameters.src, parameters.element_size,
          parameters.num_dims,
          &parameters.array_info[0],
          &parameters.array_info[parameters.num_dims],
          &parameters.array_info[parameters.num_dims*2],
          &parameters.array_info[parameters.num_dims*3],
          &parameters.array_info[parameters.num_dims*4]);

    OFFLOAD_DEBUG_TRACE(2, "omp_target_memcpy_rect done\n");
    OFFLOAD_TARGET_LEAVE(ofld);
}

// End of OpenMP 4.5 APIs


// OpenMP API wrappers

static void omp_send_int_to_host(
    void *ofld_,
    int setting
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_out;
    vars[0].ptr = &setting;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    OFFLOAD_TARGET_LEAVE(ofld);
}

static int omp_get_int_from_host(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    int setting;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &setting;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    OFFLOAD_TARGET_LEAVE(ofld);

    return setting;
}

DLL_LOCAL void omp_set_num_threads_lrb(
    void *ofld
)
{
    int num_threads;

    num_threads = omp_get_int_from_host(ofld);
    omp_set_num_threads(num_threads);
}

DLL_LOCAL void omp_get_max_threads_lrb(
    void *ofld
)
{
    int num_threads;

    num_threads = omp_get_max_threads();
    omp_send_int_to_host(ofld, num_threads);
}

DLL_LOCAL void omp_get_num_procs_lrb(
    void *ofld
)
{
    int num_procs;

    num_procs = omp_get_num_procs();
    omp_send_int_to_host(ofld, num_procs);
}

DLL_LOCAL void omp_set_dynamic_lrb(
    void *ofld
)
{
    int dynamic;

    dynamic = omp_get_int_from_host(ofld);
    omp_set_dynamic(dynamic);
}

DLL_LOCAL void omp_get_dynamic_lrb(
    void *ofld
)
{
    int dynamic;

    dynamic = omp_get_dynamic();
    omp_send_int_to_host(ofld, dynamic);
}

DLL_LOCAL void omp_set_nested_lrb(
    void *ofld
)
{
    int nested;

    nested = omp_get_int_from_host(ofld);
    omp_set_nested(nested);
}

DLL_LOCAL void omp_get_nested_lrb(
    void *ofld
)
{
    int nested;

    nested = omp_get_nested();
    omp_send_int_to_host(ofld, nested);
}

DLL_LOCAL void omp_set_schedule_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[2] = {0};
    omp_sched_t kind;
    int modifier;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &kind;

    vars[1].type.src = c_data;
    vars[1].type.dst = c_data;
    vars[1].direction.bits = c_parameter_in;
    vars[1].ptr = &modifier;

    OFFLOAD_TARGET_ENTER(ofld, 2, vars, NULL);
    omp_set_schedule(kind, modifier);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_get_schedule_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[2] = {0};
    omp_sched_t kind;
    int modifier;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_out;
    vars[0].ptr = &kind;

    vars[1].type.src = c_data;
    vars[1].type.dst = c_data;
    vars[1].direction.bits = c_parameter_out;
    vars[1].ptr = &modifier;

    OFFLOAD_TARGET_ENTER(ofld, 2, vars, NULL);
    omp_get_schedule(&kind, &modifier);
    OFFLOAD_TARGET_LEAVE(ofld);
}

// lock API functions

DLL_LOCAL void omp_init_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_out;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_init_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_destroy_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_destroy_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_set_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_inout;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_set_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_unset_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_inout;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_unset_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_test_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[2] = {0};
    omp_lock_target_t lock;
    int result;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_inout;
    vars[0].ptr = &lock;

    vars[1].type.src = c_data;
    vars[1].type.dst = c_data;
    vars[1].direction.bits = c_parameter_out;
    vars[1].ptr = &result;

    OFFLOAD_TARGET_ENTER(ofld, 2, vars, NULL);
    result = omp_test_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

// nested lock API functions

DLL_LOCAL void omp_init_nest_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_nest_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_out;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_init_nest_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_destroy_nest_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_nest_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_in;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_destroy_nest_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_set_nest_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_nest_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_inout;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_set_nest_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_unset_nest_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[1] = {0};
    omp_nest_lock_target_t lock;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_inout;
    vars[0].ptr = &lock;

    OFFLOAD_TARGET_ENTER(ofld, 1, vars, NULL);
    omp_unset_nest_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

DLL_LOCAL void omp_test_nest_lock_lrb(
    void *ofld_
)
{
    OFFLOAD ofld = (OFFLOAD) ofld_;
    VarDesc vars[2] = {0};
    omp_nest_lock_target_t lock;
    int result;

    vars[0].type.src = c_data;
    vars[0].type.dst = c_data;
    vars[0].direction.bits = c_parameter_inout;
    vars[0].ptr = &lock;

    vars[1].type.src = c_data;
    vars[1].type.dst = c_data;
    vars[1].direction.bits = c_parameter_out;
    vars[1].ptr = &result;

    OFFLOAD_TARGET_ENTER(ofld, 2, vars, NULL);
    result = omp_test_nest_lock(&lock.lock);
    OFFLOAD_TARGET_LEAVE(ofld);
}

// Target-side stubs for the host functions (to avoid unresolveds)
// These are needed for the offloadm table

void omp_set_num_threads_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
)
{
}

int omp_get_max_threads_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return 0;
}

int omp_get_num_procs_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return 0;
}

void omp_set_dynamic_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
)
{
}

int omp_get_dynamic_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return 0;
}

void omp_set_nested_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
)
{
}

int omp_get_nested_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return 0;
}

void omp_set_schedule_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_sched_t kind,
    int modifier
)
{
}

void omp_get_schedule_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_sched_t *kind,
    int *modifier
)
{
}

void omp_init_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
}

void omp_destroy_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
}

void omp_set_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
}

void omp_unset_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
}

int omp_test_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
    return 0;
}

void omp_init_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
}

void omp_destroy_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
}

void omp_set_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
}

void omp_unset_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
}

int omp_test_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
    return 0;
}
