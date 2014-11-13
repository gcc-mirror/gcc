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


#include <omp.h>
#include "offload.h"
#include "compiler_if_host.h"

// OpenMP API

void omp_set_default_device(int num) __GOMP_NOTHROW
{
    if (num >= 0) {
        __omp_device_num = num;
    }
}

int omp_get_default_device(void) __GOMP_NOTHROW
{
    return __omp_device_num;
}

int omp_get_num_devices() __GOMP_NOTHROW
{
    __offload_init_library();
    return mic_engines_total;
}

// OpenMP API wrappers

static void omp_set_int_target(
    TARGET_TYPE target_type,
    int target_number,
    int setting,
    const char* f_name
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          f_name, 0);
    if (ofld) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_in;
        vars[0].size = sizeof(int);
        vars[0].count = 1;
        vars[0].ptr = &setting;

        OFFLOAD_OFFLOAD(ofld, f_name, 0, 1, vars, NULL, 0, 0, 0);
    }
}

static int omp_get_int_target(
    TARGET_TYPE target_type,
    int target_number,
    const char * f_name
)
{
    int setting = 0;

    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          f_name, 0);
    if (ofld) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_out;
        vars[0].size = sizeof(int);
        vars[0].count = 1;
        vars[0].ptr = &setting;

        OFFLOAD_OFFLOAD(ofld, f_name, 0, 1, vars, NULL, 0, 0, 0);
    }
    return setting;
}

void omp_set_num_threads_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
)
{
    omp_set_int_target(target_type, target_number, num_threads,
                       "omp_set_num_threads_target");
}

int omp_get_max_threads_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return omp_get_int_target(target_type, target_number,
                              "omp_get_max_threads_target");
}

int omp_get_num_procs_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return omp_get_int_target(target_type, target_number,
                              "omp_get_num_procs_target");
}

void omp_set_dynamic_target(
    TARGET_TYPE target_type,
    int target_number,
    int num_threads
)
{
    omp_set_int_target(target_type, target_number, num_threads,
                       "omp_set_dynamic_target");
}

int omp_get_dynamic_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return omp_get_int_target(target_type, target_number,
                              "omp_get_dynamic_target");
}

void omp_set_nested_target(
    TARGET_TYPE target_type,
    int target_number,
    int nested
)
{
    omp_set_int_target(target_type, target_number, nested,
                       "omp_set_nested_target");
}

int omp_get_nested_target(
    TARGET_TYPE target_type,
    int target_number
)
{
    return omp_get_int_target(target_type, target_number,
                              "omp_get_nested_target");
}

void omp_set_schedule_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_sched_t kind,
    int modifier
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[2] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_in;
        vars[0].size = sizeof(omp_sched_t);
        vars[0].count = 1;
        vars[0].ptr = &kind;

        vars[1].type.src = c_data;
        vars[1].type.dst = c_data;
        vars[1].direction.bits = c_parameter_in;
        vars[1].size = sizeof(int);
        vars[1].count = 1;
        vars[1].ptr = &modifier;

        OFFLOAD_OFFLOAD(ofld, "omp_set_schedule_target",
                        0, 2, vars, NULL, 0, 0, 0);
    }
}

void omp_get_schedule_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_sched_t *kind,
    int *modifier
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[2] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_out;
        vars[0].size = sizeof(omp_sched_t);
        vars[0].count = 1;
        vars[0].ptr = kind;

        vars[1].type.src = c_data;
        vars[1].type.dst = c_data;
        vars[1].direction.bits = c_parameter_out;
        vars[1].size = sizeof(int);
        vars[1].count = 1;
        vars[1].ptr = modifier;

        OFFLOAD_OFFLOAD(ofld, "omp_get_schedule_target",
                        0, 2, vars, NULL, 0, 0, 0);
    }
}

// lock API functions

void omp_init_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_out;
        vars[0].size = sizeof(omp_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_init_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

void omp_destroy_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_in;
        vars[0].size = sizeof(omp_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_destroy_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

void omp_set_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_inout;
        vars[0].size = sizeof(omp_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_set_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

void omp_unset_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_inout;
        vars[0].size = sizeof(omp_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_unset_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

int omp_test_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_lock_target_t *lock
)
{
    int result = 0;

    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[2] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_inout;
        vars[0].size = sizeof(omp_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        vars[1].type.src = c_data;
        vars[1].type.dst = c_data;
        vars[1].direction.bits = c_parameter_out;
        vars[1].size = sizeof(int);
        vars[1].count = 1;
        vars[1].ptr = &result;

        OFFLOAD_OFFLOAD(ofld, "omp_test_lock_target",
                        0, 2, vars, NULL, 0, 0, 0);
    }
    return result;
}

// nested lock API functions

void omp_init_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_out;
        vars[0].size = sizeof(omp_nest_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_init_nest_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

void omp_destroy_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_in;
        vars[0].size = sizeof(omp_nest_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_destroy_nest_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

void omp_set_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_inout;
        vars[0].size = sizeof(omp_nest_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_set_nest_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

void omp_unset_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_inout;
        vars[0].size = sizeof(omp_nest_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        OFFLOAD_OFFLOAD(ofld, "omp_unset_nest_lock_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

int omp_test_nest_lock_target(
    TARGET_TYPE target_type,
    int target_number,
    omp_nest_lock_target_t *lock
)
{
    int result = 0;

    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(target_type, target_number, 0, NULL,
                                          __func__, 0);
    if (ofld != 0) {
        VarDesc vars[2] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_inout;
        vars[0].size = sizeof(omp_nest_lock_target_t);
        vars[0].count = 1;
        vars[0].ptr = lock;

        vars[1].type.src = c_data;
        vars[1].type.dst = c_data;
        vars[1].direction.bits = c_parameter_out;
        vars[1].size = sizeof(int);
        vars[1].count = 1;
        vars[1].ptr = &result;

        OFFLOAD_OFFLOAD(ofld, "omp_test_nest_lock_target",
                        0, 2, vars, NULL, 0, 0, 0);
    }
    return result;
}
