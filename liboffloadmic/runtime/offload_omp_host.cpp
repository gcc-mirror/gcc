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
//#include <stdlib.h>
//#include "offload.h"
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

// OpenMP 4.5 APIs

// COI supports 3-dim multiD transfers
#define MAX_ARRAY_RANK 3

int omp_get_initial_device(
    void
) __GOMP_NOTHROW
{
    return -1;
}

void* omp_target_alloc(
    size_t size, 
    int    device_num
) __GOMP_NOTHROW
{
    __offload_init_library();

    OFFLOAD_TRACE(2, "omp_target_alloc(%lld, %d)\n", size, device_num);

    if (device_num < -1) {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    void* result = 0;

    // malloc on CPU
    if (device_num == -1) {
        // We do not check for malloc returning NULL because the 
        // specification of this API includes the possibility of failure.
        // The user will check the returned result
        result = malloc(size);
        return result;
    }

    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(
                       TARGET_MIC, device_num, 0, NULL, __func__, 0);
    if (ofld != 0) {
        VarDesc vars[2] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_in;
        vars[0].size = sizeof(size);
        vars[0].count = 1;
        vars[0].ptr = &size;

        vars[1].type.src = c_data;
        vars[1].type.dst = c_data;
        vars[1].direction.bits = c_parameter_out;
        vars[1].size = sizeof(result);
        vars[1].count = 1;
        vars[1].ptr = &result;

        OFFLOAD_OFFLOAD(ofld, "omp_target_alloc_target",
                        0, 2, vars, NULL, 0, 0, 0);
    }
    return result;
}

void omp_target_free(
    void *device_ptr, 
    int   device_num
) __GOMP_NOTHROW
{
    __offload_init_library();

    OFFLOAD_TRACE(2, "omp_target_free(%p, %d)\n", device_ptr, device_num);

    if (device_num < -1) {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    // free on CPU
    if (device_num == -1) {
        free(device_ptr);
        return;
    }

    OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(
                       TARGET_MIC, device_num, 0, NULL, __func__, 0);
    if (ofld) {
        VarDesc vars[1] = {0};

        vars[0].type.src = c_data;
        vars[0].type.dst = c_data;
        vars[0].direction.bits = c_parameter_in;
        vars[0].size = sizeof(device_ptr);
        vars[0].count = 1;
        vars[0].ptr = &device_ptr;
        
        OFFLOAD_OFFLOAD(ofld, "omp_target_free_target",
                        0, 1, vars, NULL, 0, 0, 0);
    }
}

int omp_target_is_present(
    void *ptr,
    int device_num
) __GOMP_NOTHROW
{
    __offload_init_library();

    OFFLOAD_TRACE(2, "omp_target_is_present(%p, %d)\n", ptr, device_num);

    if (device_num < -1) {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    if (device_num == -1) {
        return false;
    }

    // If OpenMP allows wrap-around for device numbers, enable next line
    //device_num %= mic_engines_total;

    // lookup existing association in pointer table
    PtrData* ptr_data = mic_engines[device_num].find_ptr_data(ptr);
    if (ptr_data == 0) {
        OFFLOAD_TRACE(3, "Address %p is not mapped on device %d\n",
                      ptr, device_num);
        return false;
    }

    OFFLOAD_TRACE(3, "Address %p found mapped on device %d\n",
                  ptr, device_num);
    return true;
}

int omp_target_memcpy(
    void   *dst, 
    void   *src, 
    size_t  length, 
    size_t  dst_offset, 
    size_t  src_offset, 
    int     dst_device,
    int     src_device
) __GOMP_NOTHROW
{
    __offload_init_library();

    OFFLOAD_TRACE(2, "omp_target_memcpy(%p, %p, %lld, %lld, %lld, %d, %d)\n",
                  dst, src, length, dst_offset, src_offset, dst_device, src_device);

    if (dst_device < -1 || src_device < -1) {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    char* srcp = (char *)src + src_offset;
    char* dstp = (char *)dst + dst_offset;

    if (src_device == -1) {
        // Source is CPU
        if (dst_device == -1) {
            // CPU -> CPU
            memcpy(dstp, srcp, length);
            return 0;
        } else {
            // CPU -> MIC
            // COIBufferWrite
            // If OpenMP allows wrap-around for device numbers, enable next line
            //dst_device %= mic_engines_total;

            OFFLOAD_TRACE(3, "Creating buffer from sink memory %llx\n", dstp);
            COIBUFFER mic_buf;
            COIRESULT res = COI::BufferCreateFromMemory(length,
                                COI_BUFFER_NORMAL, COI_SINK_MEMORY, dstp,
                                1, &mic_engines[dst_device].get_process(),
                                &mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_create_from_mem, res);
                return 1;
            }
            res = COI::BufferWrite(mic_buf, 0, srcp, length,
                      COI_COPY_UNSPECIFIED, 0, 0, 0);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_write, res);
                return 1;
            }
            res = COI::BufferDestroy(mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_destroy, res);
                return 1;
            }
            return 0;
        }
    } else {
        // Source is device
        if (dst_device == -1) {
            // MIC -> CPU
            // COIBufferRead

            // If OpenMP allows wrap-around for device numbers, enable next line
            //src_device %= mic_engines_total;

            OFFLOAD_TRACE(3, "Creating buffer from sink memory %llx\n", srcp);
            COIBUFFER mic_buf;
            COIRESULT res = COI::BufferCreateFromMemory(length,
                                COI_BUFFER_NORMAL, COI_SINK_MEMORY, srcp,
                                1, &mic_engines[src_device].get_process(),
                                &mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_create_from_mem, res);
                return 1;
            }
            res = COI::BufferRead(mic_buf, 0, dstp, length,
                      COI_COPY_UNSPECIFIED, 0, 0, 0);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_read, res);
                return 1;
            }
            res = COI::BufferDestroy(mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_destroy, res);
                return 1;
            }
            return 0;
        } else {
            // some MIC -> some MIC
            if (src_device == dst_device) {
                // MIC local copy will be done as remote memcpy

                OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(TARGET_MIC, src_device,
                                                      0, NULL, __func__, 0);
                if (ofld) {
                    VarDesc vars[3] = {0};

                    vars[0].type.src = c_data;
                    vars[0].type.dst = c_data;
                    vars[0].direction.bits = c_parameter_in;
                    vars[0].size = sizeof(dstp);
                    vars[0].count = 1;
                    vars[0].ptr = &dstp;

                    vars[1].type.src = c_data;
                    vars[1].type.dst = c_data;
                    vars[1].direction.bits = c_parameter_in;
                    vars[1].size = sizeof(srcp);
                    vars[1].count = 1;
                    vars[1].ptr = &srcp;

                    vars[2].type.src = c_data;
                    vars[2].type.dst = c_data;
                    vars[2].direction.bits = c_parameter_in;
                    vars[2].size = sizeof(length);
                    vars[2].count = 1;
                    vars[2].ptr = &length;
        
                    OFFLOAD_OFFLOAD(ofld, "omp_target_memcpy_target",
                                    0, 3, vars, NULL, 0, 0, 0);
                    return 0;
                } else {
                    return 1;
                }
            } else {
                // MICx -> MICy
                // Allocate CPU buffer
                char *cpu_mem = (char *)malloc(length);
                if (cpu_mem == 0) {
                    LIBOFFLOAD_ERROR(c_malloc);
                    return 1;
                }
                int retval = 1;
                if (omp_target_memcpy(
                        cpu_mem, srcp, length, 0, 0, -1, src_device) == 0) {
                    retval = omp_target_memcpy(
                                 dstp, cpu_mem, length, 0, 0, dst_device, -1);
                }
                free(cpu_mem);
                return retval;
            }
        }
    }
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

int omp_target_memcpy_rect(
    void         *dst_,
    void         *src_,
    size_t        element_size,
    int           num_dims,
    const size_t *volume,
    const size_t *dst_offsets,
    const size_t *src_offsets,
    const size_t *dst_dimensions,
    const size_t *src_dimensions,
    int           dst_device,
    int           src_device
) __GOMP_NOTHROW
{
    char *dst = (char *)dst_;
    char *src = (char *)src_;

    __offload_init_library();

    OFFLOAD_TRACE(2, "omp_target_memcpy_rect(%p, %p, %lld, %d, "
                  "%p, %p, %p, %p, %p, %d, %d)\n",
                  dst, src, element_size, num_dims,
                  volume, dst_offsets, src_offsets,
                  dst_dimensions, src_dimensions, dst_device, src_device);
    
    // MAX_ARRAY_RANK dimensions are supported
    if (dst == 0 && src == 0) {
        return MAX_ARRAY_RANK;
    }

    if (num_dims < 1 || num_dims > MAX_ARRAY_RANK ||
        element_size < 1 ||
        volume == 0 || dst_offsets == 0 || src_offsets == 0 ||
        dst_dimensions == 0 || src_dimensions == 0) {
        return 1;
    }

    if (dst_device < -1 || src_device < -1) {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    if (src_device == -1) {
        // Source is CPU
        if (dst_device == -1) {
            // CPU -> CPU
            memcpy_rect((char*)dst, (char*)src, element_size, num_dims, volume,
                        dst_offsets, src_offsets,
                        dst_dimensions, src_dimensions);
            return 0;
        } else {
            // CPU -> MIC
            // COIBufferWriteMultiD
            struct arr_desc dst_desc;
            struct arr_desc src_desc;

            dst_desc.base = (int64_t)dst;
            dst_desc.rank = num_dims;
            
            src_desc.base = (int64_t)src;
            src_desc.rank = num_dims;

            for (int i=0; i<num_dims; i++)
            {
                dst_desc.dim[i].size   = bytesize_at_this_dimension(
                                             element_size,
                                             num_dims - i,
                                             dst_dimensions + i);
                dst_desc.dim[i].lindex = 0;
                dst_desc.dim[i].lower  = dst_offsets[i];
                dst_desc.dim[i].upper  = dst_offsets[i] + volume[i] - 1;
                dst_desc.dim[i].stride = 1;
                
                src_desc.dim[i].size   = bytesize_at_this_dimension(
                                             element_size,
                                             num_dims - i,
                                             src_dimensions + i);
                src_desc.dim[i].lindex = 0;
                src_desc.dim[i].lower  = src_offsets[i];
                src_desc.dim[i].upper  = src_offsets[i] + volume[i] - 1;
                src_desc.dim[i].stride = 1;
            }
            __arr_desc_dump("", "dst", (const Arr_Desc*)&dst_desc, false, false);
            __arr_desc_dump("", "src", (const Arr_Desc*)&src_desc, false, false);
            
            // If OpenMP allows wrap-around for device numbers, enable next line
            //dst_device %= mic_engines_total;
                
            // Compute MIC buffer size
            size_t dst_length = dst_dimensions[0] * bytesize_at_this_dimension(
                                                        element_size,
                                                        num_dims,
                                                        dst_dimensions);

            OFFLOAD_TRACE(3,
                "Creating buffer from sink memory %llx of size %lld\n",
                dst, dst_length);
            COIBUFFER mic_buf;
            COIRESULT res = COI::BufferCreateFromMemory(dst_length,
                                COI_BUFFER_NORMAL, COI_SINK_MEMORY, dst,
                                1, &mic_engines[dst_device].get_process(),
                                &mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_create_from_mem, res);
                return 1;
            }
            res = COI::BufferWriteMultiD(mic_buf,
                      mic_engines[dst_device].get_process(),
                      0, &dst_desc, &src_desc,
                      COI_COPY_UNSPECIFIED, 0, 0, 0);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_write, res);
                return 1;
            }
            res = COI::BufferDestroy(mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_destroy, res);
                return 1;
            }
            return 0;
        }
    } else {
        // Source is device
        if (dst_device == -1) {
            // COIBufferReadMultiD
            struct arr_desc dst_desc;
            struct arr_desc src_desc;

            dst_desc.base = (int64_t)dst;
            dst_desc.rank = num_dims;
            
            src_desc.base = (int64_t)src;
            src_desc.rank = num_dims;

            for (int i=0; i<num_dims; i++)
            {
                dst_desc.dim[i].size   = bytesize_at_this_dimension(
                                             element_size,
                                             num_dims - i,
                                             dst_dimensions + i);
                dst_desc.dim[i].lindex = 0;
                dst_desc.dim[i].lower  = dst_offsets[i];
                dst_desc.dim[i].upper  = dst_offsets[i] + volume[i] - 1;
                dst_desc.dim[i].stride = 1;
                
                src_desc.dim[i].size   = bytesize_at_this_dimension(
                                             element_size,
                                             num_dims - i,
                                             src_dimensions + i);
                src_desc.dim[i].lindex = 0;
                src_desc.dim[i].lower  = src_offsets[i];
                src_desc.dim[i].upper  = src_offsets[i] + volume[i] - 1;
                src_desc.dim[i].stride = 1;
            }
            __arr_desc_dump("", "dst", (const Arr_Desc*)&dst_desc, false, false);
            __arr_desc_dump("", "src", (const Arr_Desc*)&src_desc, false, false);
            
            // If OpenMP allows wrap-around for device numbers, enable next line
            //src_device %= mic_engines_total;
                
            // Compute MIC buffer size
            size_t src_length = src_dimensions[0] * bytesize_at_this_dimension(
                                                        element_size,
                                                        num_dims,
                                                        src_dimensions);

            OFFLOAD_TRACE(3,
                "Creating buffer from sink memory %llx of size %lld\n",
                src, src_length);
            COIBUFFER mic_buf;
            COIRESULT res = COI::BufferCreateFromMemory(src_length,
                                COI_BUFFER_NORMAL, COI_SINK_MEMORY, src,
                                1, &mic_engines[src_device].get_process(),
                                &mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_create_from_mem, res);
                return 1;
            }
            res = COI::BufferReadMultiD(mic_buf, 0,
                      &dst_desc, &src_desc,
                      COI_COPY_UNSPECIFIED, 0, 0, 0);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_write, res);
                return 1;
            }
            res = COI::BufferDestroy(mic_buf);
            if (res != COI_SUCCESS) {
                LIBOFFLOAD_ERROR(c_buf_destroy, res);
                return 1;
            }
            return 0;
        } else {
            // some MIC -> some MIC
            if (src_device == dst_device) {
                // MIC local copy will be done as remote memcpy_rect
                struct parameters {
                    void   *dst;
                    void   *src;
                    size_t element_size;
                    int    num_dims;
                    size_t array_info[MAX_ARRAY_RANK*5];
                } parameters = {dst, src, element_size, num_dims};
                int result;
                
                for (int i=0; i<num_dims; i++)
                {
                    parameters.array_info[i]            = volume[i];
                    parameters.array_info[i+num_dims]   = dst_offsets[i];
                    parameters.array_info[i+num_dims*2] = src_offsets[i];
                    parameters.array_info[i+num_dims*3] = dst_dimensions[i];
                    parameters.array_info[i+num_dims*4] = src_dimensions[i];
                }

                OFFLOAD ofld = OFFLOAD_TARGET_ACQUIRE(TARGET_MIC, src_device,
                                                      0, NULL, __func__, 0);
                if (ofld) {
                    VarDesc vars[1] = {0};

                    vars[0].type.src = c_data;
                    vars[0].type.dst = c_data;
                    vars[0].direction.bits = c_parameter_in;
                    vars[0].size = sizeof(parameters) -
                                       (MAX_ARRAY_RANK - num_dims) *
                                       5 * sizeof(size_t);
                    vars[0].count = 1;
                    vars[0].ptr = &parameters;
        
                    OFFLOAD_OFFLOAD(ofld, "omp_target_memcpy_rect_target",
                                    0, 1, vars, NULL, 0, 0, 0);
                    return 0;
                } else {
                    return 1;
                }
            } else {
                // MICx -> MICy

                // Compute transfer byte-count
                size_t dst_length = element_size;
                for (int i=0; i<num_dims; i++) {
                    dst_length *= volume[i];
                }

                // Allocate CPU buffer
                char *cpu_mem = (char *)malloc(dst_length);
                if (cpu_mem == 0) {
                    LIBOFFLOAD_ERROR(c_malloc);
                    return 1;
                }

                // Create CPU offset and dimension arrays
                // The CPU array collects the data in a contiguous block
                size_t cpu_offsets[MAX_ARRAY_RANK];
                size_t cpu_dimensions[MAX_ARRAY_RANK];
                for (int i=0; i<num_dims; i++) {
                    cpu_offsets[i] = 0;
                    cpu_dimensions[i] = volume[i];
                }

                int retval = 1;
                if (omp_target_memcpy_rect(
                        cpu_mem, src, element_size, num_dims, volume,
                        cpu_offsets, src_offsets,
                        cpu_dimensions, src_dimensions,
                        -1, src_device) == 0) {
                    retval = omp_target_memcpy_rect(
                                 dst, cpu_mem, element_size, num_dims, volume,
                                 dst_offsets, cpu_offsets,
                                 dst_dimensions, cpu_dimensions,
                                 dst_device, -1);
                }
                free(cpu_mem);
                return retval;
            }
        }
    }
}

// host_ptr is key in table that yields association on device
// A COIBUFFER of specified size is created from the memory at 
//     device_ptr+device_offset on device_num
int omp_target_associate_ptr(
    void   *host_ptr, 
    void   *device_ptr,
    size_t  size,
    size_t  device_offset,
    int     device_num
) __GOMP_NOTHROW
{
    COIRESULT res;

    __offload_init_library();

    OFFLOAD_TRACE(2, "omp_target_associate_ptr(%p, %p, %lld, %lld, %d)\n",
                  host_ptr, device_ptr, size, device_offset, device_num);

    if (device_num < -1) {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    // Associating to CPU is treated as failure
    if (device_num == -1) {
        return 1;
    }

    // An incorrect size is treated as failure
    if (size < 0) {
        return 1;
    }
    
    // If OpenMP allows wrap-around for device numbers, enable next line
    //Engine& device = mic_engines[device_num % mic_engines_total];
    Engine& device = mic_engines[device_num];
    
    // Does host pointer have association already?
    // lookup existing association in pointer table
    PtrData* ptr_data = device.find_ptr_data(host_ptr);
    if (ptr_data != 0) {
        OFFLOAD_TRACE(3, "Address %p is already mapped on device %d\n",
                      host_ptr, device_num);
        // Is current device pointer and offset same as existing?
        if ((void*)ptr_data->mic_addr == device_ptr &&
            (size_t)ptr_data->alloc_disp == device_offset) {
            return 0;
        } else {
            return 1;
        }
    }

    // Create association
    OFFLOAD_TRACE(3, "Creating association for data: addr %p, length %lld\n",
                  host_ptr, size);

    bool is_new;
    ptr_data = device.insert_ptr_data(host_ptr, size, is_new);
    ptr_data->is_omp_associate = true;

    // create CPU buffer
    OFFLOAD_TRACE(3,
              "Creating buffer from source memory %p, length %lld\n",
              host_ptr, size);

    // result is not checked because we can continue without cpu
    // buffer. In this case we will use COIBufferRead/Write
    // instead of COIBufferCopy.

    COI::BufferCreateFromMemory(size,
                            COI_BUFFER_OPENCL,
                            0,
                            host_ptr,
                            1,
                            &device.get_process(),
                            &ptr_data->cpu_buf);

    // create MIC buffer
    OFFLOAD_TRACE(3,
              "Creating buffer from sink memory: addr %p, size %lld\n",
              (char *)device_ptr + device_offset, size);
    res = COI::BufferCreateFromMemory(size,
                                      COI_BUFFER_NORMAL,
                                      COI_SINK_MEMORY,
                                      device_ptr,
                                      1,
                                      &device.get_process(),
                                      &ptr_data->mic_buf);
    if (res != COI_SUCCESS) {
        ptr_data->alloc_ptr_data_lock.unlock();
        return 1;
    }

    // make buffer valid on the device.
    res = COI::BufferSetState(ptr_data->mic_buf,
        device.get_process(),
        COI_BUFFER_VALID,
        COI_BUFFER_NO_MOVE,
        0, 0, 0);
    if (res != COI_SUCCESS) {
        ptr_data->alloc_ptr_data_lock.unlock();
        return 1;
    }

    res = COI::BufferSetState(ptr_data->mic_buf,
        COI_PROCESS_SOURCE,
        COI_BUFFER_INVALID,
        COI_BUFFER_NO_MOVE,
        0, 0, 0);
    if (res != COI_SUCCESS) {
        ptr_data->alloc_ptr_data_lock.unlock();
        return 1;
    }
    ptr_data->alloc_disp = device_offset;
    ptr_data->alloc_ptr_data_lock.unlock();

    return 0;
}

int omp_target_disassociate_ptr(
    void   *host_ptr,
    int     device_num
) __GOMP_NOTHROW
{
    COIRESULT res;

    __offload_init_library();

    OFFLOAD_TRACE(2, "omp_target_disassociate_ptr(%p, %d)\n",
                  host_ptr, device_num);

    if (device_num < -1) {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    // Dissociating from CPU is treated as failure
    if (device_num == -1) {
        return 1;
    }
    
    // If OpenMP allows wrap-around for device numbers, enable next line
    //Engine& device = mic_engines[device_num % mic_engines_total];
    Engine& device = mic_engines[device_num];

    // Lookup existing association in pointer table
    PtrData* ptr_data = device.find_ptr_data(host_ptr);

    // Attempt to disassociate unassociated pointer is a failure
    if (ptr_data == 0) {
        return 1;
    }
                    
    // Destroy buffers
    if (ptr_data->cpu_buf != 0) {
        OFFLOAD_TRACE(3, "Destroying CPU buffer %p\n", ptr_data->cpu_buf);
        COI::BufferDestroy(ptr_data->cpu_buf);
    }
    if (ptr_data->mic_buf != 0) {
        OFFLOAD_TRACE(3, "Destroying MIC buffer %p\n", ptr_data->mic_buf);
        COI::BufferDestroy(ptr_data->mic_buf);
    }
    
    // Remove association from map
    OFFLOAD_TRACE(3, "Removing association for addr %p\n",
                  ptr_data->cpu_addr.start());
    device.remove_ptr_data(ptr_data->cpu_addr.start());

    return 0;
}

// End of OpenMP 4.5 APIs


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
