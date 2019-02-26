/* CUDA API description.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.

This header provides the minimum amount of typedefs, enums and function
declarations to be able to compile plugin-nvptx.c if cuda.h and
libcuda.so.1 are not available.  */

#ifndef GCC_CUDA_H
#define GCC_CUDA_H

#include <stdlib.h>

#define CUDA_VERSION 8000

typedef void *CUcontext;
typedef int CUdevice;
#if defined(__LP64__) || defined(_WIN64)
typedef unsigned long long CUdeviceptr;
#else
typedef unsigned CUdeviceptr;
#endif
typedef void *CUevent;
typedef void *CUfunction;
typedef void *CUlinkState;
typedef void *CUmodule;
typedef size_t (*CUoccupancyB2DSize)(int);
typedef void *CUstream;

typedef enum {
  CUDA_SUCCESS = 0,
  CUDA_ERROR_INVALID_VALUE = 1,
  CUDA_ERROR_OUT_OF_MEMORY = 2,
  CUDA_ERROR_INVALID_CONTEXT = 201,
  CUDA_ERROR_NOT_FOUND = 500,
  CUDA_ERROR_NOT_READY = 600,
  CUDA_ERROR_LAUNCH_FAILED = 719,
  CUDA_ERROR_COOPERATIVE_LAUNCH_TOO_LARGE = 720,
  CUDA_ERROR_NOT_PERMITTED = 800,
  CUDA_ERROR_NOT_SUPPORTED = 801,
  CUDA_ERROR_UNKNOWN = 999
} CUresult;

typedef enum {
  CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 1,
  CU_DEVICE_ATTRIBUTE_WARP_SIZE = 10,
  CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK = 12,
  CU_DEVICE_ATTRIBUTE_CLOCK_RATE = 13,
  CU_DEVICE_ATTRIBUTE_GPU_OVERLAP = 15,
  CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT = 16,
  CU_DEVICE_ATTRIBUTE_INTEGRATED = 18,
  CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY = 19,
  CU_DEVICE_ATTRIBUTE_COMPUTE_MODE = 20,
  CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS = 31,
  CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR = 39,
  CU_DEVICE_ATTRIBUTE_ASYNC_ENGINE_COUNT = 40,
  CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_MULTIPROCESSOR = 82
} CUdevice_attribute;

enum {
  CU_EVENT_DEFAULT = 0,
  CU_EVENT_DISABLE_TIMING = 2
};

typedef enum {
  CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 0,
  CU_FUNC_ATTRIBUTE_NUM_REGS = 4
} CUfunction_attribute;

typedef enum {
  CU_JIT_WALL_TIME = 2,
  CU_JIT_INFO_LOG_BUFFER = 3,
  CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES = 4,
  CU_JIT_ERROR_LOG_BUFFER = 5,
  CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES = 6,
  CU_JIT_OPTIMIZATION_LEVEL = 7,
  CU_JIT_LOG_VERBOSE = 12
} CUjit_option;

typedef enum {
  CU_JIT_INPUT_PTX = 1
} CUjitInputType;

enum {
  CU_CTX_SCHED_AUTO = 0
};

#define CU_LAUNCH_PARAM_END ((void *) 0)
#define CU_LAUNCH_PARAM_BUFFER_POINTER ((void *) 1)
#define CU_LAUNCH_PARAM_BUFFER_SIZE ((void *) 2)

enum {
  CU_STREAM_DEFAULT = 0,
  CU_STREAM_NON_BLOCKING = 1
};

#define cuCtxCreate cuCtxCreate_v2
CUresult cuCtxCreate (CUcontext *, unsigned, CUdevice);
#define cuCtxDestroy cuCtxDestroy_v2
CUresult cuCtxDestroy (CUcontext);
CUresult cuCtxGetCurrent (CUcontext *);
CUresult cuCtxGetDevice (CUdevice *);
#define cuCtxPopCurrent cuCtxPopCurrent_v2
CUresult cuCtxPopCurrent (CUcontext *);
#define cuCtxPushCurrent cuCtxPushCurrent_v2
CUresult cuCtxPushCurrent (CUcontext);
CUresult cuCtxSynchronize (void);
CUresult cuDeviceGet (CUdevice *, int);
CUresult cuDeviceGetAttribute (int *, CUdevice_attribute, CUdevice);
CUresult cuDeviceGetCount (int *);
CUresult cuEventCreate (CUevent *, unsigned);
#define cuEventDestroy cuEventDestroy_v2
CUresult cuEventDestroy (CUevent);
CUresult cuEventElapsedTime (float *, CUevent, CUevent);
CUresult cuEventQuery (CUevent);
CUresult cuEventRecord (CUevent, CUstream);
CUresult cuEventSynchronize (CUevent);
CUresult cuFuncGetAttribute (int *, CUfunction_attribute, CUfunction);
CUresult cuGetErrorString (CUresult, const char **);
CUresult cuInit (unsigned);
CUresult cuLaunchKernel (CUfunction, unsigned, unsigned, unsigned, unsigned,
			 unsigned, unsigned, unsigned, CUstream, void **, void **);
#define cuLinkAddData cuLinkAddData_v2
CUresult cuLinkAddData (CUlinkState, CUjitInputType, void *, size_t, const char *,
			unsigned, CUjit_option *, void **);
CUresult cuLinkComplete (CUlinkState, void **, size_t *);
#define cuLinkCreate cuLinkCreate_v2
CUresult cuLinkCreate (unsigned, CUjit_option *, void **, CUlinkState *);
CUresult cuLinkDestroy (CUlinkState);
#define cuMemAlloc cuMemAlloc_v2
CUresult cuMemAlloc (CUdeviceptr *, size_t);
#define cuMemAllocHost cuMemAllocHost_v2
CUresult cuMemAllocHost (void **, size_t);
CUresult cuMemcpy (CUdeviceptr, CUdeviceptr, size_t);
#define cuMemcpyDtoDAsync cuMemcpyDtoDAsync_v2
CUresult cuMemcpyDtoDAsync (CUdeviceptr, CUdeviceptr, size_t, CUstream);
#define cuMemcpyDtoH cuMemcpyDtoH_v2
CUresult cuMemcpyDtoH (void *, CUdeviceptr, size_t);
#define cuMemcpyDtoHAsync cuMemcpyDtoHAsync_v2
CUresult cuMemcpyDtoHAsync (void *, CUdeviceptr, size_t, CUstream);
#define cuMemcpyHtoD cuMemcpyHtoD_v2
CUresult cuMemcpyHtoD (CUdeviceptr, const void *, size_t);
#define cuMemcpyHtoDAsync cuMemcpyHtoDAsync_v2
CUresult cuMemcpyHtoDAsync (CUdeviceptr, const void *, size_t, CUstream);
#define cuMemFree cuMemFree_v2
CUresult cuMemFree (CUdeviceptr);
CUresult cuMemFreeHost (void *);
#define cuMemGetAddressRange cuMemGetAddressRange_v2
CUresult cuMemGetAddressRange (CUdeviceptr *, size_t *, CUdeviceptr);
#define cuMemHostGetDevicePointer cuMemHostGetDevicePointer_v2
CUresult cuMemHostGetDevicePointer (CUdeviceptr *, void *, unsigned);
CUresult cuModuleGetFunction (CUfunction *, CUmodule, const char *);
#define cuModuleGetGlobal cuModuleGetGlobal_v2
CUresult cuModuleGetGlobal (CUdeviceptr *, size_t *, CUmodule, const char *);
CUresult cuModuleLoad (CUmodule *, const char *);
CUresult cuModuleLoadData (CUmodule *, const void *);
CUresult cuModuleUnload (CUmodule);
CUresult cuOccupancyMaxPotentialBlockSize(int *, int *, CUfunction,
					  CUoccupancyB2DSize, size_t, int);
typedef void (*CUstreamCallback)(CUstream, CUresult, void *);
CUresult cuStreamAddCallback(CUstream, CUstreamCallback, void *, unsigned int);
CUresult cuStreamCreate (CUstream *, unsigned);
#define cuStreamDestroy cuStreamDestroy_v2
CUresult cuStreamDestroy (CUstream);
CUresult cuStreamQuery (CUstream);
CUresult cuStreamSynchronize (CUstream);
CUresult cuStreamWaitEvent (CUstream, CUevent, unsigned);

#endif /* GCC_CUDA_H */
