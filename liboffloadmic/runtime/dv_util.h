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


#ifndef DV_UTIL_H_INCLUDED
#define DV_UTIL_H_INCLUDED

#include <stdint.h>
#include "offload_util.h"

// Dope vector declarations
#define ArrDescMaxArrayRank         31

// Dope vector flags
#define ArrDescFlagsDefined         1
#define ArrDescFlagsNodealloc       2
#define ArrDescFlagsContiguous      4

typedef int64_t dv_size;

typedef struct DimDesc {
    dv_size        Extent;      // Number of elements in this dimension
    dv_size        Mult;        // Multiplier for this dimension.
                                // The number of bytes between successive
                                // elements in this dimension.
    dv_size        LowerBound;  // LowerBound of this dimension
} DimDesc ;

typedef struct ArrDesc {
    dv_size        Base;        // Base address
    dv_size        Len;         // Length of data type, used only for
                                // character strings.
    dv_size        Offset;
    dv_size        Flags;       // Flags
    dv_size        Rank;        // Rank of pointer
    dv_size        Reserved;    // reserved for openmp requests
    DimDesc Dim[ArrDescMaxArrayRank];
} ArrDesc ;

typedef ArrDesc* pArrDesc;

DLL_LOCAL bool __dv_is_contiguous(const ArrDesc *dvp);

DLL_LOCAL bool __dv_is_allocated(const ArrDesc *dvp);

DLL_LOCAL uint64_t __dv_data_length(const ArrDesc *dvp);

DLL_LOCAL uint64_t __dv_data_length(const ArrDesc *dvp, int64_t nelems);

DLL_LOCAL CeanReadRanges * init_read_ranges_dv(const ArrDesc *dvp);

#if OFFLOAD_DEBUG > 0
DLL_LOCAL void    __dv_desc_dump(const char *name, const ArrDesc *dvp);
#else // OFFLOAD_DEBUG
#define __dv_desc_dump(name, dvp)
#endif // OFFLOAD_DEBUG

#endif // DV_UTIL_H_INCLUDED
