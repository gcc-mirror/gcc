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


#ifndef CEAN_UTIL_H_INCLUDED
#define CEAN_UTIL_H_INCLUDED

#include <stdint.h>
#include "offload_util.h"

// CEAN expression representation
struct Dim_Desc {
    int64_t size;       // Length of data type
    int64_t lindex;     // Lower index
    int64_t lower;      // Lower section bound
    int64_t upper;      // Upper section bound
    int64_t stride;     // Stride
};

struct Arr_Desc {
    int64_t base;       // Base address
    int64_t rank;       // Rank of array
    Dim_Desc dim[1];
};

struct CeanReadDim {
    int64_t count; // The number of elements in this dimension
    int64_t size;  // The number of bytes between successive
                   // elements in this dimension.
};

struct CeanReadRanges {
    Arr_Desc* arr_desc;
    void *  ptr;
    int64_t current_number;   // the number of ranges read
    int64_t range_max_number; // number of contiguous ranges
    int64_t range_size;       // size of max contiguous range
    int     last_noncont_ind; // size of Dim array
    int64_t init_offset;      // offset of 1-st element from array left bound
    CeanReadDim Dim[1];
};

struct IntervalDesc {
    int64_t lower;   // Lower  index
    int64_t size;    // Size of each element at this interval
};

struct NonContigDesc {
    int64_t base;            // Base address
    int64_t interval_cnt;    // Number of intervals
    struct IntervalDesc interval[1];
};

// array descriptor length
#define __arr_desc_length(rank) \
    (sizeof(int64_t) + sizeof(Dim_Desc) * (rank))

// returns offset and length of the data to be transferred
DLL_LOCAL void __arr_data_offset_and_length(const Arr_Desc *adp,
                                  int64_t &offset,
                                  int64_t &length);

// define if data array described by argument is contiguous one
DLL_LOCAL bool is_arr_desc_contiguous(const Arr_Desc *ap);

// allocate element of CeanReadRanges type initialized
// to read consequently contiguous ranges described by "ap" argument
DLL_LOCAL CeanReadRanges * init_read_ranges_arr_desc(const Arr_Desc *ap);

// check if ranges described by 1 argument could be transferred into ranges
// described by 2-nd one
DLL_LOCAL bool cean_ranges_match(
    CeanReadRanges * read_rng1,
    CeanReadRanges * read_rng2
);

// first argument - returned value by call to init_read_ranges_arr_desc.
// returns true if offset and length of next range is set successfuly.
// returns false if the ranges is over.
DLL_LOCAL bool get_next_range(
    CeanReadRanges * read_rng,
    int64_t *offset
);

// returns number of transferred bytes
DLL_LOCAL int64_t cean_get_transf_size(CeanReadRanges * read_rng);

#if OFFLOAD_DEBUG > 0
// prints array descriptor contents to stderr
DLL_LOCAL void    __arr_desc_dump(
    const char *spaces,
    const char *name,
    const Arr_Desc *adp,
    bool dereference,
    bool print_values);

DLL_LOCAL void noncont_struct_dump(
    const char *spaces,
    const char *name,
    struct NonContigDesc *desc_p);

DLL_LOCAL int64_t get_noncont_struct_size(struct NonContigDesc *desc_p);

#define ARRAY_DESC_DUMP(spaces, name, adp, dereference, print_values) \
    if (console_enabled >= 2) \
        __arr_desc_dump(spaces, name, adp, dereference, print_values);
#else
#define ARRAY_DESC_DUMP(spaces, name, adp, dereference, print_values)
#endif // OFFLOAD_DEBUG

#endif // CEAN_UTIL_H_INCLUDED
