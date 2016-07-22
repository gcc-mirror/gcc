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


#include "cean_util.h"
#include "offload_common.h"

// 1. allocate element of CeanReadRanges type
// 2. initialized it for reading consequently contiguous ranges
//    described by "ap" argument
CeanReadRanges * init_read_ranges_arr_desc(const Arr_Desc *ap)
{
    CeanReadRanges * res;

    // find the max contiguous range
    int64_t rank = ap->rank - 1;
    int64_t length = ap->dim[rank].size;
    for (; rank >= 0; rank--) {
        if (ap->dim[rank].stride == 1) {
            length *= (ap->dim[rank].upper - ap->dim[rank].lower + 1);
            if (rank > 0 && length != ap->dim[rank - 1].size) {
                break;
            }
        }
        else {
            break;
        }
    }

    res =(CeanReadRanges *)malloc(sizeof(CeanReadRanges) +
                                  (ap->rank - rank) * sizeof(CeanReadDim));
    if (res == NULL)
      LIBOFFLOAD_ERROR(c_malloc);

    res->arr_desc = const_cast<Arr_Desc*>(ap);
    res->current_number = 0;
    res->range_size = length;
    res->last_noncont_ind = rank;

    // calculate number of contiguous ranges inside noncontiguous dimensions
    int count = 1;
    bool prev_is_cont = true;
    int64_t offset = 0;

    for (; rank >= 0; rank--) {
        res->Dim[rank].count = count;
        res->Dim[rank].size = ap->dim[rank].stride * ap->dim[rank].size;
        count *= (prev_is_cont && ap->dim[rank].stride == 1? 1 :
            (ap->dim[rank].upper - ap->dim[rank].lower +
            ap->dim[rank].stride) / ap->dim[rank].stride);
        prev_is_cont = false;
        offset +=(ap->dim[rank].lower - ap->dim[rank].lindex) *
                 ap->dim[rank].size;
    }
    res->range_max_number = count;
    res -> ptr = (void*)ap->base;
    res -> init_offset = offset;
    return res;
}

// check if ranges described by 1 argument could be transferred into ranges
// described by 2-nd one
bool cean_ranges_match(
    CeanReadRanges * read_rng1,
    CeanReadRanges * read_rng2
)
{
    return ( read_rng1 == NULL || read_rng2 == NULL ||
            (read_rng1->range_size % read_rng2->range_size == 0 ||
            read_rng2->range_size % read_rng1->range_size == 0));
}

// Set next offset and length and returns true for next range.
// Returns false if the ranges are over.
bool get_next_range(
    CeanReadRanges * read_rng,
    int64_t *offset
)
{
    if (++read_rng->current_number > read_rng->range_max_number) {
        read_rng->current_number = 0;
        return false;
    }
    int rank = 0;
    int num = read_rng->current_number - 1;
    int64_t cur_offset = 0;
    int num_loc;
    for (; rank <= read_rng->last_noncont_ind; rank++) {
        num_loc = num / read_rng->Dim[rank].count;
        cur_offset += num_loc * read_rng->Dim[rank].size;
        num = num % read_rng->Dim[rank].count;
    }
    *offset = cur_offset + read_rng->init_offset;
    return true;
}

bool is_arr_desc_contiguous(const Arr_Desc *ap)
{
    int64_t rank = ap->rank - 1;
    int64_t length = ap->dim[rank].size;
    for (; rank >= 0; rank--) {
        if (ap->dim[rank].stride > 1 &&
            ap->dim[rank].upper - ap->dim[rank].lower != 0) {
                return false;
        }
        else if (length != ap->dim[rank].size) {
            for (; rank >= 0; rank--) {
                if (ap->dim[rank].upper - ap->dim[rank].lower != 0) {
                    return false;
                }
            }
            return true;
        }
        length *= (ap->dim[rank].upper - ap->dim[rank].lower + 1);
    }
    return true;
}

int64_t cean_get_transf_size(CeanReadRanges * read_rng)
{
    return(read_rng->range_max_number * read_rng->range_size);
}

static uint64_t last_left, last_right;

typedef void (*fpp)(
    const char *spaces,
    uint64_t low,
    uint64_t high,
    int esize,
    bool print_values
);

static void generate_one_range(
    const char *spaces,
    uint64_t lrange,
    uint64_t rrange,
    fpp fp,
    int esize,
    bool print_values
)
{
    OFFLOAD_TRACE(3,
        "%s    generate_one_range(lrange=%p, rrange=%p, esize=%d)\n",
        spaces, (void*)lrange, (void*)rrange, esize);
    if (last_left == -1) {
        // First range
        last_left = lrange;
    }
    else {
        if (lrange == last_right+1) {
            // Extend previous range, don't print
        }
        else {
            (*fp)(spaces, last_left, last_right, esize, print_values);
            last_left = lrange;
        }
    }
    last_right = rrange;
}

static bool element_is_contiguous(
    uint64_t rank,
    const struct Dim_Desc *ddp
)
{    
    if (rank == 1) {
        return (ddp[0].lower == ddp[0].upper || ddp[0].stride == 1);
    }
    else {
        return ((ddp[0].size == (ddp[1].upper-ddp[1].lower+1)*ddp[1].size) &&
                 element_is_contiguous(rank-1, ddp++));
    }
}

static void generate_mem_ranges_one_rank(
    const char *spaces,
    uint64_t base,
    uint64_t rank,
    const struct Dim_Desc *ddp,
    fpp fp,
    int esize,
    bool print_values
)
{
    uint64_t lindex = ddp->lindex;
    uint64_t lower = ddp->lower;
    uint64_t upper = ddp->upper;
    uint64_t stride = ddp->stride;
    uint64_t size = ddp->size;
    OFFLOAD_TRACE(3,
        "%s    "
        "generate_mem_ranges_one_rank(base=%p, rank=%lld, lindex=%lld, "
        "lower=%lld, upper=%lld, stride=%lld, size=%lld, esize=%d)\n",
        spaces, (void*)base, rank, lindex, lower, upper, stride, size, esize);

    if (element_is_contiguous(rank, ddp)) {
        uint64_t lrange, rrange;
        lrange = base + (lower-lindex)*size;
        rrange = lrange + (upper-lower+1)*size - 1;
        generate_one_range(spaces, lrange, rrange, fp, esize, print_values);
    }
    else {
        if (rank == 1) {
            for (int i=lower-lindex; i<=upper-lindex; i+=stride) {
                uint64_t lrange, rrange;
                lrange = base + i*size;
                rrange = lrange + size - 1;
                generate_one_range(spaces, lrange, rrange,
		                   fp, esize, print_values);
            }
        }
        else {
            for (int i=lower-lindex; i<=upper-lindex; i+=stride) {
                generate_mem_ranges_one_rank(
                    spaces, base+i*size, rank-1, ddp+1,
                    fp, esize, print_values);

            }
        }
    }
}

static void generate_mem_ranges(
    const char *spaces,
    const Arr_Desc *adp,
    bool deref,
    fpp fp,
    bool print_values
)
{
    uint64_t esize;

    OFFLOAD_TRACE(3,
        "%s    "
        "generate_mem_ranges(adp=%p, deref=%d, fp)\n",
        spaces, adp, deref);
    last_left = -1;
    last_right = -2;

    // Element size is derived from last dimension
    esize = adp->dim[adp->rank-1].size;

    generate_mem_ranges_one_rank(
        // For c_cean_var the base addr is the address of the data
        // For c_cean_var_ptr the base addr is dereferenced to get to the data
        spaces, deref ? *((uint64_t*)(adp->base)) : adp->base,
        adp->rank, &adp->dim[0], fp, esize, print_values);
    (*fp)(spaces, last_left, last_right, esize, print_values);
}

// returns offset and length of the data to be transferred
void __arr_data_offset_and_length(
    const Arr_Desc *adp,
    int64_t &offset,
    int64_t &length
)
{
    int64_t rank = adp->rank - 1;
    int64_t size = adp->dim[rank].size;
    int64_t r_off = 0; // offset from right boundary

    // find the rightmost dimension which takes just part of its
    // range. We define it if the size of left rank is not equal
    // the range's length between upper and lower boungaries
    while (rank > 0) {
        size *= (adp->dim[rank].upper - adp->dim[rank].lower + 1);
        if (size != adp->dim[rank - 1].size) {
            break;
        }
        rank--;
    }

    offset = (adp->dim[rank].lower - adp->dim[rank].lindex) *
             adp->dim[rank].size;

    // find gaps both from the left - offset and from the right - r_off
    for (rank--; rank >= 0; rank--) {
        offset += (adp->dim[rank].lower - adp->dim[rank].lindex) *
                  adp->dim[rank].size;
        r_off += adp->dim[rank].size -
                 (adp->dim[rank + 1].upper - adp->dim[rank + 1].lindex + 1) *
                 adp->dim[rank + 1].size;
    }
    length = (adp->dim[0].upper - adp->dim[0].lindex + 1) *
             adp->dim[0].size - offset - r_off;
}

#if OFFLOAD_DEBUG > 0

static void print_range(
    const char *spaces,
    uint64_t low,
    uint64_t high,
    int esize,
    bool print_values
)
{
    char buffer[1024];
    char number[32];

    OFFLOAD_TRACE(3, "%s        print_range(low=%p, high=%p, esize=%d)\n",
        spaces, (void*)low, (void*)high, esize);

    if (console_enabled < 4 || !print_values) {
        return;
    }
    OFFLOAD_TRACE(4, "%s            values:\n", spaces);
    int count = 0;
    buffer[0] = '\0';
    while (low <= high)
    {
        switch (esize)
        {
        case 1:
            sprintf(number, "%d ", *((char *)low));
            low += 1;
            break;
        case 2:
            sprintf(number, "%d ", *((short *)low));
            low += 2;
            break;
        case 4:
            sprintf(number, "%d ", *((int *)low));
            low += 4;
            break;
        default:
            sprintf(number, "0x%016x ", *((uint64_t *)low));
            low += 8;
            break;
        }
        strcat(buffer, number);
        count++;
        if (count == 10) {
            OFFLOAD_TRACE(4, "%s            %s\n", spaces, buffer);
            count = 0;
            buffer[0] = '\0';
        }
    }
    if (count != 0) {
        OFFLOAD_TRACE(4, "%s            %s\n", spaces, buffer);
    }
}

void __arr_desc_dump(
    const char *spaces,
    const char *name,
    const Arr_Desc *adp,
    bool deref,
    bool print_values
)
{
    OFFLOAD_TRACE(2, "%s%s CEAN expression %p\n", spaces, name, adp);

    if (adp != 0) {
        OFFLOAD_TRACE(2, "%s    base=%llx, rank=%lld\n",
            spaces, adp->base, adp->rank);

        for (int i = 0; i < adp->rank; i++) {
            OFFLOAD_TRACE(2,
                          "%s    dimension %d: size=%lld, lindex=%lld, "
                          "lower=%lld, upper=%lld, stride=%lld\n",
                          spaces, i, adp->dim[i].size, adp->dim[i].lindex,
                          adp->dim[i].lower, adp->dim[i].upper,
                          adp->dim[i].stride);
        }
        // For c_cean_var the base addr is the address of the data
        // For c_cean_var_ptr the base addr is dereferenced to get to the data
        generate_mem_ranges(spaces, adp, deref, &print_range, print_values);
    }
}

void noncont_struct_dump(
    const char *spaces,
    const char *name,
    struct NonContigDesc *desc_p)
{
   OFFLOAD_TRACE(2, "%s%s NonCont Struct expression %p\n",
                 spaces, name, desc_p->base);
   if (desc_p) {
       OFFLOAD_TRACE(2, "%s%s    base=%p\n", spaces, name, desc_p->base);
       for (int i = 0; i < desc_p->interval_cnt; i++) {
           OFFLOAD_TRACE(2,"%s    dimension %d: lower=%lld, size=%lld\n",
               spaces, i, desc_p->interval[i].lower, desc_p->interval[i].size);
       }
   }
}

int64_t get_noncont_struct_size(struct NonContigDesc *desc_p)
{
    int index = desc_p->interval_cnt - 1;
    return(desc_p->interval[index].lower + desc_p->interval[index].size);
}

#endif // OFFLOAD_DEBUG
