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


// The parts of the offload library used only on the target

#ifndef OFFLOAD_TARGET_H_INCLUDED
#define OFFLOAD_TARGET_H_INCLUDED

#include "offload_common.h"
#include "coi/coi_server.h"

// The offload descriptor.
class OffloadDescriptor
{
public:
    ~OffloadDescriptor() {
        if (m_vars != 0) {
            free(m_vars);
            free(m_vars_extra);
        }
    }

    // Entry point for COI. Synchronously execute offloaded region given
    // the provided buffers, misc and return data.
    static void offload(
        uint32_t  buffer_count,
        void**    buffers,
        void*     misc_data,
        uint16_t  misc_data_len,
        void*     return_data,
        uint16_t  return_data_len
    );

    // scatters input data from in buffer to target variables
    void scatter_copyin_data();

    // gathers output data to the buffer
    void gather_copyout_data();

    // merges local variable descriptors with the descriptors received from
    // host
    void merge_var_descs(VarDesc *vars, VarDesc2 *vars2, int vars_total);

    int get_offload_number() const {
        return m_offload_number;
    }

    void set_offload_number(int number) {
        m_offload_number = number;
    }

private:
    // Constructor
    OffloadDescriptor() : m_vars(0)
    {}

private:
    typedef std::list<void*> BufferList;

    // The Marshaller for the inputs of the offloaded region.
    Marshaller m_in;

    // The Marshaller for the outputs of the offloaded region.
    Marshaller m_out;

    // List of buffers that are passed to dispatch call
    BufferList m_buffers;

    // Variable descriptors received from host
    VarDesc* m_vars;
    int      m_vars_total;
    int      m_offload_number;

    // extra data associated with each variable descriptor
    struct VarExtra {
        uint16_t type_src;
        uint16_t type_dst;
    };

    VarExtra* m_vars_extra;
};

// one time target initialization in main
DLL_LOCAL extern void __offload_target_init(void);

// logical device index
DLL_LOCAL extern int mic_index;

// total number of available logical devices
DLL_LOCAL extern int mic_engines_total;

// device frequency (from COI)
DLL_LOCAL extern uint64_t mic_frequency;

struct RefInfo {
    RefInfo(bool is_add, long amount):is_added(is_add),count(amount)
    {}
    bool is_added;
    long count;
};

#endif // OFFLOAD_TARGET_H_INCLUDED
