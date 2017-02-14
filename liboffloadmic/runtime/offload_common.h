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


/*! \file
    \brief The parts of the runtime library common to host and target
*/

#ifndef OFFLOAD_COMMON_H_INCLUDED
#define OFFLOAD_COMMON_H_INCLUDED

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>

#include "offload.h"
#include "offload_table.h"
#include "offload_trace.h"
#include "offload_timer.h"
#include "offload_util.h"
#include "cean_util.h"
#include "dv_util.h"
#include "liboffload_error_codes.h"

#include <stdarg.h>

// Use secure getenv if it's supported
#ifdef HAVE_SECURE_GETENV
  #define getenv(x)	    secure_getenv(x)
#elif HAVE___SECURE_GETENV
  #define getenv(x)	    __secure_getenv(x)
#endif

// Offload Library versioning
DLL_LOCAL extern int offload_version;
DLL_LOCAL extern int offload_version_count;

// The debug routines

// Host console and file logging
DLL_LOCAL extern int console_enabled;
DLL_LOCAL extern int offload_report_level;


DLL_LOCAL extern const char *prefix;
DLL_LOCAL extern int offload_number;
#if !HOST_LIBRARY
DLL_LOCAL extern int mic_index;
#define OFFLOAD_DO_TRACE (offload_report_level == 3)
#else
#define OFFLOAD_DO_TRACE (offload_report_enabled && (offload_report_level == 3))
#endif

#if HOST_LIBRARY
DLL_LOCAL void Offload_Report_Prolog(OffloadHostTimerData* timer_data);
DLL_LOCAL void Offload_Report_Epilog(OffloadHostTimerData* timer_data);
DLL_LOCAL void offload_report_free_data(OffloadHostTimerData * timer_data);
DLL_LOCAL void Offload_Timer_Print(void);

#ifndef TARGET_WINNT
#define OFFLOAD_DEBUG_INCR_OFLD_NUM() \
        __sync_add_and_fetch(&offload_number, 1)
#else
#define OFFLOAD_DEBUG_INCR_OFLD_NUM() \
        _InterlockedIncrement(reinterpret_cast<long*>(&offload_number))
#endif

#define OFFLOAD_DEBUG_PRINT_TAG_PREFIX() \
        printf("%s:  ", prefix);

#define OFFLOAD_DEBUG_PRINT_PREFIX() \
        printf("%s:  ", prefix);
#else
#define OFFLOAD_DEBUG_PRINT_PREFIX() \
        printf("%s%d:  ", prefix, mic_index);
#endif // HOST_LIBRARY

#define OFFLOAD_TRACE(trace_level, ...)  \
    if (console_enabled >= trace_level) { \
        OFFLOAD_DEBUG_PRINT_PREFIX(); \
        printf(__VA_ARGS__); \
        fflush(NULL); \
    }

#if OFFLOAD_DEBUG > 0

#define OFFLOAD_DEBUG_TRACE(level, ...) \
    OFFLOAD_TRACE(level, __VA_ARGS__)

#define OFFLOAD_REPORT(level, offload_number, stage, ...) \
    if (OFFLOAD_DO_TRACE) { \
        offload_stage_print(stage, offload_number, __VA_ARGS__); \
        fflush(NULL); \
    }

#define OFFLOAD_DEBUG_TRACE_1(level, offload_number, stage, ...) \
    if (OFFLOAD_DO_TRACE) { \
        offload_stage_print(stage, offload_number, __VA_ARGS__); \
        fflush(NULL); \
    } \
    if (!OFFLOAD_DO_TRACE) { \
        OFFLOAD_TRACE(level, __VA_ARGS__) \
    }

#define OFFLOAD_DEBUG_DUMP_BYTES(level, a, b) \
    __dump_bytes(level, a, b)

DLL_LOCAL extern void __dump_bytes(
    int level,
    const void *data,
    int len
);

#else

#define OFFLOAD_DEBUG_LOG(level, ...)
#define OFFLOAD_DEBUG_DUMP_BYTES(level, a, b)

#endif

// Runtime interface

#define OFFLOAD_PREFIX(a) __offload_##a

#define OFFLOAD_MALLOC            OFFLOAD_PREFIX(malloc)
#define OFFLOAD_FREE(a)           _mm_free(a)

// Forward functions

extern void *OFFLOAD_MALLOC(size_t size, size_t align);

// The Marshaller

// Flags describing an offload

//! Flags describing an offload
union OffloadFlags{
    uint32_t flags;
    struct {
        uint32_t fortran_traceback : 1; //!< Fortran traceback requested
        uint32_t omp_async         : 1; //!< OpenMP asynchronous offload
    } bits;
};

//! \enum Indicator for the type of entry on an offload item list.
enum OffloadItemType {
    c_data =   1,       //!< Plain data
    c_data_ptr,         //!< Pointer data
    c_func_ptr,         //!< Function pointer
    c_void_ptr,         //!< void*
    c_string_ptr,       //!< C string
    c_dv,               //!< Dope vector variable
    c_dv_data,          //!< Dope-vector data
    c_dv_data_slice,    //!< Dope-vector data's slice
    c_dv_ptr,           //!< Dope-vector variable pointer
    c_dv_ptr_data,      //!< Dope-vector pointer data
    c_dv_ptr_data_slice,//!< Dope-vector pointer data's slice
    c_cean_var,         //!< CEAN variable
    c_cean_var_ptr,     //!< Pointer to CEAN variable
    c_data_ptr_array,   //!< Pointer to data pointer array
    c_extended_type,    //!< Is used to extend OffloadItemType
                        //!< Actual OffloadItemType is in the
                        //!< structure VarDescExtendedType
    c_func_ptr_array,   //!< Pointer to function pointer array
    c_void_ptr_array,   //!< Pointer to void* pointer array
    c_string_ptr_array, //!< Pointer to char* pointer array
    c_data_ptr_ptr,     //!< Pointer to pointer to data (struct member)
    c_func_ptr_ptr,     //!< Pointer to pointer to function (struct member)
    c_void_ptr_ptr,     //!< Pointer to pointer to void* (struct member)
    c_string_ptr_ptr,   //!< Pointer to pointer to string (struct member)
    c_cean_var_ptr_ptr  //!< Pointer to pointer to cean var (struct member)
};

#define TYPE_IS_PTR_TO_PTR(t) ((t) == c_string_ptr_ptr || \
                            (t) == c_data_ptr_ptr || \
                            (t) == c_func_ptr_ptr || \
                            (t) == c_void_ptr_ptr || \
                            (t) == c_cean_var_ptr_ptr)

#define VAR_TYPE_IS_PTR(t) ((t) == c_string_ptr || \
                            (t) == c_data_ptr || \
                            (t) == c_cean_var_ptr || \
                            (t) == c_dv_ptr || \
                            TYPE_IS_PTR_TO_PTR(t))

#define VAR_TYPE_IS_SCALAR(t) ((t) == c_data || \
                               (t) == c_void_ptr || \
                               (t) == c_cean_var || \
                               (t) == c_dv)

#define VAR_TYPE_IS_DV_DATA(t) ((t) == c_dv_data || \
                                (t) == c_dv_ptr_data)

#define VAR_TYPE_IS_DV_DATA_SLICE(t) ((t) == c_dv_data_slice || \
                                      (t) == c_dv_ptr_data_slice)

//! \enum Specify direction to copy offloaded variable.
enum OffloadParameterType {
    c_parameter_unknown = -1, //!< Unknown clause
    c_parameter_nocopy,       //!< Variable listed in "nocopy" clause
    c_parameter_in,           //!< Variable listed in "in" clause
    c_parameter_out,          //!< Variable listed in "out" clause
    c_parameter_inout         //!< Variable listed in "inout" clause
};


//! Flags describing an offloaded variable
union varDescFlags {
    struct {
        //! source variable has persistent storage
        uint32_t is_static : 1;
        //! destination variable has persistent storage
        uint32_t is_static_dstn : 1;
        //! has length for c_dv && c_dv_ptr
        uint32_t has_length : 1;
        //! persisted local scalar is in stack buffer
        uint32_t is_stack_buf : 1;
        //! "targetptr" modifier used
        uint32_t targetptr : 1;
        //! "preallocated" modifier used
        uint32_t preallocated : 1;
        //! pointer to a pointer array
        uint32_t is_pointer : 1;

        //! buffer address is sent in data
        uint32_t sink_addr : 1;
        //! alloc displacement is sent in data
        uint32_t alloc_disp : 1;
        //! source data is noncontiguous
        uint32_t is_noncont_src : 1;
        //! destination data is noncontiguous
        uint32_t is_noncont_dst : 1;

        //! "OpenMP always" modifier used
        uint32_t always_copy : 1;
        //! "OpenMP delete" modifier used
        uint32_t always_delete : 1;
        //! structured data is noncontiguous
        uint32_t is_non_cont_struct : 1;
        //! CPU memory pinning/unpinning operation
        uint32_t pin : 1;
        //! Pointer to device memory
        uint32_t is_device_ptr : 1;    
        //! Hostpointer with associated device pointer
        uint32_t use_device_ptr : 1;
    };
    uint32_t bits;
};

//! An Offload Variable descriptor
struct VarDesc {
    //! OffloadItemTypes of source and destination
    union {
        struct {
            uint8_t dst : 4; //!< OffloadItemType of destination
            uint8_t src : 4; //!< OffloadItemType of source
        };
        uint8_t bits;
    } type;

    //! OffloadParameterType that describes direction of data transfer
    union {
        struct {
            uint8_t in  : 1; //!< Set if IN or INOUT
            uint8_t out : 1; //!< Set if OUT or INOUT
        };
        uint8_t bits;
    } direction;

    uint8_t alloc_if;        //!< alloc_if modifier value
    uint8_t free_if;         //!< free_if modifier value
    uint32_t align;          //!< MIC alignment requested for pointer data
    //! Not used by compiler; set to 0
    /*! Used by runtime as offset to data from start of MIC buffer */
    uint32_t mic_offset;
    //! Flags describing this variable
    varDescFlags flags;
    //! Not used by compiler; set to 0
    /*! Used by runtime as offset to base from data stored in a buffer */
    int64_t offset;
    //! Element byte-size of data to be transferred
    /*! For dope-vector, the size of the dope-vector      */
    int64_t size;
    union {
        //! Set to 0 for array expressions and dope-vectors
        /*! Set to 1 for scalars                          */
        /*! Set to value of length modifier for pointers  */
        int64_t count;
        //! Displacement not used by compiler
        int64_t disp;
    };

    //! This field not used by OpenMP 4.0
    /*! The alloc section expression in #pragma offload   */
    union {
       void *alloc;
       int64_t ptr_arr_offset;
    };

    //! This field not used by OpenMP 4.0
    /*! The into section expression in #pragma offload    */
    /*! For c_data_ptr_array this is the into ptr array   */
    void *into;

    //! For an ordinary variable, address of the variable
    /*! For c_cean_var (C/C++ array expression),
        pointer to arr_desc, which is an array descriptor. */
    /*! For c_data_ptr_array (array of data pointers),
        pointer to ptr_array_descriptor,
        which is a descriptor for pointer array transfers. */
    void *ptr;
};

//! Auxiliary struct used when -g is enabled that holds variable names
struct VarDesc2 {
    const char *sname; //!< Source name
    const char *dname; //!< Destination name (when "into" is used)
};

/*! When the OffloadItemType is c_data_ptr_array
    the ptr field of the main descriptor points to this struct.          */
/*! The type in VarDesc1 merely says c_cean_data_ptr, but the pointer
    type can be c_data_ptr, c_func_ptr, c_void_ptr, or c_string_ptr.
    Therefore the actual pointer type is in the flags field of VarDesc3. */
/*! If flag_align_is_array/flag_alloc_if_is_array/flag_free_if_is_array
    is 0 then alignment/alloc_if/free_if are specified in VarDesc1.      */
/*! If flag_align_is_array/flag_alloc_if_is_array/flag_free_if_is_array
    is 1 then align_array/alloc_if_array/free_if_array specify
    the set of alignment/alloc_if/free_if values.                        */
/*! For the other fields, if neither the scalar nor the array flag
    is set, then that modifier was not specified. If the bits are set
    they specify which modifier was set and whether it was a
    scalar or an array expression.                                       */
struct VarDesc3
{
    void *ptr_array;        //!< Pointer to arr_desc of array of pointers
    void *align_array;      //!< Scalar value or pointer to arr_desc
    void *alloc_if_array;   //!< Scalar value or pointer to arr_desc
    void *free_if_array;    //!< Scalar value or pointer to arr_desc
    void *extent_start;     //!< Scalar value or pointer to arr_desc
    void *extent_elements;  //!< Scalar value or pointer to arr_desc
    void *into_start;       //!< Scalar value or pointer to arr_desc
    void *into_elements;    //!< Scalar value or pointer to arr_desc
    void *alloc_start;      //!< Scalar value or pointer to arr_desc
    void *alloc_elements;   //!< Scalar value or pointer to arr_desc
    /*! Flags that describe the pointer type and whether each field
        is a scalar value or an array expression.        */
    /*! First 6 bits are pointer array element type:
        c_data_ptr, c_func_ptr, c_void_ptr, c_string_ptr */
    /*! Then single bits specify:                        */
    /*!     align_array is an array                      */
    /*!     alloc_if_array is an array                   */
    /*!     free_if_array is an array                    */
    /*!     extent_start is a scalar expression          */
    /*!     extent_start is an array expression          */
    /*!     extent_elements is a scalar expression       */
    /*!     extent_elements is an array expression       */
    /*!     into_start is a scalar expression            */
    /*!     into_start is an array expression            */
    /*!     into_elements is a scalar expression         */
    /*!     into_elements is an array expression         */
    /*!     alloc_start is a scalar expression           */
    /*!     alloc_start is an array expression           */
    /*!     alloc_elements is a scalar expression        */
    /*!     alloc_elements is an array expression        */
    uint32_t array_fields;
};
const int flag_align_is_array = 6;
const int flag_alloc_if_is_array = 7;
const int flag_free_if_is_array = 8;
const int flag_extent_start_is_scalar = 9;
const int flag_extent_start_is_array = 10;
const int flag_extent_elements_is_scalar = 11;
const int flag_extent_elements_is_array = 12;
const int flag_into_start_is_scalar = 13;
const int flag_into_start_is_array = 14;
const int flag_into_elements_is_scalar = 15;
const int flag_into_elements_is_array = 16;
const int flag_alloc_start_is_scalar = 17;
const int flag_alloc_start_is_array = 18;
const int flag_alloc_elements_is_scalar = 19;
const int flag_alloc_elements_is_array = 20;

//! Extended Variable Descriptor.  Since VarDesc uses 16 bits for
//! OffloadItemType, we have exceeded that limit,  So any Type 
//! greater than 15 will have Type set in VarDesc as c_extended_type
//! and this structure will be used to represent those Types.
typedef struct VarDescExtendedType {

    // Represents overflow of OffloadItemType
    uint32_t extended_type; 

    //! For extended_type 
    //! address of the variable
    //! Future Types can point to other descriptors
    void *ptr;   
} VarDescExtendedType;

// The Marshaller
class Marshaller
{
private:
    // Start address of buffer
    char *buffer_start;

    // Current pointer within buffer
    char *buffer_ptr;

    // Physical size of data sent (including flags)
    long long buffer_size;

    // User data sent/received
    long long tfr_size;

public:
    // Constructor
    Marshaller() :
        buffer_start(0), buffer_ptr(0),
        buffer_size(0), tfr_size(0)
    {
    }

    // Return count of user data sent/received
    long long get_tfr_size() const
    {
        return tfr_size;
    }

    // Return pointer to buffer
    char *get_buffer_start() const
    {
        return buffer_start;
    }

    // Return current size of data in buffer
    long long get_buffer_size() const
    {
        return buffer_size;
    }

    // Set buffer pointer
    void init_buffer(
        char *d,
        long long s
    )
    {
        buffer_start = buffer_ptr = d;
        buffer_size = s;
    }

    // Send data
    void send_data(
        const void *data,
        int64_t length
    );

    // Receive data
    void receive_data(
        void *data,
        int64_t length
    );

    // Send function pointer
    void send_func_ptr(
        const void* data
    );

    // Receive function pointer
    void receive_func_ptr(
        const void** data
    );
};

// End of the Marshaller

// The offloaded function descriptor.
// Sent from host to target to specify which function to run.
// Also, sets console and file tracing levels.
struct FunctionDescriptor
{
    // Input data size.
    long long in_datalen;

    // Output data size.
    long long out_datalen;

    // Whether trace is requested on console.
    // A value of 1 produces only function name and data sent/received.
    // Values > 1 produce copious trace information.
    uint8_t console_enabled;

    // Flag controlling timing on the target side.
    // Values > 0 enable timing on sink.
    uint8_t timer_enabled;

    int offload_report_level;
    int offload_number;

    // number of variable descriptors
    int vars_num;

    // inout data offset if data is passed as misc/return data
    // otherwise it should be zero.
    int data_offset;

    // The name of the offloaded function
    char data[];
};

// typedef OFFLOAD.
// Pointer to OffloadDescriptor.
typedef struct OffloadDescriptor *OFFLOAD;

// Use for setting affinity of a stream
enum affinity_type {
    affinity_compact,
    affinity_scatter
};
struct affinity_spec {
    uint64_t sink_mask[16];
    int affinity_type;
    int num_cores;
    int num_threads;
};

#endif // OFFLOAD_COMMON_H_INCLUDED
