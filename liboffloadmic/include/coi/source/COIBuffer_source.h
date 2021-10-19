/*
 * Copyright 2010-2016 Intel Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, version 2.1.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * Disclaimer: The codes contained in these modules may be specific
 * to the Intel Software Development Platform codenamed Knights Ferry,
 * and the Intel product codenamed Knights Corner, and are not backward
 * compatible with other Intel products. Additionally, Intel will NOT
 * support the codes or instruction set in future products.
 *
 * Intel offers no warranty of any kind regarding the code. This code is
 * licensed on an "AS IS" basis and Intel is not obligated to provide
 * any support, assistance, installation, training, or other services
 * of any kind. Intel is also not obligated to provide any updates,
 * enhancements or extensions. Intel specifically disclaims any warranty
 * of merchantability, non-infringement, fitness for any particular
 * purpose, and any other warranty.
 *
 * Further, Intel disclaims all liability of any kind, including but
 * not limited to liability for infringement of any proprietary rights,
 * relating to the use of the code, even if Intel is notified of the
 * possibility of such liability. Except as expressly stated in an Intel
 * license agreement provided with this code and agreed upon with Intel,
 * no license, express or implied, by estoppel or otherwise, to any
 * intellectual property rights is granted herein.
 */

#ifndef _COIBUFFER_SOURCE_H
#define _COIBUFFER_SOURCE_H

/** @ingroup COIBuffer
 *  @addtogroup COIBufferSource
@{

* @file source\COIBuffer_source.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    #include "../common/COITypes_common.h"
    #include "../common/COIResult_common.h"
#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifdef __cplusplus
extern "C" {
#endif


///////////////////////////////////////////////////////////////////////////////
/// The valid buffer types that may be created using COIBufferCreate.
/// Please see the COI_VALID_BUFFER_TYPES_AND_FLAGS matrix
/// below which describes the valid combinations of buffer types and flags.
///
typedef enum COI_BUFFER_TYPE
{
    /// Normal buffers exist as a single physical buffer in either Source or
    /// Sink physical memory. Mapping the buffer may stall the pipelines.
    COI_BUFFER_NORMAL = 1,

    // Reserved values, not used by COI any more
    COI_BUFFER_RESERVED_1,
    COI_BUFFER_RESERVED_2,
    COI_BUFFER_RESERVED_3,

    /// OpenCL buffers are similar to Normal buffers except they don't
    /// stall pipelines and don't follow any read write dependencies.
    COI_BUFFER_OPENCL

} COI_BUFFER_TYPE;


/// @name COIBUFFER creation flags.
/// Please see the COI_VALID_BUFFER_TYPES_AND_FLAGS matrix
/// below which describes the valid combinations of buffer types and flags.
//@{

/// Create the buffer such that it has the same virtual address on all of the
/// sink processes with which it is associated.
#define COI_SAME_ADDRESS_SINKS             0x00000001

/// Create the buffer such that it has the same virtual address on all of the
/// sink processes with which it is associated and in the source process.
#define COI_SAME_ADDRESS_SINKS_AND_SOURCE  0x00000002

/// Hint to the runtime that the source will frequently read the buffer
#define COI_OPTIMIZE_SOURCE_READ           0x00000004

/// Hint to the runtime that the source will frequently write the buffer
#define COI_OPTIMIZE_SOURCE_WRITE          0x00000008

/// Hint to the runtime that the sink will frequently read the buffer
#define COI_OPTIMIZE_SINK_READ             0x00000010

/// Hint to the runtime that the sink will frequently write the buffer
#define COI_OPTIMIZE_SINK_WRITE            0x00000020

/// Used to delay the pinning of memory into physical pages, until required
/// for DMA. This can be used to delay the cost of time spent pinning memory
/// until absolutely necessary. Might speed up the execution of COIBufferCreate
/// calls, but slow down the first access of the buffer in
/// COIPipelineRunFunction(s) or other COIBuffer access API's.
/// Also of important note, that with this flag enabled COI will not be able to
/// check to see if this memory is read only. Ordinarily this is checked
/// and an error is thrown upon buffer creation. With this flag, the error
/// might occur later, and cause undetermined behavior. Be sure to always
/// use writable memory for COIBuffers.
#define COI_OPTIMIZE_NO_DMA                0x00000040

/// Hint to the runtime to try to use huge page sizes for backing store on the
/// sink. Is currently not compatible with the SAME_ADDRESS
/// flags or the SINK_MEMORY flag. It is important to note that this is a hint
/// and internally the runtime may not actually promote to huge pages.
/// Specifically if the buffer is too small (less than 4KiB for example) then
/// the runtime will not promote the buffer to use huge pages.
#define COI_OPTIMIZE_HUGE_PAGE_SIZE        0x00000080

/// Used to tell Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// to create a buffer using memory that has already been
/// allocated on the sink. This flag is only valid when passed in to the
/// COIBufferCreateFromMemory API.
#define COI_SINK_MEMORY                    0x00000100

//@}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// Make the flag mask
#ifdef F
#undef F
#endif
#define F 0
#ifdef T
#undef T
#endif
#define T 1
#define MTM(_BUFFER, B1, B2, B3, B4, B5, B6, B7, B8, B9) \
    (B1 | B2<<1 | B3<<2 | B4<<3 | B5<<4 | B6<<5 | B7<<6 | B8<<7 | B9<<8)
#endif

/// \enum COI_BUFFER_TYPE
/// This matrix shows the valid combinations of buffer types and buffer flags
/// that may be passed in to COIBufferCreate and COIBufferCreateFromMemory.
/// \code
static const uint64_t
COI_VALID_BUFFER_TYPES_AND_FLAGS[COI_BUFFER_OPENCL + 1] =
{
    /*           |       | SAME |      |       |      |       |     |      |      |
                 | SAME  | ADDR | OPT  | OPT   | OPT  | OPT   | OPT | HUGE | COI  |
                 | ADDR  | SINK | SRC  | SRC   | SINK | SINK  | NO  | PAGE | SINK |
                 | SINKS | SRC  | READ | WRITE | READ | WRITE | DMA | SIZE | MEM  |
                 +-------+------+------+-------+------+-------+-----+------+-----*/
    MTM(INVALID   ,   F   ,   F  ,   F  ,   F   ,   F  ,   F   ,  F  ,   F  ,  F),
    MTM(NORMAL    ,   T   ,   T  ,   T  ,   T   ,   T  ,   T   ,  T  ,   T  ,  T),
    MTM(RESERVED1 ,   F   ,   F  ,   F  ,   F   ,   F  ,   F   ,  F  ,   F  ,  F),
    MTM(RESERVED2 ,   F   ,   F  ,   F  ,   F   ,   F  ,   F   ,  F  ,   F  ,  F),
    MTM(RESERVED3 ,   F   ,   F  ,   F  ,   F   ,   F  ,   F   ,  F  ,   F  ,  F),
    MTM(OPENCL    ,   T   ,   T  ,   T  ,   T   ,   T  ,   T   ,  T  ,   T  ,  F),
};
///\endcode
#undef MTM

//////////////////////////////////////////////////////////////////////////////
/// These flags control how the buffer will be accessed on the source after
/// it is mapped.
/// Please see the COI_VALID_BUFFER_TYPES_AND_MAP matrix below for the
/// valid buffer type and map operation combinations.
typedef enum COI_MAP_TYPE
{
    /// Allows the application to read and write the contents of the buffer
    /// after it is mapped.
    COI_MAP_READ_WRITE = 1,

    /// If this flag is set then the application must only read from the
    /// buffer after it is mapped. If the application writes to the buffer
    /// the contents will not be reflected back to the sink or stored for
    /// the next time the buffer is mapped on the source.
    /// This allows the runtime to make significant performance optimizations
    /// in buffer handling.
    COI_MAP_READ_ONLY,

    /// Setting this flag means that the source will overwrite the entire
    /// buffer once it is mapped. The app must not read from the buffer and
    /// must not expect the contents of the buffer to be synchronized from
    /// the sink side during the map operation.
    /// This allows the runtime to make significant performance optimizations
    /// in buffer handling.
    COI_MAP_WRITE_ENTIRE_BUFFER
} COI_MAP_TYPE;

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// Make the flag mask
#define MMM(_BUFFER, B1, B2, B3) \
    {  F  , B1, B2, B3}
#endif
/// \enum COI_MAP_TYPE
/// This matrix shows the valid combinations of buffer types and map
/// operations that may be passed in to COIBufferMap.
/// \code
static const uint64_t
COI_VALID_BUFFER_TYPES_AND_MAP
[COI_BUFFER_OPENCL + 1][COI_MAP_WRITE_ENTIRE_BUFFER + 1] =
{
    /*                      | MAP   | MAP   | MAP   |
                            | READ  | READ  | WRITE |
                            | WRITE | ONLY  | ENTIRE|
                            +-------+-------+-------+*/
    MMM(INVALID             ,   F   ,   F   ,   F),
    MMM(NORMAL              ,   T   ,   T   ,   T),
    MMM(RESERVED1           ,   F   ,   F   ,   F),
    MMM(RESERVED2           ,   F   ,   F   ,   F),
    MMM(RESERVED3           ,   F   ,   F   ,   F),
    MMM(OPENCL              ,   T   ,   T   ,   T),
};
///\endcode
#undef MMM
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#undef F
#undef T
#endif

//////////////////////////////////////////////////////////////////////////////
/// The valid copy operation types for the COIBufferWrite, COIBufferRead,
/// and COIBufferCopy APIs.
///
typedef enum COI_COPY_TYPE
{
    /// The runtime can pick the best suitable way to copy the data.
    COI_COPY_UNSPECIFIED = 0,

    /// The runtime should use DMA to copy the data.
    COI_COPY_USE_DMA,

    /// The runtime should use a CPU copy to copy the data.
    COI_COPY_USE_CPU,

    /// Same as above, but forces moving entire buffer to target process in Ex
    /// extended APIs, even if the full buffer is not written.
    COI_COPY_UNSPECIFIED_MOVE_ENTIRE,

    /// Same as above, but forces moving entire buffer to target process in Ex
    /// extended APIs, even if the full buffer is not written.
    COI_COPY_USE_DMA_MOVE_ENTIRE,

    /// Same as above, but forces moving entire buffer to target process in Ex
    /// extended APIs, even if the full buffer is not written.
    COI_COPY_USE_CPU_MOVE_ENTIRE

} COI_COPY_TYPE;


//////////////////////////////////////////////////////////////////////////////
/// The buffer states are used to indicate whether a buffer is available for
/// access in a COIPROCESS. This is used with COIBufferSetState.
///
/// Rules on State Transition of the buffer:
/// -. When a Buffer is created by default it is valid only on the source,
///    except for buffers created with COI_SINK_MEMORY flag which are valid
///    only on the sink where the memory lies when created.
/// -. Apart from SetState following APIs also alters the state of the buffer
///    internally:
///
///    - COIBufferMap alters state of buffer depending on the COI_MAP_TYPE.
///      COI_MAP_READ_ONLY: Makes Valid on the Source. Doesn't affect the state
///                         of the buffer on the other devices.
///      COI_MAP_READ_WRITE: Makes it Valid only the Source and Invalid
///                         everywhere else. OPENCL buffers are invalidated
///                         only if it is not in use.
///      COI_MAP_WRITE_ENTIRE_BUFFER: Makes it valid only on the Source. OPENCL
///                         buffers are invalidated only if not in use.
///
///    - COIPipelineRunfunction alters the state of the buffer depending on the
///      COI_ACCESS_FLAGS
///      COI_SINK_READ: Makes it valid on the sink where RunFunction is being
///                     issued. Doesn't affect the state of the buffer on other
///                     devices.
///      COI_SINK_WRITE: Makes it valid only on the sink where Runfunction is
///                     being issued and invalid everywhere else. OPENCL
///                     buffers are invalidated only if the buffer is not in
///                     use.
///      COI_SINK_WRITE_ENTIRE: Makes it valid only on the sink where
///                     Runfunction is being issued and invalid everywhere else
///                     OPENCL buffers are invalidated only if the buffer is
///                     not in use.
///
///    - COIBufferWrite makes the buffer exclusively valid where the write
///      happens. Write gives preference to Source over Sink. In other words
///      if a buffer is valid on the Source and multiple Sinks, Write will
///      happen on the Source and will Invalidate all other Sinks. If the
///      buffer is valid on multiple Sinks ( and not on the Source) then
///      Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
///      selects process handle with the lowest numerical value to do the
///      exclusive write Again, OPENCL buffers are invalidated only if the
///      buffer is not in use on that SINK/SOURCE.
///
///      The preference rule mentioned above holds true even for SetState API,
///      when data needs to be moved from a valid location. The selection of
///      valid location happens as stated above.
///
/// - It is possible to alter only parts of the buffer and change it state
///   In other words it is possible for different parts of the buffer to have
///   different states on different devices. A byte is the minimum size at
///   which state can be maintained internally. Granularity level is completely
///   determined by how the buffer gets fragmented.
///
/// Note: Buffer is considered 'in use' if is
///         - Being used in RunFunction : In use on a Sink
///         - Mapped: In use on a Source
///         - AddRef'd: In use on Sink
///

//////////////////////////////////////////////////////////////////////////////
/// The buffer states used with COIBufferSetState call to indicate the new
/// state of the buffer on a given process
///
typedef enum
{
    COI_BUFFER_VALID = 0,      // Buffer is valid and up-to-date on the process
    COI_BUFFER_INVALID ,       // Buffer is not valid, need valid data
    COI_BUFFER_VALID_MAY_DROP, // Same as valid but will drop the content when
    // evicted to avoid overwriting the shadow
    // memory
    COI_BUFFER_RESERVED        // Reserved for internal use
} COI_BUFFER_STATE;
///
/// Note: A VALID_MAY_DROP declares a buffer's copy as secondary on a given
/// process. This means that there needs to be at least one primary copy of the
/// the buffer somewhere in order to mark the buffer as VALID_MAY_DROP on a
/// process. In other words to make a buffer VALID_MAY_DROP on a given process
/// it needs to be in COI_BUFFER_VALID state somewhere else. The operation gets
/// ignored (or is a nop) if there is no primary copy of the buffer. The nature
/// of this state to "drop the content" when evicted is a side effect of
/// marking the buffer as secondary copy. So when a buffer marked
/// VALID_MAY_DROP is evicted Intel(R) Coprocessor Offload Infrastructure
/// (Intel(R) COI) doesn't back it up as it is assumed that
/// there is a primary copy somewhere.

//////////////////////////////////////////////////////////////////////////////
/// The buffer move flags are used to indicate when a buffer should be moved
/// when it's state is changed. This is used with COIBufferSetState.
typedef enum
{
    COI_BUFFER_MOVE = 0,// Dirty data is moved if state change requires it
    COI_BUFFER_NO_MOVE  // Change state without moving data
} COI_BUFFER_MOVE_FLAG;

// A process handle for COIBufferSetState call to indicate all the sink
// processes where the given buffer is valid
#define COI_SINK_OWNERS ((COIPROCESS)-2)

// Matrix descriptors used with MultiD Read/Write
typedef struct dim_desc
{
    int64_t size;       // Size of data type
    int64_t lindex;     // Lower index, used in Fortran
    int64_t lower;      // Lower section bound
    int64_t upper;      // Upper section bound
    int64_t stride;     // Stride, or number of bytes between the start
    // of one element and start of next one divided
    // by size.
} dim_desc;

typedef struct arr_desc
{
    int64_t base;       // Base address
    int64_t rank;       // Rank of array, i.e. number of dimensions
    dim_desc dim[3];    // This array has as many elements as 'rank'
    // currently limited to 3.
} arr_desc;

//////////////////////////////////////////////////////////////////////////////
///
/// Creates a buffer that can be used in RunFunctions that are queued in
/// pipelines. The address space for the buffer is reserved when it is
/// created although the memory may not be committed until the buffer is
/// used for the first time. Please note that the Intel(R) Coprocessor Offload
/// Infrastructure (Intel(R) COI) runtime may also allocate space for the
/// source process to use as shadow memory for certain types of buffers.
/// If Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// does allocate this memory it will not be released or reallocated
/// until the COIBuffer is destroyed.
///
/// @param  in_Size
///         [in] The number of bytes to allocate for the buffer. If in_Size
///         is not page aligned, it will be rounded up.
///
/// @param  in_Type
///         [in] The type of the buffer to create.
///
/// @param  in_Flags
///         [in] A bitmask of attributes for the newly created buffer.
///         Some of these flags are required for correctness while others
///         are provided as hints to the runtime system so it can make
///         certain performance optimizations.
///
/// @param  in_pInitData
///         [in] If non-NULL the buffer will be initialized with the data
///         pointed to by pInitData. The memory at in_pInitData must hold
///         at least in_Size bytes.
///
/// @param  in_NumProcesses
///         [in] The number of processes with which this buffer might be used.
///
/// @param  in_pProcesses
///         [in] An array of COIPROCESS handles identifying the processes with
///         which this buffer might be used.
///
/// @param  out_pBuffer
///         [out] Pointer to a buffer handle. The handle will be filled in
///         with a value that uniquely identifies the newly created buffer.
///         This handle should be disposed of via COIBufferDestroy()
///         once it is no longer needed.
///
/// @return COI_SUCCESS if the buffer was created
///
/// @return COI_ARGUMENT_MISMATCH if the in_Type and in_Flags parameters
///         are not compatible with one another. Please see the
///         COI_VALID_BUFFER_TYPES_AND_FLAGS map above for information about
///         which flags and types are compatible.
///
/// @return COI_OUT_OF_RANGE if in_Size is zero, if the bits set in
///         the in_Flags parameter are not recognized flags, or if in_NumProcesses is zero.
///
/// @return COI_INVALID_POINTER if the in_pProcesses or out_pBuffer parameter
///         is NULL.
///
/// @return COI_NOT_SUPPORTED if in_Type has invalid value or if
///        one of the in_Flags is COI_SINK_MEMORY.
///
/// @return COI_NOT_SUPPORTED if the flags include either
///         COI_SAME_ADDRESS_SINKS or COI_SAME_ADDRESS_SINKS_AND_SOURCE and
///         COI_OPTIMIZE_HUGE_PAGE_SIZE.
///
/// @return COI_INVALID_HANDLE if one of the COIPROCESS handles in the
///         in_pProcesses array does not identify a valid process.
///
/// @return COI_OUT_OF_MEMORY if allocating the buffer fails.
///
/// @return COI_RESOURCE_EXHAUSTED if the sink is out of buffer memory.
///
COIACCESSAPI
COIRESULT
COIBufferCreate(
    uint64_t            in_Size,
    COI_BUFFER_TYPE     in_Type,
    uint32_t            in_Flags,
    const   void               *in_pInitData,
    uint32_t            in_NumProcesses,
    const   COIPROCESS         *in_pProcesses,
    COIBUFFER          *out_pBuffer);

//////////////////////////////////////////////////////////////////////////////
///
/// Creates a buffer from some existing memory that can be used in
/// RunFunctions that are queued in pipelines. If the flag COI_SINK_MEMORY
/// is specified then Intel(R) Coprocessor Offload
/// Infrastructure (Intel(R) COI) will use that memory for the buffer on the sink.
/// If that flag isn't set then the memory provided is used as backing store
/// for the buffer on the source. In either case the memory must not be freed
/// before the buffer is destroyed.
/// While the user still owns the memory passed in they must use the
/// appropriate access flags when accessing the buffer in COIPipelinRunFunction
/// or COIBufferMap calls so that the runtime knows when the
/// memory has been modified. If the user just writes directly to the memory
/// location then those changes may not be visible when the corresponding
/// buffer is accessed.
/// Whatever values are already present in the memory location when this call
/// is made are preserved. The memory values are also preserved when
/// COIBufferDestroy is called.
///
/// @warning: Use of this function is highly discouraged if the calling
/// program forks at all (including calls to system(3), popen(3), or similar
/// functions) during the life of this buffer. See the discussion around the
/// in_Memory parameter below regarding this.
///
/// @param  in_Size
///         [in] The size of in_Memory in bytes. If in_Size
///         is not page aligned, it will be rounded up.
///
/// @param  in_Type
///         [in] The type of the buffer to create. Only COI_BUFFER_NORMAL
///         buffer type is supported.
///
/// @param  in_Flags
///         [in] A bitmask of attributes for the newly created buffer.
///         Some of these flags are required for correctness while others
///         are provided as hints to the runtime system so it can make
///         certain performance optimizations. Note that the flag
///         COI_SAME_ADDRESS_SINKS_AND_SOURCE is still valid but may fail
///         if the same address as in_Memory cannot be allocated on the sink.
///
/// @param  in_Memory
///         [in] A pointer to an already allocated memory region
///         that should be turned into a COIBUFFER. Although the user still
///         owns this memory they should not free it before calling
///         COIBufferDestroy. They must also only access the memory using
///         COIBUFFER semantics, for example using COIBufferMap/COIBufferUnmap
///         when they wish to read or write the data. There are no alignment
///         or size requirements for this memory region.
///
///         WARNING:
///         Since the backing memory passed in can be the target of a DMA
///         the caller must ensure that there is no call to clone(2) (without
///         the CLONE_VM argument) during the life of this buffer. This
///         includes higher level functions that call clone such as fork(2),
///         system(3), popen(3), among others).
///
///         For forked processes, Linux uses copy-on-write semantics for
///         performance reasons. Consequently, if the parent forks and then
///         writes to this memory, the physical page mapping changes causing
///         the DMA to fail (and thus data corruption).
///
///         In Linux you can mark a set of pages to not be copied across
///         across the clone by calling madvise(2) with an argument of
///         MADV_DONTFORK and then safely use that memory in this scenario.
///         Alternately, if the memory is from a region marked MAP_SHARED,
///         this will work.
///
/// @param  in_NumProcesses
///         [in] The number of processes with which this buffer might be used.
///         If the flag COI_SINK_MEMORY is specified then this must be 1.
///
/// @param  in_pProcesses
///         [in] An array of COIPROCESS handles identifying the processes with
///         which this buffer might be used.
///
/// @param  out_pBuffer
///         [out] Pointer to a buffer handle. The handle will be filled in
///         with a value that uniquely identifies the newly created buffer.
///         This handle should be disposed of via COIBufferDestroy()
///         once it is no longer needed.
///
/// @return COI_SUCCESS if the buffer was created
///
/// @return COI_NOT_SUPPORTED if the in_Type value is not COI_BUFFER_NORMAL,
///         or COI_BUFFER_OPENCL.
///
/// @return COI_NOT_SUPPORTED if in_Memory is read-only memory
///
/// @return COI_NOT_SUPPORTED if one of the in_Flags is COI_SINK_MEMORY and
///         in_Type is not COI_BUFFER_NORMAL
///
/// @return COI_NOT_SUPPORTED if the flag COI_SAME_ADDRESS_SINKS is set
///
/// @return COI_NOT_SUPPORTED if the flag COI_SAME_ADDRESS_SINKS_AND_SOURCE is
///         set
///
/// @return COI_ARGUMENT_MISMATCH if the in_Type and in_Flags parameters
///         are not compatible with one another. Please see the
///         COI_VALID_BUFFER_TYPES_AND_FLAGS map above for information about
///         which flags and types are compatible.
///
/// @return COI_ARGUMENT_MISMATCH if the flag COI_SINK_MEMORY is specified and
///         in_NumProcesses > 1.
///
/// @return COI_ARGUMENT_MISMATCH if the flags COI_SINK_MEMORY and
///         COI_OPTIMIZE_HUGE_PAGE_SIZE are both set.
///
/// @return COI_OUT_OF_RANGE if in_Size is zero, if the bits set in
///         the in_Flags parameter are not recognized flags,  or if in_NumProcesses is zero.
///
/// @return COI_INVALID_POINTER if in_Memory, in_pProcesses or
///         out_pBuffer parameter is NULL.
///
/// @return COI_INVALID_HANDLE if one of the COIPROCESS handles in the
///         in_pProcesses array does not identify a valid process.
///
COIACCESSAPI
COIRESULT
COIBufferCreateFromMemory(
    uint64_t            in_Size,
    COI_BUFFER_TYPE     in_Type,
    uint32_t            in_Flags,
    void               *in_Memory,
    uint32_t            in_NumProcesses,
    const   COIPROCESS         *in_pProcesses,
    COIBUFFER          *out_pBuffer);


//////////////////////////////////////////////////////////////////////////////
///
/// Destroys a buffer. Will block on completion of any operations on the
/// buffer, such as COIPipelineRunFunction or COIBufferCopy. Will block until
/// all COIBufferAddRef calls have had a matching COIBufferReleaseRef call
/// made. will not block on an outstanding COIBufferUnmap but will instead
/// return COI_RETRY.
///
/// @param  in_Buffer
///         [in] Handle of the buffer to destroy.
///
/// @return COI_SUCCESS if the buffer was destroyed.
///
/// @return COI_INVALID_HANDLE if the buffer handle was invalid.
///
/// @return COI_RETRY if the buffer is currently mapped. The buffer must
///         first be unmapped before it can be destroyed.
///
/// @return COI_RETRY if the sub-buffers created from this buffer are not yet
///         destroyed
///
COIACCESSAPI
COIRESULT
COIBufferDestroy(
    COIBUFFER           in_Buffer);


//////////////////////////////////////////////////////////////////////////////
///
/// This call initiates a request to access a region of a buffer. Multiple
/// overlapping (or non overlapping) regions can be mapped simultaneously for
/// any given buffer. If a completion event is specified this call will
/// queue a request for the data which will be satisfied when the buffer is
/// available. Once all conditions are met the completion event will be
/// signaled and the user can access the data at out_ppData. The user can call
/// COIEventWait with out_pCompletion to find out when the map operation has
/// completed. If the user accesses the data before the map operation is
/// complete the results are undefined. If out_pCompletion is NULL then this
/// call blocks until the map operation completes and when this call returns
/// out_ppData can be safely accessed. This call returns a map instance handle
/// in an out parameter which must be passed into COIBufferUnmap when the user
/// no longer needs access to that region of the buffer.
///
/// The address returned from COIBufferMap may point to memory that
/// Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// manages on behalf of the user. The user must not free or reallocate this
/// memory, Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// will perform any necessary cleanup when the buffer is
/// destroyed.
///
/// Note that different types of buffers behave differently when mapped.
/// For instance, mapping a COI_BUFFER_NORMAL for write must stall if the
/// buffer is currently being written to by a run function.
/// The asynchronous operation of COIBufferMap will likely be most useful when
/// paired with a COI_BUFFER_NORMAL.
///
/// @param  in_Buffer
///         [in] Handle for the buffer to map.
///
/// @param  in_Offset
///         [in] Offset into the buffer that a pointer should be returned
///         for. The value 0 can be passed in to signify that the mapped
///         region should start at the beginning of the buffer.
///
/// @param  in_Length
///         [in] Length of the buffer area to map. This parameter, in
///         combination with in_Offset, allows the caller to specify
///         that only a subset of an entire buffer need be mapped. A
///         value of 0 can be passed in only if in_Offset is 0, to signify
///         that the mapped region is the entire buffer.
///
/// @param  in_Type
///         [in] The access type that is needed by the application. This will
///         affect how the data can be accessed once the map operation
///         completes. See the COI_MAP_TYPE enum for more details.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the map
///         call initiation to wait for any events to be signaled before
///         starting the map operations.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this map operation will wait for before starting.
///         This allows the user to create dependencies between asynchronous
///         map calls and other operations such as run functions or other
///         asynchronous map calls. The user may pass in NULL if they do not
///         wish to wait for any dependencies to complete before initiating map
///         operations.
///
/// @param  out_pCompletion
///         [out] An optional pointer to a COIEVENT object
///         that will be signaled when a map call with the passed in buffer
///         would complete immediately, that is, the buffer memory has been
///         allocated on the source and its contents updated. The user may pass
///         in NULL if the user wants COIBufferMap to perform a blocking map
///         operation.
///
/// @param  out_pMapInstance
///         [out] A pointer to a COIMAPINSTANCE which represents this mapping
///         of the buffer and must be passed in to COIBufferUnmap when access
///         to this region of the buffer data is no longer needed.
///
/// @param  out_ppData
///         [out] Pointer to the buffer data. The data will only be valid
///         when the completion object is signaled, or for a synchronous
///         map operation with the call to map returns.
///
///
/// @return COI_SUCCESS if the map request succeeds.
///
/// @return COI_OUT_OF_RANGE if in_Offset of (in_Offset + in_Length) exceeds
///         the size of the buffer.
///
/// @return COI_OUT_OF_RANGE if in_Length is 0, but in_Offset is not 0.
///
/// @return COI_OUT_OF_RANGE if in_Type is not a valid COI_MAP_TYPE.
///
/// @return COI_ARGUMENT_MISMATCH if in_NumDependencies is non-zero while
///         in_pDependencies was passed in as NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is non-NULL but
///         in_NumDependencies is zero.
///
/// @return COI_ARGUMENT_MISMATCH if the in_Type of map is not a valid type
///         for in_Buffer's type of buffer.
///
/// @return COI_INVALID_HANDLE if in_Buffer is not a valid buffer handle.
///
/// @return COI_INVALID_POINTER if out_pMapInstance or out_ppData is NULL.
///
COIACCESSAPI
COIRESULT
COIBufferMap(
    COIBUFFER           in_Buffer,
    uint64_t            in_Offset,
    uint64_t            in_Length,
    COI_MAP_TYPE        in_Type,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion,
    COIMAPINSTANCE     *out_pMapInstance,
    void              **out_ppData);

//////////////////////////////////////////////////////////////////////////////
///
/// Disables Source access to the region of the buffer that was provided
/// through the corresponding call to COIBufferMap. The number of calls to
/// COIBufferUnmap() should always match the number of calls made to
/// COIBufferMap(). The data pointer returned from the COIBufferMap() call
/// will be invalid after this call.
///
/// @param  in_MapInstance
///         [in] buffer map instance handle to unmap.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the unmap call to
///         wait for any events to be signaled before performing the unmap
///         operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this unmap operation will wait for before starting.
///         This allows the user to create dependencies between asynchronous
///         unmap calls and other operations such as run functions or other
///         asynchronous unmap calls. The user may pass in NULL if they do not
///         wish to wait for any dependencies to complete before initiating
///         unmap operations.
///
/// @param  out_pCompletion
///         [out] An optional pointer to a COIEVENT object that will be
///         signaled when the unmap is complete. The user may pass in NULL if
///         the user wants COIBufferUnmap to perform a blocking unmap
///         operation.
///
/// @return COI_SUCCESS upon successful unmapping of the buffer instance.
///
/// @return COI_INVALID_HANDLE if the passed in map instance handle was NULL.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
COIACCESSAPI
COIRESULT
COIBufferUnmap(
    COIMAPINSTANCE      in_MapInstance,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Gets the Sink's virtual address of the buffer for the first process
/// that is using the buffer. This is the same address
/// that is passed to the run function on the Sink. The virtual
/// address assigned to the buffer for use on the sink is fixed;
/// the buffer will always be present at that virtual address on the sink
/// and will not get a different virtual address across different
/// RunFunctions.
/// This address is only valid on the Sink and should not be dereferenced on
/// the Source (except for the special case of buffers created with the
/// COI_SAME_ADDRESS flag).
///
/// @param  in_Buffer
///         [in] Buffer handle
///
/// @param  out_pAddress
///         [out] pointer to a uint64_t* that will be filled with the address.
///
/// @return COI_SUCCESS upon successful return of the buffer's address.
///
/// @return COI_INVALID_HANDLE if the passed in buffer handle was invalid.
///
/// @return COI_INVALID_POINTER if the out_pAddress parameter was invalid.
///
COIACCESSAPI
COIRESULT
COIBufferGetSinkAddress(
    COIBUFFER           in_Buffer,
    uint64_t           *out_pAddress);

//////////////////////////////////////////////////////////////////////////////
///
/// Gets the Sink's virtual address of the buffer. This is the same
/// address that is passed to the run function on the Sink. The virtual
/// address assigned to the buffer for use on the sink is fixed;
/// the buffer will always be present at that virtual address on the sink
/// and will not get a different virtual address across different
/// RunFunctions.
/// This address is only valid on the Sink and should not be dereferenced on
/// the Source (except for the special case of buffers created with the
/// COI_SAME_ADDRESS flag).
///
/// @param  in_Process
///         [in] The process for which the address should be returned.
///         Special handle value 0 can be passed to the function;
///         in this case, address for the first valid process will be returned
///
/// @param  in_Buffer
///         [in] Buffer handle
///
/// @param  out_pAddress
///         [out] pointer to a uint64_t* that will be filled with the address.
///
/// @return COI_SUCCESS upon successful return of the buffer's address.
///
/// @return COI_INVALID_HANDLE if the passed in buffer or process
///         handle was invalid.
///
/// @return COI_INVALID_POINTER if the out_pAddress parameter was invalid.
///
/// @return COI_OUT_OF_RANGE if the in_Process is not valid for in_Buffer at the
///         moment of calling the function.
///
COIACCESSAPI
COIRESULT
COIBufferGetSinkAddressEx(
    COIPROCESS          in_Process,
    COIBUFFER           in_Buffer,
    uint64_t           *out_pAddress);

//////////////////////////////////////////////////////////////////////////////
///
/// Copy data from a normal virtual address into an existing COIBUFFER.
/// Please note that COIBufferWrite does not follow implicit buffer
/// dependencies. If a buffer is in use in a run function or has been added
/// to a process using COIBufferAddRef the call to COIBufferWrite will not
/// wait, it will still copy data immediately.
/// This is to facilitate a usage model where a buffer is being used outside
/// of a run function, for example in a spawned thread, but data still needs
/// to be transferred to or from the buffer.
/// Additionally this means that if more than one DMA channel is enabled,
/// (See COIProcessConfigureDMA) operations to the same buffer may
/// happen in parallel if they can be assigned to different DMA hardware.
/// So it is highly recommended to use explicit event dependencies to
/// order operations where needed.
///
/// @param  in_DestBuffer
///         [in] Buffer to write into.
///
/// @param  in_DestProcess
///         [in] A pointer to the process to which the data will be written.
///         Buffer is updated only in this process and invalidated in other
///         processes. Only a single process can be specified.
///         Can be left NULL and default behavior will be chosen, which
///         chooses the first valid process in which regions are found. Other
///         buffer regions are invalidated if not updated.
///
/// @param  in_Offset
///         [in] Location in the buffer to start writing to.
///
/// @param  in_pSourceData
///         [in] A pointer to local memory that should be copied into the
///         provided buffer.
///
/// @param  in_Length
///         [in] The number of bytes to write from in_pSourceData into
///         in_DestBuffer. Must not be larger than the size of in_DestBuffer
///         and must not over run in_DestBuffer if an in_Offset is provided.
///
/// @param  in_Type
///         [in] The type of copy operation to use, one of either
///         COI_COPY_UNSPECIFIED, COI_COPY_USE_DMA, COI_COPY_USE_CPU.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the write call to
///         wait for any additional events to be signaled before starting the
///         write operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this write operation will wait for before starting.
///         This allows the user to create dependencies between buffer write
///         calls and other operations such as run functions and map calls. The
///         user may pass in NULL if they do not wish to wait for any
///         additional dependencies to complete before doing the write.
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the write has
///         completed. This event can be used as a dependency to order
///         the write with regard to future operations.
///         If no completion event is passed in then the write is
///         synchronous and will block until the transfer is complete.
///
///
/// @return COI_SUCCESS if the buffer was written successfully.
///
/// @return COI_INVALID_HANDLE if the buffer handle was invalid.
///
/// @return COI_OUT_OF_RANGE if in_Offset is beyond the end of the buffer.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
/// @return COI_INVALID_POINTER if the in_pSourceData pointer is NULL.
///
/// @return COI_OUT_OF_RANGE if in_Offset + in_Length exceeds the size of
///         the buffer.
///
/// @return COI_OUT_OF_RANGE if in_Length is 0.
///
/// @return COI_RETRY if in_DestBuffer is mapped and is not COI_BUFFER_OPENCL
///         buffer.
///
COIACCESSAPI
COIRESULT
COIBufferWriteEx(
    COIBUFFER           in_DestBuffer,
    const   COIPROCESS          in_DestProcess,
    uint64_t            in_Offset,
    const   void               *in_pSourceData,
    uint64_t            in_Length,
    COI_COPY_TYPE       in_Type,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Copy data specified by multi-dimensional array data structure into another
/// multi-dimensional array in an existing COIBUFFER.
/// Arrays with more than 3 dimensions are not supported.
/// Different numbers of elements between src and destination is not supported.
/// Please note that COIBufferWriteMultiD does not follow implicit buffer
/// dependencies. If a buffer is in use in a run function or has been added
/// to a process using COIBufferAddRef the call to COIBufferWriteMultiD will not
/// wait, it will still copy data immediately.
/// This is to facilitate a usage model where a buffer is being used outside
/// of a run function, for example in a spawned thread, but data still needs
/// to be transferred to or from the buffer.
/// Additionally this means that if more than one DMA channel is enabled,
/// (See COIProcessConfigureDMA) operations to the same buffer may
/// happen in parallel if they can be assigned to different DMA hardware.
/// So it is highly recommended to use explicit event dependencies to
/// order operations where needed.
///
///
/// @param  in_DestBuffer
///         [in] Buffer to write into.
///
/// @param  in_DestProcess
///         [in] A pointer to the process to which the data will be written.
///         Buffer is updated only in this process and invalidated in other
///         processes. Only a single process can be specified.
///         Can be left NULL and default behavior will be chosen, which
///         chooses the first valid process in which regions are found. Other
///         buffer regions are invalidated if not updated.
///
/// @param  in_Offset
///         [in] Start location of the destination array within the buffer.
///
/// @param  in_DestArray
///         [in] A pointer to a data structure describing the structure of
///         the data array in the buffer. Total size must not be larger than
///         the size of in_DestBuffer. The base field of this structure will
///         be ignored.
///
/// @param  in_SrcArray
///         [in] A pointer to a data structure describing the structure of
///         the data array in local memory that should be copied. in_SrcArray
///         and in_DestArry must have the same number of elements. The base
///         field of this structure should be the virtual pointer to the local
///         memory in which this array is located.
///
/// @param  in_Type
///         [in] The type of copy operation to use, one of either
///         COI_COPY_UNSPECIFIED, COI_COPY_USE_DMA, COI_COPY_USE_CPU.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the write call to
///         wait for any additional events to be signaled before starting the
///         write operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this write operation will wait for before starting.
///         This allows the user to create dependencies between buffer write
///         calls and other operations such as run functions and map calls. The
///         user may pass in NULL if they do not wish to wait for any
///         additional dependencies to complete before doing the write.
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the write has
///         completed. This event can be used as a dependency to order
///         the write with regard to future operations.
///         If no completion event is passed in then the write is
///         synchronous and will block until the transfer is complete.
///
///
/// @return COI_SUCCESS if the buffer was copied successfully.
///
/// @return COI_INVALID_HANDLE if the buffer or process handle was invalid.
///
/// @return COI_OUT_OF_RANGE if in_Offset is beyond the end of the buffer.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
/// @return COI_NOT_SUPPORTED or dimension of destination or source arrays
///         are greater than 3 or less than 1
///
/// @return COI_INVALID_POINTER if the pointer in_SrcArray->base is NULL.
///
/// @return COI_OUT_OF_RANGE if in_Offset + size of in_DestArray exceeds the
///         size of the buffer.
///
/// @return COI_OUT_OF_MEMORY if any allocation of memory fails
///
/// @return COI_RETRY if in_DestBuffer is mapped and is not
///         a COI_BUFFER_OPENCL buffer.
///
COIACCESSAPI
COIRESULT
COIBufferWriteMultiD(
    COIBUFFER          in_DestBuffer,
    const   COIPROCESS         in_DestProcess,
    uint64_t           in_Offset,
    struct arr_desc   *in_DestArray,
    struct arr_desc   *in_SrcArray,
    COI_COPY_TYPE      in_Type,
    uint32_t           in_NumDependencies,
    const   COIEVENT          *in_pDependencies,
    COIEVENT          *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Copy data specified by multi-dimensional array data structure from an
/// existing COIBUFFER to another multi-dimensional array located in memory.
/// Arrays with more than 3 dimensions are not supported.
/// Different numbers of elements between source and destination are not supported.
/// Please note that COIBufferReadMultiD does not follow implicit buffer
/// dependencies. If a buffer is in use in a run function or has been added
/// to a process using COIBufferAddRef the call to COIBufferReadMultiD will not
/// wait, it will still copy data immediately.
/// This is to facilitate a usage model where a buffer is being used outside
/// of a run function, for example in a spawned thread, but data still needs
/// to be transferred to or from the buffer.
/// Additionally this means that if more than one DMA channel is enabled,
/// (See COIProcessConfigureDMA) operations to the same buffer may
/// happen in parallel if they can be assigned to different DMA hardware.
/// So it is highly recommended to use explicit event dependencies to
/// order operations where needed.
///
///
/// @param  in_SourceBuffer
///         [in] Buffer to read from.
///
/// @param  in_Offset
///         [in] Start location of the source array within the buffer.
///
/// @param  in_DestArray
///         [in] A pointer to a data structure describing the structure of
///         the data array in the buffer. Total size must not be larger than
///         the size of in_DestBuffer. The base field of this structure will
///         be ignored.
///
/// @param  in_SrcArray
///         [in] A pointer to a data structure describing the structure of
///         the data array in local memory that should be copied. in_SrcArray
///         and in_DestArry must have the same number of elements. The base
///         field of this structure should be the virtual pointer to the local
///         memory in which this array is located.
///
/// @param  in_Type
///         [in] The type of copy operation to use, one of either
///         COI_COPY_UNSPECIFIED, COI_COPY_USE_DMA, COI_COPY_USE_CPU.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the write call to
///         wait for any additional events to be signaled before starting the
///         write operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this write operation will wait for before starting.
///         This allows the user to create dependencies between buffer write
///         calls and other operations such as run functions and map calls. The
///         user may pass in NULL if they do not wish to wait for any
///         additional dependencies to complete before doing the write.
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the write has
///         completed. This event can be used as a dependency to order
///         the write with regard to future operations.
///         If no completion event is passed in then the write is
///         synchronous and will block until the transfer is complete.
///
///
/// @return COI_SUCCESS if the buffer was written successfully.
///
/// @return COI_INVALID_HANDLE if the buffer or process handle was invalid.
///
/// @return COI_OUT_OF_RANGE if in_Offset is beyond the end of the buffer.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
/// @return COI_NOT_SUPPORTED or dimension of destination or source arrays
///         are greater than 3 or less than 1
///
/// @return COI_INVALID_POINTER if the pointer in_DestArray->base is NULL.
///
/// @return COI_OUT_OF_RANGE if in_Offset + size of in_SourceArray exceeds the
///         size of the buffer.
///
/// @return COI_OUT_OF_MEMORY if any allocation of memory fails
///
/// @return COI_RETRY if in_SourceBuffer is mapped and is not
///         a COI_BUFFER_OPENCL buffer.
///
COIACCESSAPI
COIRESULT
COIBufferReadMultiD(
    COIBUFFER          in_SourceBuffer,
    uint64_t           in_Offset,
    struct arr_desc   *in_DestArray,
    struct arr_desc   *in_SrcArray,
    COI_COPY_TYPE      in_Type,
    uint32_t           in_NumDependencies,
    const   COIEVENT          *in_pDependencies,
    COIEVENT          *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Copy data from a normal virtual address into an existing COIBUFFER.
/// Please note that COIBufferWrite does not follow implicit buffer
/// dependencies. If a buffer is in use in a run function or has been added
/// to a process using COIBufferAddRef the call to COIBufferWrite will not
/// wait, it will still copy data immediately.
/// This is to facilitate a usage model where a buffer is being used outside
/// of a run function, for example in a spawned thread, but data still needs
/// to be transferred to or from the buffer.
/// Additionally this means that if more than one DMA channel is enabled,
/// (See COIProcessConfigureDMA) operations to the same buffer may
/// happen in parallel if they can be assigned to different DMA hardware.
/// So it is highly recommended to use explicit event dependencies to
/// order operations where needed.
///
/// @param  in_DestBuffer
///         [in] Buffer to write into.
///
/// @param  in_Offset
///         [in] Location in the buffer to start writing to.
///
/// @param  in_pSourceData
///         [in] A pointer to local memory that should be copied into the
///         provided buffer.
///
/// @param  in_Length
///         [in] The number of bytes to write from in_pSourceData into
///         in_DestBuffer. Must not be larger than the size of in_DestBuffer
///         and must not over run in_DestBuffer if an in_Offset is provided.
///
/// @param  in_Type
///         [in] The type of copy operation to use, one of either
///         COI_COPY_UNSPECIFIED, COI_COPY_USE_DMA, COI_COPY_USE_CPU.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the write call to
///         wait for any additional events to be signaled before starting the
///         write operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this write operation will wait for before starting.
///         This allows the user to create dependencies between buffer write
///         calls and other operations such as run functions and map calls. The
///         user may pass in NULL if they do not wish to wait for any
///         additional dependencies to complete before doing the write.
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the write has
///         completed. This event can be used as a dependency to order
///         the write with regard to future operations.
///         If no completion event is passed in then the write is
///         synchronous and will block until the transfer is complete.
///
///
/// @return COI_SUCCESS if the buffer was copied successfully.
///
/// @return COI_INVALID_HANDLE if the buffer handle was invalid.
///
/// @return COI_OUT_OF_RANGE if in_Offset is beyond the end of the buffer.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
/// @return COI_INVALID_POINTER if the in_pSourceData pointer is NULL.
///
/// @return COI_OUT_OF_RANGE if in_Offset + in_Length exceeds the size of
///         the buffer.
///
/// @return COI_OUT_OF_RANGE if in_Length is 0.
///
/// @return COI_RETRY if in_DestBuffer is mapped and is not
///         a COI_BUFFER_OPENCL buffer.
///
COIACCESSAPI
COIRESULT
COIBufferWrite(
    COIBUFFER           in_DestBuffer,
    uint64_t            in_Offset,
    const   void               *in_pSourceData,
    uint64_t            in_Length,
    COI_COPY_TYPE       in_Type,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Copy data from a buffer into local memory.
/// Please note that COIBufferRead does not follow implicit buffer
/// dependencies. If a buffer is in use in a run function or has been added
/// to a process using COIBufferAddRef the call to COIBufferRead will not
/// wait, it will still copy data immediately.
/// This is to facilitate a usage model where a buffer is being used outside
/// of a run function, for example in a spawned thread, but data still needs
/// to be transferred to or from the buffer.
/// Additionally this means that if more than one DMA channel is enabled,
/// (See COIProcessConfigureDMA) operations to the same buffer may
/// happen in parallel if they can be assigned to different DMA hardware.
/// So it is highly recommended to use explicit event dependencies to
/// order operations where needed.
///
///
/// @param  in_SourceBuffer
///         [in] Buffer to read from.
///
/// @param  in_Offset
///         [in] Location in the buffer to start reading from.
///
/// @param  in_pDestData
///         [in] A pointer to local memory that should be written into from
///         the provided buffer.
///
/// @param  in_Length
///         [in] The number of bytes to write from in_SourceBuffer into
///         in_pDestData. Must not be larger than the size of in_SourceBuffer
///         and must not over run in_SourceBuffer if an in_Offset is provided.
///
/// @param  in_Type
///         [in] The type of copy operation to use, one of either
///         COI_COPY_UNSPECIFIED, COI_COPY_USE_DMA, COI_COPY_USE_CPU.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the read call to
///         wait for any additional events to be signaled before starting the
///         read operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this read operation will wait for before starting.
///         This allows the user to create dependencies between buffer read
///         calls and other operations such as run functions and map calls. The
///         user may pass in NULL if they do not wish to wait for any
///         additional dependencies to complete before doing the read.
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the read has
///         completed. This event can be used as a dependency to order
///         the read with regard to future operations.
///         If no completion event is passed in then the read is
///         synchronous and will block until the transfer is complete.
///
/// @return COI_SUCCESS if the buffer was copied successfully.
///
/// @return COI_INVALID_HANDLE if the buffer handle was invalid.
///
/// @return COI_OUT_OF_RANGE if in_Offset is beyond the end of the buffer.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
/// @return COI_OUT_OF_RANGE if in_Offset + in_Length exceeds the size of
///         the buffer.
///
/// @return COI_OUT_OF_RANGE if in_Length is 0.
///
/// @return COI_INVALID_POINTER if the in_pDestData pointer is NULL.
///
/// @return COI_RETRY if in_SourceBuffer is mapped and is not
///         a COI_BUFFER_OPENCL buffer.
///
COIACCESSAPI
COIRESULT
COIBufferRead(
    COIBUFFER           in_SourceBuffer,
    uint64_t            in_Offset,
    void               *in_pDestData,
    uint64_t            in_Length,
    COI_COPY_TYPE       in_Type,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Copy data between two buffers. It also allows copying within the same
/// buffer. For copy within the same buffer, if source and destination regions
/// overlap then this API returns error.
/// Please note that COIBufferCopy does not follow implicit buffer
/// dependencies. If a buffer is in use in a run function or has been added
/// to a process using COIBufferAddRef the call to COIBufferCopy will not
/// wait, it will still copy data immediately.
/// This is to facilitate a usage model where a buffer is being used outside
/// of a run function, for example in a spawned thread, but data still needs
/// to be transferred to or from the buffer.
/// Additionally this means that if more than one DMA channel is enabled,
/// (See COIProcessConfigureDMA) operations to the same buffer may
/// happen in parallel if they can be assigned to different DMA hardware.
/// So it is highly recommended to use explicit event dependencies to
/// order operations where needed.
/// When a destroyed buffer (destination or source) is provided to the
/// function, then behavior is unspecified.
///
/// @param  in_DestBuffer
///         [in] Buffer to copy into.
///
/// @param  in_DestProcess
///         [in] A pointer to the process to which the data will be written.
///         Buffer is updated only in this process and invalidated in other
///         processes. Only a single process can be specified.
///         Can be left NULL and default behavior will be chosen, which
///         chooses the first valid process in which regions are found. Other
///         buffer regions are invalidated if not updated.
///
/// @param  in_SourceBuffer
///         [in] Buffer to copy from.
///
/// @param  in_DestOffset
///         [in] Location in the destination buffer to start writing to.
///
/// @param  in_SourceOffset
///         [in] Location in the source buffer to start reading from.
///
/// @param  in_Length
///         [in] The number of bytes to copy from in_SourceBuffer into
///         in_DestinationBuffer.
///         If the length is specified as zero then length to be copied
//          is entire destination buffer's length.
///         Must not be larger than the size of in_SourceBuffer or
///         in_DestBuffer and must not over run in_SourceBuffer or
///         in_DestBuffer if offsets are specified.
///
/// @param  in_Type
///         [in] The type of copy operation to use, one of either
///         COI_COPY_UNSPECIFIED, COI_COPY_USE_DMA, COI_COPY_USE_CPU.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the copy call to
///         wait for any additional events to be signaled before starting the
///         copy operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this copy operation will wait for before starting.
///         This allows the user to create dependencies between buffer copy
///         calls and other operations such as run functions and map calls. The
///         user may pass in NULL if they do not wish to wait for any
///         additional dependencies to complete before doing the copy.
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the copy has
///         completed. This event can be used as a dependency to order
///         the copy with regard to future operations.
///         If no completion event is passed in then the copy is
///         synchronous and will block until the transfer is complete.
///
/// @return COI_SUCCESS if the buffer was copied successfully.
///
/// @return COI_INVALID_HANDLE if either buffer handle was invalid.
///
/// @return COI_MEMORY_OVERLAP if in_SourceBuffer and in_DestBuffer are the
///         same buffer(or have the same parent buffer) and the source and
///         destination regions overlap
///
/// @return COI_OUT_OF_RANGE if in_DestOffset is is beyond the end of
///         in_DestBuffer
///
/// @return COI_OUT_OF_RANGE if in_SourceOffset is beyond the end of
///         in_SourceBuffer.
///
/// @return COI_OUT_OF_RANGE if in_DestOffset + in_Length exceeds the size of
///         the in_DestBuffer
///
/// @return COI_OUT_OF_RANGE if in_SourceOffset + in_Length exceeds
///         the size of in_SourceBuffer.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
/// @return COI_RETRY if in_DestBuffer or in_SourceBuffer are mapped and not
///         COI_BUFFER_OPENCL buffers.
///
COIACCESSAPI
COIRESULT
COIBufferCopyEx(
    COIBUFFER           in_DestBuffer,
    const   COIPROCESS          in_DestProcess,
    COIBUFFER           in_SourceBuffer,
    uint64_t            in_DestOffset,
    uint64_t            in_SourceOffset,
    uint64_t            in_Length,
    COI_COPY_TYPE       in_Type,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Copy data between two buffers. It also allows copying within the same
/// buffer. For copy within the same buffer, if source and destination regions
/// overlap then this API returns error.
/// Please note that COIBufferCopy does not follow implicit buffer
/// dependencies. If a buffer is in use in a run function or has been added
/// to a process using COIBufferAddRef the call to COIBufferCopy will not
/// wait, it will still copy data immediately.
/// This is to facilitate a usage model where a buffer is being used outside
/// of a run function, for example in a spawned thread, but data still needs
/// to be transferred to or from the buffer.
/// Additionally this means that if more than one DMA channel is enabled,
/// (See COIProcessConfigureDMA) operations to the same buffer may
/// happen in parallel if they can be assigned to different DMA hardware.
/// So it is highly recommended to use explicit event dependencies to
/// order operations where needed.
/// When a destroyed buffer (destination or source) is provided to the
/// function, then behavior is unspecified.
///
/// @param  in_DestBuffer
///         [in] Buffer to copy into.
///
/// @param  in_SourceBuffer
///         [in] Buffer to copy from.
///
/// @param  in_DestOffset
///         [in] Location in the destination buffer to start writing to.
///
/// @param  in_SourceOffset
///         [in] Location in the source buffer to start reading from.
///
/// @param  in_Length
///         [in] The number of bytes to copy from in_SourceBuffer into
///         in_DestinationBuffer.
///         If the length is specified as zero then length to be copied
///         is entire destination buffer's length.
///         Must not be larger than the size of in_SourceBuffer or
///         in_DestBuffer and must not over run in_SourceBuffer or
///         in_DestBuffer if offsets are specified.
///
/// @param  in_Type
///         [in] The type of copy operation to use, one of either
///         COI_COPY_UNSPECIFIED, COI_COPY_USE_DMA, COI_COPY_USE_CPU.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the copy call to
///         wait for any additional events to be signaled before starting the
///         copy operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this copy operation will wait for before starting.
///         This allows the user to create dependencies between buffer copy
///         calls and other operations such as run functions and map calls. The
///         user may pass in NULL if they do not wish to wait for any
///         additional dependencies to complete before doing the copy.
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the copy has
///         completed. This event can be used as a dependency to order
///         the copy with regard to future operations.
///         If no completion event is passed in then the copy is
///         synchronous and will block until the transfer is complete.
///
/// @return COI_SUCCESS if the buffer was copied successfully.
///
/// @return COI_INVALID_HANDLE if either buffer handle was invalid.
///
/// @return COI_MEMORY_OVERLAP if in_SourceBuffer and in_DestBuffer are the
///         same buffer(or have the same parent buffer) and the source and
///         destination regions overlap
///
/// @return COI_OUT_OF_RANGE if in_DestOffset is is beyond the end of
///         in_DestBuffer
///
/// @return COI_OUT_OF_RANGE if in_SourceOffset is beyond the end of
///         in_SourceBuffer.
///
/// @return COI_OUT_OF_RANGE if in_DestOffset + in_Length exceeds the size of
///         the in_DestBuffer
///
/// @return COI_OUT_OF_RANGE if in_SourceOffset + in_Length exceeds
///         the size of in_SourceBuffer.
///
/// @return COI_ARGUMENT_MISMATCH if the in_pDependencies is non NULL but
///         in_NumDependencies is 0.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is NULL but
///         in_NumDependencies is not 0.
///
/// @return COI_RETRY if in_DestBuffer or in_SourceBuffer are mapped and not
///         COI_BUFFER_OPENCL buffers.
///
COIACCESSAPI
COIRESULT
COIBufferCopy(
    COIBUFFER           in_DestBuffer,
    COIBUFFER           in_SourceBuffer,
    uint64_t            in_DestOffset,
    uint64_t            in_SourceOffset,
    uint64_t            in_Length,
    COI_COPY_TYPE       in_Type,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// This API allows an experienced Intel(R) Coprocessor Offload Infrastructure
/// (Intel(R) COI) developer to set where a COIBUFFER is
/// located and when the COIBUFFER's data is moved. This functionality is
/// useful when the developer knows when and where a buffer is going to be
/// accessed. It allows the data movement to happen sooner than if the
/// Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// runtime tried to manage the buffer placement itself. The advantage of
/// this API is that the developer knows much more about their own
/// application's data access patterns and can therefore optimize the data
/// access to be much more efficient than the Intel(R)Coprocessor Offload
/// Infrastructure (Intel(R) COI) runtime. Using this API may yield better
/// memory utilization, lower latency and overall improved workload
/// throughput.
/// This API does respect implicit dependencies for buffer read/write hazards.
/// For example, if the buffer is being written in one COIPROCESS and the user
/// requests the buffer be placed in another COIPROCESS then this API will wait
/// for the first access to complete before moving the buffer.
/// This API is not required for program correctness. It is intended solely
/// for advanced Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// developers who wish to fine tune their application performance
/// Cases where "a change in state" is an error condition the change just gets
/// ignored without any error. This is because the SetState can be a
/// nonblocking call and in such cases we can't rely on the state of the buffer
/// at the time of the call. We can do the transition checks only at the time
/// when the actual state change happens (which is something in future).
/// Currently there is no way to report an error from something that happens in
/// future and that is why such state transitions are nop. One example is using
/// VALID_MAY_DROP with COI_SINK_OWNERS when buffer is not valid at source.
/// This operation will be a nop if at the time of actual state change the
/// buffer is not valid at source.
///
/// @param  in_Buffer
///         [in] The buffer to modify.
///
/// @param  in_Process
///         [in] The process where the state is being modified for this
///         buffer. To modify buffer's state on source process use
///         COI_PROCESS_SOURCE as process handle. To modify buffer's
///         state on all processes where buffer is valid use COI_SINK_OWNERS
///         as the process handle.
///
/// @param  in_State
///         [in] The new state for the buffer. The buffer's state could be
///         set to invalid on one of the sink processes where it is being
///         used.
///
/// @param  in_DataMove
///         [in] A flag to indicate if the buffer's data should be moved
///         when the state is changed. For instance, a buffer's state may
///         be set to valid on a process and the data move flag may be set to
///         COI_BUFFER_MOVE which would cause the buffer contents to be
///         copied to the process where it is now valid.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the SetState call
///         to wait for any additional events to be signaled before starting
///         this operation.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this SetState operation will wait for before starting
///         This allows the user to create dependencies between buffer
///         SetState calls and other operations such as run functions and map
///         calls. The user may pass in NULL if they do not wish to wait for
///         any additional dependencies to complete before doing the SetState
///
/// @param  out_pCompletion
///         [out] An optional event to be signaled when the SetState has
///         completed. This event can be used as a dependency to order
///         the SetState with regard to future operations.
///         If no completion event is passed in then the state changing is
///         synchronous and will block until the SetState and dma transfers
///         related to this operation are complete.
///
/// @return COI_SUCCESS if the buffer's state was changed successfully.
///
/// @return COI_INVALID_HANDLE if in_Buffer or in_Process is invalid.
///
/// @return COI_NOT_SUPPORTED if the in_Buffer is of any type other than
///         COI_BUFFER_NORMAL or COI_BUFFER_OPENCL.
///
/// @return COI_ARGUMENT_MISMATCH if the in_State is COI_BUFFER_VALID_MAY_DROP
///         and the in_Process is COI_PROCESS_SOURCE.
///
/// @return COI_ARGUMENT_MISMATCH if the in_Process is COI_SINK_OWNERS and the
///         COI_BUFFER_MOVE is passed as move flag.
///
/// @return COI_MISSING_DEPENDENCY if buffer was not created on the process
///         handle that was passed in.
///
COIACCESSAPI
COIRESULT
COIBufferSetState(
    COIBUFFER               in_Buffer,
    COIPROCESS              in_Process,
    COI_BUFFER_STATE        in_State,
    COI_BUFFER_MOVE_FLAG    in_DataMove,
    uint32_t                in_NumDependencies,
    const   COIEVENT               *in_pDependencies,
    COIEVENT               *out_pCompletion);

//////////////////////////////////////////////////////////////////////////////
///
/// Creates a sub-buffer that is a reference to a portion of an existing
/// buffer. The returned buffer handle can be used in all API calls that the
/// original buffer handle could be used in except COIBufferCreateSubBuffer.
/// Sub buffers out of Huge Page Buffer are also supported but the original
/// buffer needs to be a OPENCL buffer created with COI_OPTIMIZE_HUGE_PAGE_SIZE
/// flag.
///
/// When the sub-buffer is used only the corresponding sub-section of the
/// original buffer is used or affected.
///
/// @param  in_Buffer
///         [in] The original buffer that this new sub-buffer is a reference
///         to.
///
/// @param  in_Length
///         [in] The length of the sub-buffer in number of bytes.
///
/// @param  in_Offset
///         [in] Where in the original buffer to start this sub-buffer.
///
/// @param  out_pSubBuffer
///         [out] Pointer to a buffer handle that is filled in with the newly
///         created sub-buffer.
///
/// @return COI_SUCCESS if the sub-buffer was created
///
/// @return COI_INVALID_HANDLE if in_Buffer is not a valid buffer handle.
///
/// @return COI_OUT_OF_RANGE if in_Length is zero, or if in_Offset + in_Length
///         is greater than the size of the original buffer.
///
/// @return COI_OUT_OF_MEMORY if allocating the buffer fails.
///
/// @return COI_INVALID_POINTER if the out_pSubBuffer pointer is NULL.
///
/// @return COI_NOT_SUPPORTED if the in_Buffer is of any type other than
///         COI_BUFFER_OPENCL
///
COIACCESSAPI
COIRESULT
COIBufferCreateSubBuffer(
    COIBUFFER   in_Buffer,
    uint64_t    in_Length,
    uint64_t    in_Offset,
    COIBUFFER  *out_pSubBuffer);

//////////////////////////////////////////////////////////////////////////////
///
/// Releases the reference count on the specified buffer and process by
/// in_ReleaseRefcnt. The returned result being COI_SUCCESS indicates that the
/// specified process contains a reference to the specified buffer that has a
/// refcnt that can be decremented. Otherwise, if the buffer or process
/// specified do not exist, then COI_INVALID_HANDLE will be returned. If the
/// process does not contain a reference to the specified buffer then
/// COI_OUT_OF_RANGE will be returned.
///
///
/// @param  in_Process
///         [in] The COI Process whose reference count for the specified buffer
///         the user wants to decrement.
///
/// @param  in_Buffer
///         [in] The buffer used in the specified coi process in which the user
///         wants to decrement the reference count.
///
/// @param  in_ReleaseRefcnt
///         [in] The value the reference count will be decremented by.
///
/// @return COI_SUCCESS if the reference count was successfully decremented.
///
/// @return COI_INVALID_HANDLE if in_Buffer or in_Process are invalid handles.
///
/// @return COI_OUT_OF_RANGE if the reference for the specified buffer or
///         process does not exist.
///

COIACCESSAPI
COIRESULT
COIBufferReleaseRefcnt(
    COIPROCESS          in_Process,
    COIBUFFER           in_Buffer,
    uint64_t            in_ReleaseRefcnt);

//////////////////////////////////////////////////////////////////////////////
///
/// Increments the reference count on the specified buffer and process by
/// in_AddRefcnt. The returned result being COI_SUCCESS indicates that the
/// specified process contains a reference to the specified buffer or a new
/// reference has been created and that reference has a new refcnt. Otherwise,
/// if the buffer or process specified do not exist, then COI_INVALID_HANDLE
/// will be returned. If the input buffer is not valid on the target process
/// then COI_NOT_INITIALIZED will be returned since the buffer is not current
/// or allocated on the process.
///
/// @param  in_Process
///         [in] The COI Process whose reference count for the specified buffer
///         the user wants to increment.
///
/// @param  in_Buffer
///         [in] The buffer used in the specified coi process in which the user
///         wants to increment the reference count.
///
/// @param  in_AddRefcnt
///         [in] The value the reference count will be incremented by.
///
/// @return COI_SUCCESS if the reference count was successfully incremented.
///
/// @return COI_INVALID_HANDLE if in_Buffer or in_Process are invalid handles.
///
/// @return COI_NOT_INITIALIZED if in_Buffer does not have a buffer state of
///         COI_BUFFER_VALID on the in_Process.
///
COIACCESSAPI
COIRESULT
COIBufferAddRefcnt(
    COIPROCESS          in_Process,
    COIBUFFER           in_Buffer,
    uint64_t            in_AddRefcnt);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIBUFFER_SOURCE_H */

/*! @} */
