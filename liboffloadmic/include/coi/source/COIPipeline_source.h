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

#ifndef _COIPIPELINE_SOURCE_H
#define _COIPIPELINE_SOURCE_H

/** @ingroup COIPipeline
 *  @addtogroup COIPipelineSource
@{
* @file source/COIPipeline_source.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS

#include "../common/COITypes_common.h"
#include "../common/COIResult_common.h"

#ifdef __cplusplus
extern "C" {
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS



//////////////////////////////////////////////////////////////////////////////
/// These flags specify how a buffer will be used within a run function. They
/// allow the runtime to make optimizations in how it moves the data around.
/// These flags can affect the correctness of an application, so they must be
/// set properly. For example, if a buffer is used in a run function with the
/// COI_SINK_READ flag and then mapped on the source, the runtime may use a
/// previously cached version of the buffer instead of retrieving data from
/// the sink.
typedef enum COI_ACCESS_FLAGS
{
    /// Specifies that the run function will only read the associated buffer.
    COI_SINK_READ = 1,

    /// Specifies that the run function will write to the associated buffer.
    COI_SINK_WRITE,

    /// Specifies that the run function will overwrite the entire associated
    /// buffer and therefore the buffer will not be synchronized with the
    /// source before execution.
    COI_SINK_WRITE_ENTIRE,

    /// Specifies that the run function will only read the associated buffer
    /// and will maintain the reference count on the buffer after
    /// run function exit.
    COI_SINK_READ_ADDREF,

    /// Specifies that the run function will write to the associated buffer
    /// and will maintain the reference count on the buffer after
    /// run function exit.
    COI_SINK_WRITE_ADDREF,

    /// Specifies that the run function will overwrite the entire associated
    /// buffer and therefore the buffer will not be synchronized with the
    /// source before execution and will maintain the reference count on the
    /// buffer after run function exit.
    COI_SINK_WRITE_ENTIRE_ADDREF
} COI_ACCESS_FLAGS;

#define COI_PIPELINE_MAX_PIPELINES 512
#define COI_PIPELINE_MAX_IN_BUFFERS 16384
#define COI_PIPELINE_MAX_IN_MISC_DATA_LEN 32768


///////////////////////////////////////////////////////////////////////////////
///
/// Create a pipeline associated with a remote process. This pipeline can
/// then be used to execute remote functions and to share data using
/// COIBuffers.
///
/// @param  in_Process
///         [in] A handle to an already existing process that the pipeline
///         will be associated with.
///
/// @param  in_Mask
///         [in] An optional mask of the set of hardware threads on which the
///         sink pipeline command processing thread could run.
///
/// @param  in_StackSize
///         [in] An optional value that will be used when the pipeline
///         processing thread is created on the sink. If the user passes in
///         0 the OS default stack size will be used. Otherwise the value
///         must be PTHREAD_STACK_MIN (16384) bytes or larger and must be
///         a multiple of a page (4096 bytes).
///
/// @param  out_pPipeline
///         [out] Handle returned to uniquely identify the pipeline that was
///         created for use in later API calls.
///
///
/// @return COI_SUCCESS if the pipeline was successfully created.
///
/// @return COI_INVALID_HANDLE if the in_Process handle passed in was invalid.
///
/// @return COI_INVALID_POINTER if the out_pPipeline pointer was NULL.
///
/// @return COI_RESOURCE_EXHAUSTED if no more COIPipelines can be created. The
///         maximum number of pipelines allowed is COI_PIPELINE_MAX_PIPELINES.
///         It is recommended in most cases to not exceed the number of CPU's
///         that are reported on the offload device, performance will suffer.
///
///
/// @return COI_OUT_OF_RANGE if the in_StackSize > 0 &&
///         in_StackSize < PTHREAD_STACK_MIN or if in_StackSize is not a
///         multiple of a page (4096 bytes).
///
/// @return COI_OUT_OF_RANGE if the in_Mask is set to all zeroes. If no mask
///         is desired then the in_Mask should be passed as NULL, otherwise
///         at least one thread must be set.
///
/// @return COI_TIME_OUT_REACHED if establishing the communication channel with
///         the remote pipeline timed out.
///
/// @return COI_RETRY if the pipeline cannot be created due to the number of
///         source-to-sink connections in use. A subsequent call to
///         COIPipelineCreate may succeed if resources are freed up.
///
/// @return COI_PROCESS_DIED if in_Process died.
///
COIACCESSAPI
COIRESULT
COIPipelineCreate(
    COIPROCESS          in_Process,
    COI_CPU_MASK        in_Mask,
    uint32_t            in_StackSize,
    COIPIPELINE        *out_pPipeline);

///////////////////////////////////////////////////////////////////////////////
///
/// Destroys the indicated pipeline, releasing its resources.
///
/// @param  in_Pipeline
///         [in] Pipeline to destroy.
///
///
/// @return COI_SUCCESS if the pipeline was destroyed
///
COIACCESSAPI
COIRESULT
COIPipelineDestroy(
    COIPIPELINE         in_Pipeline);


//////////////////////////////////////////////////////////////////////////////
///
/// Enqueues a function in the remote process binary to be executed. The
/// function execution is asynchronous in regards to the Source and all
/// run functions enqueued on a pipeline are executed in-order. The run
/// function will only execute when all of the required buffers are present
/// in the Sink's memory.
///
/// Potential Hazards while using Runfunctions:
///
/// 1. Proper care has to be taken while setting the input dependencies for
///    RunFunctions. Setting it incorrectly can lead to cyclic dependencies
///    and can cause the respective pipeline to stall.
/// 2. RunFunctions can also segfault if enough memory space is not available
///    on the sink for the buffers passed in. Buffers that are AddRef'd
///    need to be accounted for available memory space. In other
///    words, this memory is not available for use until it is freed up.
/// 3. Unexpected segmentation faults or erroneous behavior can occur if
///    handles or data passed in to Runfunction gets destroyed before the
///    RunFunction finishes.
///    For example, if a variable passed in as Misc data or the buffer gets
///    destroyed before the runtime receives the completion notification
///    of the Runfunction, it can cause unexpected behavior. So it is always
///    recommended to wait for RunFunction completion event before any related
///    destroy event occurs.
///
/// The runtime expects users to handle such scenarios. COIPipelineRunFunction
/// returns COI_SUCCESS for above cases because it was queued up successfully.
/// Also if you try to destroy a pipeline with a stalled function then the
/// destroy call will hang. COIPipelineDestroy waits until all the functions
/// enqueued are finished executing.
///
/// @param  in_Pipeline
///         [in] Handle to a previously created pipeline that this run
///         function should be enqueued to.
///
/// @param  in_Function
///         [in] Previously returned handle from a call to
///         COIPipelineGetFunctionHandle() that represents a function in the
///         application running on the Sink process.
///
/// @param  in_NumBuffers
///         [in] The number of buffers that are being passed to the run
///         function. This number must match the number of buffers in the
///         in_pBuffers and in_pBufferAccessFlags arrays. Must be less than
///         COI_PIPELINE_MAX_IN_BUFFERS.
///
/// @param  in_pBuffers
///         [in] An array of COIBUFFER handles that the function is expected
///         to use during its execution. Each buffer when it arrives at the
///         Sink process will be at least 4k page aligned, thus, using a very
///         large number of small buffers is memory inefficient and should be
///         avoided.
///
/// @param  in_pBufferAccessFlags
///         [in] An array of flag values which correspond to the buffers
///         passed in the in_pBuffers parameter. These flags are used to
///         track dependencies between different run functions being
///         executed from different pipelines.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the run function
///         to wait for any dependencies.
///
/// @param  in_pDependencies
///         [in] An optional array of COIEVENT objects that this run
///         function will wait for before executing. This allows the user to
///         create dependencies between run functions in different pipelines.
///         The user may pass in NULL if they do not wish to wait for any
///         dependencies to complete.
///
/// @param  in_pMiscData
///         [in] Pointer to user defined data, typically used to pass
///         parameters to Sink side functions. Should only be used for small
///         amounts data since the data will be placed directly in the
///         Driver's command buffer. COIBuffers should be used to pass large
///         amounts of data.
///
/// @param  in_MiscDataLen
///         [in] Size of the in_pMiscData in bytes. Must be less than
///         COI_PIPELINE_MAX_IN_MISC_DATA_LEN, and should usually be much
///         smaller, see documentation for the parameter in_pMiscData.
///
/// @param  out_pAsyncReturnValue
///         [out] Pointer to user-allocated memory where the return value from
///         the run function will be placed. This memory should not be read
///         until out_pCompletion has been signaled.
///
/// @param  in_AsyncReturnValueLen
///         [in] Size of the out_pAsyncReturnValue in bytes.
///
/// @param  out_pCompletion
///         [out] An optional pointer to a COIEVENT object
///         that will be signaled when this run function has completed
///         execution. The user may pass in NULL if they wish for this function
///         to be synchronous, otherwise if a COIEVENT object is passed in the
///         function is then asynchronous and closes after enqueuing the
///         RunFunction and passes back the COIEVENT that will be signaled
///         once the RunFunction has completed.
///
/// @return COI_SUCCESS if the function was successfully placed in a
///         pipeline for future execution. Note that the actual
///         execution of the function will occur in the future.
///
/// @return COI_OUT_OF_RANGE if in_NumBuffers is greater than
///         COI_PIPELINE_MAX_IN_BUFFERS or if in_MiscDataLen is greater than
///         COI_PIPELINE_MAX_IN_MISC_DATA_LEN.
///
/// @return COI_INVALID_HANDLE if the pipeline handle passed in was invalid.
///
/// @return COI_INVALID_HANDLE if the function handle passed in was invalid.
///
/// @return COI_INVALID_HANDLE if any of the buffers passed in are invalid.
///
/// @return COI_ARGUMENT_MISMATCH if in_NumDependencies is non-zero while
///         in_pDependencies was passed in as NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_pDependencies is non-NULL but
///         in_NumDependencies is zero.
///
/// @return COI_ARGUMENT_MISMATCH if in_MiscDataLen is non-zero while
///         in_pMiscData was passed in as NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_pMiscData is non-NULL but
///         in_MiscDataLen is zero.
///
/// @return COI_ARGUMENT_MISMATCH if in_NumBuffers is non-zero and in_pBuffers
///         or in_pBufferAccessFlags are NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_pBuffers is non-NULL but
///         in_NumBuffers is zero.
///
/// @return COI_ARGUMENT_MISMATCH if in_pBufferAccessFlags is non-NULL but
///         in_NumBuffers is zero.
///
/// @return COI_ARGUMENT_MISMATCH if in_ReturnValueLen is non-zero while
///         in_pReturnValue was passed in as NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_pReturnValue is non-NULL but
///         in_ReturnValueLen is zero.
///
/// @return COI_RETRY if any input buffers are still mapped when
///         passed to the run function.
///
/// @return COI_MISSING_DEPENDENCY if buffer was not created on the process
///         associated with the pipeline that was passed in.
///
/// @return COI_OUT_OF_RANGE if any of the access flags in
///         in_pBufferAccessFlags is not a valid COI_ACCESS_FLAGS.
///
COIACCESSAPI
COIRESULT
COIPipelineRunFunction(
    COIPIPELINE         in_Pipeline,
    COIFUNCTION         in_Function,
    uint32_t            in_NumBuffers,
    const   COIBUFFER          *in_pBuffers,
    const   COI_ACCESS_FLAGS   *in_pBufferAccessFlags,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    const   void               *in_pMiscData,
    uint16_t            in_MiscDataLen,
    void               *out_pAsyncReturnValue,
    uint16_t            in_AsyncReturnValueLen,
    COIEVENT           *out_pCompletion);


//////////////////////////////////////////////////////////////////////////////
///
/// Retrieve the engine that the pipeline is associated with.
///
/// @param  in_Pipeline
///         [in] Pipeline to query.
///
/// @param  out_pEngine
///         [out] The handle of the Engine.
///
/// @return COI_SUCCESS if the engine was retrieved.
///
/// @return COI_INVALID_HANDLE if the pipeline handle passed in was invalid.
///
/// @return COI_INVALID_POINTER if the out_pEngine parameter is NULL.
///
/// @return COI_PROCESS_DIED if the process associated with this engine died.
///
COIACCESSAPI
COIRESULT
COIPipelineGetEngine(
    COIPIPELINE         in_Pipeline,
    COIENGINE          *out_pEngine);

//////////////////////////////////////////////////////////////////////////////
///
/// Add a particular core:thread pair to a COI_CPU_MASK.
///
/// @param  in_Process
///         [in] A handle to an already existing process that the pipeline
///         will be associated with.
///
/// @param  in_CoreID
///         [in] Core to affinitize to; must be less than the number of cores
///         on the device.
///
/// @param  in_ThreadID
///         [in] Thread on the core to affinitize to (0 - 3).
///
/// @param  out_pMask
///         [out] Pointer to the mask to set.
///
/// @warning Unless it is explicitly done, the contents of the mask may not
///          be zero when creating or declaring a COI_CPU_MASK variable.
///
/// @return COI_SUCCESS if the mask was set.
///
/// @return COI_OUT_OF_RANGE if the in_CoreID or in_ThreadID is out of range.
///
/// @return COI_INVALID_POINTER if out_pMask is invalid.
///
/// @return COI_INVALID_HANDLE if in_Process is invalid.
///
COIACCESSAPI
COIRESULT
COIPipelineSetCPUMask(
    COIPROCESS          in_Process,
    uint32_t            in_CoreID,
    uint8_t             in_ThreadID,
    COI_CPU_MASK       *out_pMask);

//////////////////////////////////////////////////////////////////////////////
///
/// Clears a given mask. Note that the memory contents of COI_CPU_MASK are not
/// guaranteed to be zero when declaring a COI_CPU_MASK variable. Thus, prior
/// to setting a specific affinity to in_Mask it is important to call this
/// function first.
///
/// @param  in_Mask
///         [in] Pointer to the mask to clear.
///
/// @return COI_SUCCESS if the mask was cleared.
///
/// @return COI_INVALID_POINTER if in_Mask is invalid.
///
COIACCESSAPI
COIRESULT
COIPipelineClearCPUMask(
    COI_CPU_MASK       *in_Mask);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIPIPELINE_SOURCE_H */

/*! @} */
