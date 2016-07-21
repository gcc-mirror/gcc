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

#ifndef _COIBUFFER_SINK_H
#define _COIBUFFER_SINK_H

/** @ingroup COIBuffer
 *  @addtogroup COIBufferSink
@{

* @file sink\COIBuffer_sink.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    #include "../common/COITypes_common.h"
    #include "../common/COIResult_common.h"
#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifdef __cplusplus
extern "C" {
#endif

//////////////////////////////////////////////////////////////////////////////
///
/// Adds a reference to the memory of a buffer. The memory of the buffer
/// will remain on the device until both a corresponding COIBufferReleaseRef()
/// call is made and the run function that delivered the buffer returns.
///
/// Running this API in a thread spawned within the run function is not
/// supported and will cause unpredictable results and may cause data corruption.
///
/// @warning 1.It is possible for enqueued run functions to be unable to
///            execute due to all card memory being occupied by AddRef'd
///            buffers. As such, it is important that whenever a buffer is
///            AddRef'd that there be no dependencies on future run functions
///            for progress to be made towards releasing the buffer.
///          2.It is important that AddRef is called within the scope of
///            run function that carries the buffer to be AddRef'd.
///
/// @param  in_pBuffer
///         [in] Pointer to the start of a buffer being AddRef'd, that was
///         passed in at the start of the run function.
///
/// @return COI_SUCCESS if the buffer ref count was successfully incremented.
///
/// @return COI_INVALID_POINTER if the buffer pointer is NULL.
///
/// @return COI_INVALID_HANDLE if the buffer pointer is invalid.
///
COIRESULT
COIBufferAddRef(
    void           *in_pBuffer);


//////////////////////////////////////////////////////////////////////////////
///
/// Removes a reference to the memory of a buffer. The memory of the buffer
/// will be eligible for being freed on the device when the following
/// conditions are met: the run function that delivered the buffer
/// returns, and the number of calls to COIBufferReleaseRef() matches the
/// number of calls to COIBufferAddRef().
//
/// Running this API in a thread spawned within the run function is not
/// supported and will cause unpredictable results and may cause data corruption.
///
/// @warning When a buffer is AddRef'd it is assumed that it is in use and all
///          other operations on that buffer waits for ReleaseRef() to happen.
///          So you cannot pass the AddRef'd buffer's handle to RunFunction
///          that calls ReleaseRef(). This is a circular dependency and will
///          cause a deadlock. Buffer's pointer (buffer's sink side
///          address/pointer which is different than source side BUFFER handle)
///          needs to be stored somewhere to retrieve it later to use in
///          ReleaseRef.
///
/// @param  in_pBuffer
///         [in] Pointer to the start of a buffer previously AddRef'd, that
///         was passed in at the start of the run function.
///
/// @return COI_SUCCESS if the buffer refcount was successfully decremented.
///
/// @return COI_INVALID_POINTER if the buffer pointer was invalid.
///
/// @return COI_INVALID_HANDLE if the buffer did not have COIBufferAddRef()
///         previously called on it.
///
COIRESULT
COIBufferReleaseRef(
    void           *in_pBuffer);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIBUFFER_SINK_H */

/*! @} */
