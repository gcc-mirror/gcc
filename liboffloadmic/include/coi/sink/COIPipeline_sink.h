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

#ifndef _COIPIPELINE_SINK_H
#define _COIPIPELINE_SINK_H

/** @ingroup COIPipeline
 *  @addtogroup COIPipelineSink
@{
* @file sink/COIPipeline_sink.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS

#include "../common/COITypes_common.h"
#include "../common/COIResult_common.h"

#ifdef __FreeBSD__
    #define COINATIVELIBEXPORT_VISIBILITY "extern"
#else
    #define COINATIVELIBEXPORT_VISIBILITY "default"
#endif

#ifdef __cplusplus
#define COINATIVELIBEXPORT \
    extern "C" __attribute__ ((visibility(COINATIVELIBEXPORT_VISIBILITY)))
#else
#define COINATIVELIBEXPORT \
    __attribute__ ((visibility(COINATIVELIBEXPORT_VISIBILITY)))
#endif

#ifdef __cplusplus
extern "C" {
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS

//////////////////////////////////////////////////////////////////////////////
///
/// This is the prototype that run functions should follow.
///
/// @param   in_BufferCount
///          The number of buffers passed to the run function.
///
/// @param   in_ppBufferPointers
///          An array that is in_BufferCount in length that contains the
///          sink side virtual addresses for each buffer passed in to
///          the run function.
///
/// @param   in_pBufferLengths
///          An array that is in_BufferCount in length of uint32_t integers
///          describing the length of each passed in buffer in bytes.
///
/// @param   in_pMiscData
///          Pointer to the MiscData passed in when the run function
///          was enqueued on the source.
///
/// @param   in_MiscDataLen
///          Length in bytes of the MiscData passed in when the run function
///          was enqueued on the source.
///
/// @param   in_pReturnValue
///          Pointer to the location where the return value from this run
///          function will be stored.
///
/// @param   in_ReturnValueLength
///          Length in bytes of the user-allocated ReturnValue pointer.
///
/// @return  A uint64_t that can be retrieved in the out_UserData parameter
///          from the COIPipelineWaitForEvent function.
///
typedef void
(*RunFunctionPtr_t)(
    uint32_t        in_BufferCount,
    void          **in_ppBufferPointers,
    uint64_t       *in_pBufferLengths,
    void           *in_pMiscData,
    uint16_t        in_MiscDataLength,
    void           *in_pReturnValue,
    uint16_t        in_ReturnValueLength);

///////////////////////////////////////////////////////////////////////////////
///
/// Start processing pipelines on the Sink. This should be done after any
/// required initialization in the Sink's application has finished. No
/// run functions will actually be executed (although they may be queued)
/// until this function is called.
///
///
/// @return COI_SUCCESS if the pipelines were successfully started.
///
COIRESULT
COIPipelineStartExecutingRunFunctions();


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIPIPELINE_SINK_H */

/*! @} */
