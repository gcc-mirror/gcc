/*
 * Copyright 2010-2013 Intel Corporation.
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

#ifndef _COIPROCESS_SINK_H
#define _COIPROCESS_SINK_H

/** @ingroup COIProcess
 *  @addtogroup COIProcessSink
@{
* @file sink/COIProcess_sink.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS

#include "../common/COITypes_common.h"
#include "../common/COIResult_common.h"

#ifdef __cplusplus
extern "C" {
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS

//////////////////////////////////////////////////////////////////////////////
///
/// This call will block while waiting for the source to send a process destroy
/// message. This provides the sink side application with an event to keep the
/// main() function from exiting until it is directed to by the source. When
/// the shutdown message is received this function will stop any future run
/// functions from executing but will wait for any current run functions to
/// complete. All Intel® Coprocessor Offload Infrastructure (Intel® COI)  resources will be cleaned up and no additional Intel® Coprocessor Offload Infrastructure (Intel® COI)  APIs
/// should be called after this function returns.  This function does not
/// invoke exit() so the application can perform any of its own cleanup once
/// this call returns.
///
/// @return COI_SUCCESS once the process receives the shutdown message.
///
COIRESULT
COIProcessWaitForShutdown();

//////////////////////////////////////////////////////////////////////////////
///
/// This call will block until all stdout and stderr output has been proxied
/// to and written by the source. This call guarantees that any output in a
/// run function is transmitted to the source before the run function signals
/// its completion event back to the source.
///
/// Note that having an additional thread printing forever while another
/// calls COIProxyFlush may lead to a hang because the process will be forced
/// to wait until all that output can be flushed to the source before returning
/// from this call.
///
/// @return COI_SUCCESS once the proxy output has been flushed to and written
///         written by the host. Note that Intel® Coprocessor Offload Infrastructure (Intel® COI)  on the source writes to stdout
///         and stderr, but does not flush this output.
/// @return COI_SUCCESS if the process was created without enabling
///         proxy IO this function.
///
COIRESULT
COIProcessProxyFlush();

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIPROCESS_SINK_H */

/*! @} */
