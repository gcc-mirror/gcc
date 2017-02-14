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
/// complete. All Intel® Coprocessor Offload Infrastructure (Intel® COI)
/// resources will be cleaned up and no additional Intel® Coprocessor Offload
/// Infrastructure (Intel® COI) APIs should be called after this function
/// returns. This function does not invoke exit() so the application
/// can perform any of its own cleanup once this call returns.
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
///         written by the host. Note that Intel® Coprocessor Offload
///         Infrastructure (Intel® COI) on the source writes to stdout and
///         stderr, but does not flush this output.
/// @return COI_SUCCESS if the process was created without enabling
///         proxy IO this function.
///
COIRESULT
COIProcessProxyFlush();


//////////////////////////////////////////////////////////////////////////////
///
/// Loads a shared library from host filesystem into the current sink
/// process, akin to using dlopen() on a local process in Linux or
/// LoadLibrary() in Windows.
///
/// @param  in_pFileName
///         [in] The name of the shared library file on the source's file
///         system that is being loaded. If the file name is not an absolute
///         path, the file is searched for in the same manner as dependencies.
///
/// @param  in_pLibraryName
///         [in] Name for the shared library. This optional parameter can
///         be specified in case the dynamic library doesn't have an
///         SO_NAME field. If specified, it will take precedence over
///         the SO_NAME if it exists. If it is not specified then
///         the library must have a valid SO_NAME field.
///
///@param   in_LibrarySearchPath
///         [in] a path to locate dynamic libraries dependencies for the
///         library being loaded. If not NULL, this path will override the
///         environment variable SINK_LD_LIBRARY_PATH. If NULL it will use
///         SINK_LD_LIBRARY_PATH to locate dependencies.
///
/// @param  in_Flags
///         [in] Bitmask of the flags that will be passed in as the dlopen()
///         "flag" parameter on the sink.
///
/// @param  out_pLibrary
///         [out] If COI_SUCCESS or COI_ALREADY_EXISTS is returned, the handle
///         that uniquely identifies the loaded library.
///
/// @return COI_SUCCESS if the library was successfully loaded.
///
/// @return COI_INVALID_POINTER if in_pFileName is NULL.
///
/// @return COI_DOES_NOT_EXIST if in_pFileName cannot be found.
///
/// @return COI_INVALID_FILE if the file is not a valid shared library.
///
/// @return COI_MISSING_DEPENDENCY if a dependent library is missing from
///         either SINK_LD_LIBRARY_PATH or the in_LibrarySearchPath parameter.
///
/// @return COI_ARGUMENT_MISMATCH if the shared library is missing an SONAME
///         and in_pLibraryName is NULL.
///
/// @return COI_UNDEFINED_SYMBOL if we are unable to load the library due to
///         an undefined symbol.
///
/// @return COI_ALREADY_EXISTS if there is an existing COILIBRARY handle
///         that identifies this library, and this COILIBRARY hasn't been
///         unloaded yet.
///
/// @return COI_BINARY_AND_HARDWARE_MISMATCH if the target machine of the
///         binary or any of its recursive dependencies does not match the
///         engine associated with Process.
///
/// @return COI_NOT_INITIALIZED if setup of remote process on host is not
///         completed yet.
///
COIRESULT
COIProcessLoadSinkLibraryFromFile(
    const   char               *in_pFileName,
    const   char               *in_pLibraryName,
    const   char               *in_LibrarySearchPath,
    uint32_t            in_Flags,
    COILIBRARY         *out_pLibrary);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIPROCESS_SINK_H */

/*! @} */
