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

#ifndef _COIRESULT_COMMON_H
#define _COIRESULT_COMMON_H

/** @ingroup COIResult
 *  @addtogroup COIResultCommon
@{

* @file common/COIResult_common.h
* Result codes and definitions. */

#include "../common/COITypes_common.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum COIRESULT
{
    COI_SUCCESS = 0,                  ///< The function succeeded without error.
    COI_ERROR,                        ///< Unspecified error.
    COI_NOT_INITIALIZED,              ///< The function was called before the
    ///< system was initialized.
    COI_ALREADY_INITIALIZED,          ///< The function was called after the
    ///< system was initialized.
    COI_ALREADY_EXISTS,               ///< Cannot complete the request due to
    ///< the existence of a similar object.
    COI_DOES_NOT_EXIST,               ///< The specified object was not found.
    COI_INVALID_POINTER,              ///< One of the provided addresses was not
    ///< valid.
    COI_OUT_OF_RANGE,                 ///< One of the arguments contains a value
    ///< that is invalid.
    COI_NOT_SUPPORTED,                ///< This function is not currently
    ///< supported as used.
    COI_TIME_OUT_REACHED,             ///< The specified time out caused the
    ///< function to abort.
    COI_MEMORY_OVERLAP,               ///< The source and destination range
    ///< specified overlaps for the same
    ///< buffer.
    COI_ARGUMENT_MISMATCH,            ///< The specified arguments are not
    ///< compatible.
    COI_SIZE_MISMATCH,                ///< The specified size does not match the
    ///< expected size.
    COI_OUT_OF_MEMORY,                ///< The function was unable to allocate
    ///< the required memory.
    COI_INVALID_HANDLE,               ///< One of the provided handles was not
    ///< valid.
    COI_RETRY,                        ///< This function currently can't
    ///< complete, but might be able to later.
    COI_RESOURCE_EXHAUSTED,           ///< The resource was not large enough.
    COI_ALREADY_LOCKED,               ///< The object was expected to be
    ///< unlocked, but was locked.
    COI_NOT_LOCKED,                   ///< The object was expected to be locked,
    ///< but was unlocked.
    COI_MISSING_DEPENDENCY,           ///< One or more dependent components
    ///< could not be found.
    COI_UNDEFINED_SYMBOL,             ///< One or more symbols the component
    ///< required was not defined in any
    ///< library.
    COI_PENDING,                      ///< Operation is not finished
    COI_BINARY_AND_HARDWARE_MISMATCH, ///< A specified binary will not run on
    ///< the specified hardware.
    COI_PROCESS_DIED,
    COI_INVALID_FILE,                 ///< The file is invalid for its intended
    ///< usage in the function.
    COI_EVENT_CANCELED,               ///< Event wait on a user event that
    ///< was unregistered or is being
    ///< unregistered returns
    ///< COI_EVENT_CANCELED.
    COI_VERSION_MISMATCH,             ///< The version of Intel(R) Coprocessor
    ///< Offload Infrastructure on the host
    ///< is not compatible with the version
    ///< on the device.
    COI_BAD_PORT,                     ///< The port that the host is set to
    ///< connect to is invalid.
    COI_AUTHENTICATION_FAILURE,       ///< The daemon was unable to authenticate
    ///< the user that requested an engine.
    ///< Only reported if daemon is set up for
    ///< authorization. Is also reported in
    ///< Windows if host can not find user.
    COI_COMM_NOT_INITIALIZED,         ///< The function was called before the
    ///< comm was initialized.
    COI_INCORRECT_FORMAT,             ///< Format of data is incorrect
    COI_NUM_RESULTS                   ///< Reserved, do not use.
} COIRESULT;

//////////////////////////////////////////////////////////////////////////////
///
/// Returns the string version of the passed in COIRESULT. Thus if
/// COI_RETRY is passed in, this function returns the string "COI_RETRY". If
/// the error code passed ins is not valid then "COI_ERROR" will be returned.
///
/// @param in_ResultCode
///        [in] COIRESULT code to return the string version of.
///
/// @return String version of the passed in COIRESULT code.
///
COIACCESSAPI
const char *
COIResultGetName(
    COIRESULT       in_ResultCode);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIRESULT_COMMON_H */

/*! @} */
