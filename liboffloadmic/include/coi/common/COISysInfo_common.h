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

#ifndef _COISYSINFO_COMMON_H
#define _COISYSINFO_COMMON_H

/** @ingroup COISysInfo
 *  @addtogroup COISysInfoCommon
@{
* @file common/COISysInfo_common.h
* This interface allows developers to query the platform for system level
* information. */

#ifndef DOXYGEN_SHOULD_SKIP_THIS
#include "../common/COITypes_common.h"
#include <assert.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS

#define INITIAL_APIC_ID_BITS 0xFF000000   // EBX[31:24] unique APIC ID

///////////////////////////////////////////////////////////////////////////////
/// \fn uint32_t COISysGetAPICID(void)
/// @return The Advanced Programmable Interrupt Controller (APIC) ID of
/// the hardware thread on which the caller is running.
///
/// @warning APIC IDs are unique to each hardware thread within a processor,
/// but may not be sequential.
COIACCESSAPI
uint32_t COISysGetAPICID(void);

///////////////////////////////////////////////////////////////////////////////
///
/// @return The number of cores exposed by the processor on which the caller is
/// running. Returns 0 if there is an error loading the processor info.
COIACCESSAPI
uint32_t COISysGetCoreCount(void);

///////////////////////////////////////////////////////////////////////////////
///
/// @return The number of hardware threads exposed by the processor on which
/// the caller is running. Returns 0 if there is an error loading processor
/// info.
COIACCESSAPI
uint32_t COISysGetHardwareThreadCount(void);

///////////////////////////////////////////////////////////////////////////////
///
/// @return The index of the hardware thread on which the caller is running.
///
/// The indexes of neighboring hardware threads will differ by a value of one
/// and are within the range zero through COISysGetHardwareThreadCount()-1.
/// Returns ((uint32_t)-1) if there was an error loading processor info.
COIACCESSAPI
uint32_t COISysGetHardwareThreadIndex(void);

///////////////////////////////////////////////////////////////////////////////
///
/// @return The index of the core on which the caller is running.
///
/// The indexes of neighboring cores will differ by a value of one and are
/// within the range zero through COISysGetCoreCount()-1. Returns ((uint32_t)-1)
/// if there was an error loading processor info.
COIACCESSAPI
uint32_t COISysGetCoreIndex(void);

///////////////////////////////////////////////////////////////////////////////
///
/// @return The number of level 2 caches within the processor on which the
/// caller is running. Returns ((uint32_t)-1) if there was an error loading
/// processor info.
COIACCESSAPI
uint32_t COISysGetL2CacheCount(void);

///////////////////////////////////////////////////////////////////////////////
///
/// @return The index of the level 2 cache on which the caller is running.
/// Returns ((uint32_t)-1) if there was an error loading processor info.
///
/// The indexes of neighboring cores will differ by a value of one and are
/// within the range zero through COISysGetL2CacheCount()-1.
COIACCESSAPI
uint32_t COISysGetL2CacheIndex(void);

#ifdef __cplusplus
} /* extern "C" */
#endif
/*! @} */

#endif /* _COISYSINFO_COMMON_H */
