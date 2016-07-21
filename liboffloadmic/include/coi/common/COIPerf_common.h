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

#ifndef _COIPERF_COMMON_H
#define _COIPERF_COMMON_H

/** @ingroup COIPerf
 *  @addtogroup COIPerfCommon
@{

* @file common/COIPerf_common.h
* Performance Analysis API */
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#include "../common/COITypes_common.h"

#ifdef __cplusplus
extern "C" {
#endif

#endif // DOXYGEN_SHOULD_SKIP_THIS

///////////////////////////////////////////////////////////////////////////////
///
/// Returns a performance counter value
///
/// This function returns a performance counter value that increments
/// at a constant rate for all time and is coherent across all cores.
///
/// @return Current performance counter value or 0 if no performance counter
/////         is available
///
///
COIACCESSAPI
uint64_t COIPerfGetCycleCounter(void);

///////////////////////////////////////////////////////////////////////////////
///
/// Returns the calculated system frequency in hertz.
///
/// @return Current system frequency in hertz.
///
COIACCESSAPI
uint64_t COIPerfGetCycleFrequency(void);

#ifdef __cplusplus
} /* extern "C" */
#endif
/*! @} */

#endif /* _COIPERF_COMMON_H */
