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

#ifndef _COIENGINE_SOURCE_H
#define _COIENGINE_SOURCE_H

/** @ingroup COIEngine
 *  @addtogroup COIEngineSource
@{

* @file source\COIEngine_source.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    #include <wchar.h>
    #include "../common/COITypes_common.h"
    #include "../common/COIResult_common.h"
    #include "../common/COIEngine_common.h"
#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifdef __cplusplus
extern "C" {
#endif

#define COI_MAX_DRIVER_VERSION_STR_LEN 255

#define COI_MAX_HW_THREADS 1024

///////////////////////////////////////////////////////////////////////////////
/// This enum defines miscellaneous information returned from the
/// COIGetEngineInfo() function.
///
typedef enum
{
    COI_ENG_ECC_DISABLED = 0,            //ECC is not enabled on this engine
    COI_ENG_ECC_ENABLED = 0x00000001,    //ECC is enabled on this engine
    COI_ENG_ECC_UNKNOWN = 0x00000002     //ECC is mode is unknown
} coi_eng_misc;


///////////////////////////////////////////////////////////////////////////////
/// This structure returns information about an Intel(R) Xeon Phi(TM)
/// coprocessor.
/// A pointer to this structure is passed into the COIGetEngineInfo() function,
/// which fills in the data before returning to the caller.
///
typedef struct COI_ENGINE_INFO
{
    /// The version string identifying the driver.
    coi_wchar_t  DriverVersion[COI_MAX_DRIVER_VERSION_STR_LEN];

    /// The DeviceType supported by the engine.
    COI_DEVICE_TYPE ISA;

    /// The number of cores on the engine.
    uint32_t     NumCores;

    /// Miscellaneous fields
    coi_eng_misc MiscFlags;

    /// The number of hardware threads on the engine.
    uint32_t     NumThreads;

    /// The maximum frequency (in MHz) of the cores on the engine.
    uint32_t     CoreMaxFrequency;

    /// The load percentage for each of the hardware threads on the engine.
    /// Currently this is limited to reporting out a maximum of 1024 HW threads
    uint32_t     Load[COI_MAX_HW_THREADS];

    /// The amount of physical memory managed by the OS.
    uint64_t     PhysicalMemory;

    /// The amount of free physical memory in the OS.
    uint64_t     PhysicalMemoryFree;

    /// The amount of swap memory managed by the OS.
    uint64_t     SwapMemory;

    /// The amount of free swap memory in the OS.
    uint64_t     SwapMemoryFree;

    /// The pci config vendor id
    uint16_t     VendorId;

    /// The pci config device id
    uint16_t     DeviceId;

    /// The pci config subsystem id
    uint16_t     SubSystemId;

    /// The stepping of the board, A0, A1, C0, D0 etc.
    uint16_t     BoardStepping;

    /// The SKU of the stepping, EB, ED, etc.
    uint16_t     BoardSKU;
} COI_ENGINE_INFO;

///////////////////////////////////////////////////////////////////////////////
///
/// Returns information related to a specified engine. Note that if the runtime
/// is unable to query a value it will be returned as zero but the call will
/// still succeed.
///
///
/// @param  in_EngineHandle
///         [in] The COIENGINE structure as provided from COIEngineGetHandle()
///         which to query for device level information.
///
/// @param  in_EngineInfoSize
///         [in] The size of the structure that out_pEngineInfo points to.
///         Used for version safety of the function call.
///
/// @param  out_pEngineInfo
///         [out] The address of a user allocated COI_ENGINE_INFO structure.
///         Upon success, the contents of the structure will be updated
///         to contain information related to the specified engine.
///
///
/// @return  COI_SUCCESS if the function completed without error.
///
/// @return  COI_INVALID_HANDLE if the in_EngineHandle handle is not valid.
///
/// @return  COI_SIZE_MISMATCH if in_EngineInfoSize does not match any current
///          or previous COI_ENGINE_INFO structure sizes.
///
/// @return  COI_INVALID_POINTER if the out_pEngineInfo pointer is NULL.
///
COIACCESSAPI
COIRESULT
COIEngineGetInfo(
    COIENGINE           in_EngineHandle,
    uint32_t            in_EngineInfoSize,
    COI_ENGINE_INFO    *out_pEngineInfo);


///////////////////////////////////////////////////////////////////////////////
///
/// Returns the number of engines in the system that match the provided device type.
///
/// The number of available coprocessor devices (i.e. cards connected via PCIe)
/// is detected by the COI runtime.
///
/// @param  in_DeviceType
///         [in] Specifies the ISA type of the engine requested.
///
/// @param  out_pNumEngines
///         [out] The number of engines available. This can be used to index
///         into the engines using COIEngineGetHandle().
///
/// @return COI_SUCCESS if the function completed without error.
///
/// @return COI_DOES_NOT_EXIST if the in_DeviceType parameter is not valid.
///
/// @return COI_INVALID_POINTER if the out_pNumEngines parameter is NULL.
///
/// @return COI_OUT_OF_RANGE if number of selected devices is greater than 8.
///
COIACCESSAPI
COIRESULT
COIEngineGetCount(
    COI_DEVICE_TYPE in_DeviceType,
    uint32_t       *out_pNumEngines);


///////////////////////////////////////////////////////////////////////////////
///
/// Returns the handle of a user specified engine.
///
/// @param  in_DeviceType
///         [in] Specifies the ISA type of the engine requested.
///
/// @param  in_EngineIndex
///         [in] A unsigned integer which specifies the zero-based position of
///         the engine in a collection of engines. The makeup of this
///         collection is defined by the in_DeviceType parameter.
///
/// @param  out_pEngineHandle
///         [out] The address of a COIENGINE handle.
///
/// @return COI_SUCCESS if the function completed without error.
///
/// @return COI_DOES_NOT_EXIST if the in_DeviceType parameter is not valid.
///
/// @return COI_OUT_OF_RANGE if in_EngineIndex is greater than or equal to
///         the number of engines that match the in_DeviceType parameter.
///
/// @return COI_INVALID_POINTER if the out_pEngineHandle parameter is NULL.
///
/// @return COI_VERSION_MISMATCH if the version of Intel(R) Coprocessor Offload
///         Infrastructure (Intel(R) COI) on the host is not
///         compatible with the version on the device.
///
/// @return COI_NOT_INITIALIZED if the engine requested exists but is offline.
///
COIACCESSAPI
COIRESULT
COIEngineGetHandle(
    COI_DEVICE_TYPE in_DeviceType,
    uint32_t        in_EngineIndex,
    COIENGINE      *out_pEngineHandle);

///////////////////////////////////////////////////////////////////////////////
///
/// Returns the hostname for a specified COIEngine.
///
/// @param  in_EngineHandle
///         [in] The connected COI Engine Handle passed in by the user that is
///         used to request the hostname of the device connected by this COIEngine.
///
/// @param  out_Hostname
///         [out] The hostname of the device connected by this COIEngine.
///         COI will write at most 4096 bytes and the user must make sure that the size
///         of the memory pointed by this argument is large enough.
///
/// @return COI_SUCCESS if the hostname was retrieved without error.
///
/// @return COI_ERROR if the function was unable to retrieve the hostname and/or
///         the retrieved out_Hostname is NULL.
///
/// @return COI_INVALID_HANDLE if the in_EngineHandle is invalid.
///
/// @return COI_INVALID_POINTER if the out_Hostname is NULL.
///
COIACCESSAPI
COIRESULT
COIEngineGetHostname(
    COIENGINE in_EngineHandle,
    char     *out_Hostname);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIENGINE_SOURCE_H */

/*! @} */
