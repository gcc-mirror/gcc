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

// Originally generated via:
//   cd include;
//   ctags -x --c-kinds=fp -R sink/ source/ common/ | grep -v COIX | awk '{print "__asm__(\".symver "$1"1,"$1"@@COI_1.0\");"}'
//
// These directives must have an associated linker script with VERSION stuff.
// See coi_version_linker_script.map
// Passed in as
//    -Wl,--version-script coi_version_linker_script.map
// when building Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
//
// See http://sourceware.org/binutils/docs/ld/VERSION.html#VERSION for more info
//
// This is not strictly a .h file, so no need to #pragma once or anything.
// You must include these asm directives in the same translation unit as the
// one where the function body is.
// Otherwise we'd have add this file to the list of files needed to build
// libcoi*, instead of including it in each of the api/*/*cpp files.
//
__asm__(".symver COIBufferAddRef1,COIBufferAddRef@@COI_1.0");
__asm__(".symver COIBufferCopy1,COIBufferCopy@@COI_1.0");
__asm__(".symver COIBufferCreate1,COIBufferCreate@@COI_1.0");
__asm__(".symver COIBufferCreateFromMemory1,COIBufferCreateFromMemory@@COI_1.0");
__asm__(".symver COIBufferDestroy1,COIBufferDestroy@@COI_1.0");
__asm__(".symver COIBufferGetSinkAddress1,COIBufferGetSinkAddress@@COI_1.0");
__asm__(".symver COIBufferMap1,COIBufferMap@@COI_1.0");
__asm__(".symver COIBufferRead1,COIBufferRead@@COI_1.0");
__asm__(".symver COIBufferReleaseRef1,COIBufferReleaseRef@@COI_1.0");
__asm__(".symver COIBufferSetState1,COIBufferSetState@@COI_1.0");
__asm__(".symver COIBufferUnmap1,COIBufferUnmap@@COI_1.0");
__asm__(".symver COIBufferWrite1,COIBufferWrite@@COI_1.0");
__asm__(".symver COIEngineGetCount1,COIEngineGetCount@@COI_1.0");
__asm__(".symver COIEngineGetHandle1,COIEngineGetHandle@@COI_1.0");
__asm__(".symver COIEngineGetIndex1,COIEngineGetIndex@@COI_1.0");
__asm__(".symver COIEngineGetInfo1,COIEngineGetInfo@@COI_1.0");
__asm__(".symver COIEventRegisterCallback1,COIEventRegisterCallback@@COI_1.0");
__asm__(".symver COIEventWait1,COIEventWait@@COI_1.0");
__asm__(".symver COIPerfGetCycleFrequency1,COIPerfGetCycleFrequency@@COI_1.0");
__asm__(".symver COIPipelineClearCPUMask1,COIPipelineClearCPUMask@@COI_1.0");
__asm__(".symver COIPipelineCreate1,COIPipelineCreate@@COI_1.0");
__asm__(".symver COIPipelineDestroy1,COIPipelineDestroy@@COI_1.0");
__asm__(".symver COIPipelineRunFunction1,COIPipelineRunFunction@@COI_1.0");
__asm__(".symver COIPipelineSetCPUMask1,COIPipelineSetCPUMask@@COI_1.0");
__asm__(".symver COIPipelineStartExecutingRunFunctions1,COIPipelineStartExecutingRunFunctions@@COI_1.0");
__asm__(".symver COIProcessCreateFromFile1,COIProcessCreateFromFile@@COI_1.0");
__asm__(".symver COIProcessCreateFromMemory1,COIProcessCreateFromMemory@@COI_1.0");
__asm__(".symver COIProcessDestroy1,COIProcessDestroy@@COI_1.0");
__asm__(".symver COIProcessGetFunctionHandles1,COIProcessGetFunctionHandles@@COI_1.0");
__asm__(".symver COIProcessLoadLibraryFromMemory2,COIProcessLoadLibraryFromMemory@COI_2.0");
__asm__(".symver COIProcessRegisterLibraries1,COIProcessRegisterLibraries@@COI_1.0");
__asm__(".symver COIProcessUnloadLibrary1,COIProcessUnloadLibrary@@COI_1.0");
__asm__(".symver COIProcessWaitForShutdown1,COIProcessWaitForShutdown@@COI_1.0");
