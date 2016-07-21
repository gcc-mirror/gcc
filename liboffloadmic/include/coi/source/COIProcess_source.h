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

#ifndef _COIPROCESS_SOURCE_H
#define _COIPROCESS_SOURCE_H

/** @ingroup COIProcess
 *  @addtogroup COIProcessSource
@{
* @file source/COIProcess_source.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS

#include "../common/COITypes_common.h"
#include "../common/COIResult_common.h"

#ifdef __cplusplus
extern "C" {
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS


///////////////////////////////////////////////////////////////////////////////
/// This is a special COIPROCESS handle that can be used to indicate that
/// the source process should be used for an operation.
///
#define COI_PROCESS_SOURCE ((COIPROCESS)-1)

#define COI_MAX_FILE_NAME_LENGTH 256

///////////////////////////////////////////////////////////////////////////////
/// This is a flag for COIProcessCreateFromMemory that indicates the passed in
/// memory pointer is a fat binary file and should not have regular validation.
///
#define COI_FAT_BINARY ((uint64_t)-1)

///////////////////////////////////////////////////////////////////////////////
///
/// Create a remote process on the Sink and start executing its main()
/// function.
///
/// For more details about creating a process see COIProcessCreateFromMemory.
///
/// @param  in_Engine
///         [in] A handle retrieved via a call to COIEngineGetHandle() that
///         indicates which device to create the process on. This is
///         necessary because there can be more than one device
///         within the system.
///
/// @param  in_pBinaryName
///         [in] Pointer to a null-terminated string that contains the
///         path to the program binary to be instantiated as a process on
///         the sink device. The file name will be accessed via
///         fopen and fread, as such, the passed in binary name must
///         be locatable via these commands. Also, the file name (without
///         directory information) will be used automatically by the system
///         to create the argv[0] of the new process.
///
/// @param  in_Argc
///         [in] The number of arguments being passed in to the process in the
///         in_ppArgv parameter.
///
/// @param  in_ppArgv
///         [in] An array of strings that represent the arguments being passed
///         in. The system will auto-generate argv[0] using in_pBinaryName and
///         thus that parameter cannot be passed in using in_ppArgv. Instead,
///         in_ppArgv contains the rest of the parameters being passed in.
///
/// @param  in_DupEnv
///         [in] A boolean that indicates whether the process that is being
///         created should inherit the environment of the caller.
///
/// @param  in_ppAdditionalEnv
///         [in] An array of strings that represent additional environment
///         variables. This parameter must terminate the array with a NULL
///         string. For convenience it is also allowed to be NULL if there are
///         no additional environment variables that need adding. Note that
///         any environment variables specified here will be in addition to
///         but override those that were inherited via in_DupEnv.
///
/// @param  in_ProxyActive
///         [in] A boolean that specifies whether the process that is to be
///         created wants I/O proxy support. If this flag is enabled, then
///         stdout and stderr are forwarded back to the calling process's
///         output and error streams.
///
/// @param  in_Reserved
///         Reserved for future use, best set at NULL.
///
/// @param  in_InitialBufferSpace
///         [in] The initial memory (in bytes) that will be pre-allocated at
///         process creation for use by buffers associated with this remote
///         process. In addition to allocating, Intel(R) Coprocessor Offload
///         Infrastructure (Intel(R) COI) will also fault in the
///         memory during process creation. If the total size of the buffers
///         in use by this process exceed this initial size, memory on the
///         sink may continue to be allocated on demand, as needed, subject
///         to the system constraints on the sink.
///
///@param   in_LibrarySearchPath
///         [in] a path to locate dynamic libraries dependencies for the sink
///         application. If not NULL, this path will override the environment
///         variable SINK_LD_LIBRARY_PATH. If NULL it will use
///         SINK_LD_LIBRARY_PATH to locate dependencies.
///
/// @param  out_pProcess
///         [out] Handle returned to uniquely identify the process that was
///         created for use in later API calls.
///
/// @return COI_SUCCESS if the remote process was successfully created.
///
/// @return COI_INVALID_POINTER if in_pBinaryName was NULL.
///
/// @return COI_INVALID_FILE if in_pBinaryName is not a "regular file" as
///         determined by stat or if its size is 0.
///
/// @return COI_DOES_NOT_EXIST if in_pBinaryName cannot be found.
///
/// @return See COIProcessCreateFromMemory for additional errors.
///
COIACCESSAPI
COIRESULT
COIProcessCreateFromFile(
    COIENGINE           in_Engine,
    const   char               *in_pBinaryName,
    int                 in_Argc,
    const   char              **in_ppArgv,
    uint8_t             in_DupEnv,
    const   char              **in_ppAdditionalEnv,
    uint8_t             in_ProxyActive,
    const   char               *in_Reserved,
    uint64_t            in_InitialBufferSpace,
    const   char               *in_LibrarySearchPath,
    COIPROCESS         *out_pProcess);

///////////////////////////////////////////////////////////////////////////////
///
/// Create a remote process on the Sink and start executing its main()
/// function. This will also automatically load any dependent shared objects
/// on to the device. Once the process is created, remote calls can be
/// initiated by using the RunFunction mechanism found in the COIPipeline APIs.
///
/// If instead of creating a process you only wish to check for dynamic
/// library dependencies set the environment variable
/// SINK_LD_TRACE_LOADED_OBJECTS to be non empty before making this call.
///
/// If there are dynamic link libraries on the source file system that need to
/// be preloaded when the process is created on the device, callers of this
/// API can set the environment variable SINK_LD_PRELOAD to a colon separated
/// list of libraries that need to be copied to the sink and preloaded as part
/// of process creation.
///
/// For more information on how dependencies are loaded, see
/// COIProcessLoadLibraryFromMemory.
///
/// @param  in_Engine
///         [in] A handle retrieved via a call to COIEngineGetHandle() that
///         indicates which device to create the process on. This is
///         necessary because there can be more than one device
///         within the system.
///
/// @param  in_pBinaryName
///         [in] Pointer to a null-terminated string that contains the name to
///         give the process that will be created. Note that the final name
///         will strip out any directory information from in_pBinaryName and
///         use the file information to generate an argv[0] for the new
///         process.
///
/// @param  in_pBinaryBuffer
///         [in] Pointer to a buffer whose contents represent the sink-side
///         process that we want to create.
///
/// @param  in_BinaryBufferLength
///         [in] Number of bytes in in_pBinaryBuffer.
///
/// @param  in_Argc
///         [in] The number of arguments being passed in to the process in the
///         in_ppArgv parameter.
///
/// @param  in_ppArgv
///         [in] An array of strings that represent the arguments being passed
///         in. The system will auto-generate argv[0] using in_pBinaryName and
///         thus that parameter cannot be passed in using in_ppArgv. Instead,
///         in_ppArgv contains the rest of the parameters being passed in.
///
/// @param  in_DupEnv
///         [in] A boolean that indicates whether the process that is being
///         created should inherit the environment of the caller.
///
/// @param  in_ppAdditionalEnv
///         [in] An array of strings that represent additional environment
///         variables. This parameter must terminate the array with a NULL
///         string. For convenience it is also allowed to be NULL if there are
///         no additional environment variables that need adding. Note that
///         any environment variables specified here will be in addition to
///         but override those that were inherited via in_DupEnv.
///
/// @param  in_ProxyActive
///         [in] A boolean that specifies whether the process that is to be
///         created wants I/O proxy support.
///
/// @param  in_Reserved
///         Reserved for future use, best set to NULL.
///
/// @param  in_InitialBufferSpace
///         [in] The initial memory (in bytes) that will be pre-allocated at
///         process creation for use by buffers associated with this remote
///         process. In addition to allocating, Intel(R) Coprocessor
///         Offload Infrastructure (Intel(R) COI) will also fault in the
///         memory during process creation. If the total size of the buffers
///         in use by this process exceed this initial size, memory on the
///         sink may continue to be allocated on demand, as needed, subject
///         to the system constraints on the sink.
///
/// @param  in_LibrarySearchPath
///         [in] A path to locate dynamic libraries dependencies for the sink
///         application. If not NULL, this path will override the environment
///         variable SINK_LD_LIBRARY_PATH. If NULL it will use
///         SINK_LD_LIBRARY_PATH to locate dependencies.
///
/// @param  in_FileOfOrigin
///         [in] If not NULL, this parameter indicates the file from which the
///         in_pBinaryBuffer was obtained. This parameter is optional.
///
/// @param  in_FileOfOriginOffset
///         [in] If in_FileOfOrigin is not NULL, this parameter indicates the
///         offset within that file where in_pBinaryBuffer begins.
///
/// @param  out_pProcess
///         [out] Handle returned to uniquely identify the process that was
///         created for use in later API calls.
///
/// @return COI_SUCCESS if the remote process was successfully created.
///
/// @return COI_INVALID_HANDLE if the in_Engine handle passed in was invalid.
///
/// @return COI_INVALID_POINTER if out_pProcess was NULL.
///
/// @return COI_INVALID_POINTER if in_pBinaryName or in_pBinaryBuffer was NULL.
///
/// @return COI_MISSING_DEPENDENCY if a dependent library is missing from
///         either SINK_LD_LIBRARY_PATH or the in_LibrarySearchPath parameter.
///
/// @return COI_BINARY_AND_HARDWARE_MISMATCH if in_pBinaryName or any of its
///         recursive dependencies were built for a target machine that does
///         not match the engine specified.
///
/// @return COI_RESOURCE_EXHAUSTED if no more COIProcesses can be created,
///         possibly, but not necessarily because in_InitialBufferSpace is too
///         large.
///
/// @return COI_ARGUMENT_MISMATCH if in_Argc is 0 and in_ppArgv is not NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_Argc is greater than 0 and in_ppArgv is
///         NULL.
///
/// @return COI_OUT_OF_RANGE if in_Argc is less than 0.
///
/// @return COI_OUT_OF_RANGE if the length of in_pBinaryName is greater than or
///         equal to COI_MAX_FILE_NAME_LENGTH.
///
/// @return COI_OUT_OF_RANGE if in_BinaryBufferLength is 0.
///
/// @return COI_TIME_OUT_REACHED if establishing the communication channel with
///         the remote process timed out.
///
/// @return COI_DOES_NOT_EXIST if in_FileOfOrigin is not NULL and does not
///         exist.
///
/// @return COI_ARGUMENT_MISMATCH if in_FileOfOrigin is NULL and
///         in_FileOfOriginOffset is not 0.
///
/// @return COI_INVALID_FILE if in_FileOfOrigin is not a "regular file" as
///         determined by stat or if its size is 0.
///
/// @return COI_OUT_OF_RANGE if in_FileOfOrigin exists but its size is
///         less than in_FileOfOriginOffset + in_BinaryBufferLength.
///
/// @return COI_NOT_INITIALIZED if the environment variable
///         SINK_LD_TRACE_LOADED_OBJECTS is set to a non empty string and there
///         are no errors locating the shared library dependencies.
///
/// @return COI_PROCESS_DIED if at some point during the loading of the remote
///         process the remote process terminated abnormally.
///
/// @return COI_VERSION_MISMATCH if the version of Intel(R) Coprocessor
///         Offload Infrastructure (Intel(R) COI) on the host is not
///         compatible with the version on the device.
///
COIACCESSAPI
COIRESULT
COIProcessCreateFromMemory(
    COIENGINE           in_Engine,
    const   char               *in_pBinaryName,
    const   void               *in_pBinaryBuffer,
    uint64_t            in_BinaryBufferLength,
    int                 in_Argc,
    const   char              **in_ppArgv,
    uint8_t             in_DupEnv,
    const   char              **in_ppAdditionalEnv,
    uint8_t             in_ProxyActive,
    const   char               *in_Reserved,
    uint64_t            in_InitialBufferSpace,
    const   char               *in_LibrarySearchPath,
    const   char               *in_FileOfOrigin,
    uint64_t            in_FileOfOriginOffset,
    COIPROCESS         *out_pProcess);

//////////////////////////////////////////////////////////////////////////////
///
/// Destroys the indicated process, releasing its resources. Note, this
/// will destroy any outstanding pipelines created in this process as well.
///
/// @param  in_Process
///         [in] Process to destroy.
///
/// @param  in_WaitForMainTimeout
///         [in] The number of milliseconds to wait for the main() function
///         to return in the sink process before timing out. -1 means to wait
///         indefinitely.
///
/// @param  in_ForceDestroy
///         [in] If this flag is set to true, then the sink process will be
///         forcibly terminated after the timeout has been reached. A timeout
///         value of 0 will kill the process immediately, while a timeout of
///         -1 is invalid. If the flag is set to false then a message will
///         be sent to the sink process requesting a clean shutdown. A value
///         of false along with a timeout of 0 does not send a shutdown
///         message, instead simply polls the process to see if it is alive.
///         In most cases this flag should be set to false. If a sink process
///         is not responding then it may be necessary to set this flag to
///         true.
///
/// @param  out_pProcessReturn
///         [out] The value returned from the main() function executing in
///         the sink process. This is an optional parameter. If the caller
///         is not interested in the return value from the remote process
///         they may pass in NULL for this parameter. The output value of
///         this pointer is only meaningful if COI_SUCCESS is returned.
///
/// @param  out_pTerminationCode
///         [out] This parameter specifies the termination code. This will
///         be 0 if the remote process exited cleanly. If the remote process
///         exited abnormally this will contain the termination code given
///         by the operating system of the remote process. This is an optional
///         parameter and the caller may pass in NULL if they are not
///         interested in the termination code. The output value of this
///         pointer is only meaningful if COI_SUCCESS is returned.
///
/// @return COI_SUCCESS if the process was destroyed.
///
/// @return COI_INVALID_HANDLE if the process handle passed in was invalid.
///
/// @return COI_OUT_OF_RANGE for any negative in_WaitForMainTimeout value
///         except -1.
///
/// @return COI_ARGUMENT_MISMATCH if in_WaitForMainTimeout is -1 and
///         in_ForceDestroy is true.
///
/// @return COI_TIME_OUT_REACHED if the sink process is still running after
///         waiting in_WaitForMainTimeout milliseconds and in_ForceDestroy
///         is false. This is true even if in_WaitForMainTimeout was 0.
///         In this case, out_pProcessReturn and out_pTerminationCode
///         are undefined.
///
COIACCESSAPI
COIRESULT
COIProcessDestroy(
    COIPROCESS              in_Process,
    int32_t                 in_WaitForMainTimeout,
    uint8_t                 in_ForceDestroy,
    int8_t                 *out_pProcessReturn,
    uint32_t               *out_pTerminationCode);


#define COI_MAX_FUNCTION_NAME_LENGTH 256

//////////////////////////////////////////////////////////////////////////////
///
/// Given a loaded native process, gets an array of function handles that can
/// be used to schedule run functions on a pipeline associated with that
/// process. See the documentation for COIPipelineRunFunction() for
/// additional information. All functions that are to be retrieved in this
/// fashion must have the define COINATIVEPROCESSEXPORT preceding their type
/// specification. For functions that are written in C++, either the entries
/// in in_pFunctionNameArray in must be pre-mangled, or the functions must be
/// declared as extern "C". It is also necessary to link the binary containing
/// the exported functions with the -rdynamic linker flag.
/// It is possible for this call to successfully find function handles for
/// some of the names passed in but not all of them. If this occurs
/// COI_DOES_NOT_EXIST will return and any handles not found will be returned
/// as NULL.
///
/// @param  in_Process
///         [in] Process handle previously returned via COIProcessCreate().
///
/// @param  in_NumFunctions
///         [in] Number of function names passed in to the in_pFunctionNames
///         array.
///
/// @param  in_ppFunctionNameArray
///         [in] Pointer to an array of null-terminated strings that match
///         the name of functions present in the code of the binary
///         previously loaded via COIProcessCreate(). Note that if a C++
///         function is used, then the string passed in must already be
///         properly name-mangled, or extern "C" must be used for where
///         the function is declared.
///
/// @param  out_pFunctionHandleArray
///         [in out] Pointer to a location created by the caller large
///         enough to hold an array of COIFUNCTION sized elements that has
///         in_numFunctions entries in the array.
///
/// @return COI_SUCCESS if all function names indicated were found.
///
/// @return COI_INVALID_HANDLE if the in_Process handle passed in was invalid.
///
/// @return COI_OUT_OF_RANGE if in_NumFunctions is zero.
///
/// @return COI_INVALID_POINTER if the in_ppFunctionNameArray or
///         out_pFunctionHandleArray pointers was NULL.
///
/// @return COI_DOES_NOT_EXIST if one or more function names were not
///         found. To determine the function names that were not found,
///         check which elements in the out_pFunctionHandleArray
///         are set to NULL.
///
/// @return COI_OUT_OF_RANGE if any of the null-terminated strings passed in
///         via in_ppFunctionNameArray were more than
///         COI_MAX_FUNCTION_NAME_LENGTH characters in length including
///         the null.
///
/// @warning This operation can take several milliseconds so it is recommended
///          that it only be done at load time.
///
COIACCESSAPI
COIRESULT
COIProcessGetFunctionHandles(
    COIPROCESS          in_Process,
    uint32_t            in_NumFunctions,
    const   char              **in_ppFunctionNameArray,
    COIFUNCTION        *out_pFunctionHandleArray);

#if COI_LIBRARY_VERSION >= 2
/// @name COIProcessLoadLibrary* flags, named after the corresponding
/// RTLD flags that are passed into dlopen().
/// Please consult a Linux manual for more information about these flags.
//@{
#define COI_LOADLIBRARY_LOCAL      0x00000
#define COI_LOADLIBRARY_GLOBAL     0x00100

#define COI_LOADLIBRARY_LAZY       0x00001
#define COI_LOADLIBRARY_NOW        0x00002
#define COI_LOADLIBRARY_NOLOAD     0x00004
#define COI_LOADLIBRARY_DEEPBIND   0x00008
#define COI_LOADLIBRARY_NODELETE   0x01000

/// Flags to replicate the behaviour of the original version of
/// COIProcessLoadLibrary* APIs.
#define COI_LOADLIBRARY_V1_FLAGS   (COI_LOADLIBRARY_GLOBAL|COI_LOADLIBRARY_NOW)

//@}

#endif

//////////////////////////////////////////////////////////////////////////////
///
/// Loads a shared library into the specified remote process, akin to using
/// dlopen() on a local process in Linux or LoadLibrary() in Windows.
/// Dependencies for this library that are not listed with absolute paths
/// are searched for first in current working directory, then in the
/// colon-delimited paths in the environment variable SINK_LD_LIBRARY_PATH,
/// and finally on the sink in the standard search paths as defined by the
/// sink's operating system / dynamic loader.
///
/// @param  in_Process
///         [in] Process to load the library into.
///
/// @param  in_pLibraryBuffer
///         [in] The memory buffer containing the shared library to load.
///
/// @param  in_LibraryBufferLength
///         [in] The number of bytes in the memory buffer in_pLibraryBuffer.
///
/// @param  in_pLibraryName
///         [in] Name for the shared library. This optional parameter can
///         be specified in case the dynamic library doesn't have an
///         SO_NAME field. If specified, it will take precedence over
///         the SO_NAME if it exists. If it is not specified then
///         the library must have a valid SO_NAME field.
///
///@param   in_LibrarySearchPath
///         [in] A path to locate dynamic libraries dependencies for the
///         library being loaded. If not NULL, this path will override the
///         environment variable SINK_LD_LIBRARY_PATH. If NULL it will use
///         SINK_LD_LIBRARY_PATH to locate dependencies.
///
///@param   in_LibrarySearchPath
///         [in] A path to locate dynamic libraries dependencies for the sink
///         application. If not NULL, this path will override the environment
///         variable SINK_LD_LIBRARY_PATH. If NULL it will use
///         SINK_LD_LIBRARY_PATH to locate dependencies.
///
/// @param  in_FileOfOrigin
///         [in] If not NULL, this parameter indicates the file from which the
///         in_pBinaryBuffer was obtained. This parameter is optional.
///
/// @param  in_FileOfOriginOffset
///         [in] If in_FileOfOrigin is not NULL, this parameter indicates the
///         offset within that file where in_pBinaryBuffer begins.
///
#if COI_LIBRARY_VERSION >= 2
/// @param  in_Flags
///         [in] Bitmask of the flags that will be passed in as the dlopen()
///         "flag" parameter on the sink.
///
#endif
///
/// @param  out_pLibrary
///         [out] If COI_SUCCESS or COI_ALREADY_EXISTS is returned, the handle
///         that uniquely identifies the loaded library.
///
/// @return COI_SUCCESS if the library was successfully loaded.
///
/// @return COI_INVALID_HANDLE if the process handle passed in was invalid.
///
/// @return COI_OUT_OF_RANGE if in_LibraryBufferLength is 0.
///
/// @return COI_INVALID_FILE if in_pLibraryBuffer does not represent a valid
///         shared library file.
///
/// @return COI_MISSING_DEPENDENCY if a dependent library is missing from
///         either SINK_LD_LIBRARY_PATH or the in_LibrarySearchPath parameter.
///
/// @return COI_ARGUMENT_MISMATCH if the shared library is missing an SONAME
///         and in_pLibraryName is NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_pLibraryName is the same as that of
///         any of the dependencies (recursive) of the library being loaded.
///
/// @return COI_ALREADY_EXISTS if there is an existing COILIBRARY handle
///         that identifies this library, and this COILIBRARY hasn't been
///         unloaded yet.
///
/// @return COI_BINARY_AND_HARDWARE_MISMATCH if the target machine of the
///         binary or any of its recursive dependencies does not match the
///         engine associated with in_Process.
///
/// @return COI_UNDEFINED_SYMBOL if we are unable to load the library due to
///         an undefined symbol.
///
/// @return COI_PROCESS_DIED if loading the library on the device caused
///         the remote process to terminate.
///
/// @return COI_DOES_NOT_EXIST if in_FileOfOrigin is not NULL and does not
///         exist.
///
/// @return COI_ARGUMENT_MISMATCH if in_FileOfOrigin is NULL and
///         in_FileOfOriginOffset is not 0.
///
/// @return COI_INVALID_FILE if in_FileOfOrigin is not a "regular file" as
///         determined by stat or if its size is 0.
///
/// @return COI_OUT_OF_RANGE if in_FileOfOrigin exists but its size is
///         less than in_FileOfOriginOffset + in_BinaryBufferLength.
///
/// @return COI_INVALID_POINTER if out_pLibrary or in_pLibraryBuffer are NULL.
///
#if COI_LIBRARY_VERSION >= 2
COIACCESSAPI
COIRESULT
COIProcessLoadLibraryFromMemory(
    COIPROCESS          in_Process,
    const   void               *in_pLibraryBuffer,
    uint64_t            in_LibraryBufferLength,
    const   char               *in_pLibraryName,
    const   char               *in_LibrarySearchPath,
    const   char               *in_FileOfOrigin,
    uint64_t            in_FileOfOriginOffset,
    uint32_t            in_Flags,
    COILIBRARY         *out_pLibrary);
__asm__(".symver COIProcessLoadLibraryFromMemory,"
        "COIProcessLoadLibraryFromMemory@COI_2.0");
#else

COIRESULT
COIProcessLoadLibraryFromMemory(
    COIPROCESS          in_Process,
    const   void               *in_pLibraryBuffer,
    uint64_t            in_LibraryBufferLength,
    const   char               *in_pLibraryName,
    const   char               *in_LibrarySearchPath,
    const   char               *in_FileOfOrigin,
    uint64_t            in_FileOfOriginOffset,
    COILIBRARY         *out_pLibrary);
__asm__(".symver COIProcessLoadLibraryFromMemory,"
        "COIProcessLoadLibraryFromMemory@COI_1.0");
#endif


//////////////////////////////////////////////////////////////////////////////
///
/// Loads a shared library into the specified remote process, akin to using
/// dlopen() on a local process in Linux or LoadLibrary() in Windows.
///
/// For more details, see COIProcessLoadLibraryFromMemory.
///
/// @param  in_Process
///         [in] Process to load the library into.
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
#if COI_LIBRARY_VERSION >= 2
/// @param  in_Flags
///         [in] Bitmask of the flags that will be passed in as the dlopen()
///         "flag" parameter on the sink.
///
#endif
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
/// @return See COIProcessLoadLibraryFromMemory for additional errors.
///
#if COI_LIBRARY_VERSION >= 2

COIACCESSAPI
COIRESULT
COIProcessLoadLibraryFromFile(
    COIPROCESS          in_Process,
    const   char               *in_pFileName,
    const   char               *in_pLibraryName,
    const   char               *in_LibrarySearchPath,
    uint32_t            in_Flags,
    COILIBRARY         *out_pLibrary);
__asm__(".symver COIProcessLoadLibraryFromFile,"
        "COIProcessLoadLibraryFromFile@COI_2.0");
#else

COIRESULT
COIProcessLoadLibraryFromFile(
    COIPROCESS          in_Process,
    const   char               *in_pFileName,
    const   char               *in_pLibraryName,
    const   char               *in_LibrarySearchPath,
    COILIBRARY         *out_pLibrary);
__asm__(".symver COIProcessLoadLibraryFromFile,"
        "COIProcessLoadLibraryFromFile@COI_1.0");
#endif

//////////////////////////////////////////////////////////////////////////////
///
/// Unloads a a previously loaded shared library from the specified
/// remote process.
///
/// @param  in_Process
///         [in] Process that we are unloading a library from.
///
/// @param  in_Library
///         [in] Library that we want to unload.
///
/// @return COI_SUCCESS if the library was successfully loaded.
///
/// @return COI_INVALID_HANDLE if the process or library handle were invalid.
///
COIACCESSAPI
COIRESULT
COIProcessUnloadLibrary(
    COIPROCESS          in_Process,
    COILIBRARY          in_Library);


//////////////////////////////////////////////////////////////////////////////
///
/// Registers shared libraries that are already in the host process's memory
/// to be used during the shared library dependency resolution steps that take
/// place during subsequent calls to COIProcessCreate* and
/// COIProcessLoadLibrary*. If listed as a dependency, the registered library
/// will be used to satisfy the dependency, even if there is another library
/// on disk that also satisfies that dependency.
///
/// Addresses registered must remain valid during subsequent calls to
/// COIProcessCreate* and COIProcessLoadLibrary*.
///
/// If the Sink is Linux, the shared libraries must have a library name
/// (DT_SONAME field). On most compilers this means built with -soname.
///
/// If successful, this API registers all the libraries. Otherwise none
/// are registered.
///
/// @param  in_NumLibraries
///         [in] The number of libraries that are being registered.
///
/// @param  in_ppLibraryArray
///         [in] An array of pointers that point to the starting addresses
///         of the libraries.
///
/// @param  in_pLibrarySizeArray
///         [in] An array of pointers that point to the number of bytes in
///         each of the libraries.
///
/// @param  in_ppFileOfOriginArray
///         [in] An array of strings indicating the file from which the
///         library was obtained. This parameter is optional. Elements
///         in the array may be set to NULL.
///
/// @param  in_pFileOfOriginOffSetArray
///         [in] If the corresponding entry in in_ppFileOfOriginArray is not
///         NULL, this parameter indicates the offsets within those files
///         where the corresponding libraries begin.
///
/// @return COI_SUCCESS if the libraries were registered successfully.
///
/// @return COI_OUT_OF_RANGE if in_NumLibraries is 0.
///
/// @return COI_INVALID_POINTER if in_ppLibraryArray or in_pLibrarySizeArray
///         are NULL.
///
/// @return COI_INVALID_POINTER if any of the pointers in in_ppLibraryArray
///         are NULL.
///
/// @return COI_OUT_OF_RANGE if any of the values in in_pLibrarySizeArray is 0.
///
/// @return COI_ARGUMENT_MISMATCH if either one of in_ppFileOfOriginArray
///         and in_pFileOfOriginOffSetArray is NULL and the other is not.
///
/// @return COI_OUT_OF_RANGE if one of the addresses being registered does not
///         represent a valid library.
///
COIACCESSAPI
COIRESULT
COIProcessRegisterLibraries(
    uint32_t            in_NumLibraries,
    const   void              **in_ppLibraryArray,
    const   uint64_t           *in_pLibrarySizeArray,
    const   char              **in_ppFileOfOriginArray,
    const   uint64_t           *in_pFileOfOriginOffSetArray);


//////////////////////////////////////////////////////////////////////////////
/// The user can choose to have notifications for these internal events
/// so that they can build their own profiling and performance layer on
/// top of Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI).
///
typedef enum COI_NOTIFICATIONS
{
    /// This event occurs when all explicit and implicit dependencies are
    /// satisfied and Intel(R) Coprocessor Offload Infrastructure
    /// (Intel(R) COI) schedules the run function to begin execution.
    RUN_FUNCTION_READY = 0,

    /// This event occurs just before the run function actually starts
    /// executing. There may be some latency between the ready and start
    /// events if other run functions are already queued and ready to run.
    RUN_FUNCTION_START,

    /// This event occurs when the run function finishes. This is when the
    /// completion event for that run function would be signaled.
    RUN_FUNCTION_COMPLETE,

    /// This event occurs when all explicit and implicit dependencies are
    /// met for the pending buffer operation. Assuming buffer needs to be
    /// moved, copied, read, etc... Will not be invoked if no actual memory
    /// is moved, copied, read, etc. This means that COIBufferUnmap will
    /// never result in a callback as it simply updates the status of the
    /// buffer but doesn't initiate any data movement. COIBufferMap,
    /// COIBufferSetState, COIBufferWrite, COIBufferRead and COIBufferCopy
    /// do initiate data movement and therefore will invoke the callback.
    BUFFER_OPERATION_READY,

    /// This event occurs when the buffer operation is completed.
    BUFFER_OPERATION_COMPLETE,

    /// This event occurs when a user event is signaled from the remotely
    /// a sink process. Local (source triggered) events do not trigger this.
    USER_EVENT_SIGNALED
} COI_NOTIFICATIONS;

//////////////////////////////////////////////////////////////////////////////
///
/// A callback that will be invoked to notify the user of an internal
/// Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// event. Note that the callback is registered per process so any of the
/// above notifications that happen on the registered process will receive
/// the callback.
/// As with any callback mechanism it is up to the user to make sure that
/// there are no possible deadlocks due to reentrancy (i.e. the callback being
/// invoked in the same context that triggered the notification) and also
/// that the callback does not slow down overall processing. If the user
/// performs too much work within the callback it could delay further
/// processing. The callback will be invoked prior to the signaling of
/// the corresponding COIEvent. For example, if a user is waiting
/// for a COIEvent associated with a run function completing they will
/// receive the callback before the COIEvent is marked as signaled.
///
///
/// @param  in_Type
///         [in] The type of internal event that has occurred.
///
/// @param  in_Process
///         [in] The process associated with the operation.
///
/// @param  in_Event
///         [in] The completion event that is associated with the
///         operation that is being notified.
///
/// @param  in_UserData
///         [in] Opaque data that was provided when the callback was
///         registered. Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
///         simply passes this back to the user so that
///         they can interpret it as they choose.
///
typedef void (*COI_NOTIFICATION_CALLBACK)(
    COI_NOTIFICATIONS   in_Type,
    COIPROCESS          in_Process,
    COIEVENT            in_Event,
    const   void               *in_UserData);


//////////////////////////////////////////////////////////////////////////////
///
/// Register a callback to be invoked to notify that an internal
/// Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI) event
/// has occured on the process that is associated with the callback.
/// Note that it is legal to have more than one callback registered with
/// a given process but those must all be unique callback pointers.
/// Note that setting a UserData value with COINotificationCallbackSetContext
/// will override a value set when registering the callback.
///
/// @param  in_Process
///         [in] Process that the callback is associated with. The callback
///         will only be invoked to notify an event for this specific process.
///
/// @param  in_Callback
///         [in] Pointer to a user function used to signal a notification.
///
/// @param  in_UserData
///         [in] Opaque data to pass to the callback when it is invoked.
///
/// @return COI_SUCCESS if the callback was registered successfully.
///
/// @return COI_INVALID_HANDLE if the in_Process parameter does not identify
///         a valid process.
///
/// @return COI_INVALID_POINTER if the in_Callback parameter is NULL.
///
/// @return COI_ALREADY_EXISTS if the user attempts to reregister the same
///         callback for a process.
///
COIACCESSAPI
COIRESULT COIRegisterNotificationCallback(
    COIPROCESS                  in_Process,
    COI_NOTIFICATION_CALLBACK   in_Callback,
    const   void               *in_UserData);


//////////////////////////////////////////////////////////////////////////////
///
/// Unregisters a callback, notifications will no longer be signaled.
///
/// @param  in_Process
///         [in] Process that we are unregistering.
///
/// @param  in_Callback
///         [in] The specific callback to unregister.
///
/// @return COI_SUCCESS if the callback was unregistered.
///
/// @return COI_INVALID_HANDLE if the in_Process parameter does not identify
///         a valid process.
///
/// @return COI_INVALID_POINTER if the in_Callback parameter is NULL.
///
/// @return COI_DOES_NOT_EXIST if in_Callback was not previously registered
///         for in_Process.
///
COIACCESSAPI
COIRESULT COIUnregisterNotificationCallback(
    COIPROCESS                  in_Process,
    COI_NOTIFICATION_CALLBACK   in_Callback);


//////////////////////////////////////////////////////////////////////////////
///
/// Set the user data that will be returned in the notification callback.
/// This data is sticky and per thread so must be set prior to the
/// Intel(R) Coprocessor Offload Infrastructure (Intel(R) COI)
/// operation being invoked. If you wish to set the context to be returned
/// for a specific instance of a user event notification then the context
/// must be set using this API prior to registering that user event with
/// COIEventRegisterUserEvent.
/// The value may be set prior to each Intel(R) Coprocessor Offload
/// Infrastructure (Intel(R) COI) operation being called to
/// effectively have a unique UserData per callback.
/// Setting this value overrides any value that was set when the
/// callback was registered and will also override any future registrations
/// that occur.
///
/// @param  in_UserData
///         [in] Opaque data to pass to the callback when it is invoked.
///         Note that this data is set per thread.
///
COIACCESSAPI
void COINotificationCallbackSetContext(
    const   void                       *in_UserData);


/// @name COIProcessSetCacheSize flags.
/// Flags are divided into two categories: _MODE_ and _ACTION_
/// only one of each is valid with each call.
/// _ACTIONS_ and _MODES_ should be bitwised OR'ed together, i.e. |
//@{

/// Current set of DEFINED bits for _MODE_, can be used
/// to clear or check fields, not useful to pass into APIs. Used internally.
#define COI_CACHE_MODE_MASK                 0x00000007

/// Flag to indicate to keep the previous mode of operation. By default
/// this would be COI_CACHE_MODE_ONDEMAND_SYNC. As of this release
/// This is the only mode available. This mode is valid with _ACTION_
/// flags.
#define COI_CACHE_MODE_NOCHANGE             0x00000001

/// Mode of operation that indicates that COI will allocate physical
/// cache memory exactly when it is is needed. COIPipeline execution in
/// the given process will momentarily block until the allocation request
/// is completed. This is and has been the default mode.
#define COI_CACHE_MODE_ONDEMAND_SYNC        0x00000002

/// Not yet implemented. Future mode that will not stall a COIPipeline
/// but prefer eviction/paging if possible as to immediately execute pipeline.
/// At the same time, enqueue background requests to allocate extra cache
/// so as to provide optimze behavior on subsequent runs.
#define COI_CACHE_MODE_ONDEMAND_ASYNC       0x00000004


/// Current set of DEFINED bits for _ACTION_ can be used
/// to clear fields, but not useful to pass into API's. Used internally.
#define COI_CACHE_ACTION_MASK               0x00070000

/// No action requested. With this flag specified
/// it is recommended to NOT provide a out_pCompletion event,
/// as with this flag, modes and values are immediately set.
/// This is valid with _MODE_ flags.
#define COI_CACHE_ACTION_NONE               0x00010000

/// This _ACTION_ flag will immediately attempt to increase the cache
/// physical memory size to the current set pool size(s). Used to
/// pre-allocate memory on remote processes, so that runfunction will
/// enqueue faster. Also may prevent unused buffer eviction from process
/// reducing overhead in trade for memory allocation cost.
#define COI_CACHE_ACTION_GROW_NOW           0x00020000

/// Not yet implemented. Future _ACTION_ that will attempt to find unused
/// allocated cache and free it, with the express goal of reducing the
/// footprint on the remote process down to the value of the currently set
/// pool size(s).
#define COI_CACHE_ACTION_FREE_UNUSED        0x00040000

//@}

//////////////////////////////////////////////////////////////////////////////
///
/// Set the minimum preferred COIProcess cache size. By default these values
/// are set to 1GB. With the default size of 1GB, Intel(R) COI will only
/// grow the cache with each new buffer up until the set limit is consumed,
/// after which, only required to accommodate additional buffers.
/// This means that after the cache preference is met, a process will act
/// as conservative as possible for memory consumption.
/// This API will allow users to adjust memory consumption aggressiveness.
///
/// Additional performance may be gained if the user sets a value higher than
/// default. With high memory consumption user can choose to trade performance
/// between memory allocation cost and transfer speeds to and from the
/// remote process. A last consideration is that if buffers are used only
/// once, it may be best to keep a small cache size, or ensure buffers are
/// fully destroyed after their use.
///
/// Adjusting this value to high may result in out of resource conditions.
///
/// @param  in_pProcess
///         [in] Handle to uniquely identify the process for which the cache
///         is to be adjusted.
///
/// @param  in_HugePagePoolSize
///         [in] The suggested size of the remote huge page cache in bytes.
///         This value defaults to 1GB. A process will only allocate cache
///         memory if the current cache is smaller than this limit, or it is
///         absolutely necessary to fulfill a request, but preferring to
///         re-use existing memory and paging unused buffers back to the host
///         Increasing this value will cause a process to
///         aggressively allocate memory on demand up to this value, before
///         evicting/paging memory from the remote process back to the host
///         process.
///
///         The net result is that memory consumption is increased, but the
///         user can 'cache' more buffers on the remote process. More time
///         may be spent during first use of run functions as more memory
///         may be allocated, but subsequent run functions will likely
///         see an increase in queueing performance as the data is already
///         valid in the remote process.
///
///         Users should tune this value for optimum performance balanced
///         against memory consumption. This value does not affect 4K page
///         cache. Please use in_SmallPagePoolSize for 4K pages.
///
/// @param  in_HugeFlags
///         [in] Flags to select mode or action for huge page cache. One _MODE_
///         and one _ACTION_ flag are specified together. Default _MODE_ is
///         COI_CACHE_MODE_ONDEMAND_SYNC. See all COI_CACHE_MODE_* and
///         COI_CACHE_ACTION_* for other modes and actions. Default _ACTION_
///         is COI_CACHE_ACTION_NONE.
///
/// @param  in_SmallPagePoolSize
///         [in] The suggested size of the remote 4K cache in bytes. Same
///         function as in_HugePagePoolSize but affecting only 4K page cache.
///         Defaults to 1GB.
///
/// @param  in_SmallFlags
///         [in] Flags to select mode or action for 4K page cache. One _MODE_
///         and one _ACTION_ flag are be specified together. Default _MODE_ is
///         COI_CACHE_MODE_ONDEMAND_SYNC. See all COI_CACHE_MODE_* and
///         COI_CACHE_ACTION_* for other modes and actions.
///
/// @param  in_NumDependencies
///         [in] The number of dependencies specified in the in_pDependencies
///         array. This may be 0 if the caller does not want the call to
///         wait for any events to be signaled.
///
/// @param  in_pDependencies
///         [in] An optional array of handles to previously created COIEVENT
///         objects that this operation will wait for before starting.
///         This allows the user to create dependencies between asynchronous
///         calls and other operations such as run functions. The user may
///         pass in NULL if they do not wish to wait for any dependencies.
///         Only useful with _ACTION_ flags, otherwise there is no action
///         to wait on. All _MODE_ changes happen immediately.
///
/// @param  out_pCompletion
///         [out] An optional pointer to a COIEVENT object that will be
///         signaled when the operation is complete. The user may pass in
///         NULL if the user wants the operation to block until completed.
///         Note: This flag is not useful unless paired with a
///               valid _ACTION_ flag.
///
/// @return COI_SUCCESS if the cache was successfully adjusted. In case of
///         valid flags including _ACTION_, if out_pCompletion was specified,
///         this does not indicate the operation succeeded, but rather only
///         it was successfully queued. For further information see
///         that COIEventWait() for getting return values.
///
/// @return COI_INVALID_HANDLE if the in_Process handle passed in was invalid.
///
/// @return COI_RESOURCE_EXHAUSTED if no more cache can be created,
///         possibly, but not necessarily because a pool size was set to large
///         and COI_CACHE_ACTION_GROW_NOW was specified.
///
/// @return COI_NOT_SUPPORTED if more than one _MODE_ or _ACTION_ was
///         specified.
///
/// @return COI_NOT_SUPPORTED if an invalid _MODE_ or _ACTION_ was
///         specified.
///
/// @return COI_ARGUMENT_MISMATCH if in_NumDependencies is non-zero while
///         in_pDependencies was passed in as NULL.
///
/// @return COI_OUT_OF_RANGE if one of the pool sizes was invalid.
///
/// @return COI_PROCESS_DIED if at some point during the mode or action the
///         remote process terminated abnormally. Possible due to an out of
///         memory condition.
///
COIACCESSAPI
COIRESULT COIProcessSetCacheSize(
    const   COIPROCESS          in_Process,
    const   uint64_t            in_HugePagePoolSize,
    const   uint32_t            in_HugeFlags,
    const   uint64_t            in_SmallPagePoolSize,
    const   uint32_t            in_SmallFlags,
    uint32_t            in_NumDependencies,
    const   COIEVENT           *in_pDependencies,
    COIEVENT           *out_pCompletion);


//////////////////////////////////////////////////////////////////////////////
/// These are the different modes of operation that can be selected for
/// the COI_DMA_MODE by the API COIProcessConfigureDMA. They allow the user
/// to customize the DMA layer behaviour.
///
typedef enum COI_DMA_MODE
{
    /// This mode will use one common logical channel for all DMA operations.
    /// Using this mode requires a channel count of one.
    COI_DMA_MODE_SINGLE = 0,

    /// This mode will dedicate on logical channel for write operations
    /// and one logical channel for read operations. Requires a minimum of
    /// two logical channels, if more than two are used they are ignored
    /// in the current implementation.
    COI_DMA_MODE_READ_WRITE,

    /// This mode is not yet implemented and is a placeholder for future
    /// releases. Check here for updates when it is implemented.
    /// Will require a minimum of two logical channels and a maximum
    /// of four channels.
    COI_DMA_MODE_ROUND_ROBIN,

    /// Reserved for internal use.
    COI_DMA_RESERVED
} COI_DMA_MODE;


//////////////////////////////////////////////////////////////////////////////
///
/// Set the number and mode of the physical DMA channels that each COIProcess
/// will establish during COIProcess creation.
///
/// By default the runtime will operate in COI_DMA_MODE_SINGLE mode.
/// This API is intended to be called before COIProcessCreateFromFile() or
/// COIProcessCreateFromMemory(). The values are stored globally and will
/// be used by the creation API's. It is possible to call this API once
/// before each new COIPROCESS is created and thus have each COIPROCESS
/// run in different modes. It is not possible to change the mode on an
/// existing COIPROCESS.
///
/// The larger number of logical connections requested will impose a
/// performance penalty on the COIBUFFER creation API's, but unlock better
/// parallelism for DMA transfers during runtime.
///
/// A maximum value of four (4) channels is available today, but current
/// implementation will only take advantage of two DMA channels. The option
/// is left available for programmers to use in case future implementations
/// provide performance advantages.
///
/// It is important to note that for some operations that enabling this
/// options may increase parallelism and require the user to enforce
/// explicit dependencies for operations on the same buffers. See documentation
/// for COIBufferRead/Write/Copy operations for more details.
///
/// @param  in_Channels
///         [in] Number of logical connections to the remote COIProcess that
///         the runtime will establish and use for DMA transfer requests.
///         Will be ignored if in_Mode is set to COI_DMA_MODE_SINGLE.
///
/// @param  in_Mode
///         [in] The mode of operation in which the runtime will use the
///         logical connections to the remote COIProcess.
///
/// @return COI_SUCCESS if the mode and number of DMA channels requested
///         is valid. The actual create creation of channels and modes is
///         done during COIProcessCreateFromFile() and
///         COIProcessCreateFromMemory().
///
/// @return COI_NOT_SUPPORTED if an invalid value for in_Channels or
///         in_Mode was requested.
///
/// @return COI_ARGUMENT_MISMATCH if an invalid combination of in_Channels and
///          in_Mode was requested. Example could be 2 channels with
///          COI_DMA_MODE_SINGLE, or 1 channel with COI_DMA_MODE_READ_WRITE.
///
COIACCESSAPI
COIRESULT COIProcessConfigureDMA(
    const   uint64_t            in_Channels,
    const   COI_DMA_MODE        in_Mode);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIPROCESS_SOURCE_H */

/*! @} */
