/*
 * Copyright 2010-2015 Intel Corporation.
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
/**
 Description:
    Define APIs of MYO for compiler or pre-processor to transfer original programs.
 */

#ifndef _MYO_IMPL_H_
#define _MYO_IMPL_H_

/** @ingroup MYO
 *  @addtogroup MYOIMPL_API
@{
* @file myoimpl.h 
*/

#ifdef __cplusplus
extern "C" {
#define EXTERN_C extern "C"
#else
#define EXTERN_C /* nothing */
#endif

#       define MYOACCESSAPI /* nothing */

#ifdef DEFINE_ARENA_API_CILK_SHARED
#define CILK_SHARED _Cilk_shared
#else
#define CILK_SHARED /* nothing */
#endif

/* **************************************************************************** *\
    APIs to enable functions being remotely called
\* **************************************************************************** */

typedef void *(*MyoiRemoteFuncType)(void *);

/** @fn extern MyoError myoiRemoteFuncRegister(MyoiRemoteFuncType in_pFuncAddr,
 * const char *in_pFuncName)
 * @brief Register a function so that it can be remotely called. This should be
 * done in myoiUserInit or before calling myoiLibInit. After myoiLibInit,
 * there will be a table on all peers, which contains the information for
 * all remotely callable functions.
 *
 * @param in_pWrapFuncAddr address of the wrapper function.
 * @param in_pFuncName name of the function.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/

MYOACCESSAPI
MyoError myoiRemoteFuncRegister(MyoiRemoteFuncType in_pFuncAddr,
        const char *in_pFuncName);
/** @fn extern MyoError myoiRemoteFuncLookupByName(char *in_pFuncName, 
 * MyoiRemoteFuncType *out_pWrapFuncAddr)
 * @brief Get the address of the wrapper function by looking up the table
 * by name. This API can be used when assigning a function pointer to
 * remotely callable functions.
 *
 * @param in_pFuncName name of the function.
 * @param out_pWrapFuncAddr address of the wrapper function.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI MyoError myoiRemoteFuncLookupByName(char *in_pFuncName,
        MyoiRemoteFuncType *out_pWrapFuncAddr);

/** @fn extern MyoError myoiRemoteFuncLookupByAddr(MyoiRemoteFuncType 
 * in_pWrapFuncAddr,char **out_pFuncName)
 * @brief Get the name of a remote function by looking up the table by
 * the address. This API can be used when calling a remotely callable 
 * function by a function pointer.
 *
 * @param in_pWrapFuncAddr address of the function.
 * @param out_pFuncName name of the function.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI MyoError myoiRemoteFuncLookupByAddr(MyoiRemoteFuncType in_pWrapFuncAddr,
        char **out_pFuncName);

//! Host Side Shared Function Pointer Entry Struct
typedef struct {
    //! Function Name
    const char *funcName;
    //! Function Address
    void *funcAddr;
    //! Local Thunk Address
    void *localThunkAddr;
} MyoiHostSharedFptrEntry;

//! Target Side Shared Function Pointer Entry Struct
typedef struct {
    //! Function Name
    const char *funcName;
    //! Function Address
    void *funcAddr;
    //! Wrap Function Address
    void *wrapFuncAddr;
    //! Locak Thunk Address
    void *localThunkAddr;
} MyoiTargetSharedFptrEntry;

/**
 * @cond INCLUDE_MYO_INTERNAL_DOCUMENTATION
 * @fn extern MyoError myoiHostFptrTableRegister(void *in_pAddrOfFptrTable, 
 * int in_NumEntry, int in_Ordered)
 * @brief Register shared functions on host side. A 16 byte thunk will be 
 * allocated for each function entry in non-coherent shared memory. The 
 * thunk will contain a jump instruction to the local version of the 
 * shared function, which is provided by the second item of the function 
 * entry. Also, the address of the thunk will be stored to the 3rd item 
 * of the function entry for Compiler usage.
 *
 * @param in_pAddrOfFptrTable start address of the shared function 
 * table, assuming it follows the format of MyoiHostSharedFptrEntry.
 * @param in_NumEntry number of entry in the table.
 * @param in_Ordered whether the table is ordered by function name.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoiHostFptrTableRegister(
        void *in_pAddrOfFptrTable, int in_NumEntry, int in_Ordered);

/** @fn extern MyoError myoiTargetFptrTableRegister(void *in_pAddrOfFptrTable,
 * int in_NumEntry, int in_Ordered)
 * @brief Register shared functions on target side. This function is the 
 * same as myoiHostFptrTableRegister, except it does not need to allocate 
 * thunks from non-coherent shared memory for each function entry, but 
 * instead looks up this information from a table retrieved from the 
 * Host side.
 *
 * @param in_pAddrOfFptrTable start address of the shared function 
 * table, assuming it follows the format of MyoiTargetSharedFptrEntry.
 * @param in_NumEntry number of entry in the table.
 * @param in_Ordered whether the table is ordered by function name.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
extern MyoError myoiTargetFptrTableRegister(
        void *in_pAddrOfFptrTable, int in_NumEntry, int in_Ordered);
/**
 * @endcond
 **/

/* *************************************************************************** *\
    APIs for remote function calls
\* *************************************************************************** */

typedef void * MyoiRFuncCallHandle;

/** @fn extern MyoiRFuncCallHandle myoiRemoteCall(char *in_pFuncName, 
 * void *in_pArgs, int in_deviceNum)
 * @brief Call a remote callable function. If there are multiple arguments 
 * for the function, pack them to a shared buffer beforehand and take the
 * address of the shared buffer as this function. After receiving the call 
 * requests from other peers, the arguments should be unpacked from the 
 * shared buffer before calling the target function. The shared buffer 
 * can also be used to store the return value of the function.
 *
 * @param in_pFuncName name of the function.
 * @param in_pArgs address of the shared buffer.
 * @return
 *      Handle used to check the result.
 **/
MYOACCESSAPI
MyoiRFuncCallHandle myoiRemoteCall(const char *in_pFuncName, void *in_pArgs, int in_deviceNum);

/**
 * @cond INCLUDE_MYO_INTERNAL_DOCUMENTATION
 * @fn extern MyoError myoiRemoteThunkCall(void *in_funcThunkAddr, void *in_pArgs, int in_deviceNum)
 * @brief Call a remote callable function. If there are multiple arguments for 
 * the function, pack them to a shared buffer beforehand and take the address
 * of the shared buffer as this function. After receiving the call requests
 * from other peers, the arguments should be unpacked from the shared buffer
 * before calling the target function. The shared buffer can also be used to 
 * store the return value of the function.
 *
 * @param in_funcThunkAddr pointer to function thunk in the non-coherent
 * shared memory.
 * @param in_pArgs address of the shared buffer.
 * @PARAM in_deviceNum: device ID (0-N-1) for the MIC device to run 
 * function call.  -1 request causes MYO to schedule an available device.  
 * For RPC from device to host, in_deviceNum should always be -1.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoiRemoteThunkCall(void *in_funcThunkAddr, void *in_pArgs, int in_deviceNum);
/**
 * @endcond
 **/

/** @fn extern MyoError myoiCheckResult(MyoiRFuncCallHandle in_Handle)
 * @brief Check whether the remote call is done.
 *
 * @param in_Handle handle of the remote call.
 * @return
 *      MYO_SUCCESS (done); or
 *      an error number to indicate the error.
 **/
extern MyoError myoiCheckResult(MyoiRFuncCallHandle in_Handle);

/** @fn extern MyoError myoiGetResult(MyoiRFuncCallHandle in_Handle)
 * @brief Wait till the remote call is done.
 *
 * @param in_Handle handle of the remote call.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoiGetResult(MyoiRFuncCallHandle in_Handle);

/* **************************************************************************** *\
    APIs related with shared variables.
\* **************************************************************************** */

/* 
 * It is Compiler's responsibility to make sure all shared variables
 * located in shared memory space and have the same address in all sides.
 * However, it is hard for Compiler to do this. So we propose a solution
 * which is changing the definition of all shared variables and accessing
 * them indirectly, and making sure all shared variables pointing
 * to the same shared space on all sides. For example,
 *
 * "shared int a;" is changed to "shared int *a";
 * Also all the accesses to "a" is change to "*a".
 *
 * We suppose the shared memory for each shared variable is allocated on
 * host side by Compiler. For the upper example, Compiler can call:
 *
 * a = (shared int *) myoSharedMalloc(sizeof(shared int));
 *
 * Now the issue is how to make "a" on other sides also pointing to the
 * same shared memory on other sides. We provide two methods to do this.
 * They can be used in a hybrid way.
 */

/* 
 * The first method is calling myoiVarRegister for each shared variable
 * on all sides in myoiUserInit. On host side, we will get a table containing
 * a table containing the shared address and name of each shared variable.
 * After calling myoiUserInit, we will propagate the table to other sides.
 * On card side, after getting the table from host, myoiUserInit is called.
 * When calling myoiVarRegister in myoiUserInit, we will make local pointer
 * of each shared variable pointing to the same shared memory with the local
 * pointer on host side of the same shared variable pointing to.
 */

/* 
 * The second method suppose that Compiler already have a table on all sides.
 * On host side, the table contains the name and the shared address of each
 * shared variable. On card side, the table contains the name of each shared
 * variable and the address of the local pointer which will pointing to shared
 * memory space.
 *
 * On host side, Compiler generates a call to myoiHostVarTablePropagate
 * after initializing MYO runtime and making the host side table ready.
 * On card side, Compiler uses myoiMicVarTableRegister to tell
 * the runtime where the card side table is.
 *
 * Since there may be multiple libraries on card side for the same application,
 * myoiHostVarTablePropagate and myoiMicVarTableRegister can be called multiple
 * times and called simultaneously.
 *
 * Inside runtime, the local pointer of the same shared variable on all sides
 * will be make sure pointing to the same shared space by using the information
 * of the Compiler provided tables.
 */

/* 
 * Comipler knows the following two structures to make sure the var table
 * has the following format.
 */

/* 
 * This is structure of the Shared var table entry. This table contains 
 * the shared address and name of each shared variable
 */

/** @fn extern MyoError myoiVarRegister(void *in_pAddrOfLocalPtrToShared, char *in_pSVarName)
 * @brief Register shared variables. Call it on all sides in myoiUserInit. 
 * On host side, make sure calling it after allocating shared memory for 
 * the shared variables by calling myoSharedMalloc.
 *
 * @param in_pAddrOfLocalPtrToShared the address assigned by the compiler 
 * for the shared variable, which is the address of a local pointer, 
 * pointing to shared memory space.
 * @param in_pSVarName name of shared variable.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoiVarRegister(
        void *in_pAddrOfLocalPtrToShared, const char *in_pSVarName);

/*
 * Compiler knows the following two structures to make sure the var table
 * has the following format.
 */

/**
 * This is structure of the Shared var table entry. This table contains 
 * the shared address and name of each shared variable
 **/
typedef struct {
    //! Variable Name
    const char *varName;
    //! Shared Address
    void *sharedAddr;
} MyoiSharedVarEntry;

//! Structure of the var table entry on host
typedef struct {
    //! Variable Name
    const char *varName;
    //! Variable Size
    int size;
    //! Local pointer to Shared var
    void *ptrToLocalPtrToShared;
} MyoiHostSharedVarEntry;

//! Structure of the var table entry on card 
typedef struct {
    //! Variable Name
    const char *varName;
    //! Local pointer to Shared var
    void *ptrToLocalPtrToShared;
} MyoiMicSharedVarEntry;

/** @fn extern MyoError myoiHostVarTablePropagate(void *in_pAddrOfSVarTable, int in_NumEntry)
 * @brief Send the host side var table to the card side. Card side will also 
 * have a copy of the host side var table after this propagation, although it 
 * is in an internal format different than the original host side var table, 
 * due to implementation convenience.
 *
 * @param in_pAddrOfSVarTable start address of the host side var table,
 * assuming it follows the format of MyoiSharedVarEntry.
 * @param in_NumEntry number of entry in the table.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI MyoError myoiHostVarTablePropagate(
        void *in_pAddrOfSVarTable, int in_NumEntry);

/**
 * @cond INCLUDE_MYO_INTERNAL_DOCUMENTATION
 * @fn extern MyoError myoiMicVarTableRegister(void *in_pAddrOfSVarTable, int in_NumEntry)
 * @brief Tell the runtime where the card side table is.
 *
 * @param in_pAddrOfSVarTable start address of the card side var
 * table, assuming it follows the format of MyoiMicSharedVarEntry.
 * @param in_NumEntry number of entry in the table.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
extern MyoError myoiMicVarTableRegister(
        void *in_pAddrOfSVarTable, int in_NumEntry);
/**
 * @endcond
 **/

/** @fn MyoError myoiHostSharedMallocTableRegister(void *in_pAddrOfSVarTable, int in_NumEntry, int in_Ordered)
 * @brief Allocate shared memory for all shared variables in the table. 
 * Also update local address of the shared variable with new shared address.
 *
 * @param in_pAddrOfSVarTable start address of the shared variable table,
 * assuming it follows the format of MyoiHostSharedVarEntry.
 * @param in_NumEntry number of entry in the table.
 * @param in_Ordered whether the table ordered by name.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoiHostSharedMallocTableRegister(
        void *in_pAddrOfSVarTable, int in_NumEntry, int in_Ordered);

/** @fn extern MyoError myoiTargetSharedMallocTableRegister(void *in_pAddrOfSVarTable, int in_NumEntry, int in_Ordered)
 * @brief Register the shared variables on the target side.
 *
 * @param in_pAddrOfSVarTable start address of the shared varaible table,
 * assuming it follows the format of MyoiMicSharedVarEntry.
 * @param in_NumEntry number of entry in the table.
 * @param in_Ordered whether the table ordered by name.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
extern MyoError myoiTargetSharedMallocTableRegister(
        void *in_pAddrOfSVarTable, int in_NumEntry, int in_Ordered);

/** @fn MyoError myoiLibInit(void * in_args, void (*userInitFunc))
 * @brief Init entry of the MYO library responsible for initializing 
 * the runtime.
 *
 * @param in_args mechanism to pass arguments to the Initialization 
 * routine. The default value of NULL would mean the host is blocked 
 * on the completion of myoiLibInit() on all nodes. A subset of the 
 * installed cards can be intialized by passing an array of 
 * MyoiUserParams. For example, in a system with two cards, to run a
 * MYO application only on the second card, intialize the array as 
 * follows:
 *      @code 
 *      MyoiUserParams UserParas[64];
 *      UserParas[0].type = MYOI_USERPARAMS_DEVID; 
 *      UserParas[0].nodeid = 2;
 *      UserParas[1].type = MYOI_USERPARAMS_LAST_MSG;
 *      if(MYO_SUCCESS != myoiLibInit(&UserParas, (void*)&myoiUserInit)) {
 *          printf("Failed to initialize MYO runtime\n");
 *          return -1;
 *      }
 *      @endcode
 * This intialization is required only in the client/host side
 * of the application. The server/card side executable should be
 * executed only on the second card in this case.
 *
 * Another capability for the MyoiUserParams structure in MYO is specifying
 * a remote procedure call to be executed on the host or card, immediately after
 * myoiLibInit() completes. This capability is useful because some calls in
 * MYO return immediately, but do not actually complete until after the MYO
 * library is completely initialized on all peers.  An example follows,
 * showing how to cause MYO to execute the registered function named
 * "PostMyoLibInitFunction" on the first card only:
 *      @code
 *      MyoiUserParams UserParas[64];
 *      UserParas[0].type = MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC;
 *      UserParas[0].nodeid = 1;
 *      SetPostLibInitFuncName(UserParas[1], "PostMyoLibInitFunction");
 *      UserParas[2].type = MYOI_USERPARAMS_LAST_MSG;
 *      if(MYO_SUCCESS != myoiLibInit(&UserParas, (void*)&myoiUserInit)) {
 *          printf("Failed to initialize MYO runtime\n");
 *          return -1;
 *      }
 *      @endcode
 *
 * Note, to cause PostMyoLibInitFunction to be executed on ALL cards,
 * specify: MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_ALL_NODES for the nodeid.
 * That is:
 *      @code
 *      UserParas[0].nodeid = MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_ALL_NODES;
 *      @endcode
 *
 * @param userInitFunc Shared variables and remote functions are
 * registered in this routine, which is called by the runtime during
 * library initialization. 
 * @return
 *      MYO_SUCCESS;
 *      MYO_ERROR;
 **/
MYOACCESSAPI
MyoError myoiLibInit(void * in_args, void *userInitFunc /*userInitFunc must be: MyoError (*userInitFunc)(void) */);

/** @fn extern MyoError myoiSupportsFeature(MyoFeatureType myoFeature)
 * @brief Supports runtime query to determine whether a feature is supported
 * by the myo that is installed on the system. This function is intended to
 * support client code to query the myo library to determine whether its set
 * of capabilities are able to support the client's needs.
 *
 * @param myoFeature The feature that is to be inquired about.
 * @return
 *      MYO_SUCCESS; if the feature is supported.
 *      MYO_FEATURE_NOT_IMPLEMENTED if the feature is not supported.
 *
 * (For more information, please also see the declaration of the MyoFeatureType enum declaration.)
 **/
MYOACCESSAPI
MyoError myoiSupportsFeature(MyoFeatureType myoFeature);

/** @fn void myoiLibFini()
 * @brief Finalize the MYO library, all resources held by the runtime are 
 * released by this routine.
 *
 * @return
 **/
MYOACCESSAPI
void myoiLibFini();

/* *************************************************************************** *\
    APIs to set shared memory space consistent/non-consistent.
\* *************************************************************************** */

/** @fn extern MyoError myoiSetMemNonConsistent(void *in_pAddr, size_t in_Size)
 * @brief Set part of the shared memory space to be non-consistent, which
 * means that the consistency of this part of shared memory space does not 
 * need to be maintained between HOST and cards.
 *
 * @param in_pAddr The start address of the specified shared memory space;
 * @param in_Size The size of the specified shared memory space;
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoiSetMemNonConsistent(void *in_pAddr, size_t in_Size);

/** @fn extern MyoError myoiSetMemConsistent(void *in_pAddr, size_t in_Size)
 * @brief Set part of the shared memory space to be consistent, which 
 * means that the consistency of this part of shared memory space needs 
 * to be maintained between HOST and cards.
 *
 * @param in_pAddr The start address of the specified shared 
 * memory space.
 * @param in_size The size of the specified shared memory space.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoiSetMemConsistent(void *in_pAddr, size_t in_Size);

/* A collection of external data symbols */
EXTERN_C MYOACCESSAPI unsigned int myoiMyId; /* MYO_MYID if on accelerators */
EXTERN_C MYOACCESSAPI volatile int myoiInitFlag;

 //! Structure of the array element that is passed to myoiLibInit() to initialize a subset of the available cards, or
 //! to specify a remote call function to be called after successful myo library initialization:
typedef struct {
    //!type = MYOI_USERPARAMS_DEVID or  MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC for each element in the array except
    //!the last element, type should be: MYOI_USERPARAMS_LAST_MSG.
    int type;
    //! nodeid refers to the 'one-based' card index.  Specifying, 1 represents the first card, mic0, 2 represents the
    // second card, mic1, 3 represents the third card, mic2, ....).
    // NOTE: for type == MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC, specifying MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_ALL_NODES
    // for nodeid, will execute the named function, on each card in the system, mic0, mic1, mic2, .... micn.
    int nodeid;
} MyoiUserParams;

//!The following two types are dealt with entirely with just one MyoiUserParams structure:
//!MYOI_USERPARAMS_DEVID maps node ids.
#define MYOI_USERPARAMS_DEVID                             1
//!MYOI_USERPARAMS_LAST_MSG terminates the array of MyoiUserParams.
#define MYOI_USERPARAMS_LAST_MSG                         -1

//!The following type requires setting the node id in a MyoiUserParams structure, and then following the struct
//!with a MyoiUserParamsPostLibInit union:
#define MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC            2
//!nodeid can be one of the following macros, or a number >=1, corresponding to the card number (1 == mic0,
//!2 == mic1, 3 == mic2, ....)
//!Setting nodeid to MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_ALL_NODES causes the function to be called on all
//!cards:
#define MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_ALL_NODES  0
//!Setting nodeid to MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_HOST_NODE causes the function to be called on the
//!host instead of the card:
#define MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_HOST_NODE -1

//!The postLibInit union contains two members that serves two different purposes:
//!1. It can be used to stipulate the name of the function to be remotely called from host to card, on successful
//!myo library initialization, (member postLibInitRemoveFuncName) using the type:
//!MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC.   OR
//!2. It can be an actual function pointer (member name: postLibInitHostFuncAddress) that will be called on the host,
//!on successful myo library initialization, using the type: MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC, with nodeid:
//!MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_HOST_NODE
typedef union {
   const char *postLibInitRemoveFuncName;
   void (*postLibInitHostFuncAddress)(void);
} MyoiUserParamsPostLibInit;

/* These are two macros to help get the information in a MyoiUserParamsPostLibInit union from a MyoiUserParams struct; */
#define GetPostLibInitFuncName(USERPARAMS) ((MyoiUserParamsPostLibInit *) (& (USERPARAMS)))->postLibInitRemoveFuncName
#define GetPostLibInitFuncAddr(USERPARAMS) ((MyoiUserParamsPostLibInit *) (& (USERPARAMS)))->postLibInitHostFuncAddress

/* These are two macros to help set the information in a MyoiUserParamsPostLibInit union from a MyoiUserParams struct; */
#define SetPostLibInitFuncName(USERPARAMS,FUNC_NAME) GetPostLibInitFuncName(USERPARAMS) = FUNC_NAME
#define SetPostLibInitFuncAddr(USERPARAMS,FUNC_ADDR) GetPostLibInitFuncAddr(USERPARAMS) = FUNC_ADDR

#ifdef __cplusplus
}
#endif
#endif // _MYO_IMPL_H_
/*! @} */
