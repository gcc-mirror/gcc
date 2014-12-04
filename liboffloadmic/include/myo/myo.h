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
/**
 * Description:
 *   External APIs of MYO runtime (MYO stands for Mine, Yours and Ours).
 **/

#ifndef _MYO_H_
#define _MYO_H_

#include "myotypes.h"
#include "myoimpl.h"

/** @ingroup MYO
 *  @addtogroup MYO_API
@{
* @file myo.h 
*/
#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************************
    Arena-based APIs
 ***************************************************************************/

/* 
 * Arena is a set of memory pages with the same ownership type. The ownership
 * type of all the memory pages inside the same arena can only be changed as
 * a whole. For "OURS", it is also the minimal unit of sync operations to
 * implement "release consistency".
 */

/** @fn extern MyoError myoArenaCreate(MyoOwnershipType in_Type, 
 * int in_Property, MyoArena *out_pArena)
 * @brief Create an arena with specified ownership type and property.
 * 
 * @param in_Type Specified ownership type (MYO_ARENA_OURS or 
 * MYO_ARENA_MINE).
 * @param in_Property Specified properties of the arena. Set it 
 * to 0 to use default properties.
 *
 *      MYO_RELEASE_CONSISTENCY or MYO_STRONG_RELEASE_CONSISTENCY
 *      or MYO_STRONG_CONSISTENCY: 
 *
 *          Consistency modes for "OURS" arenas. For MYO_RELEASE_CONSISTENCY,
 *          there are 2 functions, "Acquire" and "Release", which are 
 *          used for memory ordering. "Release" makes all local stores 
 *          prior to the release globally visible; "Acquire" syncs up the 
 *          local memory with all stores that have been made globally 
 *          visible. However, there is no definite answer as to whether 
 *          local stores can be globally visible before reaching a release 
 *          point, nor whether the newest globally visible stores can be 
 *          updated to local before reaching an acquire point. By using 
 *          MYO_STRONG_RELEASE_CONSISTENCY, the answer to these questions
 *          is "no". A sequential consistency model is maintained to the 
 *          arena when using MYO_STRONG_CONSISTENCY. 
 *          MYO_RELEASE_CONSISTENCY is the default property.
 *
 *      MYO_UPDATE_ON_DEMAND or MYO_UPDATE_ON_ACQUIRE: 
 *
 *          Only apply to "OURS" arenas with "Release Consistency".
 *          MYO_UPDATE_ON_ACQUIRE means that the shared pages of this 
 *          arena will be updated on acquire point; MYO_UPDATE_ON_DEMAND
 *          means that the shared pages will not be updated until they 
 *          are accessed. MYO_UPDATE_ON_DEMAND is the default property.
 *
 *      MYO_RECORD_DIRTY or MYO_NOT_RECORD_DIRTY:
 *
 *          This property controls whether to record dirty pages. 
 *          There will be runtime overhead when recording dirty pages, 
 *          as it can reduce the communication data. It is a trade-off 
 *          for performance. Also when MYO_NOT_RECORD_DIRTY is set for 
 *          "OURS" arena, the runtime cannot guarantee the correctness 
 *          when the host and card modify the same shared page between 
 *          the same sync segment. MYO_RECORD_DIRTY is the default 
 *          property.
 *
 *      MYO_ONE_VERSION or MYO_MULTI_VERSION: 
 *
 *          Only apply to "OURS" arenas with "Release Consistency". When
 *          MYO_MULTI_VERSION is set, this arena can only be "release" on 
 *          HOST side and "acquire" on CARD side. Releasing the arena on 
 *          HOST will create a new versioned data and put it into a FIFO; 
 *          acquiring the arena on CARD will get the versioned data 
 *          from the FIFO one by one. MYO_ONE_VERSION is the default 
 *          property.
 *
 *      MYO_CONSISTENCY or MYO_NO_CONSISTENCY: 
 *
 *          Only apply to "OURS" arenas with "Release Consistency". When 
 *          MYO_NO_CONSISTENCY is set, the consistency of the arena will
 *          not be maintained. That is, it is a no-op operation when 
 *          calling acquire/release for such arenas. MYO_CONSISTENCY is 
 *          the default property.
 *
 *      MYO_HOST_TO_DEVICE and MYO_DEVICE_TO_HOST: 
 *
 *          When it is certain that there is only one communication
 *          direction for this arena, it can be created with only
 *          MYO_HOST_TO_DEVICE or MYO_DEVICE_TO_HOST so the runtime 
 *          can perform optimizations. The default property is 
 *          MYO_HOST_TO_DEVICE | MYO_DEVICE_TO_HOST.
 *
 * @param out_pArena Used to store the handle of the created arena.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
CILK_SHARED MyoError myoArenaCreate(MyoOwnershipType in_Type, int in_Property, MyoArena *out_pArena);

/** @fn extern MyoError myoArenaDestroy(MyoArena in_Arena)
 * @brief Destroy an arena. As a result, the arena can not be 
 * referred any more.
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
CILK_SHARED MyoError myoArenaDestroy(MyoArena in_Arena);

/** @fn extern void *myoArenaMalloc(MyoArena in_Arena, size_t in_Size)
 * @brief Allocates size bytes from the specified arena, and returns 
 * the start address of the allocated memory. The memory is not 
 * cleared.
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @param in_Size Size (bytes) of the required memory space.
 * @return
 *      The start address of the allocated memory space.
 *      NULL: Failed.
 **/
MYOACCESSAPI
void *myoArenaMalloc(MyoArena in_Arena, size_t in_Size);

/** @fn extern void myoArenaFree(MyoArena in_Arena, void *in_pPtr)
 * @brief Frees the memory space allocated by myoArenaMalloc to the 
 * specified arena.
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @param in_pPtr The start address of the specified memory 
 * space, which must be retured by myoArenaMalloc.
 * @return
 **/
MYOACCESSAPI
void myoArenaFree(MyoArena in_Arena, void *in_pPtr);

/** @fn extern void *myoArenaAlignedMalloc(MyoArena in_Arena, 
 *                     size_t in_Size, size_t in_Alignment)
 * @brief Allocates size bytes from the specified arena. The 
 * start address of the allocated memory will be a multiple of the 
 * alignment, which must be a power of two.
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @param in_Size Size (bytes) of the required memory space.
 * @param in_Alignment The alignment value (must be a power 
 * of two).
 * @return
 *      The start address of the allocated memory space.
 *      NULL: Failed.
 **/
MYOACCESSAPI
void *myoArenaAlignedMalloc(MyoArena in_Arena, size_t in_Size, size_t in_Alignment);

/** @fn extern void myoArenaAlignedFree(MyoArena in_Arena, void *in_pPtr)
 * @brief Frees the memory space allocated by myoArenaAlignedMalloc 
 * to the specified arena.
 *
 * @param in_Arena Arena handle returned by previous call to
 * myoArenaCreate.
 * @param in_pPtr The start address of the specified memory space,
 * which must be returned by myoArenaAlignedMalloc.
 * @return
 **/
MYOACCESSAPI
void myoArenaAlignedFree(MyoArena in_Arena, void *in_pPtr);

/** @fn extern MyoError myoArenaAcquire(MyoArena in_Arena)
 * @brief myoArenaAcquire is the sync point for "OURS" arena with
 * "Release Consistency". myoArenaAcquire is used to obtain all
 * stores of "OURS" arena that have been made globally visible prior
 * to this point. 
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
CILK_SHARED MyoError myoArenaAcquire(MyoArena in_Arena);

/** @fn extern MyoError myoArenaRelease(MyoArena in_Arena)
 * @brief myoArenaRelease is the sync point for "OURS" arena with
 * "Release Consistency". myoArenaRelease is used to flush all prior
 * stores of "OURS" arena to be globally visible.
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
CILK_SHARED MyoError myoArenaRelease(MyoArena in_Arena);

/** @fn extern MyoError myoArenaAcquireOwnership(MyoArena in_Arena)
 * @brief Changes the ownership type of the arena to MYO_ARENA_MINE.
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoArenaAcquireOwnership(MyoArena in_Arena);

/** @fn extern MyoError myoArenaReleaseOwnership(MyoArena in_Arena)
 * @brief Change the ownership type of the arena to MYO_ARENA_OURS.
 *
 * @param in_Arena Arena handle returned by previous call to 
 * myoArenaCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoArenaReleaseOwnership(MyoArena in_Arena);

/** @fn extern MyoError myoArenaGetHandle(void *in_pPtr, 
 * MyoArena *out_pArena)
 * @brief Gets the arena handle of the arena that contains the memory
 * space pointed to by "in_pPtr". This API can be used when it is 
 * not clear which arena handle should be used for other arena 
 * related APIs.
 * 
 * @param in_pPtr The start address of a chunk of memory space.
 * @param out_pArena Handle of the arena.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
CILK_SHARED MyoError myoArenaGetHandle(void *in_pPtr, MyoArena *out_pArena);

/********************************************************************************
    APIs for the default arena
 *******************************************************************************/

/**
 * There will be a default arena inside MYO runtime, which will be used when
 * there is no specified arena.
 **/

/** @fn extern void* myoSharedMalloc(size_t in_Size)
 * @brief Allocates size bytes from the default arena, and returns the
 * start address of the allocated memory. The memory is not cleared.
 *
 @param in_Size Size (bytes) of the required memory space.
 * @return 
 *      The start address of the allocated memory space.
 *      NULL: Failed.
 **/
MYOACCESSAPI
void* myoSharedMalloc(size_t in_Size);

/** @fn extern void  myoSharedFree(void *in_pPtr)
 * @brief Frees the memory space allocated by myoArenaMalloc to the
 * default arena.
 *
 * @param in_pPtr The start address of the specified memory space,
 * which must be retured by myoSharedMalloc.
 * @return
 **/
MYOACCESSAPI
void  myoSharedFree(void *in_pPtr);

/** @fn extern void* myoSharedAlignedMalloc(size_t in_Size, 
 * size_t in_Alignment)
 * @brief Allocates size bytes from the default arena. The start 
 * address of the allocated memory will be a multiple of alignment, 
 * which must be a power of two.
 *
 * @param in_Size Size (bytes) of the required memory space.
 * @param in_Alignment The alignment value, which must be an power of two.
 * @return
 *      The start address of the allocated memory space.
 *      NULL: Failed.
 **/
MYOACCESSAPI
void* myoSharedAlignedMalloc(size_t in_Size, size_t in_Alignment);

/** @fn extern void  myoSharedAlignedFree(void *in_pPtr)
 * @brief Frees the memory space allocated by myoArenaAlignedMalloc 
 * to the default arena.
 *
 * @param in_pPtr The start address of the specified memory space,
 *      which must be returned by myoArenaAlignedMalloc.
 * @return
 **/
MYOACCESSAPI
void  myoSharedAlignedFree(void *in_pPtr);

/** @fn extern MyoError myoAcquire()
 * @brief myoAcquire is the sync point for the default arena with
 * "Release Consistency". myoAcquire is used to obtain all stores of
 * the default arena that have been made globally visible prior to 
 * this point.
 *
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoAcquire();

/** @fn extern MyoError myoRelease()
 * @brief myoRelease is the sync point for the default arena with
 * "Release Consistency". myoRelease is used to flush all prior stores
 * of the default arena to be globally visible.
 *
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoRelease();

/** @fn extern MyoError myoAcquireOwnership()
 * @brief Changes the ownership type of the default arena to 
 * MYO_ARENA_MINE.
 *
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoAcquireOwnership();

/** @fn extern MyoError myoReleaseOwnership()
 * @brief Change the ownership type of the default arena to 
 * MYO_ARENA_OURS.
 *
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoReleaseOwnership();

/*****************************************************************************
    APIs for global sync operations.
 *****************************************************************************/

/** @fn extern MyoError myoMutexCreate(MyoMutex *out_pMutex)
 * @brief Create a mutex and return the mutex handle.
 *
 * @param out_pMutex Used to store the handle of the created mutex.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoMutexCreate(MyoMutex *out_pMutex);

/** @fn extern MyoError myoMutexLock(MyoMutex in_Mutex)
 * @brief Lock the mutex. If the mutex is already locked by other peers,
 * the call blocks until the mutex becomes available. Currently, 
 * attempting to re-acquire the mutex will cause a deadlock.
 *
 * @param in_Mutex the mutex handle returned by myoMutexCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoMutexLock(MyoMutex in_Mutex);

/** @fn extern MyoError myoMutexUnlock(MyoMutex in_Mutex)
 * @brief Release the locked mutex.
 * Currently, attempting to release an unlocked mutex will cause
 * undefined results.
 *
 * @param in_Mutex the mutex handle returned by myoMutexCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoMutexUnlock(MyoMutex in_Mutex);

/** @fn extern MyoError myoMutexTryLock(MyoMutex in_Mutex)
 * @brief Try to lock the mutex. myoMutexTryLock is equivalent to 
 * myoMutexLock, except that myoMutexLock will return immediately if 
 * the mutex is already locked.
 *
 * @param in_Mutex the mutex handle returned by myoMutexCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoMutexTryLock(MyoMutex in_Mutex);

/** @fn extern MyoError myoMutexDestroy(MyoMutex in_Mutex)
 * @brief Destroy the mutex.
 *
 * @param in_Mutex the mutex handle returned by myoMutexCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoMutexDestroy(MyoMutex in_Mutex);

/** @fn extern MyoError myoSemCreate(int in_Value, MyoSem *out_pSem)
 * @brief Create a semaphore and return the semaphore handle.
 *
 * @param in_Value the initial value for the semaphore.
 * @param out_pSem Used to store the handle of the created semaphore.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoSemCreate(int in_Value, MyoSem *out_pSem);

/** @fn extern MyoError myoSemWait(MyoSem in_Sem)
 * @brief Decrements (locks) the semaphore. If the semaphore value is
 * greater than zero, then the decrement proceeds and the function
 * returns immediately, or else the call blocks until the semaphore
 * value rises above zero.
 *
 * @param in_Sem the semaphore handle returned by myoSemCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoSemWait(MyoSem in_Sem);

/** @fn extern MyoError myoSemPost(MyoSem in_Sem)
 * @brief Increments (unlocks) the semaphore. If the semaphore value
 * becomes greater than zero, one blocked myoSemWait call will be 
 * notified to return.
 *
 * @param in_Sem the semaphore handle returned by myoSemCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoSemPost(MyoSem in_Sem);

/** @fn extern MyoError myoSemTryWait(MyoSem in_Sem)
 * @brief Try to lock semaphore. myoSemTryWait is the same as 
 * myoSemAcquire, except that if the decrement cannot be immediately 
 * performed, then the call returns instead of blocking.
 *
 * @param in_Sem the semaphore handle returned by myoSemCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoSemTryWait(MyoSem in_Sem);

/** @fn extern MyoError myoSemDestroy(MyoSem in_Sem)
 * @brief Destroy the semaphore.
 *
 * @param in_Sem the semaphore handle returned by myoSemCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoSemDestroy(MyoSem in_Sem);

/** @fn extern MyoError myoBarrierCreate(int in_Count, MyoBarrier *out_pBarrier) 
 * @brief Create a barrier and return the barrier handle.
 *
 * @param in_Count the number of threads that must call
 * myoBarrierWait before any of them successfully return.
 * @param out_pBarrier Used to store the handle of the created 
 * barrier.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoBarrierCreate(int in_Count, MyoBarrier *out_pBarrier);

/** @fn extern MyoError myoBarrierWait(MyoBarrier in_Barrier)
 * @brief The caller will block until the required number of threads 
 * have called myoBarrierWait with the same barrier handle.
 *
 * @param in_Barrier the barrier handle returned by myoBarrierCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoBarrierWait(MyoBarrier in_Barrier);

/** @fn extern MyoError myoBarrierDestroy(MyoBarrier in_Barrier)
 * @brief Destroy the barrier.
 *
 * @param in_Barrier the barrier handle returned by myoBarrierCreate.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoBarrierDestroy(MyoBarrier in_Barrier);

/*****************************************************************************
    MISC APIs.
 *****************************************************************************/

/**
 * @cond INCLUDE_MYO_INTERNAL_DOCUMENTATION
 **/

MYOACCESSAPI
int myoMyId();
/* int myoNumNodes() returns the number of peers, minus one, to
   equal the number of cards in the system. */
MYOACCESSAPI
int myoNumNodes();

MYOACCESSAPI
unsigned long long myoTicks();
MYOACCESSAPI
unsigned long long myoWallTime();
MYOACCESSAPI
void myoStatOn();
MYOACCESSAPI
void myoStatOff();

/** @fn extern MyoError myoGetMemUsage(uint64 *out_memUsedMB) 
 * @brief Retrieves the amount of shared memory currently used.
 * myoGetMemUsage() fills in out_memUsedMB when the pointer is not NULL.
 *
 * @param out_memUsedBytes, pointer to the current size shared memory used.
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
MYOACCESSAPI
MyoError myoGetMemUsage(unsigned int *out_memUsedMB);

/** @fn extern MyoError myoHTimeOn(int in_On) 
 * @brief Toggle MYO HTime report feature on/off.
 *
 * @param in_On: 1 turn on MYO HTime report
 *               0 turn off MYO HTime report
 * @return
 *      MYO_SUCCESS; or
 *      an error number to indicate the error.
 **/
extern MyoError myoHTimeOn(int in_On);


#ifdef __cplusplus
}
#endif

#endif

 /**
 * @endcond
 **/

/*! @} */
