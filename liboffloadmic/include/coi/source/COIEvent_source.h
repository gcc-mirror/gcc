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

#ifndef _COIEVENT_SOURCE_H
#define _COIEVENT_SOURCE_H

/** @ingroup COIEvent
 *  @addtogroup COIEventSource
@{
* @file source/COIEvent_source.h
*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS

#include "../common/COITypes_common.h"
#include "../common/COIResult_common.h"

#ifdef __cplusplus
extern "C" {
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS
///////////////////////////////////////////////////////////////////////////////
///
/// Special case event values which can be passed in to APIs to specify
/// how the API should behave. In COIBuffer APIs passing in NULL for the
/// completion event is the equivalent of passing COI_EVENT_SYNC. For
/// COIPipelineRunFunction passing in NULL is the equivalent of
/// COI_EVENT_ASYNC.
/// Note that passing COI_EVENT_ASYNC can be used when the caller wishes the
/// operation to be performed asynchronously but does not care when the
/// operation completes. This can be useful for opertions that by definition
/// must complete in order (DMAs, run functions on a single pipeline). If
/// the caller does care when the operation completes then they should pass
/// in a valid completion event which they can later wait on.
///
#define COI_EVENT_ASYNC ((COIEVENT*)1)
#define COI_EVENT_SYNC  ((COIEVENT*)2)

///////////////////////////////////////////////////////////////////////////////
///
/// Wait for an arbitrary number of COIEVENTs to be signaled as completed,
/// eg when the run function or asynchronous map call associated with an event
/// has finished execution.
/// If the user sets in_WaitForAll = True and not all of the events are
/// signaled when the timeout period is reached then COI_TIME_OUT_REACHED will
/// be returned.
/// If the user sets in_WaitForAll = False then if at least one event is
/// signaled when the timeout is reached then COI_SUCCESS is returned.
///
/// @param  in_NumEvents
///         [in] The number of events to wait for.
///
/// @param  in_pEvents
///         [in] The array of COIEVENT handles to wait for.
///
/// @param  in_Timeout
///         [in] The time in milliseconds to wait for the event. 0 polls
///         and returns immediately, -1 blocks indefinitely.
///
/// @param  in_WaitForAll
///         [in] Boolean value specifying behavior.  If true, wait for all
///         events to be signaled, or for timeout, whichever happens first.
///         If false, return when any event is signaled, or at timeout.
///
/// @param  out_pNumSignaled
///         [out] The number of events that were signaled.  If in_NumEvents
///         is 1 or in_WaitForAll = True, this parameter is optional.
///
/// @param  out_pSignaledIndices
///         [out] Pointer to an array of indicies into the original event
///         array.  Those denoted have been signaled.  The user must provide an
///         array that is no smaller than the in_Events array. If in_NumEvents
///         is 1 or in_WaitForAll = True, this parameter is optional.
///
/// @return COI_SUCCESS once an event has been signaled completed.
///
/// @return COI_TIME_OUT_REACHED if the events are still in use when the
///         timeout is reached or timeout is zero (a poll).
///
/// @return COI_OUT_OF_RANGE if a negative value other than -1 is passed in to
///         the in_Timeout parameter.
///
/// @return COI_OUT_OF_RANGE if in_NumEvents is 0.
///
/// @return COI_INVALID_POINTER if in_pEvents is NULL.
///
/// @return COI_ARGUMENT_MISMATCH if in_NumEvents > 1 and if in_WaitForAll
///         is not true and out_pSignaled or out_pSignaledIndicies are NULL.
///
/// @return COI_ARGUMENT_MISMATCH if out_pNumSignaled is not NULL
///         and out_pSignaledIndices is NULL (or vice versa).
///
/// @return COI_EVENT_CANCELED if while waiting on a user event, it gets
///         unregistered this returns COI_EVENT_CANCELED
///
/// @return COI_PROCESS_DIED if the remote process died. See COIProcessDestroy
///         for more details.
///
COIACCESSAPI
COIRESULT
COIEventWait(
            uint16_t        in_NumEvents,
    const   COIEVENT*       in_pEvents,
            int32_t         in_TimeoutMilliseconds,
            uint8_t         in_WaitForAll,
            uint32_t*       out_pNumSignaled,
            uint32_t*       out_pSignaledIndices);



///////////////////////////////////////////////////////////////////////////////
///
/// Register a User COIEVENT so that it can be fired. Registered event is
/// a one shot User event; in other words once signaled it cannot be used
/// again for signaling. You have to unregister and register again to enable
/// signaling. An event will be reset if it is re-registered without
/// unregistering, resulting in loss of all outstanding signals.
///
/// @param  out_pEvent
///         [out] Pointer to COIEVENT handle being Registered
///
/// @return COI_SUCCESS an event is successfully registered
///
/// @return COI_INVALID_POINTER if out_pEvent is NULL
///
COIACCESSAPI
COIRESULT
COIEventRegisterUserEvent(
            COIEVENT* out_pEvent);


///////////////////////////////////////////////////////////////////////////////
///
/// Unregister a User COIEVENT. Unregistering a unsignaled event is similar
/// to firing an event. Except Calling COIEventWait on an event that is
/// being unregistered returns COI_EVENT_CANCELED
///
/// @param  in_Event
///         [in] Event Handle to be unregistered.
///
/// @return COI_INVALID_HANDLE if in_Event is not a UserEvent
///
/// @return COI_SUCCESS an event is successfully registered
///
COIACCESSAPI
COIRESULT
COIEventUnregisterUserEvent(
            COIEVENT in_Event);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _COIEVENT_SOURCE_H */

/*! @} */
