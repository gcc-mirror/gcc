// natVMSelectorImplWin32.cc

/* Copyright (C) 2003, 2004, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gnu/java/nio/VMSelector.h>
#include <java/lang/Thread.h>

jint
gnu::java::nio::VMSelector::select (jintArray read, jintArray write,
                                    jintArray except, jlong timeout)
{
  // FIXME: The API for implSelect is biased towards POSIX implementations.
  jint* pReadFD = elements (read);
  int nNbReadFDs = JvGetArrayLength (read);

  jint* pWriteFD = elements (write);
  int nNbWriteFDs = JvGetArrayLength (write);
  
  int nNbEvents = nNbReadFDs + nNbWriteFDs;
  
  // Create and initialize our event wrapper array
  
  // FIXME: We're creating fresh WSAEVENTs for each call.
  // This is inefficient. It would probably be better to cache these
  // in the Win32 socket implementation class.
  WSAEventWrapper aArray[nNbEvents];

  int nCurIndex = 0;
  for (int i=0; i < nNbReadFDs; ++i)
    aArray[nCurIndex++].init(pReadFD[i], FD_ACCEPT | FD_READ);

  for (int i=0; i < nNbWriteFDs; ++i)
    aArray[nCurIndex++].init(pWriteFD[i], FD_WRITE);

  // Build our array of WSAEVENTs to wait on. Also throw in our thread's
  // interrupt event in order to detect thread interruption.
  HANDLE arh[nNbEvents + 1];
  for (int i=0; i < nNbEvents; ++i)
    arh[i] = aArray[i].getEventHandle();
  arh[nNbEvents] = _Jv_Win32GetInterruptEvent ();
  
  // A timeout value of 0 needs to be treated as infinite.
  if (timeout <= 0)
    timeout = WSA_INFINITE;

  // Do the select.
  DWORD dwRet = WSAWaitForMultipleEvents (nNbEvents+1, arh, 0, timeout, false);
  
  if (dwRet == WSA_WAIT_FAILED)
    _Jv_ThrowIOException ();

  // Before we do anything else, clear output file descriptor arrays.
  memset(pReadFD, 0, sizeof(jint) * nNbReadFDs);
  memset(pWriteFD, 0, sizeof(jint) * nNbWriteFDs);
  memset(elements (except), 0, sizeof(jint) * JvGetArrayLength (except));
  
  if (dwRet == DWORD(WSA_WAIT_EVENT_0 + nNbEvents))
    {
      // We were interrupted. Set the current thread's interrupt
      // status and get out of here, with nothing selected..
      ::java::lang::Thread::currentThread ()->interrupt ();
      return 0;
    }
  else if (dwRet < DWORD(WSA_WAIT_EVENT_0 + nNbEvents))
    {
      int nSelectedEventIndex = dwRet - WSA_WAIT_EVENT_0;

      // Record the selected file descriptor.
      // FIXME: This implementation only allows one file descriptor
      // to be selected at a time. Remedy this by looping on
      // WSAWaitForMultipleEvents 'til nothing more is selected.
      jint fd = aArray[nSelectedEventIndex].getFD();
      if (nSelectedEventIndex < nNbReadFDs)
        pReadFD[0] = fd;
      else
        pWriteFD[0] = fd;

      return 1;  
    }
  else
    // None of the event objects was signalled, so nothing was
    // selected.
    return 0;
}
