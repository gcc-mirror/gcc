/* jvmti-int.h -- Internal JVMTI definitions
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

#ifndef __GCJ_JVTMI_INT_H__
#define __GCJ_JVMTI_INT_H__

/* A macro to map jvmtiEvent to an index in thread[] and enabled[]
   in the jvmtiEnv. This will only work if the order of events listed
   in jvmtiEvent and jvmtiEventCallbacks is kept the same (which should
   not be a problem). */
#define EVENT_INDEX(jvmtievent) (int)(jvmtievent - JVMTI_EVENT_VM_INIT)

/* A few globals to help limit the impact of JVMTI on normal operations.
   False means no JVMTI environment requested that event type. */
namespace JVMTI
{
  bool VMInit;
  bool VMDeath;
  bool ThreadStart;
  bool ThreadEnd;
  bool ClassFileLoadHook;
  bool ClassLoad;
  bool ClassPrepare;
  bool VMStart;
  bool Exception;
  bool ExceptionCatch;
  bool SingleStep;
  bool FramePop;
  bool Breakpoint;
  bool FieldAccess;
  bool FieldModification;
  bool MethodEntry;
  bool MethodExit;
  bool NativeMethodBind;
  bool CompiledMethodLoad;
  bool CompiledMethodUnload;
  bool DynamicCodeGenerated;
  bool DataDumpRequest;
  bool reserved72;
  bool MonitorWait;
  bool MonitorWaited;
  bool MonitorContendedEnter;
  bool MonitorContendedEntered;
  bool reserved77;
  bool reserved78;
  bool reserved79;
  bool reserved80;
  bool GarbageCollectionStart;
  bool GarbageCollectionFinish;
  bool ObjectFree;
  bool VMObjectAlloc;
};

/* A macro to test whether an event should be posted to JVMTI.*/
#define JVMTI_REQUESTED_EVENT(Event) __builtin_expect (JVMTI::Event, false)

/* Post the event to requesting JVMTI environments.

   For speed, this function should only be called after 
   JVMTI_REQUESTED_EVENT is checked. */
extern void _Jv_JVMTI_PostEvent (jvmtiEvent type, jthread event_thread,				 ...);
#endif /* __GCJ_JVMTI_INT_H__ */
