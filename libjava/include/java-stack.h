// java-stack.h - Definitions for unwinding & inspecting the call stack.

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_STACKTRACE_H__
#define __JV_STACKTRACE_H__

#include <unwind.h>

#include <gcj/cni.h>
#include <gcj/javaprims.h>

#include <java-interp.h>

#include <java/lang/Class.h>
#include <java/lang/StackTraceElement.h>
#include <java/lang/Throwable.h>

#include <gnu/gcj/runtime/NameFinder.h>

using namespace gnu::gcj::runtime;

enum _Jv_FrameType
{
  frame_native,
  frame_interpreter
};

#ifdef INTERPRETER
struct _Jv_InterpFrameInfo
{
  _Jv_InterpMethod *meth;
  pc_t pc;
};
#endif

union _Jv_FrameInfo
{
};

struct _Jv_StackFrame
{
  _Jv_FrameType type;   /* Native or interpreted.  */
  union {
#ifdef INTERPRETER
    _Jv_InterpFrameInfo interp;
#endif
    void *ip;  
  };
//  _Jv_FrameInfo info;   /* Frame-type specific data.  */
  jclass klass;
  _Jv_Method *meth;
};

class _Jv_StackTrace
{
private:
  int length;
  _Jv_StackFrame frames[];

  static void UpdateNCodeMap ();
  static jclass ClassForIP (void *ip, void **ncode);
  static void FillInFrameInfo (_Jv_StackFrame *frame);
  static void getLineNumberForFrame(_Jv_StackFrame *frame, NameFinder *finder, 
			     jstring *sourceFileName, jint *lineNum);
  
  static _Unwind_Reason_Code UnwindTraceFn (struct _Unwind_Context *context, 
    void *state_ptr);

public:
  static _Jv_StackTrace *GetStackTrace (void);
  static JArray< ::java::lang::StackTraceElement *>*
    GetStackTraceElements (_Jv_StackTrace *trace, 
    java::lang::Throwable *throwable);
  static jclass GetCallingClass (void);
};

#endif /* __JV_STACKTRACE_H__ */
