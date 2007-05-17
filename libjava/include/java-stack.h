// java-stack.h - Definitions for unwinding & inspecting the call stack.

/* Copyright (C) 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_STACKTRACE_H__
#define __JV_STACKTRACE_H__

#include <stdlib.h>
#include <unwind.h>

#include <gcj/cni.h>
#include <gcj/javaprims.h>

#include <java-interp.h>

#include <java/lang/Class.h>
#include <java/lang/StackTraceElement.h>
#include <java/lang/Throwable.h>
#include <java/lang/Thread.h>
#include <java/util/IdentityHashMap.h>

#include <gnu/gcj/runtime/NameFinder.h>

using namespace gnu::gcj::runtime;
using namespace java::lang;

extern "Java"
{
  namespace gnu
  {
    namespace classpath
    {
        class VMStackWalker;
    }
  }
}

#ifdef INTERPRETER
struct _Jv_InterpFrameInfo
{
  _Jv_InterpMethod *meth;
  pc_t pc;
};
#endif

struct _Jv_StackFrame
{
  _Jv_FrameType type;   /* Native or interpreted.  */
  union {
#ifdef INTERPRETER
    _Jv_InterpFrameInfo interp;
#endif
    struct {
      jclass proxyClass;
      _Jv_Method *proxyMethod;
    };
    struct {
      void *ip;
      void *start_ip;
    };
  };
  jclass klass;
  _Jv_Method *meth;
};

struct _Jv_UnwindState;
typedef _Unwind_Reason_Code (*_Jv_TraceFn) (_Jv_UnwindState *);

struct _Jv_UnwindState
{
  jint length;                   // length of FRAMES
  jint pos;                      // current position in FRAMES
  _Jv_StackFrame *frames;        // array of stack frame data to be filled.
#ifdef INTERPRETER
  _Jv_InterpFrame *interp_frame; // current frame in the interpreter stack.
#endif
  _Jv_TraceFn trace_function;    // function to call back after each frame
  				 // is enumerated. May be NULL.
  void *trace_data;		 // additional state data for trace_function.
  
  _Jv_UnwindState (jint ln)
    {
      length = ln;
      pos = 0;
      frames = NULL;
#ifdef INTERPRETER
      Thread *thread = Thread::currentThread();
      // Check for NULL currentThread(), in case an exception is created 
      // very early during the runtime startup.
      if (thread)
	interp_frame = (_Jv_InterpFrame *) thread->interp_frame;
      else
	interp_frame = NULL;
#endif
      trace_function = NULL;
      trace_data = NULL;
    }
};

class _Jv_StackTrace
{
private:
  int length;
  _Jv_StackFrame frames[];

  static java::util::IdentityHashMap *ncodeMap;
  static void UpdateNCodeMap ();
  static jclass ClassForFrame (_Jv_StackFrame *frame);
  static void FillInFrameInfo (_Jv_StackFrame *frame);
  static void getLineNumberForFrame(_Jv_StackFrame *frame, NameFinder *finder, 
				    jstring *sourceFileName, jint *lineNum,
				    jstring *methodName);
  
  static _Unwind_Reason_Code UnwindTraceFn (struct _Unwind_Context *context, 
    void *state_ptr);
    
  static _Unwind_Reason_Code calling_class_trace_fn (_Jv_UnwindState *state);
  static _Unwind_Reason_Code non_system_trace_fn (_Jv_UnwindState *state);
  static _Unwind_Reason_Code accesscontrol_trace_fn (_Jv_UnwindState *state);
  static _Unwind_Reason_Code stackwalker_trace_fn (_Jv_UnwindState *state);
  static _Unwind_Reason_Code stackwalker_nnl_trace_fn (_Jv_UnwindState *state);

public:
  static _Jv_StackTrace *GetStackTrace (void);
  static JArray< ::java::lang::StackTraceElement *>*
    GetStackTraceElements (_Jv_StackTrace *trace, 
    java::lang::Throwable *throwable);
  static jclass GetCallingClass (jclass);
  static void GetCallerInfo (jclass checkClass, jclass *, _Jv_Method **);
  static ClassLoader *GetFirstNonSystemClassLoader (void);
  static jobjectArray GetAccessControlStack ();
  static JArray<jclass> *GetStackWalkerStack ();
  static jclass GetStackWalkerCallingClass ();
  static ClassLoader *GetStackWalkerFirstNonNullLoader ();

  friend jclass _Jv_GetMethodDeclaringClass (jmethodID);
  friend class gnu::classpath::VMStackWalker;
};

// Information about a given address.
struct _Jv_AddrInfo
{
  // File name of the defining module.
  const char *file_name;

  // Base address of the loaded module.
  void *base;

  // Name of the nearest symbol.
  const char *sym_name;

  // Address of the nearest symbol.
  void *sym_addr;

  ~_Jv_AddrInfo (void)
    {
      // On systems with a real dladdr(), the file and symbol names given by
      // _Jv_platform_dladdr() are not dynamically allocated.  On Windows,
      // they are.

#ifdef WIN32
      if (file_name)
        free ((void *)file_name);

      if (sym_name)
        free ((void *)sym_name);
#endif /* WIN32 */
    }
};

#endif /* __JV_STACKTRACE_H__ */
