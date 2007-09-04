// stacktrace.cc - Functions for unwinding & inspecting the call stack.

/* Copyright (C) 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <java-interp.h>
#include <java-stack.h>

#include <stdio.h>

#include <java/lang/Boolean.h>
#include <java/lang/Class.h>
#include <java/lang/Long.h>
#include <java/lang/reflect/Method.h>
#include <java/security/AccessController.h>
#include <java/util/ArrayList.h>
#include <java/util/IdentityHashMap.h>
#include <gnu/classpath/jdwp/Jdwp.h>
#include <gnu/classpath/VMStackWalker.h>
#include <gnu/java/lang/MainThread.h>
#include <gnu/gcj/runtime/NameFinder.h>
#include <gnu/gcj/runtime/StringBuffer.h>

#include <sysdep/backtrace.h>
#include <sysdep/descriptor.h>

using namespace java::lang;
using namespace java::lang::reflect;
using namespace java::util;
using namespace gnu::gcj::runtime;

#ifdef __ARM_EABI_UNWINDER__
#define _URC_NORMAL_STOP _URC_FAILURE
#endif

// Maps ncode values to their containing native class.
// NOTE: Currently this Map contradicts class GC for native classes. This map
// (and the "new class stack") will need to use WeakReferences in order to 
// enable native class GC.
java::util::IdentityHashMap *_Jv_StackTrace::ncodeMap;

// Check the "class stack" for any classes initialized since we were last 
// called, and add them to ncodeMap.
void 
_Jv_StackTrace::UpdateNCodeMap ()
{
  // The Map should be large enough so that a typical Java app doesn't cause 
  // it to rehash, without using too much memory. ~5000 entries should be 
  // enough.
  if (ncodeMap == NULL)
    ncodeMap = new java::util::IdentityHashMap (5087);
  
  jclass klass;
  while ((klass = _Jv_PopClass ()))
    {
      //printf ("got %s\n", klass->name->data);
      for (int i = 0; i < klass->method_count; i++)
	{
	  _Jv_Method *method = &klass->methods[i];
	  void *ncode = method->ncode;
	  // Add non-abstract methods to ncodeMap.
	  if (ncode)
	    {
	      ncode = UNWRAP_FUNCTION_DESCRIPTOR (ncode);
	      ncodeMap->put ((java::lang::Object *) ncode, klass);
	    }
	}
    }
}

// Given a native frame, return the class which this code belongs 
// to. Returns NULL if this IP is not associated with a native Java class.
// If NCODE is supplied, it will be set with the ip for the entry point of the 
// enclosing method.
jclass
_Jv_StackTrace::ClassForFrame (_Jv_StackFrame *frame)
{
  JvAssert (frame->type == frame_native);
  jclass klass = NULL;

  // look it up in ncodeMap
  if (frame->start_ip)
    {
      klass = (jclass) ncodeMap->get ((jobject) frame->start_ip);

      // Exclude interpreted classes
      if (klass != NULL && _Jv_IsInterpretedClass (klass))
	klass = NULL;
    }

  return klass;
}

_Unwind_Reason_Code
_Jv_StackTrace::UnwindTraceFn (struct _Unwind_Context *context, void *state_ptr)
{
  _Jv_UnwindState *state = (_Jv_UnwindState *) state_ptr;
  jint pos = state->pos;

  // Check if the trace buffer needs to be extended.
  if (pos == state->length)
    {
      int newLength = state->length * 2;
      void *newFrames = _Jv_AllocBytes (newLength * sizeof(_Jv_StackFrame));
      memcpy (newFrames, state->frames, state->length * sizeof(_Jv_StackFrame));      
      state->frames = (_Jv_StackFrame *) newFrames;
      state->length = newLength;
    }

  void *func_addr = (void *) _Unwind_GetRegionStart (context);

  // If we see the interpreter's main function, "pop" an entry off the 
  // interpreter stack and use that instead, so that the trace goes through 
  // the java code and not the interpreter itself. This assumes a 1:1 
  // correspondance between call frames in the interpreted stack and occurances
  // of _Jv_InterpMethod::run() on the native stack.
#ifdef INTERPRETER
  void *interp_run = NULL;
  
  if (::gnu::classpath::jdwp::Jdwp::isDebugging)
  	interp_run = (void *) &_Jv_InterpMethod::run_debug;
  else
    interp_run = (void *) &_Jv_InterpMethod::run;
  	
  if (func_addr == UNWRAP_FUNCTION_DESCRIPTOR (interp_run))
    {
      state->frames[pos].type = frame_interpreter;
      _Jv_Frame *frame = static_cast<_Jv_Frame *> (state->interp_frame);
      state->frames[pos].interp.meth 
        = static_cast<_Jv_InterpMethod *> (frame->self);
      state->frames[pos].interp.pc = state->interp_frame->pc;
      state->interp_frame = state->interp_frame->next_interp;
    }
  else 
  // We handle proxies in the same way as interpreted classes
  if (_Jv_is_proxy (func_addr))
    {
      state->frames[pos].type = frame_proxy;
      state->frames[pos].proxyClass = state->interp_frame->proxyClass;
      state->frames[pos].proxyMethod = state->interp_frame->proxyMethod;
      state->interp_frame = state->interp_frame->next_interp;
    }
  else 
#endif
    {
#ifdef HAVE_GETIPINFO
      _Unwind_Ptr ip;
      int ip_before_insn = 0;
      ip = _Unwind_GetIPInfo (context, &ip_before_insn);

      // If the unwinder gave us a 'return' address, roll it back a little
      // to ensure we get the correct line number for the call itself.
      if (! ip_before_insn)
	--ip;
#endif
      state->frames[pos].type = frame_native;
#ifdef HAVE_GETIPINFO
      state->frames[pos].ip = (void *) ip;
#else
      state->frames[pos].ip = (void *) _Unwind_GetIP (context);
#endif
      state->frames[pos].start_ip = func_addr;
    }

  _Unwind_Reason_Code result = _URC_NO_REASON;
  if (state->trace_function != NULL)
    result = (state->trace_function) (state);
  state->pos++;
  return result;
}

// Return a raw stack trace from the current point of execution. The raw 
// trace will include all functions that have unwind info.
_Jv_StackTrace *
_Jv_StackTrace::GetStackTrace(void)
{
  int trace_size = 100;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;

  _Unwind_Backtrace (UnwindTraceFn, &state);
  
  // Copy the trace and return it.
  int traceSize = sizeof (_Jv_StackTrace) + 
    (sizeof (_Jv_StackFrame) * state.pos);
  _Jv_StackTrace *trace = (_Jv_StackTrace *) _Jv_AllocBytes (traceSize);
  trace->length = state.pos;
  memcpy (trace->frames, state.frames, sizeof (_Jv_StackFrame) * state.pos);  
  return trace;
}

void
_Jv_StackTrace::getLineNumberForFrame(_Jv_StackFrame *frame, NameFinder *finder, 
				      jstring *sourceFileName, jint *lineNum,
				      jstring *methodName)
{
#ifdef INTERPRETER
  if (frame->type == frame_interpreter)
    {
      _Jv_InterpMethod *interp_meth = frame->interp.meth;
      _Jv_InterpClass *interp_class = 
	 (_Jv_InterpClass *) interp_meth->defining_class->aux_info;
      *sourceFileName = interp_class->source_file_name;
      // The interpreter advances the PC before executing an instruction,
      // so roll-back 1 byte to ensure the line number is accurate.
      *lineNum = interp_meth->get_source_line(frame->interp.pc - 1);
      return;
    }
#endif

  if (frame->type == frame_proxy)
    {
      *sourceFileName = NULL;
      *lineNum = 0;
      return;
    }

  // Use _Jv_platform_dladdr() to determine in which binary the address IP
  // resides.
  _Jv_AddrInfo info;
  jstring binaryName = NULL;
  const char *argv0 = _Jv_GetSafeArg(0);

  void *ip = frame->ip;
  _Unwind_Ptr offset = 0;

  if (_Jv_platform_dladdr (ip, &info))
    {
      if (info.file_name)
	binaryName = JvNewStringUTF (info.file_name);
      else
        return;

      if (*methodName == NULL && info.sym_name)
	*methodName = JvNewStringUTF (info.sym_name);

      // addr2line expects relative addresses for shared libraries.
      if (strcmp (info.file_name, argv0) == 0)
        offset = (_Unwind_Ptr) ip;
      else
        offset = (_Unwind_Ptr) ip - (_Unwind_Ptr) info.base;

#ifndef HAVE_GETIPINFO
      // The unwinder gives us the return address. In order to get the right
      // line number for the stack trace, roll it back a little.
      offset -= 1;
#endif

      finder->lookup (binaryName, (jlong) offset);
      *sourceFileName = finder->getSourceFile();
      *lineNum = finder->getLineNum();
      if (*lineNum == -1 && NameFinder::showRaw())
        {
          gnu::gcj::runtime::StringBuffer *t =
            new gnu::gcj::runtime::StringBuffer(binaryName);
          t->append ((jchar)' ');
          t->append ((jchar)'[');
          // + 1 to compensate for the - 1 adjustment above;
          t->append (Long::toHexString (offset + 1));
          t->append ((jchar)']');
          *sourceFileName = t->toString();
        }
    }
}

// Look up class and method info for the given stack frame, setting 
// frame->klass and frame->meth if they are known.
void
_Jv_StackTrace::FillInFrameInfo (_Jv_StackFrame *frame)
{
  jclass klass = NULL;
  _Jv_Method *meth = NULL;
  
  if (frame->type == frame_native)
    {
      klass = _Jv_StackTrace::ClassForFrame (frame);

      if (klass != NULL)
	// Find method in class
	for (int j = 0; j < klass->method_count; j++)
	  {
	    void *wncode = UNWRAP_FUNCTION_DESCRIPTOR (klass->methods[j].ncode);
	    if (wncode == frame->start_ip)
	      {
		meth = &klass->methods[j];
		break;
	      }
	  }
    }
  else if (frame->type == frame_proxy)
    {
      klass = frame->proxyClass;
      meth = frame->proxyMethod;
    }
#ifdef INTERPRETER
  else if (frame->type == frame_interpreter)
    {
      _Jv_InterpMethod *interp_meth = frame->interp.meth;
      klass = interp_meth->defining_class;
      meth = interp_meth->self;
    }
#endif
  else
    JvFail ("Unknown frame type");
  
  frame->klass = klass;
  frame->meth = meth;
}

// Convert raw stack frames to a Java array of StackTraceElement objects.
JArray< ::java::lang::StackTraceElement *>*
_Jv_StackTrace::GetStackTraceElements (_Jv_StackTrace *trace, 
  Throwable *throwable __attribute__((unused)))
{
  ArrayList *list = new ArrayList ();

#if defined (SJLJ_EXCEPTIONS) && ! defined (WIN32)
  // We can't use the nCodeMap without unwinder support. Instead,
  // fake the method name by giving the IP in hex - better than nothing.  
  jstring hex = JvNewStringUTF ("0x");

  for (int i = 0; i < trace->length; i++)
    {
      jstring sourceFileName = NULL;
      jint lineNum = -1;
      _Jv_StackFrame *frame = &trace->frames[i];
      
      jstring className = NULL;
      jstring methodName = hex->concat (Long::toHexString ((jlong) frame->ip));

      StackTraceElement *element = new StackTraceElement (sourceFileName, 
	lineNum, className, methodName, 0);    
      list->add (element);
    }

#else /* SJLJ_EXCEPTIONS && !WIN32 */

  //JvSynchronized (ncodeMap);
  UpdateNCodeMap ();

  NameFinder *finder = new NameFinder();
  int start_idx = 0;
  int end_idx = trace->length - 1;

  // First pass: strip superfluous frames from beginning and end of the trace.  
  for (int i = 0; i < trace->length; i++)
    {
      _Jv_StackFrame *frame = &trace->frames[i];
      FillInFrameInfo (frame);

      if (!frame->klass || !frame->meth)
        // Not a Java frame.
        continue;

      // Throw away the top of the stack till we see:
      //  - the constructor(s) of this Throwable, or
      //  - the Throwable.fillInStackTrace call.
      if (frame->klass == throwable->getClass()
          && strcmp (frame->meth->name->chars(), "<init>") == 0)
        start_idx = i + 1;

      if (frame->klass == &Throwable::class$
          && strcmp (frame->meth->name->chars(), "fillInStackTrace") == 0)
	start_idx = i + 1;

      // End the trace at the application's main() method if we see call_main.
      if (frame->klass == &gnu::java::lang::MainThread::class$
          && strcmp (frame->meth->name->chars(), "call_main") == 0)
	end_idx = i - 1;
    }
  
  const jboolean remove_unknown 
    = gnu::gcj::runtime::NameFinder::removeUnknown();

  // Second pass: Look up line-number info for remaining frames.
  for (int i = start_idx; i <= end_idx; i++)
    {
      _Jv_StackFrame *frame = &trace->frames[i];
      
      if (frame->klass == NULL && remove_unknown)
	// Not a Java frame.
	continue;

      jstring className = NULL;
      if (frame->klass != NULL)
	className = frame->klass->getName ();

      jstring methodName = NULL;
      if (frame->meth)
        methodName = JvNewStringUTF (frame->meth->name->chars());
      
      jstring sourceFileName = NULL;
      jint lineNum = -1;
      
      getLineNumberForFrame(frame, finder, &sourceFileName, &lineNum, 
			    &methodName);
      
      StackTraceElement *element = new StackTraceElement (sourceFileName, lineNum,
        className, methodName, 0);
      list->add (element);
    }
  
  finder->close();
#endif /* SJLJ_EXCEPTIONS && !WIN32 */

  JArray<Object *> *array = JvNewObjectArray (list->size (), 
    &StackTraceElement::class$, NULL);

  return (JArray<StackTraceElement *>*) list->toArray (array);
}

struct CallingClassTraceData
{
  jclass checkClass;    
  jclass foundClass;
  _Jv_Method *foundMeth;
  bool seen_checkClass;
};

_Unwind_Reason_Code
_Jv_StackTrace::calling_class_trace_fn (_Jv_UnwindState *state)
{
  CallingClassTraceData *trace_data = (CallingClassTraceData *)
    state->trace_data;
  _Jv_StackFrame *frame = &state->frames[state->pos];
  FillInFrameInfo (frame);

  if (trace_data->seen_checkClass
      && frame->klass
      && frame->klass != trace_data->checkClass)
    {
      trace_data->foundClass = frame->klass;
      trace_data->foundMeth = frame->meth;
      return _URC_NORMAL_STOP;
    }
  
  if (frame->klass == trace_data->checkClass)
    trace_data->seen_checkClass = true;
    
  return _URC_NO_REASON;
}

// Find the class immediately above the given class on the call stack. Any 
// intermediate non-Java 
// frames are ignored. If the calling class could not be determined (eg because 
// the unwinder is not supported on this platform), NULL is returned.
// This function is used to implement calling-classloader checks and reflection
// accessibility checks.
// CHECKCLASS is typically the class calling GetCallingClass. The first class
// above CHECKCLASS on the call stack will be returned.
jclass
_Jv_StackTrace::GetCallingClass (jclass checkClass)
{
  jclass result = NULL;
  GetCallerInfo (checkClass, &result, NULL);
  return result;
}

void
_Jv_StackTrace::GetCallerInfo (jclass checkClass, jclass *caller_class,
  _Jv_Method **caller_meth)
{
  int trace_size = 20;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;

  CallingClassTraceData trace_data;
  trace_data.checkClass = checkClass;
  trace_data.seen_checkClass = false;
  trace_data.foundClass = NULL;
  trace_data.foundMeth = NULL;

  state.trace_function = calling_class_trace_fn;
  state.trace_data = (void *) &trace_data;

  //JvSynchronized (ncodeMap);
  UpdateNCodeMap ();

  _Unwind_Backtrace (UnwindTraceFn, &state);
  
  if (caller_class)
    *caller_class = trace_data.foundClass;
  if (caller_meth)
    *caller_meth = trace_data.foundMeth;
}

_Unwind_Reason_Code
_Jv_StackTrace::non_system_trace_fn (_Jv_UnwindState *state)
{
  _Jv_StackFrame *frame = &state->frames[state->pos];
  FillInFrameInfo (frame);
  
  ClassLoader *classLoader = NULL;

  if (frame->klass)
    {
      classLoader = frame->klass->getClassLoaderInternal();
#ifdef INTERPRETER
      if (classLoader != NULL)
        {
          state->trace_data = (void *) classLoader;
	  return _URC_NORMAL_STOP;
	}
#endif
    }

  return _URC_NO_REASON;
}

ClassLoader *
_Jv_StackTrace::GetFirstNonSystemClassLoader ()
{
  int trace_size = 32;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;
  state.trace_function = non_system_trace_fn;
  state.trace_data = NULL;

  //JvSynchronized (ncodeMap);
  UpdateNCodeMap ();
  
  _Unwind_Backtrace (UnwindTraceFn, &state);

  if (state.trace_data)
    return (ClassLoader *) state.trace_data;
  
  return NULL;
}

struct AccessControlTraceData
{
  jint length;
  jboolean privileged;
};

_Unwind_Reason_Code
_Jv_StackTrace::accesscontrol_trace_fn (_Jv_UnwindState *state)
{
  AccessControlTraceData *trace_data = (AccessControlTraceData *)
    state->trace_data;
  _Jv_StackFrame *frame = &state->frames[state->pos];
  FillInFrameInfo (frame);

  if (!(frame->klass && frame->meth))
    return _URC_NO_REASON;

  trace_data->length++;

  // If the previous frame was a call to doPrivileged, then this is
  // the last frame we look at.
  if (trace_data->privileged)
    return _URC_NORMAL_STOP;
  
  if (frame->klass == &::java::security::AccessController::class$
      && strcmp (frame->meth->name->chars(), "doPrivileged") == 0)
    trace_data->privileged = true;

  return _URC_NO_REASON;
}

jobjectArray
_Jv_StackTrace::GetAccessControlStack (void)
{
  int trace_size = 100;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;

  AccessControlTraceData trace_data;
  trace_data.length = 0;
  trace_data.privileged = false;
  
  state.trace_function = accesscontrol_trace_fn;
  state.trace_data = (void *) &trace_data;

  UpdateNCodeMap();
  _Unwind_Backtrace (UnwindTraceFn, &state);

  JArray<jclass> *classes = (JArray<jclass> *)
    _Jv_NewObjectArray (trace_data.length, &::java::lang::Class::class$, NULL);
  jclass *c = elements (classes);

  for (int i = 0, j = 0; i < state.pos; i++)
    {
      _Jv_StackFrame *frame = &state.frames[i];
      if (!frame->klass || !frame->meth)
	continue;
      c[j] = frame->klass;
      j++;
    }

  jobjectArray result =
    (jobjectArray) _Jv_NewObjectArray (2, &::java::lang::Object::class$,
					 NULL);
  jobject *r = elements (result);
  r[0] = (jobject) classes;
  r[1] = (jobject) new Boolean (trace_data.privileged);
  
  return result;
}

JArray<jclass> *
_Jv_StackTrace::GetStackWalkerStack ()
{
  int trace_size = 100;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;

  UpdateNCodeMap ();
  _Unwind_Backtrace (UnwindTraceFn, &state);

  int num_frames = 0, start_frame = -1;
  enum
    {
      VMSW_GETCLASSCONTEXT,
      JLRM_INVOKE_OR_USER_FN,
      USER_FN
    }
  expect = VMSW_GETCLASSCONTEXT;
  for (int i = 0; i < state.pos; i++)
    {
      _Jv_StackFrame *frame = &state.frames[i];
      FillInFrameInfo (frame);
      if (!frame->klass || !frame->meth)
	continue;

      switch (expect)
	{
	case VMSW_GETCLASSCONTEXT:
	  JvAssert (
	    frame->klass == &::gnu::classpath::VMStackWalker::class$
	    && strcmp (frame->meth->name->chars(), "getClassContext") == 0);
	  expect = JLRM_INVOKE_OR_USER_FN;
	  break;

	case JLRM_INVOKE_OR_USER_FN:
	  if (frame->klass != &::java::lang::reflect::Method::class$
	      || strcmp (frame->meth->name->chars(), "invoke") != 0)
	    start_frame = i;
	  expect = USER_FN;
	  break;

	case USER_FN:
	  if (start_frame == -1)
	    start_frame = i;
	  break;
	}

      if (start_frame != -1)
	{
	  if (frame->klass == &::gnu::java::lang::MainThread::class$)
	    break;
	  num_frames++;
	}
    }
  JvAssert (num_frames > 0 && start_frame > 0);

  JArray<jclass> *result = (JArray<jclass> *)
    _Jv_NewObjectArray (num_frames, &::java::lang::Class::class$, NULL);
  jclass *c = elements (result);

  for (int i = start_frame, j = 0; i < state.pos && j < num_frames; i++)
    {
      _Jv_StackFrame *frame = &state.frames[i];
      if (!frame->klass || !frame->meth)
	continue;
      c[j] = frame->klass;
      j++;
    }
 
  return result;
}

typedef enum
  {
    VMSW_GET_CALLING_ITEM,
    JLRM_INVOKE_OR_CALLER,
    CALLER,
    CALLER_OF_CALLER
  } gswcc_expect;

struct StackWalkerTraceData
{
  gswcc_expect expect;
  jclass result;
};

_Unwind_Reason_Code
_Jv_StackTrace::stackwalker_trace_fn (_Jv_UnwindState *state)
{
  StackWalkerTraceData *trace_data = (StackWalkerTraceData *)
    state->trace_data;
  _Jv_StackFrame *frame = &state->frames[state->pos];
  FillInFrameInfo (frame);

  if (!(frame->klass && frame->meth))
    return _URC_NO_REASON;

  switch (trace_data->expect)
    {
    case VMSW_GET_CALLING_ITEM:
      JvAssert (frame->klass == &::gnu::classpath::VMStackWalker::class$);
      trace_data->expect = JLRM_INVOKE_OR_CALLER;
      break;

    case JLRM_INVOKE_OR_CALLER:
      if (frame->klass == &::java::lang::reflect::Method::class$
	  && strcmp (frame->meth->name->chars(), "invoke") == 0)
	trace_data->expect = CALLER;
      else
	trace_data->expect = CALLER_OF_CALLER;
      break;

    case CALLER:
      trace_data->expect = CALLER_OF_CALLER;
      break;

    case CALLER_OF_CALLER:
      trace_data->result = frame->klass;
      return _URC_NORMAL_STOP;
    }

  return _URC_NO_REASON;
}

jclass
_Jv_StackTrace::GetStackWalkerCallingClass (void)
{
  int trace_size = 100;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;

  StackWalkerTraceData trace_data;
  trace_data.expect = VMSW_GET_CALLING_ITEM;
  trace_data.result = NULL;
  
  state.trace_function = stackwalker_trace_fn;
  state.trace_data = (void *) &trace_data;

  UpdateNCodeMap();
  _Unwind_Backtrace (UnwindTraceFn, &state);

  return trace_data.result;
}

struct StackWalkerNNLTraceData
{
  gswcc_expect expect;
  ClassLoader *result;
};

_Unwind_Reason_Code
_Jv_StackTrace::stackwalker_nnl_trace_fn (_Jv_UnwindState *state)
{
  StackWalkerNNLTraceData *trace_data = (StackWalkerNNLTraceData *)
    state->trace_data;
  _Jv_StackFrame *frame = &state->frames[state->pos];
  FillInFrameInfo (frame);

  if (!(frame->klass && frame->meth))
    return _URC_NO_REASON;

  switch (trace_data->expect)
    {
    case VMSW_GET_CALLING_ITEM:
      JvAssert (frame->klass == &::gnu::classpath::VMStackWalker::class$);
      trace_data->expect = JLRM_INVOKE_OR_CALLER;
      break;

    case JLRM_INVOKE_OR_CALLER:
      if (frame->klass == &::java::lang::reflect::Method::class$
	  && strcmp (frame->meth->name->chars(), "invoke") == 0)
	trace_data->expect = CALLER;
      else
	trace_data->expect = CALLER_OF_CALLER;
      break;

    case CALLER:
      trace_data->expect = CALLER_OF_CALLER;
      break;

    case CALLER_OF_CALLER:
      ClassLoader *cl = frame->klass->getClassLoaderInternal ();
      if (cl != NULL)
	{
	  trace_data->result = cl;
	  return _URC_NORMAL_STOP;
	}
    }

  return _URC_NO_REASON;
}

ClassLoader *
_Jv_StackTrace::GetStackWalkerFirstNonNullLoader (void)
{
  int trace_size = 100;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;

  StackWalkerNNLTraceData trace_data;
  trace_data.expect = VMSW_GET_CALLING_ITEM;
  trace_data.result = NULL;
  
  state.trace_function = stackwalker_nnl_trace_fn;
  state.trace_data = (void *) &trace_data;

  UpdateNCodeMap();
  _Unwind_Backtrace (UnwindTraceFn, &state);

  return trace_data.result;
}
