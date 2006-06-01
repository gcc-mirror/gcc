// stacktrace.cc - Functions for unwinding & inspecting the call stack.

/* Copyright (C) 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <java-interp.h>
#include <java-stack.h>

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include <stdio.h>

#include <java/lang/Class.h>
#include <java/lang/Long.h>
#include <java/util/ArrayList.h>
#include <java/util/IdentityHashMap.h>
#include <gnu/java/lang/MainThread.h>
#include <gnu/gcj/runtime/NameFinder.h>

#include <sysdep/backtrace.h>
#include <sysdep/descriptor.h>

using namespace java::lang;
using namespace java::lang::reflect;
using namespace java::util;
using namespace gnu::gcj::runtime;

// Maps ncode values to their containing native class.
// NOTE: Currently this Map contradicts class GC for native classes. This map
// (and the "new class stack") will need to use WeakReferences in order to 
// enable native class GC.
static java::util::IdentityHashMap *ncodeMap;

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
    if (!_Jv_IsInterpretedClass (klass))
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
  // use _Unwind_FindEnclosingFunction to find start of method
  //void *entryPoint = _Unwind_FindEnclosingFunction (ip);

  // look it up in ncodeMap
  if (frame->start_ip)
    klass = (jclass) ncodeMap->get ((jobject) frame->start_ip);

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
  void *interp_run = (void *) &_Jv_InterpMethod::run;
  if (func_addr == UNWRAP_FUNCTION_DESCRIPTOR (interp_run))
    {
      state->frames[pos].type = frame_interpreter;
      state->frames[pos].interp.meth = state->interp_frame->self;
      state->frames[pos].interp.pc = state->interp_frame->pc;
      state->interp_frame = state->interp_frame->next;
    }
  else
#endif
    {
      state->frames[pos].type = frame_native;
      state->frames[pos].ip = (void *) _Unwind_GetIP (context);
      state->frames[pos].start_ip = func_addr;
    }

  //printf ("unwind ip: %p\n", _Unwind_GetIP (context));

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

#ifdef SJLJ_EXCEPTIONS
  // The Unwind interface doesn't work with the SJLJ exception model.
  // Fall back to a platform-specific unwinder.
  fallback_backtrace (&state);
#else /* SJLJ_EXCEPTIONS */  
  _Unwind_Backtrace (UnwindTraceFn, &state);
#endif /* SJLJ_EXCEPTIONS */
  
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
      *lineNum = interp_meth->get_source_line(frame->interp.pc);
      return;
    }
#endif
  // Use dladdr() to determine in which binary the address IP resides.
#if defined (HAVE_DLFCN_H) && defined (HAVE_DLADDR)
  Dl_info info;
  jstring binaryName = NULL;
  const char *argv0 = _Jv_GetSafeArg(0);

  void *ip = frame->ip;
  _Unwind_Ptr offset = 0;

  if (dladdr (ip, &info))
    {
      if (info.dli_fname)
	binaryName = JvNewStringUTF (info.dli_fname);
      else
        return;

      if (*methodName == NULL && info.dli_sname)
	*methodName = JvNewStringUTF (info.dli_sname);

      // addr2line expects relative addresses for shared libraries.
      if (strcmp (info.dli_fname, argv0) == 0)
        offset = (_Unwind_Ptr) ip;
      else
        offset = (_Unwind_Ptr) ip - (_Unwind_Ptr) info.dli_fbase;

      //printf ("linenum ip: %p\n", ip);
      //printf ("%s: 0x%x\n", info.dli_fname, offset);
      //offset -= sizeof(void *);
      
      // The unwinder gives us the return address. In order to get the right
      // line number for the stack trace, roll it back a little.
      offset -= 1;

      // printf ("%s: 0x%x\n", info.dli_fname, offset);
      
      finder->lookup (binaryName, (jlong) offset);
      *sourceFileName = finder->getSourceFile();
      *lineNum = finder->getLineNum();
    }
#endif
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

#ifdef SJLJ_EXCEPTIONS
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

#else /* SJLJ_EXCEPTIONS */

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
#endif /* SJLJ_EXCEPTIONS */

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
#ifndef SJLJ_EXCEPTIONS
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
#else
  return;
#endif
}

// Return a java array containing the Java classes on the stack above CHECKCLASS.
JArray<jclass> *
_Jv_StackTrace::GetClassContext (jclass checkClass)
{
  JArray<jclass> *result = NULL;

  int trace_size = 100;
  _Jv_StackFrame frames[trace_size];
  _Jv_UnwindState state (trace_size);
  state.frames = (_Jv_StackFrame *) &frames;

  //JvSynchronized (ncodeMap);
  UpdateNCodeMap ();

  _Unwind_Backtrace (UnwindTraceFn, &state);

  // Count the number of Java frames on the stack.
  int jframe_count = 0;
  bool seen_checkClass = false;
  int start_pos = -1;
  for (int i = 0; i < state.pos; i++)
    {
      _Jv_StackFrame *frame = &state.frames[i];
      FillInFrameInfo (frame);
      
      if (seen_checkClass)
	{
	  if (frame->klass)
	    {
	      jframe_count++;
	      if (start_pos == -1)
		start_pos = i;
	    }
	}
      else
	seen_checkClass = frame->klass == checkClass;
    }
  result = (JArray<jclass> *) _Jv_NewObjectArray (jframe_count, &Class::class$, NULL);
  int pos = 0;
  
  for (int i = start_pos; i < state.pos; i++)
    {
      _Jv_StackFrame *frame = &state.frames[i];
      if (frame->klass)
        elements(result)[pos++] = frame->klass;
    }
  return result;
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
