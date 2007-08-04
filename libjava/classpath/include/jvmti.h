/* jvmti.h - Java Virtual Machine Tool Interface
   Copyright (C) 2006  Free Software Foundation

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

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


/* Note: this file must be compilable by the C compiler (for now,
   assuming GNU C is ok).  This means you must never use `//'
   comments, and all C++-specific code must be conditional on
   __cplusplus.  */

#ifndef _CLASSPATH_JVMTI_H
#define _CLASSPATH_JVMTI_H
#include <jni.h>

#include "jvmti_md.h"

/* The VM might define JVMTI base types */
#ifndef _CLASSPATH_VM_JVMTI_TYPES_DEFINED

typedef jobject jthread;
typedef jobject jthreadGroup;
typedef jlong jlocation;
typedef struct _Jv_rawMonitorID *jrawMonitorID;

#endif /* !_CLASSPATH_VM_JVMTI_TYPES_DEFINED */

/* JVMTI Version */
#define JVMTI_VERSION_1_0 0x30010000
#define JVMTI_VERSION (JVMTI_VERSION_1_0 + 38)  /* Spec version is 1.0.38 */

#ifdef __cplusplus
extern "C"
{
#endif

/* These functions might be defined in libraries which we load; the
   JVMTI implementation calls them at the appropriate times.  */
extern JNIEXPORT jint JNICALL Agent_OnLoad (JavaVM *vm, char *options,
					    void *reserved);
extern JNIEXPORT void JNICALL Agent_OnUnload (JavaVM *vm);

#ifdef __cplusplus
}
#endif

/* Forward declarations */
typedef struct _jvmtiAddrLocationMap jvmtiAddrLocationMap;
#ifdef __cplusplus
typedef struct _Jv_JVMTIEnv jvmtiEnv;
#else
typedef const struct _Jv_jvmtiEnv *jvmtiEnv;
#endif

/*
 * Error constants
 */

typedef enum
{
  /* Universal Errors */
  JVMTI_ERROR_NONE = 0,
  JVMTI_ERROR_NULL_POINTER = 100,
  JVMTI_ERROR_OUT_OF_MEMORY = 110,
  JVMTI_ERROR_ACCESS_DENIED = 111,
  JVMTI_ERROR_WRONG_PHASE = 112,
  JVMTI_ERROR_INTERNAL = 113,
  JVMTI_ERROR_UNATTACHED_THREAD = 115,
  JVMTI_ERROR_INVALID_ENVIRONMENT = 116,

  /* Function-specific Required Errors */
  JVMTI_ERROR_INVALID_PRIORITY = 12,
  JVMTI_ERROR_THREAD_NOT_SUSPENDED = 13,
  JVMTI_ERROR_THREAD_SUSPENDED = 14,
  JVMTI_ERROR_THREAD_NOT_ALIVE = 15,
  JVMTI_ERROR_CLASS_NOT_PREPARED = 22,
  JVMTI_ERROR_NO_MORE_FRAMES = 31,
  JVMTI_ERROR_OPAQUE_FRAME = 32,
  JVMTI_ERROR_DUPLICATE = 40,
  JVMTI_ERROR_NOT_FOUND = 41,
  JVMTI_ERROR_NOT_MONITOR_OWNER = 51,
  JVMTI_ERROR_INTERRUPT = 52,
  JVMTI_ERROR_UNMODIFIABLE_CLASS = 79,
  JVMTI_ERROR_NOT_AVAILABLE = 98,
  JVMTI_ERROR_ABSENT_INFORMATION = 101,
  JVMTI_ERROR_INVALID_EVENT_TYPE = 102,
  JVMTI_ERROR_NATIVE_METHOD = 104,

  /* Function-specific Agent Errors */
  JVMTI_ERROR_INVALID_THREAD = 10,
  JVMTI_ERROR_INVALID_THREAD_GROUP = 11,
  JVMTI_ERROR_INVALID_OBJECT = 20,
  JVMTI_ERROR_INVALID_CLASS = 21,
  JVMTI_ERROR_INVALID_METHODID = 23,
  JVMTI_ERROR_INVALID_LOCATION = 24,
  JVMTI_ERROR_INVALID_FIELDID = 25,
  JVMTI_ERROR_TYPE_MISMATCH = 34,
  JVMTI_ERROR_INVALID_SLOT = 35,
  JVMTI_ERROR_INVALID_MONITOR = 50,
  JVMTI_ERROR_INVALID_CLASS_FORMAT = 60,
  JVMTI_ERROR_CIRCULAR_CLASS_DEFINITION = 61,
  JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_ADDED = 63,
  JVMTI_ERROR_UNSUPPORTED_REDEFINITION_SCHEMA_CHANGED = 64,
  JVMTI_ERROR_INVALID_TYPESTATE = 65,
  JVMTI_ERROR_FAILS_VERIFICATION = 62,
  JVMTI_ERROR_UNSUPPORTED_REDEFINITION_HIERARCHY_CHANGED = 66,
  JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_DELETED = 67,
  JVMTI_ERROR_UNSUPPORTED_VERSION = 68,
  JVMTI_ERROR_NAMES_DONT_MATCH = 69,
  JVMTI_ERROR_UNSUPPORTED_REDEFINITION_CLASS_MODIFIERS_CHANGED = 70,
  JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_MODIFIERS_CHANGED = 71,
  JVMTI_ERROR_MUST_POSSESS_CAPABILITY = 99,
  JVMTI_ERROR_ILLEGAL_ARGUMENT = 103
} jvmtiError;

/*
 * Enumeration Definitions
 */

typedef enum
{
  JVMTI_DISABLE = 0,
  JVMTI_ENABLE = 1
} jvmtiEventMode;

typedef enum
{
  JVMTI_HEAP_OBJECT_TAGGED = 1,
  JVMTI_HEAP_OBJECT_UNTAGGED = 2,
  JVMTI_HEAP_OBJECT_EITHER = 3
} jvmtiHeapObjectFilter;

typedef enum
{
  JVMTI_HEAP_ROOT_JNI_GLOBAL = 1,
  JVMTI_HEAP_ROOT_SYSTEM_CLASS = 2,
  JVMTI_HEAP_ROOT_MONITOR = 3,
  JVMTI_HEAP_ROOT_STACK_LOCAL = 4,
  JVMTI_HEAP_ROOT_JNI_LOCAL = 5,
  JVMTI_HEAP_ROOT_THREAD = 6,
  JVMTI_HEAP_ROOT_OTHER = 7
} jvmtiHeapRootKind;

typedef enum
{
  JVMTI_ITERATION_ABORT = 0,
  JVMTI_ITERATION_CONTINUE = 1,
  JVMTI_ITERATION_IGNORE = 2
} jvmtiIterationControl;

typedef enum
{
  JVMTI_JLOCATION_OTHER = 0,
  JVMTI_JLOCATION_JVMBCI = 1,
  JVMTI_JLOCATION_MACHINEPC = 2
} jvmtiJlocationFormat;

typedef enum
{
  JVMTI_REFERENCE_CLASS = 1,
  JVMTI_REFERENCE_FIELD = 2,
  JVMTI_REFERENCE_ARRAY_ELEMENT = 3,
  JVMTI_REFERENCE_CLASS_LOADER = 4,
  JVMTI_REFERENCE_SIGNERS = 5,
  JVMTI_REFERENCE_PROTECTION_DOMAIN = 6,
  JVMTI_REFERENCE_INTERFACE = 7,
  JVMTI_REFERENCE_STATIC_FIELD = 8,
  JVMTI_REFERENCE_CONSTANT_POOL = 9
} jvmtiObjectReferenceKind;

typedef enum
{
  JVMTI_KIND_IN = 91,
  JVMTI_KIND_IN_PTR = 92,
  JVMTI_KIND_IN_BUF = 93,
  JVMTI_KIND_ALLOC_BUF = 94,
  JVMTI_KIND_ALLOC_ALLOC_BUF = 95,
  JVMTI_KIND_OUT = 96,
  JVMTI_KIND_OUT_BUF = 97
} jvmtiParamKind;

typedef enum
{
  JVMTI_TYPE_JBYTE = 101,
  JVMTI_TYPE_JCHAR = 102,
  JVMTI_TYPE_JSHORT = 103,
  JVMTI_TYPE_JINT = 104,
  JVMTI_TYPE_JLONG = 105,
  JVMTI_TYPE_JFLOAT = 106,
  JVMTI_TYPE_JDOUBLE = 107,
  JVMTI_TYPE_JBOOLEAN = 108,
  JVMTI_TYPE_JOBJECT = 109,
  JVMTI_TYPE_JTHREAD = 110,
  JVMTI_TYPE_JCLASS = 111,
  JVMTI_TYPE_JVALUE = 112,
  JVMTI_TYPE_JFIELDID = 113,
  JVMTI_TYPE_JMETHODID = 114,
  JVMTI_TYPE_CCHAR = 115,
  JVMTI_TYPE_CVOID = 116,
  JVMTI_TYPE_JNIENV = 117
} jvmtiParamTypes;

typedef enum
{
  JVMTI_PHASE_ONLOAD = 1,
  JVMTI_PHASE_PRIMORDIAL = 2,
  JVMTI_PHASE_LIVE = 4,
  JVMTI_PHASE_START = 6,
  JVMTI_PHASE_DEAD = 8
} jvmtiPhase;

typedef enum
{
  JVMTI_TIMER_USER_CPU = 30,
  JVMTI_TIMER_TOTAL_CPU = 31,
  JVMTI_TIMER_ELAPSED = 32
} jvmtiTimerKind;

typedef enum
{
  JVMTI_VERBOSE_OTHER = 0,
  JVMTI_VERBOSE_GC = 1,
  JVMTI_VERBOSE_CLASS = 2,
  JVMTI_VERBOSE_JNI = 4
} jvmtiVerboseFlag;

/* Version information */
#define JVMTI_VERSION_INTERFACE_JNI 0x00000000
#define JVMTI_VERSION_INTERFACE_JVMTI 0x30000000
#define JVMTI_VERSION_MASK_INTERFACE_TYPE 0x70000000
#define JVMTI_VERSION_MASK_MAJOR 0x0FFF0000
#define JVMTI_VERSION_MASK_MINOR 0x0000FF00
#define JVMTI_VERSION_MASK_MICRO 0x000000FF
#define JVMTI_VERSION_SHIFT_MAJOR 16
#define JVMTI_VERSION_SHIFT_MINOR 8
#define JVMTI_VERSION_SHIFT_MICRO 0

/*
 * Events and event callbacks
 */

typedef enum
{
  JVMTI_EVENT_VM_INIT = 50,
  JVMTI_EVENT_VM_DEATH = 51,
  JVMTI_EVENT_THREAD_START = 52,
  JVMTI_EVENT_THREAD_END = 53,
  JVMTI_EVENT_CLASS_FILE_LOAD_HOOK = 54,
  JVMTI_EVENT_CLASS_LOAD = 55,
  JVMTI_EVENT_CLASS_PREPARE = 56,
  JVMTI_EVENT_VM_START = 57,
  JVMTI_EVENT_EXCEPTION = 58,
  JVMTI_EVENT_EXCEPTION_CATCH = 59,
  JVMTI_EVENT_SINGLE_STEP =  60,
  JVMTI_EVENT_FRAME_POP = 61,
  JVMTI_EVENT_BREAKPOINT = 62,
  JVMTI_EVENT_FIELD_ACCESS = 63,
  JVMTI_EVENT_FIELD_MODIFICATION = 64,
  JVMTI_EVENT_METHOD_ENTRY = 65,
  JVMTI_EVENT_METHOD_EXIT = 66,
  JVMTI_EVENT_NATIVE_METHOD_BIND = 67,
  JVMTI_EVENT_COMPILED_METHOD_LOAD = 68,
  JVMTI_EVENT_COMPILED_METHOD_UNLOAD = 69,
  JVMTI_EVENT_DYNAMIC_CODE_GENERATED = 70,
  JVMTI_EVENT_DATA_DUMP_REQUEST = 71,
  JVMTI_EVENT_MONITOR_WAIT = 73,
  JVMTI_EVENT_MONITOR_WAITED = 74,
  JVMTI_EVENT_MONITOR_CONTENDED_ENTER = 75,
  JVMTI_EVENT_MONITOR_CONTENDED_ENTERED = 76,
  JVMTI_EVENT_GARBAGE_COLLECTION_START = 81,
  JVMTI_EVENT_GARBAGE_COLLECTION_FINISH = 82,
  JVMTI_EVENT_OBJECT_FREE = 83,
  JVMTI_EVENT_VM_OBJECT_ALLOC = 84
} jvmtiEvent;

typedef void *jvmtiEventReserved;

typedef void (JNICALL *jvmtiEventSingleStep)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jlocation location);

typedef void (JNICALL *jvmtiEventBreakpoint)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jlocation location);

typedef void (JNICALL *jvmtiEventFieldAccess)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jlocation location, jclass field_klass, jobject object, jfieldID field);

typedef void (JNICALL *jvmtiEventFieldModification)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jlocation location, jclass field_klass, jobject object, jfieldID field,
   char signature_type, jvalue new_value);

typedef void (JNICALL *jvmtiEventFramePop)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jboolean was_popped_by_exception);

typedef void (JNICALL *jvmtiEventMethodEntry)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method);

typedef void (JNICALL *jvmtiEventMethodExit)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jboolean was_popped_by_exception, jvalue return_value);

typedef void (JNICALL *jvmtiEventNativeMethodBind)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   void *address, void **new_address_ptr);

typedef void (JNICALL *jvmtiEventException)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jlocation location, jobject exception, jmethodID catch_method,
   jlocation catch_location);

typedef void (JNICALL *jvmtiEventExceptionCatch)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jmethodID method,
   jlocation location, jobject exception);

typedef void (JNICALL *jvmtiEventThreadStart)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread);

typedef void (JNICALL *jvmtiEventThreadEnd)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread);

typedef void (JNICALL *jvmtiEventClassLoad)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jclass klass);

typedef void (JNICALL *jvmtiEventClassPrepare)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thraed, jclass klass);

typedef void (JNICALL *jvmtiEventClassFileLoadHook)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jclass class_being_redefined,
   jobject loader, const char *name, jobject protection_domain,
   jint class_data_len, const unsigned char *class_data,
   jint *new_class_data_len, unsigned char **new_class_data);

typedef void (JNICALL *jvmtiEventVMStart)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env);

typedef void (JNICALL *jvmtiEventVMInit)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread);

typedef void (JNICALL *jvmtiEventVMDeath)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env);

typedef void (JNICALL *jvmtiEventCompiledMethodLoad)
  (jvmtiEnv *jvmti_env, jmethodID method, jint code_size,
   const void *code_addr, jint map_length, const jvmtiAddrLocationMap *map,
   const void *compile_info);

typedef void (JNICALL *jvmtiEventCompiledMethodUnload)
  (jvmtiEnv *jvmti_env, jmethodID method, const void *code_addr);

typedef void (JNICALL *jvmtiEventDynamicCodeGenerated)
  (jvmtiEnv *jvmti_env, const char *name, const void *address, jint length);

typedef void (JNICALL *jvmtiEventDataDumpRequest)
  (jvmtiEnv *jvmti_env);

typedef void (JNICALL *jvmtiEventMonitorContendedEnter)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jobject object);

typedef void (JNICALL *jvmtiEventMonitorContendedEntered)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jobject object);

typedef void (JNICALL *jvmtiEventMonitorWait)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jobject object,
   jlong timeout);

typedef void (JNICALL *jvmtiEventMonitorWaited)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jobject object,
   jboolean timed_out);

typedef void (JNICALL *jvmtiEventVMObjectAlloc)
  (jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, jobject object,
   jclass object_klass, jlong size);

typedef void (JNICALL *jvmtiEventObjectFree)
  (jvmtiEnv *jvmti_env, jlong tag);

typedef void (JNICALL *jvmtiEventGarbageCollectionStart)
  (jvmtiEnv *jvmti_env);

typedef void (JNICALL *jvmtiEventGarbageCollectionFinish)
  (jvmtiEnv *jvmti_env);

typedef struct
{
  jvmtiEventVMInit VMInit;
  jvmtiEventVMDeath VMDeath;
  jvmtiEventThreadStart ThreadStart;
  jvmtiEventThreadEnd ThreadEnd;
  jvmtiEventClassFileLoadHook ClassFileLoadHook;
  jvmtiEventClassLoad ClassLoad;
  jvmtiEventClassPrepare ClassPrepare;
  jvmtiEventVMStart VMStart;
  jvmtiEventException Exception;
  jvmtiEventExceptionCatch ExceptionCatch;
  jvmtiEventSingleStep SingleStep;
  jvmtiEventFramePop FramePop;
  jvmtiEventBreakpoint Breakpoint;
  jvmtiEventFieldAccess FieldAccess;
  jvmtiEventFieldModification FieldModification;
  jvmtiEventMethodEntry MethodEntry;
  jvmtiEventMethodExit MethodExit;
  jvmtiEventNativeMethodBind NativeMethodBind;
  jvmtiEventCompiledMethodLoad CompiledMethodLoad;
  jvmtiEventCompiledMethodUnload CompiledMethodUnload;
  jvmtiEventDynamicCodeGenerated DynamicCodeGenerated;
  jvmtiEventDataDumpRequest DataDumpRequest;
  jvmtiEventReserved reserved72;
  jvmtiEventMonitorWait MonitorWait;
  jvmtiEventMonitorWaited MonitorWaited;
  jvmtiEventMonitorContendedEnter MonitorContendedEnter;
  jvmtiEventMonitorContendedEntered MonitorContendedEntered;
  jvmtiEventReserved reserved77;
  jvmtiEventReserved reserved78;
  jvmtiEventReserved reserved79;
  jvmtiEventReserved reserved80;
  jvmtiEventGarbageCollectionStart GarbageCollectionStart;
  jvmtiEventGarbageCollectionFinish GarbageCollectionFinish;
  jvmtiEventObjectFree ObjectFree;
  jvmtiEventVMObjectAlloc VMObjectAlloc;
} jvmtiEventCallbacks;

/*
 * Function and Structure Type Definitions
 */

struct _jvmtiAddrLocationMap
{
  const void *start_address;
  jlocation location;
};

typedef struct
{
  unsigned int can_tag_objects : 1;
  unsigned int can_generate_field_modification_events : 1;
  unsigned int can_generate_field_access_events : 1;
  unsigned int can_get_bytecodes : 1;
  unsigned int can_get_synthetic_attribute : 1;
  unsigned int can_get_owned_monitor_info : 1;
  unsigned int can_get_current_contended_monitor : 1;
  unsigned int can_get_monitor_info : 1;
  unsigned int can_pop_frame : 1;
  unsigned int can_redefine_classes : 1;
  unsigned int can_signal_thread : 1;
  unsigned int can_get_source_file_name : 1;
  unsigned int can_get_line_numbers : 1;
  unsigned int can_get_source_debug_extension : 1;
  unsigned int can_access_local_variables : 1;
  unsigned int can_maintain_original_method_order : 1;
  unsigned int can_generate_single_step_events : 1;
  unsigned int can_generate_exception_events : 1;
  unsigned int can_generate_frame_pop_events : 1;
  unsigned int can_generate_breakpoint_events : 1;
  unsigned int can_suspend : 1;
  unsigned int can_redefine_any_class : 1;
  unsigned int can_get_current_thread_cpu_time : 1;
  unsigned int can_get_thread_cpu_time : 1;
  unsigned int can_generate_method_entry_events : 1;
  unsigned int can_generate_method_exit_events : 1;
  unsigned int can_generate_all_class_hook_events : 1;
  unsigned int can_generate_compiled_method_load_events : 1;
  unsigned int can_generate_monitor_events : 1;
  unsigned int can_generate_vm_object_alloc_events : 1;
  unsigned int can_generate_native_method_bind_events : 1;
  unsigned int can_generate_garbage_collection_events : 1;
  unsigned int can_generate_object_free_events : 1;
  unsigned int : 15;
  unsigned int : 16;
  unsigned int : 16;
  unsigned int : 16;
  unsigned int : 16;
  unsigned int : 16;
} jvmtiCapabilities;

typedef struct
{
  jclass klass;
  jint class_byte_count;
  const unsigned char *class_bytes;
} jvmtiClassDefinition;

typedef struct
{
  char *name;
  jvmtiParamKind kind;
  jvmtiParamTypes base_type;
  jboolean null_ok;
} jvmtiParamInfo;

typedef struct
{
  jint extension_event_index;
  char *id;
  char *short_description;
  jint param_count;
  jvmtiParamInfo* params;
} jvmtiExtensionEventInfo;

typedef jvmtiError (JNICALL *jvmtiExtensionFunction)
  (jvmtiEnv *jvmti_enf, ...);

typedef struct
{
  jvmtiExtensionFunction func;
  char *id;
  char *short_description;
  jint param_count;
  jvmtiParamInfo *params;
  jint error_count;
  jvmtiError *errors;
} jvmtiExtensionFunctionInfo;

typedef struct
{
  jmethodID method;
  jlocation location;
} jvmtiFrameInfo;

typedef struct
{
  jlocation start_location;
  jint line_number;
} jvmtiLineNumberEntry;

typedef struct
{
  jlocation start_location;
  jint length;
  char *name;
  char *signature;
  char *generic_signature;
  jint slot;
} jvmtiLocalVariableEntry;

typedef struct
{
  jthread owner;
  jint entry_count;
  jint waiter_count;
  jthread *waiters;
  jint notify_waiter_count;
  jthread *notify_waiters;
} jvmtiMonitorUsage;

typedef struct
{
  jthread thread;
  jint state;
  jvmtiFrameInfo *frame_buffer;
  jint frame_count;
} jvmtiStackInfo;

typedef struct
{
  jthreadGroup parent;
  char *name;
  jint max_priority;
  jboolean is_daemon;
} jvmtiThreadGroupInfo;

typedef struct
{
  char *name;
  jint priority;
  jboolean is_daemon;
  jthreadGroup thread_group;
  jobject context_class_loader;
} jvmtiThreadInfo;

typedef struct
{
  jlong max_value;
  jboolean may_skip_forward;
  jboolean may_skip_backward;
  jvmtiTimerKind kind;
  jlong reserved1;
  jlong reserved2;
} jvmtiTimerInfo;

typedef void (JNICALL *jvmtiExtensionEvent)
  (jvmtiEnv *jvmti_env, ...);

typedef jvmtiIterationControl (JNICALL *jvmtiHeapObjectCallback)
  (jlong class_tag, jlong size, jlong *tag_ptr, void *user_data);

typedef jvmtiIterationControl (JNICALL *jvmtiHeapRootCallback)
  (jvmtiHeapRootKind root_kind, jlong class_tag, jlong size, jlong *tag_ptr,
   void *user_data);

typedef jvmtiIterationControl (JNICALL *jvmtiObjectReferenceCallback)
  (jvmtiObjectReferenceKind reference_kind, jlong class_tag, jlong size,
   jlong *tag_ptr, jlong referrer_tag, jint referrer_index, void *user_data);

typedef jvmtiIterationControl (JNICALL *jvmtiStackReferenceCallback)
  (jvmtiHeapRootKind root_kind, jlong class_tag, jlong size, jlong *tag_ptr,
   jlong thread_tag, jint depth, jmethodID method, jint slot, void *user_data);

typedef void (JNICALL *jvmtiStartFunction)
  (jvmtiEnv *env, JNIEnv *jni_env, void *arg);

/*
 * JVM Tool Interface Base Types
 */
typedef struct JNINativeInterface_ jniNativeInterface;

struct _Jv_jvmtiEnv
{
  void *reserved1;

  jvmtiError (JNICALL *SetEventNotificationMode) (jvmtiEnv *env,
						  jvmtiEventMode mode,
						  jvmtiEvent event_type,
						  jthread event_thread, ...);
  void *reserved3;

  jvmtiError (JNICALL *GetAllThreads) (jvmtiEnv *env,
				       jint *threads_count_ptr,
				       jthread **threads_ptr);

  jvmtiError (JNICALL *SuspendThread) (jvmtiEnv *env,
				       jthread thread);

  jvmtiError (JNICALL *ResumeThread) (jvmtiEnv *env,
				      jthread thread);

  jvmtiError (JNICALL *StopThread) (jvmtiEnv *env,
				    jthread thread,
                                    jobject exception);

  jvmtiError (JNICALL *InterruptThread) (jvmtiEnv *env,
					 jthread thread);

  jvmtiError (JNICALL *GetThreadInfo) (jvmtiEnv *env,
				       jthread thread,
				       jvmtiThreadInfo *info_ptr);

  jvmtiError (JNICALL *GetOwnedMonitorInfo) (jvmtiEnv *env,
					     jthread thread,
					     jint *owned_monitor_count_ptr,
					     jobject **owned_monitors_ptr);

  jvmtiError (JNICALL *GetCurrentContendedMonitor) (jvmtiEnv *env,
						    jthread thread,
						    jobject *monitor_ptr);

  jvmtiError (JNICALL *RunAgentThread) (jvmtiEnv *env,
					jthread thread,
					jvmtiStartFunction proc,
					const void *arg,
					jint priority);

  jvmtiError (JNICALL *GetTopThreadGroups) (jvmtiEnv *env,
					    jint *group_count_ptr,
					    jthreadGroup **groups_ptr);

  jvmtiError (JNICALL *GetThreadGroupInfo) (jvmtiEnv *env,
					    jthreadGroup group,
					    jvmtiThreadGroupInfo *info_ptr);

  jvmtiError (JNICALL *GetThreadGroupChildren) (jvmtiEnv *env,
						jthreadGroup group,
						jint *thread_count_ptr,
						jthread **threads_ptr,
						jint *group_count_ptr,
						jthreadGroup **groups_ptr);
  jvmtiError (JNICALL *GetFrameCount) (jvmtiEnv *env,
				       jthread thread,
				       jint *count_ptr);

  jvmtiError (JNICALL *GetThreadState) (jvmtiEnv *env,
					jthread thread,
					jint *thread_state_ptr);

  void *reserved18;

  jvmtiError (JNICALL *GetFrameLocation) (jvmtiEnv *env,
					  jthread thread,
					  jint depth,
					  jmethodID *method_ptr,
					  jlocation *location_ptr);

  jvmtiError (JNICALL *NotifyPopFrame) (jvmtiEnv *env,
					jthread thread,
					jint depth);

  jvmtiError (JNICALL *GetLocalObject) (jvmtiEnv *env,
					jthread thread,
					jint depth,
					jint slot,
					jobject *value_ptr);

  jvmtiError (JNICALL *GetLocalInt) (jvmtiEnv *env,
				     jthread thread,
				     jint depth,
				     jint slot,
				     jint *value_ptr);

  jvmtiError (JNICALL *GetLocalLong) (jvmtiEnv *env,
				      jthread thread,
				      jint depth,
				      jint slot,
				      jlong *value_ptr);

  jvmtiError (JNICALL *GetLocalFloat) (jvmtiEnv *env,
				       jthread thread,
				       jint depth,
				       jint slot,
				       jfloat *value_ptr);

  jvmtiError (JNICALL *GetLocalDouble) (jvmtiEnv *env,
					jthread thread,
					jint depth,
					jint slot,
					jdouble *value_ptr);

  jvmtiError (JNICALL *SetLocalObject) (jvmtiEnv *env,
					jthread thread,
					jint depth,
					jint slot,
					jobject value);

  jvmtiError (JNICALL *SetLocalInt) (jvmtiEnv *env,
				     jthread thread,
				     jint depth,
				     jint slot,
				     jint value);

  jvmtiError (JNICALL *SetLocalLong) (jvmtiEnv *env,
				      jthread thread,
				      jint depth,
				      jint slot,
				      jlong value);

  jvmtiError (JNICALL *SetLocalFloat) (jvmtiEnv *env,
				       jthread thread,
				       jint depth,
				       jint slot,
				       jfloat value);

  jvmtiError (JNICALL *SetLocalDouble) (jvmtiEnv *env,
					jthread thread,
					jint depth,
					jint slot,
					jdouble value);

  jvmtiError (JNICALL *CreateRawMonitor) (jvmtiEnv *env,
					  const char *name,
					  jrawMonitorID *monitor_ptr);

  jvmtiError (JNICALL *DestroyRawMonitor) (jvmtiEnv *env,
					   jrawMonitorID monitor);

  jvmtiError (JNICALL *RawMonitorEnter) (jvmtiEnv *env,
					 jrawMonitorID monitor);

  jvmtiError (JNICALL *RawMonitorExit) (jvmtiEnv *env,
					jrawMonitorID monitor);

  jvmtiError (JNICALL *RawMonitorWait) (jvmtiEnv *env,
					jrawMonitorID monitor,
                                        jlong millis);

  jvmtiError (JNICALL *RawMonitorNotify) (jvmtiEnv *env,
					  jrawMonitorID monitor);

  jvmtiError (JNICALL *RawMonitorNotifyAll) (jvmtiEnv *env,
					     jrawMonitorID monitor);

  jvmtiError (JNICALL *SetBreakpoint) (jvmtiEnv *env,
				       jmethodID method,
				       jlocation location);

  jvmtiError (JNICALL *ClearBreakpoint) (jvmtiEnv *env,
					 jmethodID method,
					 jlocation location);

  void *reserved40;

  jvmtiError (JNICALL *SetFieldAccessWatch) (jvmtiEnv *env,
					     jclass klass,
					     jfieldID field);

  jvmtiError (JNICALL *ClearFieldAccessWatch) (jvmtiEnv *env,
					       jclass klass,
					       jfieldID field);

  jvmtiError (JNICALL *SetFieldModificationWatch) (jvmtiEnv *env,
						   jclass klass,
						   jfieldID field);

  jvmtiError (JNICALL *ClearFieldModificationWatch) (jvmtiEnv *env,
						     jclass klass,
						     jfieldID field);

  void *reserved45;

  jvmtiError (JNICALL *Allocate) (jvmtiEnv *env,
				  jlong size,
				  unsigned char **mem_ptr);

  jvmtiError (JNICALL *Deallocate) (jvmtiEnv *env,
				    unsigned char *mem);

  jvmtiError (JNICALL *GetClassSignature) (jvmtiEnv *env,
					   jclass klass,
					   char **signature_ptr,
					   char **generic_ptr);

  jvmtiError (JNICALL *GetClassStatus) (jvmtiEnv *env,
					jclass klass,
					jint *status_ptr);

  jvmtiError (JNICALL *GetSourceFileName) (jvmtiEnv *env,
					   jclass klass,
					   char **source_name_ptr);

  jvmtiError (JNICALL *GetClassModifiers) (jvmtiEnv *env,
					   jclass klass,
					   jint *modifiers_ptr);

  jvmtiError (JNICALL *GetClassMethods) (jvmtiEnv *env,
					 jclass klass,
					 jint *method_count_ptr,
					 jmethodID **methods_ptr);

  jvmtiError (JNICALL *GetClassFields) (jvmtiEnv *env,
					jclass klass,
					jint *field_count_ptr,
					jfieldID **fields_ptr);

  jvmtiError (JNICALL *GetImplementedInterfaces) (jvmtiEnv *env,
						  jclass klass,
						  jint *interface_count_ptr,
						  jclass **interfaces_ptr);

  jvmtiError (JNICALL *IsInterface) (jvmtiEnv *env,
				     jclass klass,
				     jboolean *is_interface_ptr);

  jvmtiError (JNICALL *IsArrayClass) (jvmtiEnv *env,
				      jclass klass,
				      jboolean *is_array_class_ptr);

  jvmtiError (JNICALL *GetClassLoader) (jvmtiEnv *env,
					jclass klass,
					jobject *classloader_ptr);

  jvmtiError (JNICALL *GetObjectHashCode) (jvmtiEnv *env,
					   jobject object,
					   jint *hash_code_ptr);

  jvmtiError (JNICALL *GetObjectMonitorUsage) (jvmtiEnv *env,
					       jobject object,
					       jvmtiMonitorUsage *info_ptr);

  jvmtiError (JNICALL *GetFieldName) (jvmtiEnv *env,
				      jclass klass,
				      jfieldID field,
				      char **name_ptr,
				      char **signature_ptr,
				      char **generic_ptr);

  jvmtiError (JNICALL *GetFieldDeclaringClass) (jvmtiEnv *env,
						jclass klass,
						jfieldID field,
						jclass *declaring_class_ptr);

  jvmtiError (JNICALL *GetFieldModifiers) (jvmtiEnv *env,
					   jclass klass,
					   jfieldID field,
					   jint *modifiers_ptr);

  jvmtiError (JNICALL *IsFieldSynthetic) (jvmtiEnv *env,
					  jclass klass,
					  jfieldID field,
					  jboolean *is_synthetic_ptr);

  jvmtiError (JNICALL *GetMethodName) (jvmtiEnv *env,
				       jmethodID method,
				       char **name_ptr,
				       char **signature_ptr,
				       char **generic_ptr);

  jvmtiError (JNICALL *GetMethodDeclaringClass) (jvmtiEnv *env,
						 jmethodID method,
						 jclass *declaring_class_ptr);


  jvmtiError (JNICALL *GetMethodModifiers) (jvmtiEnv *env,
					    jmethodID method,
					    jint *modifiers_ptr);

  void *reserved67;

  jvmtiError (JNICALL *GetMaxLocals) (jvmtiEnv *env,
				      jmethodID method,
				      jint *max_ptr);

  jvmtiError (JNICALL *GetArgumentsSize) (jvmtiEnv *env,
					  jmethodID method,
					  jint *size_ptr);

  jvmtiError (JNICALL *GetLineNumberTable) (jvmtiEnv *env,
					    jmethodID method,
					    jint *entry_count_ptr,
					    jvmtiLineNumberEntry **table_ptr);

  jvmtiError (JNICALL *GetMethodLocation) (jvmtiEnv *env,
					   jmethodID method,
					   jlocation *start_location_ptr,
					   jlocation *end_location_ptr);

  jvmtiError (JNICALL *GetLocalVariableTable) (jvmtiEnv *env,
					       jmethodID method,
					       jint *entry_count_ptr,
					       jvmtiLocalVariableEntry **table_ptr);

  void *reserved73;

  void *reserved74;

  jvmtiError (JNICALL *GetBytecodes) (jvmtiEnv *env,
				      jmethodID method,
				      jint *bytecode_count_ptr,
				      unsigned char **bytecodes_ptr);

  jvmtiError (JNICALL *IsMethodNative) (jvmtiEnv *env,
					jmethodID method,
					jboolean *is_native_ptr);

  jvmtiError (JNICALL *IsMethodSynthetic) (jvmtiEnv *env,
					   jmethodID method,
					   jboolean *is_synthetic_ptr);

  jvmtiError (JNICALL *GetLoadedClasses) (jvmtiEnv *env,
					  jint *class_count_ptr,
					  jclass **classes_ptr);

  jvmtiError (JNICALL *GetClassLoaderClasses) (jvmtiEnv *env,
					       jobject initiating_loader,
					       jint *class_count_ptr,
					       jclass **classes_ptr);

  jvmtiError (JNICALL *PopFrame) (jvmtiEnv *env,
				  jthread thread);

  void *reserved81;

  void *reserved82;

  void *reserved83;

  void *reserved84;

  void *reserved85;

  void *reserved86;

  jvmtiError (JNICALL *RedefineClasses) (jvmtiEnv *env,
					 jint class_count,
					 const jvmtiClassDefinition* class_definitions);

  jvmtiError (JNICALL *GetVersionNumber) (jvmtiEnv *env,
					  jint *version_ptr);

  jvmtiError (JNICALL *GetCapabilities) (jvmtiEnv *env,
					 jvmtiCapabilities *capabilities_ptr);

  jvmtiError (JNICALL *GetSourceDebugExtension) (jvmtiEnv *env,
						 jclass klass,
						 char **source_debug_extension_ptr);

  jvmtiError (JNICALL *IsMethodObsolete) (jvmtiEnv *env,
					  jmethodID method,
					  jboolean *is_obsolete_ptr);

  jvmtiError (JNICALL *SuspendThreadList) (jvmtiEnv *env,
					   jint request_count,
					   const jthread *request_list,
					   jvmtiError *results);

  jvmtiError (JNICALL *ResumeThreadList) (jvmtiEnv *env,
					  jint request_count,
					  const jthread *request_list,
					  jvmtiError *results);

  void *reserved94;

  void *reserved95;

  void *reserved96;

  void *reserved97;

  void *reserved98;

  void *reserved99;

  jvmtiError (JNICALL *GetAllStackTraces) (jvmtiEnv *env,
					   jint max_frame_count,
					   jvmtiStackInfo **stack_info_ptr,
					   jint *thread_count_ptr);

  jvmtiError (JNICALL *GetThreadListStackTraces) (jvmtiEnv *env,
						  jint thread_count,
						  const jthread *thread_list,
						  jint max_frame_count,
						  jvmtiStackInfo **stack_info_ptr);

  jvmtiError (JNICALL *GetThreadLocalStorage) (jvmtiEnv *env,
					       jthread thread,
					       void **data_ptr);

  jvmtiError (JNICALL *SetThreadLocalStorage) (jvmtiEnv *env,
					       jthread thread,
					       const void *data);

  jvmtiError (JNICALL *GetStackTrace) (jvmtiEnv *env,
				       jthread thread,
				       jint start_depth,
				       jint max_frame_count,
				       jvmtiFrameInfo *frame_buffer,
				       jint *count_ptr);

  void *reserved105;

  jvmtiError (JNICALL *GetTag) (jvmtiEnv *env,
				jobject object,
				jlong *tag_ptr);

  jvmtiError (JNICALL *SetTag) (jvmtiEnv *env,
				jobject object,
				jlong tag);

  jvmtiError (JNICALL *ForceGarbageCollection) (jvmtiEnv *env);

  jvmtiError (JNICALL *IterateOverObjectsReachableFromObject) (jvmtiEnv *env,
							       jobject object,
							       jvmtiObjectReferenceCallback object_reference_callback,
							       void *user_data);

  jvmtiError (JNICALL *IterateOverReachableObjects) (jvmtiEnv *env,
						     jvmtiHeapRootCallback heap_root_callback,
						     jvmtiStackReferenceCallback stack_ref_callback,
						     jvmtiObjectReferenceCallback object_ref_callback,
						     void *user_data);

  jvmtiError (JNICALL *IterateOverHeap) (jvmtiEnv *env,
					 jvmtiHeapObjectFilter object_filter,
					 jvmtiHeapObjectCallback heap_object_callback,
					 void *user_data);

  jvmtiError (JNICALL *IterateOverInstanceOfClass) (jvmtiEnv *env,
						    jclass klass,
						    jvmtiHeapObjectFilter object_filter,
						    jvmtiHeapObjectCallback heap_object_callback,
						    void *user_data);

  void *reserved113;

  jvmtiError (JNICALL *GetObjectsWithTags) (jvmtiEnv *env,
					    jint tag_count,
					    const jlong *tags,
					    jint *count_ptr,
					    jobject **object_result_ptr,
					    jlong **tag_result_ptr);

  void *reserved115;

  void *reserved116;

  void *reserved117;

  void *reserved118;

  void *reserved119;

  jvmtiError (JNICALL *SetJNIFunctionTable) (jvmtiEnv *env,
					     const jniNativeInterface *function_table);

  jvmtiError (JNICALL *GetJNIFunctionTable) (jvmtiEnv *env,
					     jniNativeInterface **function_table_ptr);

  jvmtiError (JNICALL *SetEventCallbacks) (jvmtiEnv *env,
					   const jvmtiEventCallbacks *callbacks,
					   jint size_of_callbacks);

  jvmtiError (JNICALL *GenerateEvents) (jvmtiEnv *env,
					jvmtiEvent event_type);

  jvmtiError (JNICALL *GetExtensionFunctions) (jvmtiEnv *env,
					       jint *extension_count_ptr,
					       jvmtiExtensionFunctionInfo **extensions);

  jvmtiError (JNICALL *GetExtensionEvents) (jvmtiEnv *env,
					    jint *extension_count_ptr,
					    jvmtiExtensionEventInfo **extensions);

  jvmtiError (JNICALL *SetExtensionEventCallback) (jvmtiEnv *env,
						   jint extension_event_index,
						   jvmtiExtensionEvent callback);

  jvmtiError (JNICALL *DisposeEnvironment) (jvmtiEnv *env);

  jvmtiError (JNICALL *GetErrorName) (jvmtiEnv *env,
				      jvmtiError error,
				      char **name_ptr);

  jvmtiError (JNICALL *GetJLocationFormat) (jvmtiEnv *env,
					    jvmtiJlocationFormat *format_ptr);

  jvmtiError (JNICALL *GetSystemProperties) (jvmtiEnv *env,
					     jint *count_ptr,
					     char ***property_ptr);

  jvmtiError (JNICALL *GetSystemProperty) (jvmtiEnv *env,
					   const char *property,
					   char **value_ptr);

  jvmtiError (JNICALL *SetSystemProperty) (jvmtiEnv *env,
					   const char *property,
					   const char *value);

  jvmtiError (JNICALL *GetPhase) (jvmtiEnv *env,
				  jvmtiPhase *phase_ptr);

  jvmtiError (JNICALL *GetCurrentThreadCpuTimerInfo) (jvmtiEnv *env,
						      jvmtiTimerInfo *info_ptr);

  jvmtiError (JNICALL *GetCurrentThreadCpuTime) (jvmtiEnv *env,
						 jlong *nanos_ptr);

  jvmtiError (JNICALL *GetThreadCpuTimerInfo) (jvmtiEnv *env,
					       jvmtiTimerInfo *info_ptr);

  jvmtiError (JNICALL *GetThreadCpuTime) (jvmtiEnv *env,
					  jthread thread,
					  jlong *nanos_ptr);

  jvmtiError (JNICALL *GetTimerInfo) (jvmtiEnv *env,
				      jvmtiTimerInfo *info_ptr);

  jvmtiError (JNICALL *GetTime) (jvmtiEnv *env,
				 jlong *nanos_ptr);

  jvmtiError (JNICALL *GetPotentialCapabilities) (jvmtiEnv *env,
						  jvmtiCapabilities *capabilities_ptr);

  void *reserved141;

  jvmtiError (JNICALL *AddCapabilities) (jvmtiEnv *env,
					 const jvmtiCapabilities *capabilities_ptr);

  jvmtiError (JNICALL *RelinquishCapabilities) (jvmtiEnv *env,
						const jvmtiCapabilities *capabilities_ptr);

  jvmtiError (JNICALL *GetAvailableProcessors) (jvmtiEnv *env,
						jint *processor_count_ptr);

  void *reserved145;

  void *reserved146;

  jvmtiError (JNICALL *GetEnvironmentLocalStorage) (jvmtiEnv *env,
						    void **data_ptr);

  jvmtiError (JNICALL *SetEnvironmentLocalStorage) (jvmtiEnv *env,
						    const void *data);

  jvmtiError (JNICALL *AddToBootstrapClassLoaderSearch) (jvmtiEnv *env,
							 const char *segment);

  jvmtiError (JNICALL *SetVerboseFlag) (jvmtiEnv *env,
					jvmtiVerboseFlag flag,
					jboolean value);

  void *reserved151;

  void *reserved152;

  void *reserved153;

  jvmtiError (JNICALL *GetObjectSize) (jvmtiEnv *env,
				       jobject object,
				       jlong *size_ptr);
};

#ifdef __cplusplus
class _Jv_JVMTIEnv
{
 public:
  /* Method table */
  struct _Jv_jvmtiEnv *p;

#ifdef _CLASSPATH_JVMTIENV_CONTENTS
  _CLASSPATH_JVMTIENV_CONTENTS
#endif

  jvmtiError SetEventNotificationMode (jvmtiEventMode mode,
				       jvmtiEvent event_type,
				       jthread event_thread, ...)
  {
    va_list args;
    va_start (args, event_thread);
    jvmtiError result = p->SetEventNotificationMode (this, mode, event_type,
						     event_thread, args);
    va_end (args);
    return result;
  }

  jvmtiError GetAllThreads (jint *threads_count_ptr, jthread **threads_ptr)
  { return p->GetAllThreads (this, threads_count_ptr, threads_ptr); }

  jvmtiError SuspendThread (jthread thread)
  { return p->SuspendThread (this, thread); }

  jvmtiError ResumeThread (jthread thread)
  { return p->ResumeThread (this, thread); }

  jvmtiError StopThread (jthread thread, jobject exception)
  { return p->StopThread (this, thread, exception); }

  jvmtiError InterruptThread (jthread thread)
  { return p->InterruptThread (this, thread); }

  jvmtiError GetThreadInfo (jthread thread, jvmtiThreadInfo *info_ptr)
  { return p->GetThreadInfo (this, thread, info_ptr); }

  jvmtiError GetOwnedMonitorInfo (jthread thread,
				  jint *owned_monitor_count_ptr,
				  jobject **owned_monitors_ptr)
  { 
    return p->GetOwnedMonitorInfo (this, thread, owned_monitor_count_ptr,
				   owned_monitors_ptr);
  }

  jvmtiError GetCurrentContendedMonitor (jthread thread, jobject *monitor_ptr)
  { return p->GetCurrentContendedMonitor (this, thread, monitor_ptr); }

  jvmtiError RunAgentThread (jthread thread, jvmtiStartFunction proc,
			     const void *arg, jint priority)
  { return p->RunAgentThread (this, thread, proc, arg, priority); }

  jvmtiError GetTopThreadGroups (jint *group_count_ptr, 
				 jthreadGroup **groups_ptr)
  { return p->GetTopThreadGroups (this, group_count_ptr, groups_ptr); }

  jvmtiError GetThreadGroupInfo (jthreadGroup group, 
				 jvmtiThreadGroupInfo *info_ptr)
  { return p->GetThreadGroupInfo (this, group, info_ptr); }

  jvmtiError GetThreadGroupChildren (jthreadGroup group,
				     jint *thread_count_ptr,
				     jthread **threads_ptr,
				     jint *group_count_ptr,
				     jthreadGroup **groups_ptr)
  {
    return p->GetThreadGroupChildren (this, group, thread_count_ptr,
				      threads_ptr, group_count_ptr,
				      groups_ptr);
  }

  jvmtiError GetFrameCount (jthread thread, jint *count_ptr)
  { return p->GetFrameCount (this, thread, count_ptr); }

  jvmtiError GetThreadState (jthread thread, jint *thread_state_ptr)
  { return p->GetThreadState (this, thread, thread_state_ptr); }

  jvmtiError GetFrameLocation (jthread thread, jint depth,
			       jmethodID *method_ptr, jlocation *location_ptr)
  {
    return p->GetFrameLocation (this, thread, depth, method_ptr,
				location_ptr);
  }

  jvmtiError NotifyPopFrame (jthread thread, jint depth)
  { return p->NotifyPopFrame (this, thread, depth); }

  jvmtiError GetLocalObject (jthread thread, jint depth, jint slot,
			     jobject *value_ptr)
  { return p->GetLocalObject (this, thread, depth, slot, value_ptr); }

  jvmtiError GetLocalInt (jthread thread, jint depth, jint slot,
			  jint *value_ptr)
  { return p->GetLocalInt (this, thread, depth, slot, value_ptr); }

  jvmtiError GetLocalLong (jthread thread, jint depth, jint slot,
			   jlong *value_ptr)
  { return p->GetLocalLong (this, thread, depth, slot, value_ptr); }

  jvmtiError GetLocalFloat (jthread thread, jint depth, jint slot,
			    jfloat *value_ptr)
  { return p->GetLocalFloat (this, thread, depth, slot, value_ptr); }

  jvmtiError GetLocalDouble (jthread thread, jint depth, jint slot,
			     jdouble *value_ptr)
  { return p->GetLocalDouble (this, thread, depth, slot, value_ptr); }

  jvmtiError SetLocalObject (jthread thread, jint depth, jint slot,
			     jobject value)
  { return p->SetLocalObject (this, thread, depth, slot, value); }

  jvmtiError SetLocalInt (jthread thread, jint depth, jint slot,
			  jint value)
  { return p->SetLocalInt (this, thread, depth, slot, value); }

  jvmtiError SetLocalLong (jthread thread, jint depth, jint slot, 
			   jlong value)
  { return p->SetLocalLong (this, thread, depth, slot, value); }

  jvmtiError SetLocalFloat (jthread thread, jint depth, jint slot,
			    jfloat value)
  { return p->SetLocalFloat (this, thread, depth, slot, value); }

  jvmtiError SetLocalDouble (jthread thread, jint depth, jint slot,
			     jdouble value)
  { return p->SetLocalDouble (this, thread, depth, slot, value); }

  jvmtiError CreateRawMonitor (const char *name, jrawMonitorID *monitor_ptr)
  { return p->CreateRawMonitor (this, name, monitor_ptr); }

  jvmtiError DestroyRawMonitor (jrawMonitorID monitor)
  { return p->DestroyRawMonitor (this, monitor); }

  jvmtiError RawMonitorEnter (jrawMonitorID monitor)
  { return p->RawMonitorEnter (this, monitor); }

  jvmtiError RawMonitorExit (jrawMonitorID monitor)
  { return p->RawMonitorExit (this, monitor); }

  jvmtiError RawMonitorWait (jrawMonitorID monitor, jlong millis)
  { return p->RawMonitorWait (this, monitor, millis); }

  jvmtiError RawMonitorNotify (jrawMonitorID monitor)
  { return p->RawMonitorNotify (this, monitor); }

  jvmtiError RawMonitorNotifyAll (jrawMonitorID monitor)
  { return p->RawMonitorNotifyAll (this, monitor); }

  jvmtiError SetBreakpoint (jmethodID method, jlocation location)
  { return p->SetBreakpoint (this, method, location); }

  jvmtiError ClearBreakpoint (jmethodID method, jlocation location)
  { return p->ClearBreakpoint (this, method, location); }

  jvmtiError SetFieldAccessWatch (jclass klass, jfieldID field)
  { return p->SetFieldAccessWatch (this, klass, field); }

  jvmtiError ClearFieldAccessWatch (jclass klass, jfieldID field)
  { return p->ClearFieldAccessWatch (this, klass, field); }

  jvmtiError SetFieldModificationWatch (jclass klass, jfieldID field)
  { return p->SetFieldModificationWatch (this, klass, field); }

  jvmtiError ClearFieldModificationWatch (jclass klass, jfieldID field)
  { return p->ClearFieldModificationWatch (this, klass, field); }

  jvmtiError Allocate (jlong size, unsigned char **mem_ptr)
  { return p->Allocate (this, size, mem_ptr); }

  jvmtiError Deallocate (unsigned char *mem)
  { return p->Deallocate (this, mem); }

  jvmtiError GetClassSignature (jclass klass, char **signature_ptr,
				char **generic_ptr)
  { return p->GetClassSignature (this, klass, signature_ptr, generic_ptr); }

  jvmtiError GetClassStatus (jclass klass, jint *status_ptr)
  { return p->GetClassStatus (this, klass, status_ptr); }

  jvmtiError GetSourceFileName (jclass klass, char **source_name_ptr)
  { return p->GetSourceFileName (this, klass, source_name_ptr); }

  jvmtiError GetClassModifiers (jclass klass, jint *modifiers_ptr)
  { return p->GetClassModifiers (this, klass, modifiers_ptr); }

  jvmtiError GetClassMethods (jclass klass, jint *method_count_ptr,
			      jmethodID **methods_ptr)
  { return p->GetClassMethods (this, klass, method_count_ptr, methods_ptr); }

  jvmtiError GetClassFields (jclass klass, jint *field_count_ptr,
			     jfieldID **fields_ptr)
  { return p->GetClassFields (this, klass, field_count_ptr, fields_ptr); }

  jvmtiError GetImplementedInterfaces (jclass klass,
				       jint *interface_count_ptr,
				       jclass **interfaces_ptr)
  {
    return p->GetImplementedInterfaces (this, klass, interface_count_ptr,
					interfaces_ptr);
  }
 
  jvmtiError IsInterface (jclass klass, jboolean *is_interface_ptr)
  { return p->IsInterface (this, klass, is_interface_ptr); }

  jvmtiError IsArrayClass (jclass klass, jboolean *is_array_class_ptr)
  { return p->IsArrayClass (this, klass, is_array_class_ptr); }

  jvmtiError GetClassLoader (jclass klass, jobject *classloader_ptr)
  { return p->GetClassLoader (this, klass, classloader_ptr); }

  jvmtiError GetObjectHashCode (jobject object, jint *hash_code_ptr)
  { return p->GetObjectHashCode (this, object, hash_code_ptr); }

  jvmtiError GetObjectMonitorUsage (jobject object,
				    jvmtiMonitorUsage *info_ptr)
  { return p->GetObjectMonitorUsage (this, object, info_ptr); }

  jvmtiError GetFieldName (jclass klass, jfieldID field, char **name_ptr,
			   char **signature_ptr, char **generic_ptr)
  {
    return p->GetFieldName (this, klass, field, name_ptr,
			    signature_ptr, generic_ptr);
  }

  jvmtiError GetFieldDeclaringClass (jclass klass, jfieldID field,
				     jclass *declaring_class_ptr)
  {
    return p->GetFieldDeclaringClass (this, klass, field,
				      declaring_class_ptr);
  }

  jvmtiError GetFieldModifiers (jclass klass, jfieldID field,
				jint *modifiers_ptr)
  { return p->GetFieldModifiers (this, klass, field, modifiers_ptr); }

  jvmtiError IsFieldSynthetic (jclass klass, jfieldID field,
			       jboolean *is_synthetic_ptr)
  { return p->IsFieldSynthetic (this, klass, field, is_synthetic_ptr); }

  jvmtiError GetMethodName (jmethodID method, char **name_ptr,
			    char **signature_ptr, char **generic_ptr)
  {
    return p->GetMethodName (this, method, name_ptr, signature_ptr,
			     generic_ptr);
  }

  jvmtiError GetMethodDeclaringClass (jmethodID method,
				      jclass *declaring_class_ptr)
  { return p->GetMethodDeclaringClass (this, method, declaring_class_ptr); }


  jvmtiError GetMethodModifiers (jmethodID method, jint *modifiers_ptr)
  { return p->GetMethodModifiers (this, method, modifiers_ptr); }

  jvmtiError GetMaxLocals (jmethodID method, jint *max_ptr)
  { return p->GetMaxLocals (this, method, max_ptr); }

  jvmtiError GetArgumentsSize (jmethodID method, jint *size_ptr)
  { return p->GetArgumentsSize (this, method, size_ptr); }

  jvmtiError GetLineNumberTable (jmethodID method, jint *entry_count_ptr,
				 jvmtiLineNumberEntry **table_ptr)
  { return p->GetLineNumberTable (this, method, entry_count_ptr, table_ptr); }

  jvmtiError GetMethodLocation (jmethodID method,
				jlocation *start_location_ptr,
				jlocation *end_location_ptr)
  {
    return p->GetMethodLocation (this, method, start_location_ptr,
				 end_location_ptr);
  }

  jvmtiError GetLocalVariableTable (jmethodID method, jint *entry_count_ptr,
				    jvmtiLocalVariableEntry **table_ptr)
  {
    return p->GetLocalVariableTable (this, method, entry_count_ptr,
				     table_ptr);
  }

  jvmtiError GetBytecodes (jmethodID method, jint *bytecode_count_ptr,
			   unsigned char **bytecodes_ptr)
  {
    return p->GetBytecodes (this, method, bytecode_count_ptr,
			    bytecodes_ptr);
  }

  jvmtiError IsMethodNative (jmethodID method, jboolean *is_native_ptr)
  { return p->IsMethodNative (this, method, is_native_ptr); }

  jvmtiError IsMethodSynthetic (jmethodID method, jboolean *is_synthetic_ptr)
  { return p->IsMethodSynthetic (this, method, is_synthetic_ptr); }

  jvmtiError GetLoadedClasses (jint *class_count_ptr, jclass **classes_ptr)
  { return p->GetLoadedClasses (this, class_count_ptr, classes_ptr); }

  jvmtiError GetClassLoaderClasses (jobject initiating_loader,
				    jint *class_count_ptr,
				    jclass **classes_ptr)
  {
    return p->GetClassLoaderClasses (this, initiating_loader,
				     class_count_ptr, classes_ptr);
  }

  jvmtiError PopFrame (jthread thread)
  { return p->PopFrame (this, thread); }

  jvmtiError RedefineClasses (jint class_count,
			      const jvmtiClassDefinition* class_definitions)
  { return p->RedefineClasses (this, class_count, class_definitions); }

  jvmtiError GetVersionNumber (jint *version_ptr)
  { return p->GetVersionNumber (this, version_ptr); }

  jvmtiError GetCapabilities (jvmtiCapabilities *capabilities_ptr)
  { return p->GetCapabilities (this, capabilities_ptr); }

  jvmtiError GetSourceDebugExtension (jclass klass,
				      char **source_debug_extension_ptr)
  {
    return p->GetSourceDebugExtension (this, klass,
				       source_debug_extension_ptr);
  }

  jvmtiError IsMethodObsolete (jmethodID method, jboolean *is_obsolete_ptr)
  { return p->IsMethodObsolete (this, method, is_obsolete_ptr); }


  jvmtiError SuspendThreadList (jint request_count,
				const jthread *request_list,
				jvmtiError *results)
  { return p->SuspendThreadList (this, request_count, request_list, results); }

  jvmtiError ResumeThreadList (jint request_count,
			       const jthread *request_list,
			       jvmtiError *results)
  { return p->ResumeThreadList (this, request_count, request_list, results); }

  jvmtiError GetAllStackTraces (jint max_frame_count,
				jvmtiStackInfo **stack_info_ptr,
				jint *thread_count_ptr)
  {
    return p->GetAllStackTraces (this, max_frame_count, stack_info_ptr,
				 thread_count_ptr);
  }

  jvmtiError GetThreadListStackTraces (jint thread_count,
				       const jthread *thread_list,
				       jint max_frame_count,
				       jvmtiStackInfo **stack_info_ptr)
  {
    return p->GetThreadListStackTraces (this, thread_count, thread_list,
					max_frame_count, stack_info_ptr);
  }

  jvmtiError GetThreadLocalStorage (jthread thread, void **data_ptr)
  { return p->GetThreadLocalStorage (this, thread, data_ptr); }

  jvmtiError SetThreadLocalStorage (jthread thread, const void *data)
  { return p->SetThreadLocalStorage (this, thread, data); }

  jvmtiError GetStackTrace (jthread thread, jint start_depth,
			    jint max_frame_count,
			    jvmtiFrameInfo *frame_buffer, jint *count_ptr)
  {
    return p->GetStackTrace (this, thread, start_depth, max_frame_count,
			     frame_buffer, count_ptr);
  }

  jvmtiError GetTag (jobject object, jlong *tag_ptr)
  { return p->GetTag (this, object, tag_ptr); }

  jvmtiError SetTag (jobject object, jlong tag)
  { return p->SetTag (this, object, tag); }

  jvmtiError ForceGarbageCollection (void)
  { return p->ForceGarbageCollection (this); }

  jvmtiError IterateOverObjectsReachableFromObject (jobject object,
						    jvmtiObjectReferenceCallback object_reference_callback,
						    void *user_data)
  {
    return p->IterateOverObjectsReachableFromObject (this, object,
						     object_reference_callback,
						     user_data);
  }

  jvmtiError IterateOverReachableObjects (jvmtiHeapRootCallback heap_root_callback,
					  jvmtiStackReferenceCallback stack_ref_callback,
					  jvmtiObjectReferenceCallback object_ref_callback,
					  void *user_data)
  {
    return p->IterateOverReachableObjects (this, heap_root_callback,
					   stack_ref_callback,
					   object_ref_callback,
					   user_data);
  }

  jvmtiError IterateOverHeap (jvmtiHeapObjectFilter object_filter,
			      jvmtiHeapObjectCallback heap_object_callback,
			      void *user_data)
  {
    return p->IterateOverHeap (this, object_filter, heap_object_callback,
			       user_data);
  }

  jvmtiError IterateOverInstanceOfClass (jclass klass,
					 jvmtiHeapObjectFilter object_filter,
					 jvmtiHeapObjectCallback heap_object_callback,
					 void *user_data)
  {
    return p->IterateOverInstanceOfClass (this, klass, object_filter,
					  heap_object_callback, user_data);
  }

  jvmtiError GetObjectsWithTags (jint tag_count, const jlong *tags,
				 jint *count_ptr, jobject **object_result_ptr,
				 jlong **tag_result_ptr)
  {
    return p->GetObjectsWithTags (this, tag_count, tags, count_ptr,
				  object_result_ptr, tag_result_ptr);
  }

  jvmtiError SetJNIFunctionTable (const jniNativeInterface *function_table)
  { return p->SetJNIFunctionTable (this, function_table); }

  jvmtiError GetJNIFunctionTable (jniNativeInterface **function_table_ptr)
  { return p->GetJNIFunctionTable (this, function_table_ptr); }

  jvmtiError SetEventCallbacks (const jvmtiEventCallbacks *callbacks,
				jint size_of_callbacks)
  { return p->SetEventCallbacks (this, callbacks, size_of_callbacks); }

  jvmtiError GenerateEvents (jvmtiEvent event_type)
  { return p->GenerateEvents (this, event_type); }

  jvmtiError GetExtensionFunctions (jint *extension_count_ptr,
				    jvmtiExtensionFunctionInfo **extensions)
  { return p->GetExtensionFunctions (this, extension_count_ptr, extensions); }

  jvmtiError GetExtensionEvents (jint *extension_count_ptr,
				 jvmtiExtensionEventInfo **extensions)
  { return p->GetExtensionEvents (this, extension_count_ptr, extensions); }

  jvmtiError SetExtensionEventCallback (jint extension_event_index,
					jvmtiExtensionEvent callback)
  {
    return p->SetExtensionEventCallback (this, extension_event_index,
					 callback);
  }

  jvmtiError DisposeEnvironment (void)
  { return p->DisposeEnvironment (this); }

  jvmtiError GetErrorName (jvmtiError error, char **name_ptr)
  { return p->GetErrorName (this, error, name_ptr); }

  jvmtiError GetJLocationFormat (jvmtiJlocationFormat *format_ptr)
  { return p->GetJLocationFormat (this, format_ptr); }

  jvmtiError GetSystemProperties (jint *count_ptr, char ***property_ptr)
  { return p->GetSystemProperties (this, count_ptr, property_ptr); }

  jvmtiError GetSystemProperty (const char *property, char **value_ptr)
  { return p->GetSystemProperty (this, property, value_ptr); }

  jvmtiError SetSystemProperty (const char *property, const char *value)
  { return p->SetSystemProperty (this, property, value); }

  jvmtiError GetPhase (jvmtiPhase *phase_ptr)
  { return p->GetPhase (this, phase_ptr); }

  jvmtiError GetCurrentThreadCpuTimerInfo (jvmtiTimerInfo *info_ptr)
  { return p->GetCurrentThreadCpuTimerInfo (this, info_ptr); }

  jvmtiError GetCurrentThreadCpuTime (jlong *nanos_ptr)
  { return p->GetCurrentThreadCpuTime (this, nanos_ptr); }

  jvmtiError GetThreadCpuTimerInfo (jvmtiTimerInfo *info_ptr)
  { return p->GetThreadCpuTimerInfo (this, info_ptr); }

  jvmtiError GetThreadCpuTime (jthread thread, jlong *nanos_ptr)
  { return p->GetThreadCpuTime (this, thread, nanos_ptr); }

  jvmtiError GetTimerInfo (jvmtiTimerInfo *info_ptr)
  { return p->GetTimerInfo (this, info_ptr); }

  jvmtiError GetTime (jlong *nanos_ptr)
  {return p->GetTime (this, nanos_ptr); }

  jvmtiError GetPotentialCapabilities (jvmtiCapabilities *capabilities_ptr)
  { return p->GetPotentialCapabilities (this, capabilities_ptr); }

  jvmtiError AddCapabilities (const jvmtiCapabilities *capabilities_ptr)
  { return p->AddCapabilities (this, capabilities_ptr); }

  jvmtiError RelinquishCapabilities (const jvmtiCapabilities *capabilities_ptr)
  { return p->RelinquishCapabilities (this, capabilities_ptr); }

  jvmtiError GetAvailableProcessors (jint *processor_count_ptr)
  { return p->GetAvailableProcessors (this, processor_count_ptr); }

  jvmtiError GetEnvironmentLocalStorage (void **data_ptr)
  { return p->GetEnvironmentLocalStorage (this, data_ptr); }

  jvmtiError SetEnvironmentLocalStorage (const void *data)
  { return p->SetEnvironmentLocalStorage (this, data); }

  jvmtiError AddToBootstrapClassLoaderSearch (const char *segment)
  { return p->AddToBootstrapClassLoaderSearch (this, segment); }

  jvmtiError SetVerboseFlag (jvmtiVerboseFlag flag, jboolean value)
  { return p->SetVerboseFlag (this, flag, value); }

  jvmtiError GetObjectSize (jobject object, jlong *size_ptr)
  { return p->GetObjectSize (this, object, size_ptr); }
};
#endif /* __cplusplus */

/*
 * Miscellaneous flags, constants, etc
 */

/* Class status flags */
#define JVMTI_CLASS_STATUS_VERIFIED 1
#define JVMTI_CLASS_STATUS_PREPARED 2
#define JVMTI_CLASS_STATUS_INITIALIZED 4
#define JVMTI_CLASS_STATUS_ERROR 8
#define JVMTI_CLASS_STATUS_ARRAY 16
#define JVMTI_CLASS_STATUS_PRIMITIVE 32

/* Thread state flags */
#define JVMTI_THREAD_STATE_ALIVE 0x0001
#define JVMTI_THREAD_STATE_TERMINATED 0x0002
#define JVMTI_THREAD_STATE_RUNNABLE 0x0004
#define JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER 0x0400
#define JVMTI_THREAD_STATE_WAITING 0x0080
#define JVMTI_THREAD_STATE_WAITING_INDEFINITELY 0x0010
#define JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT 0x0020
#define JVMTI_THREAD_STATE_SLEEPING 0x0040
#define JVMTI_THREAD_STATE_IN_OBJECT_WAIT 0x0100
#define JVMTI_THREAD_STATE_PARKED 0x0200
#define JVMTI_THREAD_STATE_SUSPENDED 0x100000
#define JVMTI_THREAD_STATE_INTERRUPTED 0x200000
#define JVMTI_THREAD_STATE_IN_NATIVE 0x400000
#define JVMTI_THREAD_STATE_VENDOR_1 0x10000000
#define JVMTI_THREAD_STATE_VENDOR_2 0x20000000
#define JVMTI_THREAD_STATE_VENDOR_3 0x40000000

/* java.lang.Thread.State conversion masks */
#define JVMTI_JAVA_LANG_THREAD_STATE_MASK		\
  (JVMTI_THREAD_STATE_TERMINATED			\
   | JVMTI_THREAD_STATE_ALIVE				\
   | JVMTI_THREAD_STATE_RUNNABLE			\
   | JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER	\
   | JVMTI_THREAD_STATE_WAITING				\
   | JVMTI_THREAD_STATE_WAITING_INDEFINITELY		\
   | JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT)
#define JVMTI_JAVA_LANG_THREAD_STATE_NEW 0
#define JVMTI_JAVA_LANG_THREAD_STATE_TERMINATED JVMTI_THREAD_STATE_TERMINATED
#define JVMTI_JAVA_LANG_THREAD_STATE_RUNNABLE \
  (JVMTI_THREAD_STATE_ALIVE		      \
   | JVMTI_THREAD_STATE_RUNNABLE)
#define JVMTI_JAVA_LANG_THREAD_STATE_BLOCKED	\
  (JVMTI_THREAD_STATE_ALIVE			\
   | JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER)
#define JVMTI_JAVA_LANG_THREAD_STATE_WAITING	\
  (JVMTI_THREAD_STATE_ALIVE			\
   | JVMTI_THREAD_STATE_WAITING			\
   | JVMTI_THREAD_STATE_WAITING_INDEFINITELY)
#define JVMTI_JAVA_LANG_THREAD_STATE_TIMED_WAITING \
  (JVMTI_THREAD_STATE_ALIVE			   \
   | JVMTI_THREAD_STATE_WAITING			   \
   | JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT)

/* Thread priorities */
#define JVMTI_THREAD_MIN_PRIORITY 1
#define JVMTI_THREAD_NORM_PRIORITY 5
#define JVMTI_THREAD_MAX_PRIORITY 10

/* Keep c-font-lock-extra-types in order: JNI followed by JVMTI,
   all in alphabetical order */
/* Local Variables: */
/* c-font-lock-extra-types: ("\\sw+_t"
   "JNIEnv" "JNINativeMethod" "JavaVM" "JavaVMOption" "jarray"
   "jboolean" "jbooleanArray" "jbyte" "jbyteArray" "jchar"  "jcharArray"
   "jclass" "jdouble" "jdoubleArray" "jfieldID" "jfloat" "jfloatArray"
   "jint" "jintArray" "jlong" "jlongArray" "jmethodID" "jobject" "jstring" "jthrowable"
   "jvalue" "jweak"
   "jvmtiEnv" "jvmtiError"
   "jthread" "jthreadGroup" "jlocation" "jrawMonitorID") */
/* End: */
#endif /* !_CLASSPATH_JVMTI_H */
