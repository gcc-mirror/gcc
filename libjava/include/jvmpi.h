/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Note: this file must be compilable by the C compiler (for now,
   assuming GNU C is ok).  This means you must never use `//'
   comments, and all C++-specific code must be conditional on
   __cplusplus.  */

#ifndef __GCJ_JVMPI_H__
#define __GCJ_JVMPI_H__

#include <jni.h>

/* JVMPI version numbers.  FIXME: this is a semi-random number.  The
   documentation doesn't say what it should be.  */
#define JVMPI_VERSION_1 0x00020001

/* JVMPI return codes.  FIXME: These are semi-random numbers.  The
   documentation doesn't say what they should be.  */
#define JVMPI_SUCCESS       0
#define JVMPI_FAIL          1
#define JVMPI_NOT_AVAILABLE 2

/* An opaque pointer representing an object ID.  */
struct _jobjectID;
typedef struct _jobjectID * jobjectID;       

typedef struct
{
  /* Source line number.  */
  jint lineno;
  /* Method being executed.  */
  jmethodID method_id;
} JVMPI_CallFrame;

typedef struct 
{
  JNIEnv *env_id;
  /* Number of frames in the call trace.  */
  jint num_frames;
  /* An array of frames representing the trace.  Callees first.  */
  JVMPI_CallFrame *frames;
} JVMPI_CallTrace;

typedef struct
{
  /* Name of the field.  */
  char *field_name;
  /* Signature of the field.  */
  char *field_signature;
} JVMPI_Field;

/* The documentation doesn't actually specify what the
   JVMPI_DUMP_LEVEL macros should be defined to.  Here's a reasonable
   guess.  */
#define JVMPI_DUMP_LEVEL_0 0
#define JVMPI_DUMP_LEVEL_1 1
#define JVMPI_DUMP_LEVEL_2 2
#define JVMPI_DUMP_LEVEL_3 3

typedef struct
{
  /* One of JVMPI_DUMP_LEVEL_0, JVMPI_DUMP_LEVEL_1 or
     JVMPI_DUMP_LEVEL_2.  */
  jint heap_dump_level;
} JVMPI_HeapDumpArg;

typedef struct
{
  /* Offset from the beginning of the method.  */
  jint offset;
  /* Line number from the beginning of the source file.  */
  jint lineno;
} JVMPI_Lineno;

typedef struct
{
  /* Name of the method.  */
  char *method_name;
  /* Signature of the method.  */
  char *method_signature;
  /* Start line number from the beginning of the source file.  */
  jint start_lineno;
  /* End line number from the beginning of the source file.  */
  jint end_lineno;
  /* The method ID.  */
  jmethodID method_id;
} JVMPI_Method;

/* An opaque pointer representing a raw monitor.  */
struct _JVMPI_RawMonitor;
typedef struct _JVMPI_RawMonitor *JVMPI_RawMonitor;

/* JVMPI event codes.  FIXME: These are semi-random numbers.  The
   documentation doesn't say what they should be.  */
#define JVMPI_EVENT_ARENA_DELETE                   0
#define JVMPI_EVENT_ARENA_NEW                      1
#define JVMPI_EVENT_CLASS_LOAD                     2
#define JVMPI_EVENT_CLASS_LOAD_HOOK                3
#define JVMPI_EVENT_CLASS_UNLOAD                   4
#define JVMPI_EVENT_COMPILED_METHOD_LOAD           5
#define JVMPI_EVENT_COMPILED_METHOD_UNLOAD         6
#define JVMPI_EVENT_DATA_DUMP_REQUEST              7
#define JVMPI_EVENT_DATA_RESET_REQUEST             8
#define JVMPI_EVENT_GC_FINISH                      9
#define JVMPI_EVENT_GC_START                      10
#define JVMPI_EVENT_HEAP_DUMP                     11
#define JVMPI_EVENT_JNI_GLOBALREF_ALLOC           12
#define JVMPI_EVENT_JNI_GLOBALREF_FREE            13
#define JVMPI_EVENT_JNI_WEAK_GLOBALREF_ALLOC      14
#define JVMPI_EVENT_JNI_WEAK_GLOBALREF_FREE       15
#define JVMPI_EVENT_JVM_INIT_DONE                 16
#define JVMPI_EVENT_JVM_SHUT_DOWN                 17
#define JVMPI_EVENT_METHOD_ENTRY                  18
#define JVMPI_EVENT_METHOD_ENTRY2                 19
#define JVMPI_EVENT_METHOD_EXIT                   20
#define JVMPI_EVENT_MONITOR_CONTENDED_ENTER       21
#define JVMPI_EVENT_MONITOR_CONTENDED_ENTERED     22
#define JVMPI_EVENT_MONITOR_CONTENDED_EXIT        23
#define JVMPI_EVENT_MONITOR_DUMP                  24
#define JVMPI_EVENT_MONITOR_WAIT                  25
#define JVMPI_EVENT_MONITOR_WAITED                26
#define JVMPI_EVENT_OBJECT_ALLOC                  27
#define JVMPI_EVENT_OBJECT_DUMP                   28
#define JVMPI_EVENT_OBJECT_FREE                   29
#define JVMPI_EVENT_OBJECT_MOVE                   30
#define JVMPI_EVENT_RAW_MONITOR_CONTENDED_ENTER   31
#define JVMPI_EVENT_RAW_MONITOR_CONTENDED_ENTERED 32
#define JVMPI_EVENT_RAW_MONITOR_CONTENDED_EXIT    33
#define JVMPI_EVENT_THREAD_END                    34
#define JVMPI_EVENT_THREAD_START                  35
#define JVMPI_EVENT_INSTRUCTION_START             36


typedef struct
{
  /* Event type.  */
  jint event_type;

  /* Evn where this event occurred.  */
  JNIEnv *env_id;

  union 
  {
    struct
    {
      char *class_name;
      char *source_name;
      jint num_interfaces;
      jint num_methods;
      JVMPI_Method *methods;
      jint num_static_fields;
      JVMPI_Field *statics;
      jint num_instance_fields;
      JVMPI_Field *instances;
      jobjectID class_id;
    } class_load;

    struct
    {
      jobjectID class_id;
    } class_unload;

    struct
    {
      jint arena_id;
      jobjectID class_id;
      jint is_array;
      jint size;
      jobjectID obj_id;
    } obj_alloc;

    struct
    {
      char *thread_name;
      char *group_name;
      char *parent_name;
      jobjectID thread_id;
      JNIEnv *thread_env_id;
    } thread_start;

  } u;

} JVMPI_Event;

typedef struct
{
  /* JVMPI version number.  */
  jint version;
  
  /* Implemented by the user...  */
  void (*NotifyEvent) (JVMPI_Event *event);
  
  /* Implemented by the runtime...  */
  jint (*EnableEvent) (jint event_type, void *arg);
  jint (*DisableEvent) (jint event_type, void *arg);
  jint (*RequestEvent) (jint event_type, void *arg);
  void (*GetCallTrace) (JVMPI_CallTrace *trace, jint depth);
  void (*ProfilerExit) (jint);
  JVMPI_RawMonitor (*RawMonitorCreate) (char *lock_name);
  void (*RawMonitorEnter) (JVMPI_RawMonitor lock_id);
  void (*RawMonitorExit) (JVMPI_RawMonitor lock_id);
  void (*RawMonitorWait) (JVMPI_RawMonitor lock_id, jlong ms);
  void (*RawMonitorNotifyAll) (JVMPI_RawMonitor lock_id);
  void (*RawMonitorDestroy) (JVMPI_RawMonitor lock_id);
  jlong (*GetCurrentThreadCpuTime) (void);
  void (*SuspendThread) (JNIEnv *env);
  void (*ResumeThread) (JNIEnv *env);
  jint (*GetThreadStatus) (JNIEnv *env);
  jboolean (*ThreadHasRun) (JNIEnv *env);
  jint (*CreateSystemThread) (char *name, jint priority, void (*f) (void *));
  void (*SetThreadLocalStorage) (JNIEnv *env_id, void *ptr);
  void *(*GetThreadLocalStorage) (JNIEnv *env_id);
  void (*DisableGC) (void);
  void (*EnableGC) (void);
  void (*RunGC) (void);
  jobjectID (*GetThreadObject) (JNIEnv *env);
  jobjectID (*GetMethodClass) (jmethodID mid);
  
} JVMPI_Interface;

#endif /* __GCJ_JVMPI_H__ */
