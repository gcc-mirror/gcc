#include <gcj/cni.h>

#include <jvm.h>
#include <jvmti.h>
#include <stdio.h>

#include "jvmti-int.h"
#include "events.h"

void
print_events ()
{
#define DO(X)					\
  do						\
    {						\
      if (JVMTI_REQUESTED_EVENT (X))		\
	printf (#X ",");			\
    }						\
  while (0)

  printf ("RequestedEvents: ");
  DO (VMInit);
  DO (VMDeath);
  DO (ThreadStart);
  DO (ThreadEnd);
  DO (ClassFileLoadHook);
  DO (ClassLoad);
  DO (ClassPrepare);
  DO (VMStart);
  DO (Exception);
  DO (ExceptionCatch);
  DO (SingleStep);
  DO (FramePop);
  DO (Breakpoint);
  DO (FieldAccess);
  DO (FieldModification);
  DO (MethodEntry);
  DO (MethodExit);
  DO (NativeMethodBind);
  DO (CompiledMethodLoad);
  DO (CompiledMethodUnload);
  DO (DynamicCodeGenerated);
  DO (DataDumpRequest);
  DO (MonitorWait);
  DO (MonitorWaited);
  DO (MonitorContendedEnter);
  DO (MonitorContendedEntered);
  DO (GarbageCollectionStart);
  DO (GarbageCollectionFinish);
  DO (ObjectFree);
  DO (VMObjectAlloc);
  printf ("\n");
#undef DO
}

static void
VMInitCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread)
{
  printf ("VMInitCB jni_env=%#llx thread=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread);
}

static void
VMDeathCB (jvmtiEnv *env, JNIEnv *jni_env)
{
  printf ("VMDeathCB jni_env=%#llx\n", (unsigned long long) jni_env);
}

static void
ThreadStartCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread)
{
  printf ("ThreadStartCB jni_env=%#llx thread=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread);
}

static void
ThreadEndCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread)
{
  printf ("ThreadEndCB jni_env=%#llx thread=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread);
}

static void
ClassFileLoadHookCB (jvmtiEnv *env, JNIEnv *jni_env,
		     jclass class_being_redefined, jobject loader,
		     const char *name, jobject protection_domain,
		     jint class_data_len, const unsigned char *class_data,
		     jint *new_class_data_len, unsigned char **new_class_data)
{
  printf ("ClassFileLoadHookCB jni_env=%#llx class_being_redefined=%#llx"
	  " loader=%#llx", (unsigned long long) jni_env, (unsigned long long)
	  class_being_redefined, (unsigned long long) loader);
  printf (" name=%s protection_domain=%#llx class_data_len=%d class_data=%#llx",
	  name, (unsigned long long) protection_domain, (int) class_data_len,
	  (unsigned long long) class_data);
  printf (" new_class_data_len=%#llx new_class_data=%#llx\n",
	  (unsigned long long) new_class_data_len, (unsigned long long)
	  new_class_data);
}

static void
ClassLoadCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread, jclass klass)
{
  printf ("ClassLoadCB jni_env=%#llx thread=%#llx klass=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) klass);
}

static void
ClassPrepareCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread, jclass klass)
{
  printf ("ClassPrepareCB jni_env=%#llx thread=%#llx klass=%#llx\n",
	  (unsigned long long)jni_env, (unsigned long long) thread,
	  (unsigned long long) klass);
}

static void
VMStartCB (jvmtiEnv *env, JNIEnv *jni_env)
{
  printf ("VMStartCB jni_env=%#llx\n", (unsigned long long) jni_env);
}

static void
ExceptionCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread, jmethodID method,
	     jlocation location, jobject exception, jmethodID catch_method,
	     jlocation catch_location)
{
  printf ("ExceptionCB jni_env=%#llx thread=%#llx method=%#llx location=%#llx",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) method, (unsigned long long) location);
  printf (" exception=%#llx catch_method=%#llx catch_location=%#llx\n",
	  (unsigned long long) exception, (unsigned long long) catch_method,
	  (unsigned long long) catch_location);
}

static void
ExceptionCatchCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
		  jmethodID method, jlocation location, jobject exception)
{
  printf ("ExceptionCatchCB jni_env=%#llx thread=%#llx method=%#llx"
	  " location=%#llx",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) method, (unsigned long long) location);
  printf (" exception=%#llx\n", (unsigned long long) exception);
}

static void
SingleStepCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread, jmethodID method,
	      jlocation location)
{
  printf ("SingleStepCB jni_env=%#llx thread=%#llx method=%#llx"
	  " location=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) method, (unsigned long long) location);
}

static void
FramePopCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread, jmethodID method,
	    jboolean was_popped_by_exception)
{
  printf ("FramePopCB jni_env=%#llx thread=%#llx method=%#llx",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) method);
  printf (" was_pooped_by_exception=%d\n", (was_popped_by_exception ?
					    1 : 0));
}

static void
BreakpointCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread, jmethodID method,
	      jlocation location)
{
  printf ("BreakpointCB  jni_env=%#llx thread=%#llx method=%#llx"
	  " location=%#llx\n", (unsigned long long) jni_env,
	  (unsigned long long) thread, (unsigned long long) method,
	  (unsigned long long) location);
}

static void
FieldAccessCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
	       jmethodID method, jlocation location, jclass field_klass,
	       jobject object, jfieldID field)
{
  printf ("FieldAccessCB jni_env=%#llx thread=%#llx method=%#llx"
	  " location=%#llx", (unsigned long long) jni_env, (unsigned long long)
	  thread, (unsigned long long) method, (unsigned long long) location);
  printf (" field_klass=%#llx object=%#llx field=%#llx\n", (unsigned long long)
	  field_klass, (unsigned long long) object, (unsigned long long) field);
}

static void
FieldModificationCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
		     jmethodID method, jlocation location, jclass field_klass,
		     jobject object, jfieldID field, char signature_type,
		     jvalue new_value)

{
  printf ("FieldModificationCB  jni_env=%#llx thread=%#llx method=%#llx"
	  " location=%#llx", (unsigned long long) jni_env, (unsigned long long)
	  thread, (unsigned long long) method, (unsigned long long) location);
  printf (" field_klass=%#llx object=%#llx field=%#llx signature_type=%c",
	  (unsigned long long) field_klass, (unsigned long long) object,
	  (unsigned long long) field, signature_type);
  printf (" new_value=%#llx\n", (unsigned long long) new_value.l);
}

static void
MethodEntryCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
	       jmethodID method)
{
  printf ("MethodEntryCB jni_env=%#llx thread=%#llx method=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) method);
}

static void
MethodExitCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
	      jmethodID method, jboolean was_popped_by_exception,
	      jvalue return_value)
{
  printf ("MethodExitCB jni_env=%#llx thread=%#llx method=%#llx",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) method);
  printf (" was_popped_by_exception=%d return_value=%d\n",
	  (was_popped_by_exception) ? 1 : 0, (int) return_value.i);
}

static void
NativeMethodBindCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
		    jmethodID method, void *address, void **new_address_ptr)
{
  printf ("NativeMethodBindCB jni_env=%#llx thread=%#llx method=%#llx",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) method);
  printf (" address=%#llx new_address_ptr=%#llx\n", (unsigned long long)
	  address, (unsigned long long) new_address_ptr);
}

static void
CompiledMethodLoadCB (jvmtiEnv *env, jmethodID method, jint code_size,
		      const void *code_addr, jint map_length,
		      const jvmtiAddrLocationMap *map,
		      const void *compile_info)
{
  printf ("CompiledMethodLoadCB method=%#llx code_size=%#llx code_addr=%#llx",
	  (unsigned long long) method, (unsigned long long) code_size,
	  (unsigned long long) code_addr);
  printf (" map_length=%d map=%#llx compile_info=%#llx\n", (int) map_length,
	  (unsigned long long) map, (unsigned long long) compile_info);
}

static void
CompiledMethodUnloadCB (jvmtiEnv *env, jmethodID method, const void *code_addr)
{
  printf ("CompiledMethodUnloadCB method=%#llx code_addr=%#llx\n",
	  (unsigned long long) method, (unsigned long long) code_addr);
}

static void
DynamicCodeGeneratedCB (jvmtiEnv *env, const char *name, const void *address,
			jint length)
{
  printf ("DynamicCodeGeneratedCB name=%s address=%#llx length=%d\n", name,
	  (unsigned long long) address, (int) length);
}

static void
DataDumpRequestCB (jvmtiEnv *env)
{
  printf ("DataDumpRequestCB\n");
}

static void
MonitorWaitCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread, jobject object,
	       jlong timeout)
{
  printf ("MonitorWaitCB jni_env=%#llx thread=%#llx object=%#llx timeout=%ld\n",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) object, (long) timeout);
}

static void
MonitorWaitedCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
		 jobject object, jboolean timed_out)
{
  printf ("MonitorWaitedCB jni_env=%#llx thread=%#llx object=%#llx"
	  " timed_out=%d\n", (unsigned long long) jni_env, (unsigned long long)
	  thread, (unsigned long long) object, (timed_out) ? 1 : 0);
}

static void
MonitorContendedEnterCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
			 jobject object)
{
  printf ("MonitorContendedEnterCB jni_env=%#llx thread=%#llx object=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) object);
}

static void
MonitorContendedEnteredCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
			   jobject object)
{
  printf ("MonitorContendedEnteredCB jni_env=%#llx thread=%#llx object=%#llx\n",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) object);
}

static void
GarbageCollectionStartCB (jvmtiEnv *env)
{
  printf ("GarbageCollectionStartCB\n");
}

static void
GarbageCollectionFinishCB (jvmtiEnv *env)
{
  printf ("GarbageCollectionFinishCB\n");
}

static void
ObjectFreeCB (jvmtiEnv *env, jlong tag)
{
  printf ("ObjectFreeCB tag=%ld\n", (long) tag);
}

static void
VMObjectAllocCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread,
		 jobject object, jclass object_klass, jlong size)
{
  printf ("VMObjectAllocCB jni_env=%#llx thread=%#llx object=%#llx",
	  (unsigned long long) jni_env, (unsigned long long) thread,
	  (unsigned long long) object);
  printf (" object_klass=%#llx size=%ld\n", (unsigned long long) object_klass,
	  (long) size);
}

static void
do_enable_tests ()
{
  printf ("- enable tests -\n");
  JavaVM *vm = _Jv_GetJavaVM ();
  jvmtiEnv *env[3];
  int i;
  for (i = 0; i < 3; ++i)
    {
      vm->GetEnv (reinterpret_cast<void **> (&env[i]), JVMTI_VERSION_1_0);
      printf ("created JVMTI environment #%d\n", i);
    }

  jvmtiEventCallbacks callbacks;
  memset (&callbacks, 0, sizeof (jvmtiEventCallbacks));

  printf ("setting callbacks for envs\n");
  callbacks.VMInit = VMInitCB;
  env[0]->SetEventCallbacks (&callbacks, sizeof (callbacks));
  callbacks.VMDeath = VMDeathCB;
  env[1]->SetEventCallbacks (&callbacks, sizeof (callbacks));
  callbacks.ThreadEnd = ThreadEndCB;
  env[2]->SetEventCallbacks (&callbacks, sizeof (callbacks));
  print_events ();

  printf ("enable VM_INIT for env0, env1, env2\n");
  env[0]->SetEventNotificationMode (JVMTI_ENABLE, JVMTI_EVENT_VM_INIT, NULL);
  env[1]->SetEventNotificationMode (JVMTI_ENABLE, JVMTI_EVENT_VM_INIT, NULL);
  env[2]->SetEventNotificationMode (JVMTI_ENABLE, JVMTI_EVENT_VM_INIT, NULL);
  print_events ();

  printf ("enable VM_DEATH for env1,env2\n");
  env[1]->SetEventNotificationMode (JVMTI_ENABLE, JVMTI_EVENT_VM_DEATH, NULL);
  env[2]->SetEventNotificationMode (JVMTI_ENABLE, JVMTI_EVENT_VM_DEATH, NULL);
  print_events ();

  /* Used to use a non-NULL event thread, but that causes problems
     when SetEventNotificationMode tries to validate the thread. */
  printf ("enable THREAD_END for env2\n");
  env[2]->SetEventNotificationMode (JVMTI_ENABLE, JVMTI_EVENT_THREAD_END,
				    NULL);
  print_events ();

  printf ("disposing of env1\n");
  env[1]->DisposeEnvironment ();
  print_events ();

  printf ("disposing of env0\n");
  env[0]->DisposeEnvironment ();
  print_events ();

  printf ("disable VMInit in env2\n");
  env[2]->SetEventNotificationMode (JVMTI_DISABLE, JVMTI_EVENT_VM_INIT, NULL);
  print_events ();

  printf ("clear VMDeath callback in env2\n");
  callbacks.VMDeath = NULL;
  env[2]->SetEventCallbacks (&callbacks, sizeof (callbacks));
  print_events ();

  printf ("sending VMInit\n");
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_INIT, (jthread) 0x1234,
		       (JNIEnv *) 0x5678);

  printf ("sending ThreadEnd\n");
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_THREAD_END, (jthread) 0x1234,
		       (JNIEnv *) 0x5678);

  /* See comment above re: SetEventNotificationMode and validity
     checking
  printf ("sending ThreadEnd (no match)\n");
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_THREAD_END, (jthread) 0x4321,
		       (JNIEnv *) 0x5678);
  */

  printf ("sending VMDeath\n");
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_DEATH, (jthread) NULL,
		       (JNIEnv *) 0x5678);

  printf ("disposing of env2\n");
  env[2]->DisposeEnvironment ();
  print_events ();
}

static void
do_callback_arg_tests ()
{
  printf ("- callback arg tests -\n");
  JavaVM *vm = _Jv_GetJavaVM ();
  jvmtiEnv *env;
  vm->GetEnv (reinterpret_cast<void **> (&env), JVMTI_VERSION_1_0);

  // Define all the callbacks
#define DEFINE(Event) callbacks.Event = Event ## CB;
  jvmtiEventCallbacks callbacks;
  DEFINE(VMInit);
  DEFINE(VMDeath);
  DEFINE(ThreadStart);
  DEFINE(ThreadEnd);
  DEFINE(ClassFileLoadHook);
  DEFINE(ClassLoad);
  DEFINE(ClassPrepare);
  DEFINE(VMStart);
  DEFINE(Exception);
  DEFINE(ExceptionCatch);
  DEFINE(SingleStep);
  DEFINE(FramePop);
  DEFINE(Breakpoint);
  DEFINE(FieldAccess);
  DEFINE(FieldModification);
  DEFINE(MethodEntry);
  DEFINE(MethodExit);
  DEFINE(NativeMethodBind);
  DEFINE(CompiledMethodLoad);
  DEFINE(CompiledMethodUnload);
  DEFINE(DynamicCodeGenerated);
  DEFINE(DataDumpRequest);
  DEFINE(MonitorWait);
  DEFINE(MonitorWaited);
  DEFINE(MonitorContendedEnter);
  DEFINE(MonitorContendedEntered);
  DEFINE(GarbageCollectionStart);
  DEFINE(GarbageCollectionFinish);
  DEFINE(ObjectFree);
  DEFINE(VMObjectAlloc);
#undef DEFINE
  env->SetEventCallbacks (&callbacks, sizeof (callbacks));

  // Enable all the callbacks
#define ENABLE(Event)							\
  env->SetEventNotificationMode (JVMTI_ENABLE, JVMTI_EVENT_ ## Event, NULL)
  ENABLE (VM_INIT);
  ENABLE (VM_DEATH);
  ENABLE (THREAD_START);
  ENABLE (THREAD_END);
  ENABLE (CLASS_FILE_LOAD_HOOK);
  ENABLE (CLASS_LOAD);
  ENABLE (CLASS_PREPARE);
  ENABLE (VM_START);
  ENABLE (EXCEPTION);
  ENABLE (EXCEPTION_CATCH);
  ENABLE (SINGLE_STEP);
  ENABLE (FRAME_POP);
  ENABLE (BREAKPOINT);
  ENABLE (FIELD_ACCESS);
  ENABLE (FIELD_MODIFICATION);
  ENABLE (METHOD_ENTRY);
  ENABLE (METHOD_EXIT);
  ENABLE (NATIVE_METHOD_BIND);
  ENABLE (COMPILED_METHOD_LOAD);
  ENABLE (COMPILED_METHOD_UNLOAD);
  ENABLE (DYNAMIC_CODE_GENERATED);
  ENABLE (DATA_DUMP_REQUEST);
  ENABLE (MONITOR_WAIT);
  ENABLE (MONITOR_WAITED);
  ENABLE (MONITOR_CONTENDED_ENTER);
  ENABLE (MONITOR_CONTENDED_ENTERED);
  ENABLE (GARBAGE_COLLECTION_START);
  ENABLE (GARBAGE_COLLECTION_FINISH);
  ENABLE (OBJECT_FREE);
  ENABLE (VM_OBJECT_ALLOC);

  // All events should now be enabled.
  print_events ();

  _Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_INIT, (jthread) 0x2, (JNIEnv *) 0x1);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_DEATH, (jthread) 0x2, (JNIEnv *) 0x1);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_THREAD_START, (jthread) 0x2,
		       (JNIEnv *) 0x1);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_THREAD_END, (jthread) 0x2,
		       (JNIEnv *) 0x1);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_CLASS_FILE_LOAD_HOOK, (jthread) 0xb00,
		       (JNIEnv *) 0x1, (jclass) 0x2, (jobject) 0x3,
		       "4", (jobject) 0x5, (jint) 6,
		       (const unsigned char *) 0x7, (jint *) 0x8,
		       (unsigned char **) 0x9);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_CLASS_LOAD, (jthread) 0x2, (JNIEnv *) 0x1,
		       (jclass) 0x3);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_CLASS_PREPARE, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jclass) 0x3);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_START, (jthread) 0xb00, (JNIEnv *) 0x1);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_EXCEPTION, (jthread) 0x2, (JNIEnv *) 0x1,
		       (jmethodID) 0x3, (jlocation) 0x4, (jobject) 0x5,
		       (jmethodID) 0x6, (jlocation) 0x7);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_EXCEPTION_CATCH, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jmethodID) 0x3, (jlocation) 0x4,
		       (jobject) 0x5);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_SINGLE_STEP, (jthread) 0x2, (JNIEnv *) 0x1,
		       (jmethodID) 0x3, (jlocation) 0x4);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_FRAME_POP, (jthread) 0x2, (JNIEnv *) 0x1,
		       (jmethodID) 0x3, 4);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_BREAKPOINT, (jthread) 0x2, (JNIEnv *) 0x1,
		       (jmethodID) 0x3, (jlocation) 0x4);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_FIELD_ACCESS, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jmethodID) 0x3, (jlocation) 0x4,
		       (jclass) 0x5, (jobject) 0x6, (jfieldID) 0x7);
  jvalue value;
  value.l = (jobject) 0x9;
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_FIELD_MODIFICATION, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jmethodID) 0x3, (jlocation) 0x4,
		       (jclass) 0x5, (jobject) 0x6, (jfieldID) 0x7,
		       (int) '8', value);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_METHOD_ENTRY, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jmethodID) 0x3);
  jvalue value2;
  value2.i = 5;
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_METHOD_EXIT, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jmethodID) 0x3, 4, value2);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_NATIVE_METHOD_BIND, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jmethodID) 0x3, (void *) 0x4,
		       (void **) 0x5);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_COMPILED_METHOD_LOAD, (jthread) 0xb00,
		       (jmethodID) 0x1, (jint) 2, (const void *) 0x3,
		       (jint) 4, (const jvmtiAddrLocationMap *) 0x5,
		       (const void *) 0x6);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_COMPILED_METHOD_UNLOAD, (jthread) 0xb00,
		       (jmethodID) 0x1, (const void *) 0x2);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_DYNAMIC_CODE_GENERATED, (jthread) 0xb00,
		       "1", (const void *) 0x2, (jint) 3);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_DATA_DUMP_REQUEST, (jthread) 0xb00);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_MONITOR_WAIT, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jobject) 0x3, (jlong) 4);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_MONITOR_WAITED, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jobject) 0x3, (int) 4);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_MONITOR_CONTENDED_ENTER, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jobject) 0x3);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_MONITOR_CONTENDED_ENTERED, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jobject) 0x3);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_GARBAGE_COLLECTION_START, (jthread) 0xb00);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_GARBAGE_COLLECTION_FINISH, (jthread) 0xb00);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_OBJECT_FREE, (jthread) 0xb00, (jlong) 1);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_OBJECT_ALLOC, (jthread) 0x2,
		       (JNIEnv *) 0x1, (jobject) 0x3, (jclass) 0x4,
		       (jlong) 5);
}

void
events::do_events_tests ()
{
  do_enable_tests ();
  do_callback_arg_tests ();
}
