#include <assert.h>
#include <jni.h>

union env_union
{
  void *void_env;
  JNIEnv *jni_env;
};

int
main (int argc, const char** argv)
{
  union env_union tmp;
  JNIEnv* env;
  JavaVM* jvm;
  JavaVMInitArgs vm_args;
  JavaVMOption options[1];
  jclass class_id;
  jmethodID method_id;
  jint result;

  options[0].optionString = "-DPR16923=optionReceived";

  vm_args.version = JNI_VERSION_1_2;
  vm_args.ignoreUnrecognized = JNI_TRUE;
  vm_args.options = options;
  vm_args.nOptions = 1;

  result = JNI_CreateJavaVM (&jvm, &tmp.void_env, &vm_args);
  assert (result >= 0);

  env = tmp.jni_env;

  class_id = (*env)->FindClass (env, "PR16923");
  assert (class_id);

  method_id = (*env)->GetStaticMethodID (env, class_id, "printIt", "()V");
  assert (method_id);

  (*env)->CallStaticVoidMethod (env, class_id, method_id, NULL);

  return 0;
}
