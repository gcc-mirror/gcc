#include <jni.h>

#include <jvmti.h>
#include <stdio.h>
#include <stdlib.h>

#include "getargssize.h"

JNIEXPORT jint JNICALL Java_getargssize_do_1getargssize_1tests
(JNIEnv *env, jclass klass)
{
  JavaVM *vm;
  jint err = env->GetJavaVM (&vm);
  if (err < 0)
    {
      fprintf (stderr, "error getting VM\n");
      exit (1);
    }

  jvmtiEnv *jvmti = NULL;
  vm->GetEnv ((void **) &jvmti, JVMTI_VERSION_1_0);

  if (jvmti == NULL)
    {
      fprintf (stderr, "error getting jvmti environment\n");
      exit (1);
    }
  
  jint args_size;

  jvmtiError jerr;
  
  jmethodID meth_ids[4];
  
  meth_ids[0] = env->GetMethodID (klass, "aMethod", "(FI)I");
  meth_ids[1] = env->GetMethodID (klass, "bMethod", "(JDI)J");
  meth_ids[2] = env->GetStaticMethodID (klass, "cMethod", "()Z");
  meth_ids[3] = env->GetStaticMethodID (klass, "dMethod", 
                                     "(Ljava/lang/Object;)Ljava/lang/Object;");
  for (int i = 0; i < 4; i++)
    {
      jerr = jvmti->GetArgumentsSize (meth_ids[i], &args_size);
      if (jerr != JVMTI_ERROR_NONE)
        {
          char *error_name;
          jvmti->GetErrorName (jerr, &error_name);
          fprintf (stderr, "JVMTI Error: %s\n", error_name);
          jvmti->Deallocate (reinterpret_cast<unsigned char *> (error_name));
        }
      else
        {
          printf ("Method %d requires %d slots for its arguments\n", i,
                  args_size);
        }
    }
    
    return 0;
}
