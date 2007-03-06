#include <jni.h>

#include <jvmti.h>
#include <stdio.h>
#include <stdlib.h>

#include "getlocalvartable.h"

JNIEXPORT jint JNICALL Java_getlocalvartable_do_1getlocalvartable_1tests
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
  
  jint entrys;
  jvmtiLocalVariableEntry *var_table;

  jvmtiError jerr;
  
  jmethodID meth_ids[3];
  
  meth_ids[0] = env->GetMethodID (klass, "aMethod", "(FF)D");
  meth_ids[1] = env->GetMethodID (klass, "bMethod", "(II)J");
  meth_ids[2] = env->GetMethodID (klass, "cMethod", 
                                  "(Ljava/lang/Object;)Ljava/lang/Object;");
  for (int i = 0; i < 3; i++)
    {
      jerr = jvmti->GetLocalVariableTable (meth_ids[i], &entrys, &var_table);
      if (jerr != JVMTI_ERROR_NONE)
        {
          char *error_name;
          jvmti->GetErrorName (jerr, &error_name);
          fprintf (stderr, "JVMTI Error: %s\n", error_name);
          jvmti->Deallocate (reinterpret_cast<unsigned char *> (error_name));
        }
      else
        {
          for (int j = 0; j < entrys; j++)
            {
              printf ("Slot: %d\n", static_cast<int> (var_table[j].slot));
              printf ("  Name: %s\n", var_table[j].name);
              jvmti->Deallocate (reinterpret_cast<unsigned char *> (var_table[j].name));
              printf ("  Sig: %s\n", var_table[j].signature);
              jvmti->Deallocate (reinterpret_cast<unsigned char *> (var_table[j].signature));
              printf ("  Gen Sig: %s\n", var_table[j].generic_signature);
              jvmti->Deallocate (reinterpret_cast<unsigned char *> (var_table[j].generic_signature));
              printf ("  Start Loc: %ld\n", static_cast<long> (var_table[j].start_location));
              printf ("  Length: %d\n", static_cast<int> (var_table[j].length));
            }
          
          jvmti->Deallocate (reinterpret_cast<unsigned char *> (var_table));
        }
    }
    
    return 0;
}
