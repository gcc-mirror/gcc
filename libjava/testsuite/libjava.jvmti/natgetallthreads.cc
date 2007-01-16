#include <gcj/cni.h>

#include <jvm.h>
#include <jvmti.h>
#include <stdio.h>

#include "jvmti-int.h"
#include "getallthreads.h"

void
getallthreads::natPlaceholder ()
{
	 ex_frames--;

	 if (ex_frames > 0)
	   {
			 if ((getallthreads::thread_num % 2) == 0)
			   placeholder ();
			 else
				 natPlaceholder ();
		 }
	 else
	   natRunner ();
}

void
getallthreads::natRunner ()
{
	 done = true;
	 while (done)
    yield ();
}

void
getallthreads::do_getallthreads_tests ()
{
  jvmtiEnv *env;
  JavaVM *vm = _Jv_GetJavaVM ();
  vm->GetEnv (reinterpret_cast<void **> (&env), JVMTI_VERSION_1_0);

	 jint num_threads;
	 jthread *thread_arr;

	 jvmtiError jerr = env->GetAllThreads (&num_threads, &thread_arr);
	 if (jerr != JVMTI_ERROR_NONE)
	   {
		   printf ("Test Failed, JVMTI Error!\n");
			 return;
		 }
	 env->Deallocate (reinterpret_cast<unsigned char *> (thread_arr));
	
	 for (int i = 0; i < num_threads; i++)
  	 printf ("Found thread %d\n", i+1 );
}

