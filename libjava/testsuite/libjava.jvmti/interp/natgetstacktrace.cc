#include <jni.h>

#include <jvmti.h>
#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

#include "getstacktrace.h"

void
printStackTrace (jvmtiFrameInfo *frames, jint frame_cnt)
{
  printf ("Thread has %d frames\n", static_cast<int> (frame_cnt));

  for (int i = 0; i < frame_cnt; i++)
    {
      jmethodID method = frames[i].method;
      jlocation location = frames[i].location;

      if (location == -1)
        {
          printf ("Frame %d is native\n", i);
        }
      else
        {
          printf ("Frame %d is interpreted\n", i);
        }
    }
}


JNIEXPORT void JNICALL Java_getstacktrace_natPlaceholder (JNIEnv *env, jobject obj)
{
  jclass klass = env->GetObjectClass (obj);
  jfieldID done_id = env->GetFieldID (klass, "done", "Z");
  jfieldID num_frames_id = env->GetFieldID (klass, "num_frames", "I");
  jfieldID thread_num_id = env->GetFieldID (klass, "thread_num", "I");

  // num_frames--
  jint n_frames = env->GetIntField (obj, num_frames_id);
  n_frames--;
  env->SetIntField (obj, num_frames_id, n_frames);

  jint t_num = env->GetIntField (obj, thread_num_id);

  if (n_frames <= 1)
    {
      if (t_num % 2 == 1)
        {
          jmethodID natRunner_id = env->GetMethodID (klass, "natRunner", "()V");
          env->CallVoidMethod (obj, natRunner_id);
        }
      else
        {
          jmethodID runner_id = env->GetMethodID (klass, "runner", "()V");
          env->CallVoidMethod (obj, runner_id);
        }
    }
  else
    {
      if (t_num % 2 == 0)
        {
          jmethodID natPlaceholder_id = env->GetMethodID (klass,
                                        "natPlaceholder",
                                        "()V");
          env->CallVoidMethod (obj, natPlaceholder_id);
        }
      else
        {
          jmethodID placeholder_id = env->GetMethodID (klass, "placeholder",
                                     "()V");
          env->CallVoidMethod (obj, placeholder_id);
        }
    }
}

JNIEXPORT void JNICALL Java_getstacktrace_natRunner (JNIEnv *env, jobject obj)
{
  jclass klass = env->GetObjectClass (obj);
  jfieldID done_id = env->GetFieldID (klass, "done", "Z");


  jboolean done;
  done = true;
  env->SetBooleanField (obj, done_id, done);

  do
    {
      done = env->GetBooleanField (obj, done_id);
      if (done == false)
        break;
      usleep (40);
    }
  while (done != false);
}

JNIEXPORT jint JNICALL Java_getstacktrace_do_1getstacktrace_1tests
(JNIEnv *env, jclass klass, jobjectArray thr_arr)
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

  jint frame_cnt;
  jvmtiFrameInfo frames[30];

  jvmtiError jerr;
  jthread thr;

  jsize num_threads = env->GetArrayLength (thr_arr);

  for (int i = 0; i < num_threads; i++)
    {
      thr = reinterpret_cast<jthread>
            (env->GetObjectArrayElement (thr_arr, static_cast<jsize> (i)));
      fflush (stdout);
      jerr = jvmti->GetStackTrace (thr, 0, 30, frames, &frame_cnt);
      if (jerr != JVMTI_ERROR_NONE)
        {
          char *error_name;
          jvmti->GetErrorName (jerr, &error_name);
          fprintf (stderr, "JVMTI Error: %s\n", error_name);
          jvmti->Deallocate (reinterpret_cast<unsigned char *> (error_name));
        }
      else
        {
          printStackTrace (frames, frame_cnt);
        }
    }
}
