/*
 *  Guile/JNI/JVM Testing Framework
 * 
 *  Copyright (c) 1998 Free Software Foundation, Inc.
 *  Written by Paul Fisher (rao@gnu.org)
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, 
 *  USA.
 */

#include <stdlib.h>
#include <stdio.h>
#include <libguile.h>
#include <guile/gh.h>
#include <jni.h>

static JNIEnv *env;
static jclass test_class, result_class;
static jmethodID test_mid, test_name_mid, result_name_mid, result_msg_mid;

SCM
abort_test (SCM name, char *exception)
{
  (*env)->ExceptionClear (env);
  return gh_list (name,
		  gh_symbol2scm ("ERROR"), 
		  gh_str02scm (exception),
		  SCM_UNDEFINED);
}

SCM
handle_test_exception (jobject test_name_obj)
{
  jthrowable throwable;
  jclass object_class;
  jobject err_msg_obj;
  char *err_msg, *test_name;
  const char *utf;
  SCM result;
  jboolean is_copy;
  static jmethodID obj_toString_mid = NULL;

  throwable = (*env)->ExceptionOccurred (env);
  (*env)->ExceptionClear (env);

  if (obj_toString_mid == NULL)
    obj_toString_mid = (*env)->GetMethodID (env, 
					    (*env)->FindClass (env, 
							  "java/lang/Object"), 
					    "toString", 
					    "()Ljava/lang/String;");

  err_msg_obj = (*env)->CallObjectMethod (env, throwable, obj_toString_mid);

  utf = (*env)->GetStringUTFChars (env, err_msg_obj, &is_copy);
  err_msg = strdup (utf);
  (*env)->ReleaseStringUTFChars (env, err_msg_obj, utf);

  utf = (*env)->GetStringUTFChars (env, test_name_obj, &is_copy);
  test_name = strdup (utf);
  (*env)->ReleaseStringUTFChars (env, test_name_obj, utf);

  result = abort_test (gh_str02scm (test_name), err_msg);

  free (err_msg);
  free (test_name);

  return result;
}   

SCM
perform_test (SCM clazz_scm_name)
{
  char *clazz_name, *test_name, *result_name, *msg;
  const char *utf;
  jclass clazz;
  jmethodID mid;
  jobject test_obj, result_obj, test_name_obj, result_name_obj, msg_obj;
  jboolean is_copy;
  SCM scm_test_name, scm_result_name, scm_result_msg;

  clazz_name = gh_scm2newstr (clazz_scm_name, NULL);
  clazz = (*env)->FindClass (env, clazz_name);
  if (clazz == NULL)
    {
      SCM clazz_err = gh_str02scm (clazz_name);
      free (clazz_name);
      return abort_test (clazz_err, "Unable to find class");
    }

  mid = (*env)->GetMethodID (env, clazz, "<init>", "()V");
  test_obj = (*env)->NewObject (env, clazz, mid);

  if ((*env)->IsInstanceOf (env, test_obj, test_class) == JNI_FALSE)
    {
      SCM clazz_err = gh_str02scm (clazz_name);
      free (clazz_name);
      return abort_test (clazz_err, "Not an instanceof gnu.test.Test");
    }
  free (clazz_name);

  /* Call all the Java testing methods */
  test_name_obj = (*env)->CallObjectMethod (env, test_obj, test_name_mid);
  result_obj = (*env)->CallObjectMethod (env, test_obj, test_mid);

  /* Handle an exception if one occurred */
  if ((*env)->ExceptionOccurred (env))
      return handle_test_exception (test_name_obj);

  result_name_obj = (*env)->CallObjectMethod (env, result_obj, 
					      result_name_mid);
  msg_obj = (*env)->CallObjectMethod (env, result_obj, result_msg_mid);

  /* Grab all the C result messages */
  utf = (*env)->GetStringUTFChars (env, test_name_obj, &is_copy);
  test_name = strdup (utf);
  (*env)->ReleaseStringUTFChars (env, test_name_obj, utf);

  utf = (*env)->GetStringUTFChars (env, result_name_obj, &is_copy);
  result_name = strdup (utf);
  (*env)->ReleaseStringUTFChars (env, result_name_obj, utf);

  utf = (*env)->GetStringUTFChars (env, msg_obj, &is_copy);
  msg = strdup (utf);
  (*env)->ReleaseStringUTFChars (env, msg_obj, utf);

  /* Convert the C result messages to Scheme */
  scm_test_name = gh_str02scm (test_name);
  scm_result_name = gh_symbol2scm (result_name);
  scm_result_msg = gh_str02scm (msg);

  /* Free up the C result messages */
  free (test_name);
  free (result_name);
  free (msg);

  return gh_list (scm_test_name,
		  scm_result_name,
		  scm_result_msg,
		  SCM_UNDEFINED);
}

int
init_testing_framework ()
{
  JavaVM *jvm;
  JDK1_1InitArgs vm_args;

  vm_args.version = 0x00010001;
  JNI_GetDefaultJavaVMInitArgs (&vm_args);
  vm_args.classpath = getenv ("CLASSPATH");
  if (JNI_CreateJavaVM (&jvm, &env, &vm_args) < 0)
    return -1;

  test_class = (*env)->FindClass (env, "gnu/test/Test");
  if (test_class == NULL)
    {
      fprintf (stderr, "Unable to locate gnu.test.Test\n");
      return -1;
    }
  test_class = (*env)->NewGlobalRef (env, test_class);

  result_class = (*env)->FindClass (env, "gnu/test/Result");
  if (result_class == NULL)
    {
      fprintf (stderr, "Unable to locate gnu.test.Result\n");
      return -1;
    }
  result_class = (*env)->NewGlobalRef (env, result_class);

  test_mid = (*env)->GetMethodID (env, test_class, "test", 
				  "()Lgnu/test/Result;");
  test_name_mid = (*env)->GetMethodID (env, test_class, "getName", 
				       "()Ljava/lang/String;");
  if (test_mid == NULL || test_name_mid == NULL)
    {
      fprintf (stderr, "Malformed gnu.test.Test class\n");
      return -1;
    }

  result_name_mid = (*env)->GetMethodID (env, result_class, "getName", 
					 "()Ljava/lang/String;");
  result_msg_mid = (*env)->GetMethodID (env, result_class, "getMsg", 
					"()Ljava/lang/String;");
  if (result_name_mid == NULL || result_msg_mid == NULL)
    {
      fprintf (stderr, "Malformed gnu.test.Result class\n");
      return -1;
    }

  gh_new_procedure1_0 ("test", perform_test);
  return 0;
}

static void
inner_main (void *closure, int argc, char **argv)
{
  if (init_testing_framework () < 0)
    {
      fprintf (stderr, "Unable to instantiate JVM.\n");
      exit (1);
    }
  scm_shell (argc, argv);
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}
