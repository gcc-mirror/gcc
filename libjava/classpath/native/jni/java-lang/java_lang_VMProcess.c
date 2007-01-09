/* java_lang_VMProcess.c -- native code for java.lang.VMProcess
   Copyright (C) 1998, 1999, 2000, 2002, 2004, 2005 Free Software Foundation, Inc.

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

#include <config.h>

#include "java_lang_VMProcess.h"
#include "gnu_java_nio_FileChannelImpl.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>

#include "cpnative.h"
#include "cpproc.h"

/* Internal functions */
static char *copy_string (JNIEnv * env, jobject string);
static char *copy_elem (JNIEnv * env, jobject stringArray, jint i);

/*
 * Internal helper function to copy a String in UTF-8 format.
 */
static char *
copy_string (JNIEnv * env, jobject string)
{
  const char *utf;
  jclass clazz;
  char *copy;

  /* Check for null */
  if (string == NULL)
    {
      clazz = (*env)->FindClass (env, "java/lang/NullPointerException");
      if ((*env)->ExceptionOccurred (env))
	return NULL;
      (*env)->ThrowNew (env, clazz, NULL);
      (*env)->DeleteLocalRef (env, clazz);
      return NULL;
    }

  /* Extract UTF-8 */
  utf = (*env)->GetStringUTFChars (env, string, NULL);
  if ((*env)->ExceptionOccurred (env))
    return NULL;

  /* Copy it */
  if ((copy = strdup (utf)) == NULL)
    {
      clazz = (*env)->FindClass (env, "java/lang/InternalError");
      if ((*env)->ExceptionOccurred (env))
	return NULL;
      (*env)->ThrowNew (env, clazz, "strdup returned NULL");
      (*env)->DeleteLocalRef (env, clazz);
    }

  /* Done */
  (*env)->ReleaseStringUTFChars (env, string, utf);
  return copy;
}

/*
 * Internal helper function to copy a String[] element in UTF-8 format.
 */
static char *
copy_elem (JNIEnv * env, jobject stringArray, jint i)
{
  jobject elem;
  char *rtn;

  elem = (*env)->GetObjectArrayElement (env, stringArray, i);
  if ((*env)->ExceptionOccurred (env))
    return NULL;
  if ((rtn = copy_string (env, elem)) == NULL)
    return NULL;
  (*env)->DeleteLocalRef (env, elem);
  return rtn;
}

/*
 * private final native void nativeSpawn(String[], String[], File)
 *	throws java/io/IOException
 */
JNIEXPORT void JNICALL
Java_java_lang_VMProcess_nativeSpawn (JNIEnv * env, jobject this,
				      jobjectArray cmdArray,
				      jobjectArray envArray, jobject dirFile,
				      jboolean redirect)
{
  int fds[CPIO_EXEC_NUM_PIPES];
  jobject streams[CPIO_EXEC_NUM_PIPES] = { NULL, NULL, NULL };
  jobject dirString = NULL;
  char **newEnviron = NULL;
  jsize cmdArrayLen = 0;
  jsize envArrayLen = 0;
  char **strings = NULL;
  int num_strings = 0;
  char *dir = NULL;
  pid_t pid = -1;
  char errbuf[64];
  jmethodID method, vmmethod;
  jclass clazz, vmclazz;
  int i;
  int pipe_count = redirect ? 2 : 3;
  int err;

  /* Check for null */
  if (cmdArray == NULL)
    goto null_pointer_exception;

  /* Invoke dirFile.getPath() */
  if (dirFile != NULL)
    {
      clazz = (*env)->FindClass (env, "java/io/File");
      if ((*env)->ExceptionOccurred (env))
	return;
      method = (*env)->GetMethodID (env,
				    clazz, "getPath", "()Ljava/lang/String;");
      if ((*env)->ExceptionOccurred (env))
	return;
      dirString = (*env)->CallObjectMethod (env, dirFile, method);
      if ((*env)->ExceptionOccurred (env))
	return;
      (*env)->DeleteLocalRef (env, clazz);
    }

  /*
   * Allocate array of C strings. We put all the C strings we need to
   * handle the command parameters, the new environment, and the new
   * directory into a single array for simplicity of (de)allocation.
   */
  cmdArrayLen = (*env)->GetArrayLength (env, cmdArray);
  if (cmdArrayLen == 0)
    goto null_pointer_exception;
  if (envArray != NULL)
    envArrayLen = (*env)->GetArrayLength (env, envArray);
  if ((strings = malloc (((cmdArrayLen + 1)
			  + (envArray != NULL ? envArrayLen + 1 : 0)
			  + (dirString !=
			     NULL ? 1 : 0)) * sizeof (*strings))) == NULL)
    {
      strncpy (errbuf, "malloc failed", sizeof(errbuf));
      goto out_of_memory;
    }

  /* Extract C strings from the various String parameters */
  for (i = 0; i < cmdArrayLen; i++)
    {
      if ((strings[num_strings++] = copy_elem (env, cmdArray, i)) == NULL)
	goto done;
    }
  strings[num_strings++] = NULL;	/* terminate array with NULL */
  if (envArray != NULL)
    {
      newEnviron = strings + num_strings;
      for (i = 0; i < envArrayLen; i++)
	{
	  if ((strings[num_strings++] = copy_elem (env, envArray, i)) == NULL)
	    goto done;
	}
      strings[num_strings++] = NULL;	/* terminate array with NULL */
    }
  if (dirString != NULL)
    {
      if ((dir = copy_string (env, dirString)) == NULL)
	goto done;
    }

  /* Create inter-process pipes */
  err = cpproc_forkAndExec(strings, newEnviron, fds, pipe_count, &pid, dir);
  if (err != 0)
    {
      strncpy(errbuf, cpnative_getErrorString (err), sizeof(errbuf));
      goto system_error;
    }

  /* Create Input/OutputStream objects around parent file descriptors */
  vmclazz = (*env)->FindClass (env, "gnu/java/nio/VMChannel");
  clazz = (*env)->FindClass (env, "gnu/java/nio/FileChannelImpl");
  if ((*env)->ExceptionOccurred (env))
    goto done;
  vmmethod = (*env)->GetMethodID (env, vmclazz, "<init>", "(I)V");
  method = (*env)->GetMethodID (env, clazz, "<init>", "(Lgnu/java/nio/VMChannel;I)V");
  if ((*env)->ExceptionOccurred (env))
    goto done;
  for (i = 0; i < pipe_count; i++)
    {
      /* Mode is WRITE (2) for in and READ (1) for out and err. */
      const int fd = fds[i];
      const int mode = ((i == CPIO_EXEC_STDIN) ? 2 : 1);
      jclass sclazz;
      jmethodID smethod;

      jobject vmchannel;
      jobject channel;
      vmchannel = (*env)->NewObject (env, vmclazz, vmmethod, fd);
      if ((*env)->ExceptionOccurred (env))
	goto done;
      channel = (*env)->NewObject (env, clazz, method, vmchannel, mode);
      if ((*env)->ExceptionOccurred (env))
	goto done;

      if (mode == gnu_java_nio_FileChannelImpl_WRITE)
	sclazz = (*env)->FindClass (env, "java/io/FileOutputStream");
      else
	sclazz = (*env)->FindClass (env, "java/io/FileInputStream");
      if ((*env)->ExceptionOccurred (env))
	goto done;

      smethod = (*env)->GetMethodID (env, sclazz, "<init>",
				     "(Lgnu/java/nio/FileChannelImpl;)V");
      if ((*env)->ExceptionOccurred (env))
	goto done;

      streams[i] = (*env)->NewObject (env, sclazz, smethod, channel);
      if ((*env)->ExceptionOccurred (env))
	goto done;

      (*env)->DeleteLocalRef (env, sclazz);
    }
  (*env)->DeleteLocalRef (env, clazz);

  /* Invoke VMProcess.setProcessInfo() to update VMProcess object */
  method = (*env)->GetMethodID (env,
				(*env)->GetObjectClass (env, this),
				"setProcessInfo",
				"(Ljava/io/OutputStream;Ljava/io/InputStream;Ljava/io/InputStream;J)V");
  if ((*env)->ExceptionOccurred (env))
    goto done;
  (*env)->CallVoidMethod (env, this, method,
			  streams[CPIO_EXEC_STDIN],
			  streams[CPIO_EXEC_STDOUT],
			  streams[CPIO_EXEC_STDERR],
			  (jlong) pid);
  if ((*env)->ExceptionOccurred (env))
    goto done;

done:
  /*
   * We get here in both the success and failure cases in the
   * parent process. Our goal is to clean up the mess we created.
   */

  /*
   * Close parent's ends of pipes if Input/OutputStreams never got created.
   * This can only happen in a failure case. If a Stream object
   * was created for a file descriptor, we don't close it because it
   * will get closed when the Stream object is finalized.
   */
  for (i = 0; i < pipe_count; i++)
    {
      const int fd = fds[i];

      if (fd != -1 && streams[i] == NULL)
	close (fd);
    }

  /* Free C strings */
  while (num_strings > 0)
    free (strings[--num_strings]);
  free (strings);
  if (dir != NULL)
    free(dir);
  /* Done */
  return;

null_pointer_exception:
  clazz = (*env)->FindClass (env, "java/lang/NullPointerException");
  if ((*env)->ExceptionOccurred (env))
    goto done;
  (*env)->ThrowNew (env, clazz, NULL);
  (*env)->DeleteLocalRef (env, clazz);
  goto done;

out_of_memory:
  clazz = (*env)->FindClass (env, "java/lang/InternalError");
  if ((*env)->ExceptionOccurred (env))
    goto done;
  (*env)->ThrowNew (env, clazz, errbuf);
  (*env)->DeleteLocalRef (env, clazz);
  goto done;

system_error:
  clazz = (*env)->FindClass (env, "java/io/IOException");
  if ((*env)->ExceptionOccurred (env))
    goto done;
  (*env)->ThrowNew (env, clazz, errbuf);
  (*env)->DeleteLocalRef (env, clazz);
  goto done;
}

/*
 * private static final native boolean nativeReap()
 */
JNIEXPORT jboolean JNICALL
Java_java_lang_VMProcess_nativeReap (JNIEnv * env, jclass clazz)
{
  char ebuf[64];
  jfieldID field;
  jint status;
  pid_t pid;
  int err;

  /* Try to reap a child process, but don't block */
  err = cpproc_waitpid((pid_t)-1, &status, &pid, WNOHANG);
  if (err == 0 && pid == 0)
    return JNI_FALSE;

  /* Check result from waitpid() */
  if (err != 0)
    {
      if (err == ECHILD || err == EINTR)
	return JNI_FALSE;
      snprintf(ebuf, sizeof (ebuf), "waitpid(%ld): %s",
	       (long) pid, cpnative_getErrorString(errno));
      clazz = (*env)->FindClass (env, "java/lang/InternalError");
      if ((*env)->ExceptionOccurred (env))
	return JNI_FALSE;
      (*env)->ThrowNew (env, clazz, ebuf);
      (*env)->DeleteLocalRef (env, clazz);
      return JNI_FALSE;
    }

  /* Get exit code; for signal termination return negative signal value XXX */
  if (WIFEXITED (status))
    status = (jint) (jbyte) WEXITSTATUS (status);
  else if (WIFSIGNALED (status))
    status = -(jint) WTERMSIG (status);
  else
    return JNI_FALSE;		/* process merely stopped; ignore */

  /* Return process pid and exit status */
  field = (*env)->GetStaticFieldID (env, clazz, "reapedPid", "J");
  if ((*env)->ExceptionOccurred (env))
    return JNI_FALSE;
  (*env)->SetStaticLongField (env, clazz, field, (jlong) pid);
  if ((*env)->ExceptionOccurred (env))
    return JNI_FALSE;
  field = (*env)->GetStaticFieldID (env, clazz, "reapedExitValue", "I");
  if ((*env)->ExceptionOccurred (env))
    return JNI_FALSE;
  (*env)->SetStaticIntField (env, clazz, field, status);
  if ((*env)->ExceptionOccurred (env))
    return JNI_FALSE;

  /* Done */
  return JNI_TRUE;
}

/*
 * private static final native void nativeKill(long)
 */
JNIEXPORT void JNICALL
Java_java_lang_VMProcess_nativeKill (JNIEnv * env, jclass clazz, jlong pid)
{
  char ebuf[64];
  int err;
  
  err = cpproc_kill((pid_t) pid, SIGKILL);
  if (err != 0)
    {
      snprintf (ebuf, sizeof (ebuf), "kill(%ld): %s",
		(long) pid, cpnative_getErrorString (err));
      clazz = (*env)->FindClass (env, "java/lang/InternalError");
      if ((*env)->ExceptionOccurred (env))
	return;
      (*env)->ThrowNew (env, clazz, ebuf);
      (*env)->DeleteLocalRef (env, clazz);
    }
}
