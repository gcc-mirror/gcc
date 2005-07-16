/* javaio.c - Common java.io native functions
   Copyright (C) 1998, 2002, 2004 Free Software Foundation, Inc.

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

/* do not move; needed here because of some macro definitions */
#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#include <jni.h>
#include <jcl.h>

#include "target_native.h"
#ifndef WITHOUT_FILESYSTEM
#include "target_native_file.h"
#endif
#include "target_native_math_int.h"

#include "javaio.h"

/*
 * Function to open a file
 */

jint
_javaio_open_read (JNIEnv * env, jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int fd;
  int result;

  filename = JCL_jstring_to_cstring (env, name);
  if (filename == NULL)
    return (-1);

  TARGET_NATIVE_FILE_OPEN_READ (filename, fd, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);
  if (result != TARGET_NATIVE_OK)
    {
      if (TARGET_NATIVE_LAST_ERROR () == TARGET_NATIVE_ERROR_NO_SUCH_FILE)
	JCL_ThrowException (env,
			    "java/io/FileNotFoundException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());
      else
	JCL_ThrowException (env,
			    "java/io/IOException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());
    }

  JCL_free_cstring (env, name, filename);
  return (fd);
#else /* not WITHOUT_FILESYSTEM */
  return (-1);
#endif /* not WITHOUT_FILESYSTEM */
}

/*
 * Function to open a file for reading/writing
 */

jint
_javaio_open_readwrite (JNIEnv * env, jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int fd;
  int result;

  filename = JCL_jstring_to_cstring (env, name);
  if (filename == NULL)
    return (-1);

  TARGET_NATIVE_FILE_OPEN_READWRITE (filename, fd, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);
  if (result != TARGET_NATIVE_OK)
    {
      if (TARGET_NATIVE_LAST_ERROR () == TARGET_NATIVE_ERROR_NO_SUCH_FILE)
	JCL_ThrowException (env,
			    "java/io/FileNotFoundException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());
      else
	JCL_ThrowException (env,
			    "java/io/IOException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());
    }

  JCL_free_cstring (env, name, filename);
  return (fd);
#else /* not WITHOUT_FILESYSTEM */
  return (-1);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * Function to close a file
 */

void
_javaio_close (JNIEnv * env, jint fd)
{
#ifndef WITHOUT_FILESYSTEM
  int result;

  if (fd != -1)
    {
      TARGET_NATIVE_FILE_CLOSE (fd, result);
      if (result != TARGET_NATIVE_OK)
	JCL_ThrowException (env,
			    "java/io/IOException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());
    }
#else /* not WITHOUT_FILESYSTEM */
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * Skips bytes in a file
 */

jlong
_javaio_skip_bytes (JNIEnv * env, jint fd, jlong num_bytes)
{
#ifndef WITHOUT_FILESYSTEM
  jlong current_offset, new_offset;
  int result;

  TARGET_NATIVE_FILE_SEEK_CURRENT (fd, TARGET_NATIVE_MATH_INT_INT64_CONST_0,
				   current_offset, result);
  if (result != TARGET_NATIVE_OK)
    JCL_ThrowException (env,
			"java/io/IOException",
			TARGET_NATIVE_LAST_ERROR_STRING ());

  TARGET_NATIVE_FILE_SEEK_CURRENT (fd, num_bytes, new_offset, result);
  if (result != TARGET_NATIVE_OK)
    JCL_ThrowException (env,
			"java/io/IOException",
			TARGET_NATIVE_LAST_ERROR_STRING ());

  return (TARGET_NATIVE_MATH_INT_INT64_SUB (new_offset, current_offset));
#else /* not WITHOUT_FILESYSTEM */
  return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * Gets the size of the file
 */

jlong
_javaio_get_file_length (JNIEnv * env, jint fd)
{
#ifndef WITHOUT_FILESYSTEM
  jlong length;
  int result;

  TARGET_NATIVE_FILE_SIZE (fd, length, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env,
			  "java/io/IOException",
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return (TARGET_NATIVE_MATH_INT_INT64_CONST_MINUS_1);
    }

  return (length);
#else /* not WITHOUT_FILESYSTEM */
  return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * Reads data from a file
 */

jint
_javaio_read (JNIEnv * env, jint fd, jarray buf, jint offset, jint len)
{
#ifndef WITHOUT_FILESYSTEM
  jbyte *bufptr;
  int bytesRead;
  int result;

  assert (offset >= 0);
  assert (len >= 0);

  if (len == 0)
    return 0;			/* Nothing todo, and GetByteArrayElements() seems undefined. */

  bufptr = (*env)->GetByteArrayElements (env, buf, JNI_FALSE);
  if (bufptr == NULL)
    {
      JCL_ThrowException (env, "java/io/IOException",
			  "Internal Error: get byte array fail");
      return (-1);
    }

  TARGET_NATIVE_FILE_READ (fd, (bufptr + offset), len, bytesRead, result);
  (*env)->ReleaseByteArrayElements (env, buf, bufptr, 0);
  if (result != TARGET_NATIVE_OK)
    JCL_ThrowException (env,
			"java/io/IOException",
			TARGET_NATIVE_LAST_ERROR_STRING ());

  if (bytesRead == 0)
    return (-1);

  return (bytesRead);
#else /* not WITHOUT_FILESYSTEM */
  jbyte *bufptr;
  int bytesRead;

  assert (offset >= 0);
  assert (len >= 0);

  if ((fd == 0) || (fd == 1) || (fd == 2))
    {
      if (len == 0)
	return 0;		/* Nothing todo, and GetByteArrayElements() seems undefined. */

      bufptr = (*env)->GetByteArrayElements (env, buf, JNI_FALSE);
      if (bufptr == NULL)
	{
	  JCL_ThrowException (env, "java/io/IOException",
			      "Internal Error: get byte array");
	  return (-1);
	}

      TARGET_NATIVE_FILE_READ (fd, (bufptr + offset), len, bytesRead, result);
      (*env)->ReleaseByteArrayElements (env, buf, bufptr, 0);
      if (result != TARGET_NATIVE_OK)
	JCL_ThrowException (env,
			    "java/io/IOException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());

      if (bytesRead == 0)
	return (-1);

      return (bytesRead);
    }
  else
    {
      return (-1);
    }
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * Writes data to a file
 */

jint
_javaio_write (JNIEnv * env, jint fd, jarray buf, jint offset, jint len)
{
#ifndef WITHOUT_FILESYSTEM
  jbyte *bufptr;
  int bytes_written;
  int result;

  if (len == 0)
    return 0;			/* Nothing todo, and GetByteArrayElements() seems undefined. */

  bufptr = (*env)->GetByteArrayElements (env, buf, 0);
  if (bufptr == NULL)
    {
      JCL_ThrowException (env, "java/io/IOException",
			  "Internal Error: get byte array");
      return (-1);
    }

  TARGET_NATIVE_FILE_WRITE (fd, (bufptr + offset), len, bytes_written,
			    result);
  (*env)->ReleaseByteArrayElements (env, buf, bufptr, 0);
  if (result != TARGET_NATIVE_OK)
    JCL_ThrowException (env,
			"java/io/IOException",
			TARGET_NATIVE_LAST_ERROR_STRING ());

  if (bytes_written == 0)
    return (-1);

  return (bytes_written);
#else /* not WITHOUT_FILESYSTEM */
  jbyte *bufptr;
  int bytesWritten;

  if ((fd == 0) || (fd == 1) || (fd == 2))
    {
      if (len == 0)
	return 0;		/* Nothing todo, and GetByteArrayElements() seems undefined. */

      bufptr = (*env)->GetByteArrayElements (env, buf, 0);
      if (bufptr == NULL)
	{
	  JCL_ThrowException (env, "java/io/IOException", "Internal Error");
	  return (-1);
	}

      TARGET_NATIVE_FILE_WRITE (fd, (bufptr + offset), len, bytes_written,
				result);
      (*env)->ReleaseByteArrayElements (env, buf, bufptr, 0);

      if (bytes_written == -1)
	JCL_ThrowException (env,
			    "java/io/IOException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());

      if (bytes_written == 0)
	return (-1);

      return (bytes_written);
    }
  else
    {
      return (-1);
    }
#endif /* not WITHOUT_FILESYSTEM */
}
