/* java_io_VMFile.c - Native methods for java.io.File class
   Copyright (C) 1998, 2004 Free Software Foundation, Inc.

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

#include "java_io_VMFile.h"

/*************************************************************************/

/*
 * Method to create an empty file.
 *
 * Class:     java_io_VMFile
 * Method:    create
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_create (JNIEnv * env,
			    jclass clazz __attribute__ ((__unused__)),
			    jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int fd;
  int result;

  filename = JCL_jstring_to_cstring (env, name);
  if (filename == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_OPEN_CREATE (filename, fd, result);
  if (result != TARGET_NATIVE_OK)
    {
      /* XXX ??? NYI */
      if (errno != EEXIST)
	JCL_ThrowException (env,
			    "java/io/IOException",
			    TARGET_NATIVE_LAST_ERROR_STRING ());
      JCL_free_cstring (env, name, filename);
      return (0);
    }
  TARGET_NATIVE_FILE_CLOSE (fd, result);

  JCL_free_cstring (env, name, filename);
  return (1);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method checks to see if we have read permission on a file.
 *
 * Class:     java_io_VMFile
 * Method:    canRead
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_canRead (JNIEnv * env,
			     jobject obj __attribute__ ((__unused__)),
			     jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int fd;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  /* The lazy man's way out.  We actually do open the file for reading
     briefly to verify it can be done */
  TARGET_NATIVE_FILE_OPEN_READ (filename, fd, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);
  if (result != TARGET_NATIVE_OK)
    {
      return (0);
    }
  TARGET_NATIVE_FILE_CLOSE (fd, result);

  return (1);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method checks to see if we have write permission on a file.
 *
 * Class:     java_io_VMFile
 * Method:    canWrite
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_canWrite (JNIEnv * env,
			      jobject obj __attribute__ ((__unused__)),
			      jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int fd;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  /* The lazy man's way out.  We actually do open the file for writing
     briefly to verify it can be done */
  TARGET_NATIVE_FILE_OPEN_READWRITE (filename, fd, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);
  if (result != TARGET_NATIVE_OK)
    {
      return (0);
    }
  TARGET_NATIVE_FILE_CLOSE (fd, result);

  return (1);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method makes a file read only.
 *
 * Class:     java_io_VMFile
 * Method:    setReadOnly
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_setReadOnly (JNIEnv * env,
				 jobject obj __attribute__ ((__unused__)),
				 jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_SET_MODE_READONLY (filename, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method checks to see if a file exists.
 *
 * Class:     java_io_VMFile
 * Method:    exists
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_exists (JNIEnv * env,
			    jobject obj __attribute__ ((__unused__)),
			    jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_EXISTS (filename, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method checks to see if a file is a "plain" file; that is, not
 * a directory, pipe, etc.
 *
 * Class:     java_io_VMFile
 * Method:    isFile
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_isFile (JNIEnv * env,
			    jobject obj __attribute__ ((__unused__)),
			    jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_IS_FILE (filename, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method checks to see if a file is a directory or not.
 *
 * Class:     java_io_VMFile
 * Method:    isDirectory
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_isDirectory (JNIEnv * env,
				 jobject obj __attribute__ ((__unused__)),
				 jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_IS_DIRECTORY (filename, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method returns the length of the file.
 *
 * Class:     java_io_VMFile
 * Method:    length
 * Signature: (Ljava/lang/String;)J
 */

JNIEXPORT jlong JNICALL
Java_java_io_VMFile_length (JNIEnv * env,
			    jobject obj __attribute__ ((__unused__)),
			    jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int tmpfd;
  jlong length;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
    }

  /* open file for reading, get size and close file */
  TARGET_NATIVE_FILE_OPEN_READ (filename, tmpfd, result);
  if (result != TARGET_NATIVE_OK)
    {
      return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
    }
  TARGET_NATIVE_FILE_SIZE (tmpfd, length, result);
  if (result != TARGET_NATIVE_OK)
    {
      TARGET_NATIVE_FILE_CLOSE (tmpfd, result);
      return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
    }
  TARGET_NATIVE_FILE_CLOSE (tmpfd, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result ==
	   TARGET_NATIVE_OK) ? length : TARGET_NATIVE_MATH_INT_INT64_CONST_0);
#else /* not WITHOUT_FILESYSTEM */
  return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method returns the modification date of the file.
 *
 * Class:     java_io_VMFile
 * Method:    lastModified
 * Signature: (Ljava/lang/String;)J
 */

JNIEXPORT jlong JNICALL
Java_java_io_VMFile_lastModified (JNIEnv * env,
				  jobject obj __attribute__ ((__unused__)),
				  jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  jlong mtime;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
    }

  TARGET_NATIVE_FILE_GET_LAST_MODIFIED (filename, mtime, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result ==
	   TARGET_NATIVE_OK) ? mtime : TARGET_NATIVE_MATH_INT_INT64_CONST_0);
#else /* not WITHOUT_FILESYSTEM */
  return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method sets the modification date of the file.
 *
 * Class:     java_io_VMFile
 * Method:    setLastModified
 * Signature: (Ljava/lang/String;J)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_setLastModified (JNIEnv * env,
				     jobject obj __attribute__ ((__unused__)),
				     jstring name, jlong newtime)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_SET_LAST_MODIFIED (filename, newtime, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method deletes a file (actually a name for a file - additional
 * links could exist).
 *
 * Class:     java_io_VMFile
 * Method:    delete
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_delete (JNIEnv * env,
			    jobject obj __attribute__ ((__unused__)),
			    jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_DELETE (filename, result);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method creates a directory.
 *
 * Class:     java_io_VMFile
 * Method:    mkdir
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_mkdir (JNIEnv * env,
			   jobject obj __attribute__ ((__unused__)),
			   jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const char *pathname;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  pathname = (*env)->GetStringUTFChars (env, name, 0);
  if (pathname == NULL)
    {
      return (0);
    }

  TARGET_NATIVE_FILE_MAKE_DIR (pathname, result);
  (*env)->ReleaseStringUTFChars (env, name, pathname);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method renames a (link to a) file.
 *
 * Class:     java_io_VMFile
 * Method:    renameTo
 * Signature: (Ljava/lang/String;Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_renameTo (JNIEnv * env,
			      jobject obj __attribute__ ((__unused__)),
			      jstring t, jstring d)
{
#ifndef WITHOUT_FILESYSTEM
  const char *old_filename, *new_filename;
  int result;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  old_filename = (*env)->GetStringUTFChars (env, t, 0);
  if (old_filename == NULL)
    {
      return (0);
    }

  new_filename = (*env)->GetStringUTFChars (env, d, 0);
  if (new_filename == NULL)
    {
      (*env)->ReleaseStringUTFChars (env, t, old_filename);
      return (0);
    }

  TARGET_NATIVE_FILE_RENAME (old_filename, new_filename, result);
  (*env)->ReleaseStringUTFChars (env, d, new_filename);
  (*env)->ReleaseStringUTFChars (env, t, old_filename);

  return ((result == TARGET_NATIVE_OK) ? 1 : 0);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method returns an array of String representing all the files
 * in a directory except "." and "..".
 *
 * Class:     java_io_VMFile
 * Method:    list
 * Signature: (Ljava/lang/String;)[Ljava/lang/String;
 */

JNIEXPORT jobjectArray JNICALL
Java_java_io_VMFile_list (JNIEnv * env, jobject obj
			  __attribute__ ((__unused__)), jstring name)
{
#ifndef WITHOUT_FILESYSTEM
  const int REALLOC_SIZE = 10;

  const char *dirname;
  int result;
  char **filelist;
  void *handle;
  const char *filename;
  unsigned long int filelist_count, max_filelist_count;
  char **tmp_filelist;
  jclass str_clazz;
  jobjectArray filearray;
  unsigned long int i;
  jstring str;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  dirname = (*env)->GetStringUTFChars (env, name, 0);
  if (dirname == NULL)
    {
      return (0);
    }

  /* open directory for reading */
  TARGET_NATIVE_FILE_OPEN_DIR (dirname, handle, result);

  (*env)->ReleaseStringUTFChars (env, name, dirname);

  if (result != TARGET_NATIVE_OK)
    {
      return (0);
    }

  /* allocate filelist */
  filelist = (char **) JCL_malloc (env, sizeof (char *) * REALLOC_SIZE);
  if (filelist == NULL)
    {
      TARGET_NATIVE_FILE_CLOSE_DIR (handle, result);
      return (0);
    }
  filelist_count = 0;
  max_filelist_count = REALLOC_SIZE;

  /* read the files from the directory */
  TARGET_NATIVE_FILE_READ_DIR (handle, filename, result);
  while (result == TARGET_NATIVE_OK)
    {
      if ((strcmp (filename, ".") != 0) && (strcmp (filename, "..") != 0))
	{
	  /* allocate more memory if necessary */
	  if (filelist_count >= max_filelist_count)
	    {
	      tmp_filelist = (char **) JCL_realloc (env,
						    filelist,
						    (max_filelist_count +
						     REALLOC_SIZE) *
						    sizeof (char *));
	      if (tmp_filelist == NULL)
		{
		  for (i = 0; i < filelist_count; i++)
		    {
		      JCL_free (env, filelist[i]);
		    }
		  JCL_free (env, filelist);
		  TARGET_NATIVE_FILE_CLOSE_DIR (handle, result);
		  return (0);
		}
	      filelist = tmp_filelist;
	      max_filelist_count += REALLOC_SIZE;
	    }

	  /* save entry in list (avoid strdup, because it is not ANSI C, thus difficult to port) */
	  filelist[filelist_count] =
	    (char *) JCL_malloc (env, strlen (filename) + 1);
	  assert (filelist[filelist_count] != NULL);
	  strcpy (filelist[filelist_count], filename);
	  filelist_count++;
	}

      /* read next directory entry */
      TARGET_NATIVE_FILE_READ_DIR (handle, filename, result);
    }

  /* close directory */
  TARGET_NATIVE_FILE_CLOSE_DIR (handle, result);

  /* put the list of files into a Java String array and return it */
  str_clazz = (*env)->FindClass (env, "java/lang/String");
  if (str_clazz == NULL)
    {
      for (i = 0; i < filelist_count; i++)
	{
	  JCL_free (env, filelist[i]);
	}
      JCL_free (env, filelist);
      return (0);
    }
  filearray = (*env)->NewObjectArray (env, filelist_count, str_clazz, 0);
  if (filearray == NULL)
    {
      for (i = 0; i < filelist_count; i++)
	{
	  JCL_free (env, filelist[i]);
	}
      JCL_free (env, filelist);
      return (0);
    }
  for (i = 0; i < filelist_count; i++)
    {
      /* create new string */
      str = (*env)->NewStringUTF (env, filelist[i]);
      if (str == NULL)
	{
	  /* We don't clean up everything here, but if this failed,
	     something serious happened anyway */
	  for (i = 0; i < filelist_count; i++)
	    {
	      JCL_free (env, filelist[i]);
	    }
	  JCL_free (env, filelist);
	  return (0);
	}

      /* save into array */
      (*env)->SetObjectArrayElement (env, filearray, i, str);

      /* delete local reference */
      (*env)->DeleteLocalRef (env, str);
    }

  /* free resources */
  for (i = 0; i < filelist_count; i++)
    {
      JCL_free (env, filelist[i]);
    }
  JCL_free (env, filelist);

  return (filearray);
#else /* not WITHOUT_FILESYSTEM */
  return (0);
#endif /* not WITHOUT_FILESYSTEM */
}
