/* java_io_VMFile.c - Native methods for java.io.File class
   Copyright (C) 1998, 2004, 2006 Free Software Foundation, Inc.

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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#if defined (HAVE_LSTAT) && defined (HAVE_READLINK)
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#include <jni.h>
#include <jcl.h>
#include "cpio.h"
#include "cpnative.h"

#include "java_io_VMFile.h"

/* ***** PRIVATE FUNCTIONS DELCARATION ***** */

/**
 * Enables of disables the passed permission bit of a file.
 */
static jboolean set_file_permissions (JNIEnv *env, jstring name,
                                      jboolean enable,
                                      jboolean ownerOnly,
                                      int permissions);

/* ***** END: PRIVATE FUNCTIONS DELCARATION ***** */

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
      return 0;
    }

  result = cpio_openFile (filename, &fd, CPFILE_FLAG_CREATE|CPFILE_FLAG_WRITE, CPFILE_PERMISSION_NORMAL);
  if (result != CPNATIVE_OK)
    {
      if (result != EEXIST)
	JCL_ThrowException (env,
			    "java/io/IOException",
			    cpnative_getErrorString (result));
      JCL_free_cstring (env, name, filename);
      return 0;
    }
  cpio_closeFile (fd);

  JCL_free_cstring (env, name, filename);
  return 1;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
      return 0;
    }

  /* The lazy man's way out.  We actually do open the file for reading
     briefly to verify it can be done */
  result = cpio_openFile (filename, &fd, CPFILE_FLAG_READ, 0);
  (*env)->ReleaseStringUTFChars (env, name, filename);
  if (result != CPNATIVE_OK)
    return 0;
  cpio_closeFile (fd);

  return 1;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
      return 0;
    }

  /* The lazy man's way out.  We actually do open the file for writing
     briefly to verify it can be done */
  result = cpio_openFile (filename, &fd, CPFILE_FLAG_READWRITE, 0);
  (*env)->ReleaseStringUTFChars (env, name, filename);
  if (result != CPNATIVE_OK)
    {
      return 0;
    }
  cpio_closeFile (fd);

  return 1;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method checks to see if we have execute permission on a file.
 *
 * Class:     java_io_VMFile
 * Method:    canExecute
 * Signature: (Ljava/lang/String;)Z
 */

JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_canExecute (JNIEnv * env,
                                jclass clazz __attribute__ ((__unused__)),
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
      return JNI_FALSE;
    }

  result = cpio_checkAccess (filename, CPFILE_FLAG_EXEC);
  
  (*env)->ReleaseStringUTFChars (env, name, filename);
  if (result != CPNATIVE_OK)
    return JNI_FALSE;
  
  return JNI_TRUE;
#else /* not WITHOUT_FILESYSTEM */
  return JNI_FALSE;
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
      return 0;
    }

  result = cpio_setFileReadonly (filename);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * This method changes the read permission bit of a file.
 *
 * Class:     java_io_VMFile
 * Method:    setReadable
 * Signature: (Ljava/lang/String;ZZ)Z
 */
JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_setReadable (JNIEnv *env,
                                 jclass clazz __attribute__ ((__unused__)),
                                 jstring name,
                                 jboolean readable,
                                 jboolean ownerOnly)
{
  return set_file_permissions (env, name, readable, ownerOnly,
                               CPFILE_FLAG_READ);
}


/*************************************************************************/

/*
 * This method changes the write permission bit of a file.
 *
 * Class:     java_io_VMFile
 * Method:    setWritable
 * Signature: (Ljava/lang/String;ZZ)Z
 */
JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_setWritable (JNIEnv *env,
                                 jclass clazz __attribute__ ((__unused__)),
                                 jstring name,
                                 jboolean writable,
                                 jboolean ownerOnly)
{
  return set_file_permissions (env, name, writable, ownerOnly,
                               CPFILE_FLAG_WRITE);
}

/*************************************************************************/

/*
 * This method changes the execute permission bit of a file.
 *
 * Class:     java_io_VMFile
 * Method:    setExecutable
 * Signature: (Ljava/lang/String;ZZ)Z
 */
JNIEXPORT jboolean JNICALL
Java_java_io_VMFile_setExecutable (JNIEnv *env,
                                 jclass clazz __attribute__ ((__unused__)),
                                 jstring name,
                                 jboolean executable,
                                 jboolean ownerOnly)
{
  return set_file_permissions (env, name, executable, ownerOnly,
                               CPFILE_FLAG_EXEC);
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
      return 0;
    }

  result = cpio_isFileExists (filename);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
  jint entryType;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return 0;
    }

  result = cpio_checkType (filename, &entryType);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK && entryType == CPFILE_FILE ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
  jint entryType;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return 0;
    }
  
  result = cpio_checkType (filename, &entryType);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK && entryType == CPFILE_DIRECTORY ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
    return 0;

  /* open file for reading, get size and close file */
  result = cpio_openFile (filename, &tmpfd, CPFILE_FLAG_READ, 0);
  if (result != CPNATIVE_OK)
    return 0;

  result = cpio_getFileSize (tmpfd, &length);
  if (result != CPNATIVE_OK)
    {
      cpio_closeFile (tmpfd);
      return 0;
    }

  result = cpio_closeFile (tmpfd);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK ? length : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
      return 0;
    }

  result = cpio_getModificationTime (filename, &mtime);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK ? mtime : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
      return 0;
    }

  result = cpio_setModificationTime (filename, newtime);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
      return 0;
    }

  result = cpio_removeFile (filename);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return result == CPNATIVE_OK ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
      return 0;
    }

  result = cpio_mkdir (pathname);
  (*env)->ReleaseStringUTFChars (env, name, pathname);

  return (result == CPNATIVE_OK) ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
      return 0;
    }

  new_filename = (*env)->GetStringUTFChars (env, d, 0);
  if (new_filename == NULL)
    {
      (*env)->ReleaseStringUTFChars (env, t, old_filename);
      return 0;
    }

  result = cpio_rename (old_filename, new_filename);
  (*env)->ReleaseStringUTFChars (env, d, new_filename);
  (*env)->ReleaseStringUTFChars (env, t, old_filename);

  return (result == CPNATIVE_OK) ? 1 : 0;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
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
  char *filename = (char *) JCL_malloc (env, FILENAME_MAX);
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
      return 0;
    }

  /* open directory for reading */
  result = cpio_openDir (dirname, &handle);

  (*env)->ReleaseStringUTFChars (env, name, dirname);

  if (result != CPNATIVE_OK)
    {
      return 0;
    }

  /* allocate filelist */
  filelist = (char **) JCL_malloc (env, sizeof (char *) * REALLOC_SIZE);
  if (filelist == NULL)
    {
      result = cpio_closeDir (handle);
      return 0;
    }
  filelist_count = 0;
  max_filelist_count = REALLOC_SIZE;

  /* read the files from the directory */
  result = cpio_readDir (handle, filename);
  while (result == CPNATIVE_OK)
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
		  result = cpio_closeDir (handle);
		  return 0;
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
      result = cpio_readDir (handle, filename);
    }

  JCL_free (env, filename);

  /* close directory */
  result = cpio_closeDir (handle);

  /* put the list of files into a Java String array and return it */
  str_clazz = (*env)->FindClass (env, "java/lang/String");
  if (str_clazz == NULL)
    {
      for (i = 0; i < filelist_count; i++)
	{
	  JCL_free (env, filelist[i]);
	}
      JCL_free (env, filelist);
      return 0;
    }
  filearray = (*env)->NewObjectArray (env, filelist_count, str_clazz, 0);
  if (filearray == NULL)
    {
      for (i = 0; i < filelist_count; i++)
	{
	  JCL_free (env, filelist[i]);
	}
      JCL_free (env, filelist);
      return 0;
    }

  (*env)->DeleteLocalRef (env, str_clazz);

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
	  return 0;
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

  return filearray;
#else /* not WITHOUT_FILESYSTEM */
  return 0;
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/*
 * These two methods are used to maintain dynamically allocated
 * buffers for getCanonicalPath without the overhead of calling
 * realloc every time a buffer is modified.  Buffers are sized
 * at the smallest multiple of CHUNKSIZ that is greater than or
 * equal to the desired length.  The default CHUNKSIZ is 256,
 * longer than most paths, so in most cases a getCanonicalPath
 * will require only one malloc per buffer.
 */

#define CHUNKLOG 8
#define CHUNKSIZ (1 << CHUNKLOG)

static int
nextChunkSize (int size)
{
  return ((size >> CHUNKLOG) + ((size & (CHUNKSIZ - 1)) ? 1 : 0)) << CHUNKLOG;
}

static char *
maybeGrowBuf (JNIEnv *env, char *buf, int *size, int required)
{
  if (required > *size)
    {
      *size = nextChunkSize (required);
      buf = JCL_realloc (env, buf, *size);
    }
  return buf;
}

/*************************************************************************/

/*
 * This method converts a path to canonical form on GNU/Posix systems.
 * This involves the removal of redundant separators, references to
 * "." and "..", and symbolic links.
 *
 * The conversion proceeds on a component-by-component basis: symbolic
 * links and references to ".."  are resolved as and when they occur.
 * This means that if "/foo/bar" is a symbolic link to "/baz" then the
 * canonical form of "/foo/bar/.." is "/" and not "/foo".
 *
 * In order to mimic the behaviour of proprietary JVMs, non-existant
 * path components are allowed (a departure from the normal GNU system
 * convention).  This means that if "/foo/bar" is a symbolic link to
 * "/baz", the canonical form of "/non-existant-directory/../foo/bar"
 * is "/baz".
 *
 * Class:     java_io_VMFile
 * Method:    toCanonicalForm
 * Signature: (Ljava/lang/String)Ljava/lang/String
 */

JNIEXPORT jstring JNICALL
Java_java_io_VMFile_toCanonicalForm (JNIEnv *env,
				     jclass class __attribute__ ((__unused__)),
				     jstring jpath)
{
#ifndef WITHOUT_FILESYSTEM
  const char *path;
  char *src, *dst;
  int srci, dsti;
  int srcl, dstl;
  int len;
  int fschecks;
#if defined (HAVE_LSTAT) && defined (HAVE_READLINK)
  struct stat sb;
#endif /* HAVE_LSTAT && HAVE_READLINK */

  path = JCL_jstring_to_cstring (env, jpath);
  if (path == NULL)
    return NULL;

  /* It is the caller's responsibility to ensure the path is absolute. */
  if (path[0] == 0 || path[0] != '/')
    {
      JCL_free_cstring (env, jpath, path);
      JCL_ThrowException (env, "java/lang/RuntimeException", "Not absolute");
      return NULL;
    }

  len = strlen (path);
  srcl = nextChunkSize (len + 1);
  src = JCL_malloc (env, srcl);
  if (src == NULL)
    {
      JCL_free_cstring (env, jpath, path);
      return NULL;
    }
  strcpy (src, path);
  JCL_free_cstring (env, jpath, path);
  srci = 1;

  dstl = nextChunkSize (2);  
  dst = JCL_malloc (env, dstl);
  if (dst == NULL)
    {
      JCL_free (env, src);
      return NULL;
    }
  dst[0] = '/';
  dsti = 1;

  fschecks = JNI_TRUE;

  while (src[srci] != '\0')
    {
      int tmpi, dsti_save;

      /* Skip slashes. */
      while (src[srci] == '/')
	srci++;
      tmpi = srci;
      /* Find next slash. */
      while (src[srci] != '/' && src[srci] != '\0')
	srci++;
      if (srci == tmpi)
	/* We hit the end. */
	break;
      len = srci - tmpi;

      /* Handle "." and "..". */
      if (len == 1 && src[tmpi] == '.')
	continue;
      if (len == 2 && src[tmpi] == '.' && src[tmpi + 1] == '.')
	{
	  while (dsti > 1 && dst[dsti - 1] != '/')
	    dsti--;
	  if (dsti != 1)
	    dsti--;
	  /* Reenable filesystem checking if disabled, as we might
	   * have reversed over whatever caused the problem before.
	   * At least one proprietary JVM has inconsistencies because
	   * it does not do this.
	   */
	  fschecks = JNI_TRUE;
	  continue;
	}

      /* Handle real path components. */
      dst = maybeGrowBuf (env,
			  dst, &dstl, dsti + (dsti > 1 ? 1 : 0) + len + 1);
      if (dst == NULL)
	{
	  JCL_free (env, src);
	  return NULL;
	}
      dsti_save = dsti;
      if (dsti > 1)
	dst[dsti++] = '/';
      strncpy (&dst[dsti], &src[tmpi], len);
      dsti += len;
      if (fschecks == JNI_FALSE)
	continue;

#if defined (HAVE_LSTAT) && defined (HAVE_READLINK)
      dst[dsti] = '\0';
      if (lstat (dst, &sb) == 0)
	{
	  if (S_ISLNK (sb.st_mode))
	    {
	      int tmpl = CHUNKSIZ;
	      char *tmp = JCL_malloc (env, tmpl);
	      if (tmp == NULL)
		{
		  JCL_free (env, src);
		  JCL_free (env, dst);
		  return NULL;
		}

	      while (1)
		{
		  tmpi = readlink (dst, tmp, tmpl);
		  if (tmpi < 1)
		    {
		      JCL_free (env, src);
		      JCL_free (env, dst);
		      JCL_free (env, tmp);
		      JCL_ThrowException (env, "java/io/IOException",
					  "readlink failed");
		      return NULL;
		    }
		  if (tmpi < tmpl)
		    break;
		  tmpl += CHUNKSIZ;
		  tmp = JCL_realloc (env, tmp, tmpl);
		}

	      /* Prepend the link's path to src. */
	      tmp = maybeGrowBuf (env,
				  tmp, &tmpl, tmpi + strlen (&src[srci]) + 1);
	      if (tmp == NULL)
		{
		  JCL_free (env, src);
		  JCL_free (env, dst);
		  return NULL;
		}

	      strcpy (&tmp[tmpi], &src[srci]);
	      JCL_free (env, src);
	      src = tmp;
	      srcl = tmpl;
	      srci = 0;

	      /* Either replace or append dst depending on whether the
	       * link is relative or absolute.
	       */
	      dsti = src[0] == '/' ? 1 : dsti_save;
	    }
	}
      else
	{
	  /* Something doesn't exist, or we don't have permission to
	   * read it, or a previous path component is a directory, or
	   * a symlink is looped.  Whatever, we can't check the
	   * filesystem any more.
	   */
	  fschecks = JNI_FALSE;
	}
#endif /* HAVE_LSTAT && HAVE_READLINK */
    }
  dst[dsti] = '\0';

  jpath = (*env)->NewStringUTF (env, dst);
  JCL_free (env, src);
  JCL_free (env, dst);
  return jpath;
#else /* not WITHOUT_FILESYSTEM */
  return NULL;
#endif /* not WITHOUT_FILESYSTEM */
}

/*************************************************************************/

/* ***** PRIVATE FUNCTIONS IMPLEMENTATION ***** */

static jboolean set_file_permissions (JNIEnv *env, jstring name,
                                      jboolean enable,
                                      jboolean ownerOnly,
                                      int permissions)
{
#ifndef WITHOUT_FILESYSTEM
  const char *filename;
  int result = JNI_FALSE;
  
  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);
  if (filename == NULL)
    {
      return JNI_FALSE;
    }
  
  if (ownerOnly)
    {
      permissions |= CPFILE_FLAG_USR; 
    }
  
  if (!enable)
    {
      permissions |= CPFILE_FLAG_OFF;
    }
  
  result = cpio_chmod (filename, permissions);
  (*env)->ReleaseStringUTFChars (env, name, filename);
  
  return result == CPNATIVE_OK ? JNI_TRUE : JNI_FALSE;
  
#else /* not WITHOUT_FILESYSTEM */
  return JNI_FALSE;
#endif /* not WITHOUT_FILESYSTEM */
}
