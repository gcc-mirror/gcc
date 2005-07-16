/* gnu_java_nio_channels_FileChannelImpl.c -
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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

#include <stdlib.h>
#include <errno.h>

#include <jni.h>
#include <jcl.h>

#include "target_native.h"
#ifndef WITHOUT_FILESYSTEM
#include "target_native_file.h"
#endif
#include "target_native_math_int.h"

#include "gnu_java_nio_channels_FileChannelImpl.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */

/* These values must be kept in sync with FileChannelImpl.java.  */
#define FILECHANNELIMPL_READ   1
#define FILECHANNELIMPL_WRITE  2
#define FILECHANNELIMPL_APPEND 4

/* These values must be kept in sync with FileChannelImpl.java.  */
/* #define FILECHANNELIMPL_FILESEEK_SET          0 */
/* #define FILECHANNELIMPL_FILESEEK_CUR          1 */
/* #define FILECHANNELIMPL_FILESEEK_END          2 */

#define FILECHANNELIMPL_FILEOPEN_FLAG_READ    1
#define FILECHANNELIMPL_FILEOPEN_FLAG_WRITE   2
#define FILECHANNELIMPL_FILEOPEN_FLAG_APPEND  4
#define FILECHANNELIMPL_FILEOPEN_FLAG_EXCL    8
#define FILECHANNELIMPL_FILEOPEN_FLAG_SYNC   16
#define FILECHANNELIMPL_FILEOPEN_FLAG_DSYNC  32

#define IO_EXCEPTION "java/io/IOException"

/* FIXME: This can't be right.  Need converter macros. */
#define CONVERT_JLONG_TO_INT(x) TARGET_NATIVE_MATH_INT_INT64_TO_INT32(x)
#define CONVERT_INT_TO_JLONG(x) TARGET_NATIVE_MATH_INT_INT32_TO_INT64(x)

/* FIXME: This can't be right.  Need converter macros. */
#define CONVERT_JLONG_TO_OFF_T(x) TARGET_NATIVE_MATH_INT_INT64_TO_INT32(x)
#define CONVERT_OFF_T_TO_JLONG(x) TARGET_NATIVE_MATH_INT_INT32_TO_INT64(x)

/* FIXME: This can't be right.  Need converter macros */
#define CONVERT_JINT_TO_INT(x) ((int)(x & 0xFFFFFFFF))
#define CONVERT_INT_TO_JINT(x) ((int)(x & 0xFFFFFFFF))

/* FIXME: This can't be right.  Need converter macros. */
#define CONVERT_SSIZE_T_TO_JINT(x) ((jint)(x & 0xFFFFFFFF))
#define CONVERT_JINT_TO_SSIZE_T(x) (x)

/* cached fieldID of gnu.java.nio.channels.FileChannelImpl.fd */
static jfieldID native_fd_fieldID;

static jint
get_native_fd (JNIEnv * env, jobject obj)
{
  return (*env)->GetIntField (env, obj, native_fd_fieldID);
}

/*
 * Library initialization routine.  Called as part of java.io.FileDescriptor
 * static initialization.
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_init (JNIEnv * env, jclass clazz)
{
  jclass clazz_fc;
  jfieldID field;
  jmethodID constructor;
  jobject obj;

  /* Initialize native_fd_fieldID so we only compute it once! */
  clazz_fc = (*env)->FindClass (env, "gnu/java/nio/channels/FileChannelImpl");
  if (!clazz_fc)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error");
      return;
    }

  field = (*env)->GetFieldID (env, clazz_fc, "fd", "I");
  if (!field)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error");
      return;
    }

  native_fd_fieldID = field;

  constructor = (*env)->GetMethodID (env, clazz, "<init>", "(II)V");
  if (!constructor)
    return;

#define INIT_FIELD(FIELDNAME, FDVALUE, MODE)				\
  field = (*env)->GetStaticFieldID (env, clazz, FIELDNAME,		\
				    "Lgnu/java/nio/channels/FileChannelImpl;");	\
  if (! field)								\
    return;								\
  obj = (*env)->NewObject (env, clazz, constructor, FDVALUE, MODE);	\
  if (! obj)								\
    return;								\
  (*env)->SetStaticObjectField (env, clazz, field, obj);		\
  if ((*env)->ExceptionOccurred (env))					\
    return;

  INIT_FIELD ("in", 0, FILECHANNELIMPL_READ);
  INIT_FIELD ("out", 1, FILECHANNELIMPL_WRITE);
  INIT_FIELD ("err", 2, FILECHANNELIMPL_WRITE);

#undef INIT_FIELD
}

/*
 * Open the specified file and return a native file descriptor
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_open (JNIEnv * env,
						 jobject obj
						 __attribute__ ((__unused__)),
						 jstring name, jint mode)
{
  const char *filename;
  int flags;
  int permissions;
  int native_fd;
  int result;

  filename = JCL_jstring_to_cstring (env, name);
  if (filename == NULL)
    return (-1);		/* Exception will already have been thrown */

  /* get file/permission flags for open() */
  if ((mode & FILECHANNELIMPL_FILEOPEN_FLAG_READ)
      && (mode & FILECHANNELIMPL_FILEOPEN_FLAG_WRITE))
    {
      /* read/write */
      flags =
	TARGET_NATIVE_FILE_FILEFLAG_CREATE |
	TARGET_NATIVE_FILE_FILEFLAG_READWRITE;
      permissions = TARGET_NATIVE_FILE_FILEPERMISSION_NORMAL;
    }
  else if ((mode & FILECHANNELIMPL_FILEOPEN_FLAG_READ))
    {
      /* read */
      flags = TARGET_NATIVE_FILE_FILEFLAG_READ;
      permissions = TARGET_NATIVE_FILE_FILEPERMISSION_NORMAL;
    }
  else
    {
      /* write */
      flags =
	TARGET_NATIVE_FILE_FILEFLAG_CREATE |
	TARGET_NATIVE_FILE_FILEFLAG_WRITE;
      if ((mode & FILECHANNELIMPL_FILEOPEN_FLAG_APPEND))
	{
	  flags |= TARGET_NATIVE_FILE_FILEFLAG_APPEND;
	}
      else
	{
	  flags |= TARGET_NATIVE_FILE_FILEFLAG_TRUNCATE;
	}
      permissions = TARGET_NATIVE_FILE_FILEPERMISSION_NORMAL;
    }

  if ((mode & FILECHANNELIMPL_FILEOPEN_FLAG_SYNC))
    {
      flags |= TARGET_NATIVE_FILE_FILEFLAG_SYNC;
    }

  if ((mode & FILECHANNELIMPL_FILEOPEN_FLAG_DSYNC))
    {
      flags |= TARGET_NATIVE_FILE_FILEFLAG_DSYNC;
    }
#ifdef O_BINARY
  flags |= TARGET_NATIVE_FILE_FILEFLAG_BINARY;
#endif

  TARGET_NATIVE_FILE_OPEN (filename, native_fd, flags, permissions, result);
  JCL_free_cstring (env, name, filename);

  if (result != TARGET_NATIVE_OK)
    {
      /* We can only throw FileNotFoundException.  */
      JCL_ThrowException (env,
			  "java/io/FileNotFoundException",
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return TARGET_NATIVE_MATH_INT_INT64_CONST_MINUS_1;
    }

  return native_fd;
}

/*
 * Closes the specified file descriptor and return status code.
 * Exception on error
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_implCloseChannel (JNIEnv * env,
							     jobject obj)
{
  int native_fd;
  int result;

  native_fd = get_native_fd (env, obj);

  TARGET_NATIVE_FILE_CLOSE (native_fd, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
    }
}

/*
 * Return number of bytes that can be read from the file w/o blocking.
 * Exception on error
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_available (JNIEnv * env,
						      jobject obj)
{
  int native_fd;
  jlong bytes_available;
  int result;

  native_fd = get_native_fd (env, obj);

  TARGET_NATIVE_FILE_AVAILABLE (native_fd, bytes_available, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return 0;
    }

  /* FIXME NYI ??? why only jint and not jlong? */
  return TARGET_NATIVE_MATH_INT_INT64_TO_INT32 (bytes_available);
}

JNIEXPORT jlong JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_size (JNIEnv * env, jobject obj)
{
  int native_fd;
  jlong file_size;
  int result;

  native_fd = get_native_fd (env, obj);

  TARGET_NATIVE_FILE_SIZE (native_fd, file_size, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return TARGET_NATIVE_MATH_INT_INT64_CONST_MINUS_1;
    }

  return file_size;
}

/*
 * Return the current position of the file pointer
 * Exception on error
 */
JNIEXPORT jlong JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_implPosition (JNIEnv * env,
							 jobject obj)
{
  int native_fd;
  jlong current_offset;
  int result;

  native_fd = get_native_fd (env, obj);

  TARGET_NATIVE_FILE_TELL (native_fd, current_offset, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return TARGET_NATIVE_MATH_INT_INT64_CONST_MINUS_1;
    }

  return current_offset;
}

/*
 * Wrapper around lseek call.  Return new file position
 * Exception on error
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_seek (JNIEnv * env, jobject obj,
						 jlong offset)
{
  int native_fd;
  jlong new_offset;
  int result;

  native_fd = get_native_fd (env, obj);

#if 0
  /* Should there be such an exception? All native layer macros should
     be accepting 64bit-values if needed. It some target is not able
     to handle such values it should simply operate with 32bit-values
     and convert 64bit-values appriopated. In this case I assume
     problems should not occurre: if some specific target is not able
     to handle 64bit-values the system is limited to 32bit at all, thus
     the application can not do a seek() or something else beyond the
     32bit limit. It this true?
   */

  /* FIXME: What do we do if offset > the max value of off_t on this 32bit
   * system?  How do we detect that and what do we do? */
  if (CONVERT_OFF_T_TO_JLONG (native_offset) != offset)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Cannot represent position correctly on this system");
    }
#endif /* 0 */

  result = TARGET_NATIVE_ERROR;
  new_offset = TARGET_NATIVE_MATH_INT_INT64_CONST_MINUS_1;
  TARGET_NATIVE_FILE_SEEK_BEGIN (native_fd, offset, new_offset, result);

  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
    }
}

/*
 * Set the length of the file
 * Exception on error
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_implTruncate (JNIEnv * env,
							 jobject obj,
							 jlong len)
{
  int native_fd;
  jlong file_size;
  int bytes_written;
  jlong save_offset, new_offset;
  char data;
  int result;

  native_fd = get_native_fd (env, obj);

#if 0
  /* Should there be such an exception? All native layer macros should
     be accepting 64bit-values if needed. It some target is not able
     to handle such values it should simply operate with 32bit-values
     and convert 64bit-values appriopated. In this case I assume
     problems should not occurre: if some specific target is not able
     to handle 64bit-values the system is limited to 32bit at all, thus
     the application can not do a seek() or something else beyond the
     32bit limit. It this true?
   */

  /* FIXME: What do we do if len > the max value of off_t on this 32bit
   * system?  How do we detect that and what do we do? */
  if (CONVERT_OFF_T_TO_JLONG (native_len) != len)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Cannot represent position correctly on this system");
      return;
    }
#endif /* 0 */

  /* get file size */
  TARGET_NATIVE_FILE_SIZE (native_fd, file_size, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }

  /* Save off current position */
  TARGET_NATIVE_FILE_TELL (native_fd, save_offset, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }

  if (TARGET_NATIVE_MATH_INT_INT64_LT (file_size, len))
    {
      /* File is too short -- seek to one byte short of where we want,
       * then write a byte */

      /* move to position n-1 */
      TARGET_NATIVE_FILE_SEEK_BEGIN (native_fd,
				     TARGET_NATIVE_MATH_INT_INT64_SUB (len,
								       1),
				     new_offset, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}

      /* write a byte
         Note: This will fail if we somehow get here in read only mode
         * That shouldn't happen */
      data = '\0';
      TARGET_NATIVE_FILE_WRITE (native_fd, &data, 1, bytes_written, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}

      /* Reposition file pointer to where we started if not beyond new len. */
      if (TARGET_NATIVE_MATH_INT_INT64_LT (save_offset, len))
	{
	  TARGET_NATIVE_FILE_SEEK_BEGIN (native_fd, save_offset,
					 new_offset, result);
	  if (result != TARGET_NATIVE_OK)
	    {
	      JCL_ThrowException (env, IO_EXCEPTION,
				  TARGET_NATIVE_LAST_ERROR_STRING ());
	      return;
	    }
	}
    }
  else if (TARGET_NATIVE_MATH_INT_INT64_GT (file_size, len))
    {
      /* File is too long - use ftruncate if available */
#ifdef HAVE_FTRUNCATE
      TARGET_NATIVE_FILE_TRUNCATE (native_fd, len, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}
#else /* HAVE_FTRUNCATE */
      /* FIXME: Probably operation isn't supported, but this exception
       * is too harsh as it will probably crash the program without need
       JCL_ThrowException(env, "java/lang/UnsupportedOperationException",
       "not implemented - can't shorten files on this platform");
       */
      JCL_ThrowException (env, IO_EXCEPTION, "Unable to shorten file length");
#endif /* HAVE_FTRUNCATE */

      /* Reposition file pointer when it now is beyond the end of file. */
      if (TARGET_NATIVE_MATH_INT_INT64_GT (save_offset, len))
	{
	  TARGET_NATIVE_FILE_SEEK_BEGIN (native_fd, len, new_offset, result);
	  if (result != TARGET_NATIVE_OK)
	    {
	      JCL_ThrowException (env, IO_EXCEPTION,
				  TARGET_NATIVE_LAST_ERROR_STRING ());
	      return;
	    }
	}
    }
}

JNIEXPORT jobject JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_mapImpl (JNIEnv * env,
						    jobject obj
						    __attribute__ ((__unused__)), jchar mode __attribute__ ((__unused__)), jlong position __attribute__ ((__unused__)), jint size __attribute__ ((__unused__)))
{
  JCL_ThrowException (env, IO_EXCEPTION,
		      "java.nio.FileChannelImpl.nio_mmap_file(): not implemented");
  return 0;
}

/*
 * Read a single byte from the file descriptor
 * Return byte read or -1 on eof, exception on error
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_read__ (JNIEnv * env, jobject obj)
{
  int native_fd;
  char data;
  ssize_t bytes_read;
  int result;

  native_fd = get_native_fd (env, obj);

  bytes_read = 0;
  do
    {
      TARGET_NATIVE_FILE_READ (native_fd, &data, 1, bytes_read, result);
      if ((result == TARGET_NATIVE_OK) && (bytes_read == 0))
	{
	  return (-1);
	}
      if ((result != TARGET_NATIVE_OK)
	  && (TARGET_NATIVE_LAST_ERROR () !=
	      TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (-1);
	}
    }
  while (result != TARGET_NATIVE_OK);

  return ((jint) (data & 0xFF));
}

/*
 * Reads to a byte buffer from the specified file descriptor
 * Return number of bytes read or -1 on eof, exception on error
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_read___3BII (JNIEnv * env,
							jobject obj,
							jbyteArray buffer,
							jint offset,
							jint length)
{
  int native_fd;
  jbyte *bufptr;
  ssize_t bytes_read;
  ssize_t n;
  int result;

  native_fd = get_native_fd (env, obj);

  /* Must return 0 if an attempt is made to read 0 bytes. */
  if (length == 0)
    return 0;

  if (offset < 0)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "negative offset");
      return -1;
    }

  bufptr = (*env)->GetByteArrayElements (env, buffer, 0);
  if (!bufptr)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Unexpected JNI error");
      return (-1);
    }

  if (length + offset > (*env)->GetArrayLength (env, buffer))
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "length + offset > buffer.length");
      return -1;
    }

  bytes_read = 0;
  do
    {
      TARGET_NATIVE_FILE_READ (native_fd, (bufptr + offset + bytes_read),
			       (length - bytes_read), n, result);
      if ((result == TARGET_NATIVE_OK) && (n == 0))
	{
	  (*env)->ReleaseByteArrayElements (env, buffer, bufptr, 0);
	  if (bytes_read == 0)
	    return -1;		/* Signal end of file to Java */
	  else
	    return CONVERT_SSIZE_T_TO_JINT (bytes_read);
	}
      if ((result != TARGET_NATIVE_OK)
	  && (TARGET_NATIVE_LAST_ERROR () !=
	      TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  (*env)->ReleaseByteArrayElements (env, buffer, bufptr, 0);
	  return -1;
	}
      if (result == TARGET_NATIVE_OK)
	bytes_read += n;
    }
  while (bytes_read < 1);

  (*env)->ReleaseByteArrayElements (env, buffer, bufptr, 0);
  return CONVERT_SSIZE_T_TO_JINT (bytes_read);
}

/*
 * Writes a single byte to the specified file descriptor
 * Return status code, exception on error
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_write__I (JNIEnv * env,
						     jobject obj, jint b)
{
  int native_fd;
  char native_data;
  ssize_t bytes_written;
  int result;

  native_fd = get_native_fd (env, obj);
  native_data = (char) (CONVERT_JINT_TO_INT (b) & 0xFF);

  do
    {
      TARGET_NATIVE_FILE_WRITE (native_fd, &native_data, 1, bytes_written,
				result);
      if ((result != TARGET_NATIVE_OK)
	  && (TARGET_NATIVE_LAST_ERROR () !=
	      TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}
    }
  while (result != TARGET_NATIVE_OK);
}

/*
 * Copies all parts of a file to disk.
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_force (JNIEnv * env,
						  jobject obj)
{
  int native_fd;
  int result;
  native_fd = get_native_fd (env, obj);
  TARGET_NATIVE_FILE_FSYNC (native_fd, result);
  if (result != TARGET_NATIVE_OK)
    JCL_ThrowException (env, IO_EXCEPTION,
			TARGET_NATIVE_LAST_ERROR_STRING ());
}

/*
 * Writes a byte buffer to the specified file descriptor
 * Return status code, exception on error
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_write___3BII (JNIEnv * env,
							 jobject obj,
							 jbyteArray buffer,
							 jint offset,
							 jint length)
{
  int native_fd;
  jbyte *bufptr;
  ssize_t bytes_written;
  ssize_t n;
  int result;

  native_fd = get_native_fd (env, obj);

  /* Just return if an attempt is made to write 0 bytes. */
  if (length == 0)
    return;

  bufptr = (*env)->GetByteArrayElements (env, buffer, 0);
  if (!bufptr)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Unexpected JNI error");
      return;
    }

  bytes_written = 0;
  while (bytes_written < CONVERT_JINT_TO_SSIZE_T (length))
    {
      TARGET_NATIVE_FILE_WRITE (native_fd, (bufptr + offset + bytes_written),
				(length - bytes_written), n, result);
      if ((result != TARGET_NATIVE_OK)
	  && (TARGET_NATIVE_LAST_ERROR () !=
	      TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  (*env)->ReleaseByteArrayElements (env, buffer, bufptr, 0);
	  return;
	}
      if (result == TARGET_NATIVE_OK)
	bytes_written += n;
    }

  (*env)->ReleaseByteArrayElements (env, buffer, bufptr, 0);
}

JNIEXPORT jboolean JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_lock (JNIEnv *env, jobject obj,
                                                 jlong position, jlong size,
                                                 jboolean shared, jboolean wait)
{
#ifdef HAVE_FCNTL
  int fd = get_native_fd (env, obj);
  int cmd = wait ? F_SETLKW : F_SETLK;
  struct flock flock;
  int ret;

  flock.l_type = shared ? F_RDLCK : F_WRLCK;
  flock.l_whence = SEEK_SET;
  flock.l_start = (off_t) position;
  flock.l_len = (off_t) size;

  ret = fcntl (fd, cmd, &flock);
  if (ret)
    {
      /* Linux man pages for fcntl state that errno might be either
         EACCES or EAGAIN if we try F_SETLK, and another process has
         an overlapping lock. */
      if (errno != EACCES && errno != EAGAIN)
        {
          JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
        }
      return JNI_FALSE;
    }
  return JNI_TRUE;
#else
  (void) obj;
  (void) position;
  (void) size;
  (void) shared;
  (void) wait;
  JCL_ThrowException (env, "java/lang/UnsupportedOperationException",
                      "file locks not implemented on this platform");
  return JNI_FALSE;
#endif /* HAVE_FCNTL */
}

JNIEXPORT void JNICALL
Java_gnu_java_nio_channels_FileChannelImpl_unlock (JNIEnv *env,
                                                   jobject obj,
                                                   jlong position,
                                                   jlong length)
{
#ifdef HAVE_FCNTL
  int fd = get_native_fd (env, obj);
  struct flock flock;
  int ret;

  flock.l_type = F_UNLCK;
  flock.l_whence = SEEK_SET;
  flock.l_start = (off_t) position;
  flock.l_len = (off_t) length;

  ret = fcntl (fd, F_SETLK, &flock);
  if (ret)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
#else
  (void) obj;
  (void) position;
  (void) length;
  JCL_ThrowException (env, "java/lang/UnsupportedOperationException",
                      "file locks not implemented on this platform");
#endif /* HAVE_FCNTL */
}
