/* java_nio_MappedByteBufferImpl.c - Native methods for MappedByteBufferImpl
   Copyright (C) 2004,2005  Free Software Foundation, Inc.

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
#include <errno.h>

#include <jni.h>
#include <jcl.h>

#include "java_nio_MappedByteBufferImpl.h"

#include <errno.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */

#define IO_EXCEPTION "java/io/IOException"

/* FIXME these are defined in gnu_java_nio_channels_FileChannelImpl
   too; should be someplace common. */
#define ALIGN_DOWN(p,s) ((jpointer)(p) - ((jpointer)(p) % (s)))
#define ALIGN_UP(p,s) ((jpointer)(p) + ((s) - ((jpointer)(p) % (s))))

/**
 * Returns the memory page size of this platform.
 *
 * \return The page size.
 */
static long
get_pagesize (void)
{
  /* FIXME can we just try HAVE_SYSCONF? */
#if defined(HAVE_GETPAGESIZE)
  return getpagesize ();
#elif defined (HAVE_SYSCONF)
  return sysconf (_SC_PAGESIZE);
#endif /* HAVE_GETPAGESIZE / HAVE_SYSCONF */
}

/**
 * Retrieve the 'address' and 'cap' (the mapped size) fields of this
 * buffer.
 *
 * This function will align the address down to the nearest page
 * boundary, and the size up to the nearest page boundary. Thus, it is
 * safe to use these values in 'mman' functions.
 *
 * \param env The JNI environment pointer.
 * \param this The MappedByteBufferImpl instance.
 * \param address A pointer to where the actual pointer should be
 * stored.
 * \param size A pointer to where the mapped region's size should be
 * stored
 */
static void
get_raw_values (JNIEnv *env, jobject this, void **address, size_t *size)
{
  const long pagesize = get_pagesize ();
  jfieldID MappedByteBufferImpl_address;
  jfieldID MappedByteBufferImpl_size;
  jobject MappedByteBufferImpl_address_value = NULL;

  *address = NULL;
  /* 'address' is declared in java.nio.Buffer */
  MappedByteBufferImpl_address
    = (*env)->GetFieldID (env, (*env)->GetObjectClass (env, this),
			  "address", "Lgnu/classpath/Pointer;");
  /* 'cap' -- likewise, the capacity */
  MappedByteBufferImpl_size
    = (*env)->GetFieldID (env, (*env)->GetObjectClass (env, this),
			  "cap", "I");
  if (MappedByteBufferImpl_address != NULL)
    {
      MappedByteBufferImpl_address_value =
	(*env)->GetObjectField (env, this, MappedByteBufferImpl_address);
    }
  if ((*env)->ExceptionOccurred (env))
    return;
  if (MappedByteBufferImpl_address_value == NULL)
    {
      JCL_ThrowException (env, "java/lang/NullPointerException",
                          "mapped address is NULL");
      return;
    }

  *address = (void *)
    ALIGN_DOWN (JCL_GetRawData (env, MappedByteBufferImpl_address_value), pagesize);
  *size = (size_t)
    ALIGN_UP ((*env)->GetIntField (env, this, MappedByteBufferImpl_size),
	      pagesize);
}

JNIEXPORT void JNICALL
Java_java_nio_MappedByteBufferImpl_unmapImpl (JNIEnv *env, jobject this)
{
#ifdef HAVE_MUNMAP
  void *address;
  size_t size;

  get_raw_values (env, this, &address, &size);

  if (address == NULL)
    return;

  if (munmap (address, size) != 0)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return;
    }
#else
  JCL_ThrowException (env, IO_EXCEPTION,
		      "unmapping files not implemented");
#endif /* HAVE_MUNMAP */
}

JNIEXPORT jboolean JNICALL
Java_java_nio_MappedByteBufferImpl_isLoadedImpl (JNIEnv * env, jobject this)
{
#ifdef HAVE_MINCORE
  void *address;
  size_t size;
  char *vec;
  size_t count, i;
  const long pagesize = get_pagesize ();

  /*
   * FIXME on Darwin this does not work if the mapped region is
   * exactly one page long; i.e., 'mincore' tells us it isn't loaded.
   */
  get_raw_values (env, this, &address, &size);
  if (address == NULL)
    return JNI_FALSE;
  count = (size_t) ((size + pagesize - 1) / pagesize);
  vec = (char *) malloc (count * sizeof (unsigned char));

  /*
   * Darwin (and BSD?) define argument 3 of 'mincore' to be 'char *',
   * while GNU libc defines it to be 'unsigned char *'. Casting the
   * argument to 'void *' fixes this, but not with C++. So you might
   * be SOL if you compile this with g++ (!) on GNU with -Werror.
   */
#ifdef __cplusplus
  if (mincore (address, size, vec) != 0)
#else
  if (mincore (address, size, (void *) vec) != 0)
#endif /* __cplusplus */
    {
      free (vec);
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return JNI_FALSE;
    }

  for (i = 0; i < count; i++)
    {
      if ((vec[i] & 1) == 0)
        return JNI_FALSE;
    }
  return JNI_TRUE;
#else
  return JNI_FALSE;
#endif
}

JNIEXPORT void JNICALL
Java_java_nio_MappedByteBufferImpl_loadImpl (JNIEnv *env, jobject this)
{
#ifdef HAVE_MADVISE
  void *address;
  size_t size;

  get_raw_values (env, this, &address, &size);
  if (address == NULL)
    return;

  madvise (address, size, MADV_WILLNEED);
#else
  JCL_ThrowException (env, IO_EXCEPTION,
                      "forcing mapped files into core not implemented");
#endif /* HAVE_MADVISE */
}

JNIEXPORT void JNICALL
Java_java_nio_MappedByteBufferImpl_forceImpl (JNIEnv *env, jobject this)
{
#ifdef HAVE_MSYNC
  void *address;
  size_t size;

  get_raw_values (env, this, &address, &size);

  if (address == NULL)
    return;

  /* FIXME: is using MS_SYNC ok? Should we use MS_INVALIDATE? */
  if (msync (address, size, MS_SYNC) != 0)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
#else
  JCL_ThrowException (env, IO_EXCEPTION,
		      "forcing mapped files to disk not implemented");
#endif /* HAVE_MSYNC */
}
