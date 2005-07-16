/* java_nio.c - Native methods for gnu.java.nio.FileChannelImpl class
   Copyright (C) 2002 Free Software Foundation, Inc.

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

#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif

#include <jni.h>
#include <jcl.h>

#include "target_native.h"
#ifndef WITHOUT_NETWORK
#include "target_native_network.h"
#endif /* WITHOUT_NETWORK */

#include "java_nio_channels_FileChannelImpl.h"

#include "javaio.h"

#define NIO_DEBUG(X)		/* no debug */
//#define NIO_DEBUG(X) X

/***************************************
 *
 *  File Channel implementation
 *
 *************/


static char *
compare (int i, int lim, char *buffer)
{
  sprintf (buffer, "(%d >= %d)", i, lim);
  return buffer;
}

static inline int
convert_Int (int X)
{
  unsigned char *a = (unsigned char *) &X;
  int res =
    (((int) a[0]) << 24) +
    (((int) a[1]) << 16) + (((int) a[2]) << 8) + (((int) a[3]) << 0);
  return res;
}

static inline jlong
convert_Long (jlong X)
{
  unsigned char *a = (unsigned char *) &X;
  int res1 =
    (((int) a[0]) << 24) +
    (((int) a[1]) << 16) + (((int) a[2]) << 8) + (((int) a[3]) << 0);
  int res2;
  a += 4;
  res2 =
    (((int) a[0]) << 24) +
    (((int) a[1]) << 16) + (((int) a[2]) << 8) + (((int) a[3]) << 0);
  return ((jlong) res1) | ((jlong) res2) << 32LL;
}

static inline short
convert_Short (short X)
{
  unsigned char *a = (unsigned char *) &X;
  int res = (((int) a[2]) << 8) + (((int) a[3]) << 0);
  return res;
}
static inline short
convert_Char (short X)
{
  unsigned char *a = (unsigned char *) &X;
  int res = (((int) a[2]) << 8) + (((int) a[3]) << 0);
  return res;
}

static inline unsigned char
convert_Byte (unsigned char X)
{
  return X;
}

static inline float
convert_Float (float X)
{
  return X;
}

static inline double
convert_Double (double X)
{
  return X;
}


// Note: do to many get()'s on a buffer and you should throw a BufferUnderflowException
// Alas, I only found this out during testing....

#define READ_WRITE_MMAPED_FILE(TYPE,ELT)							  \
												  \
ELT Java_gnu_java_nio_MappedByteFileBuffer_nio_1read_1 ## TYPE ## _1file_1channel(JNIEnv *env,	  \
                                       jclass c, jobject b,					  \
				       int index, int limit, jlong jaddress)	\
{												  \
  char *address = *(void **) &jaddress;	char buffer[128];					  \
  if (index >= limit) JCL_ThrowException(env, "java/nio/BufferUnderflowException", compare(index,limit, buffer));	  \
 NIO_DEBUG(  fprintf(stderr, "READ:index = %d [0]=%c [1]=%c\n", index, address[0],address[1]);  ) \
  address += index;										  \
  return convert_ ## TYPE  (*(ELT *) address);							  \
}												  \
												  \
void Java_gnu_java_nio_MappedByteFileBuffer_nio_1write_1 ## TYPE ## _1file_1channel(JNIEnv *env,  \
                                          jclass c, jobject b,					  \
                                          int index, int limit, ELT value, jlong jaddress)		  \
{												  \
												  \
  char *address = *(void **) &jaddress;	char buffer[128];					  \
  if (index >= limit) JCL_ThrowException(env, "java/nio/BufferUnderflowException", compare(index,limit, buffer));	  \
NIO_DEBUG(  fprintf(stderr, "WRITE:index = %d [0]=%c [1]=%c\n", index, address[0],address[1]);  )  \
  address += index;										  \
  *(ELT *) address = value;									  \
}												  \
												  \
ELT Java_gnu_java_nio_MappedByteFileBuffer_nio_1get_1 ## TYPE(JNIEnv *env, jclass c, jobject b,	  \
			int index, int limit, jlong jaddress)						  \
{	\
 fprintf(stderr, "unimplemented\n"); return 0; \
}												  \
												  \
void Java_gnu_java_nio_MappedByteFileBuffer_nio_1put_1 ## TYPE(JNIEnv *env, jclass c, jobject b,  \
			 int index, int limit, 						  \
			 ELT value, jlong jaddress)						  \
{			\
 fprintf(stderr, "unimplemented\n");  \
}

READ_WRITE_MMAPED_FILE (Byte, u_int8_t);
READ_WRITE_MMAPED_FILE (Char, u_int16_t);
READ_WRITE_MMAPED_FILE (Short, u_int16_t);
READ_WRITE_MMAPED_FILE (Int, u_int32_t);
READ_WRITE_MMAPED_FILE (Long, u_int64_t);
READ_WRITE_MMAPED_FILE (Float, float);
READ_WRITE_MMAPED_FILE (Double, double);

u_int64_t
nio_mmap_file (jint fd, jlong pos, jint size, jint jflags)
{
#ifdef HAVE_MMAP
  u_int64_t ret = 0;
  void *address;

  int flags = (jflags != 2) ? MAP_SHARED : MAP_PRIVATE;
  int prot = PROT_READ;

  if (jflags == 1)
    prot |= PROT_WRITE;

  //  fprintf(stderr, "mapping file: %d\n", fd);

  address = mmap (0, size, prot, flags, fd, pos);

  if (address == (void *) -1)
    {
      perror ("mapping file failed");
      return 0;
    }

  //  fprintf(stderr, "address = %p, fd = %d, pos=%lld, size=%d\n", address, fd, pos, size);

  *(void **) &ret = address;

  return ret;
#else /* not HAVE_MMAP */
  return (TARGET_NATIVE_MATH_INT_INT64_CONST_0);
#endif /* not HAVE_MMAP */
}


void
nio_msync (int fd, jlong jaddress, int size)
{
#ifdef HAVE_MMAP
  int res;
  char *address = *(void **) &jaddress;

  //  fprintf(stderr, "synchronizing with file (%p -> %d bytes (%s))\n", address, size, address);

  res = msync (address, size, MS_SYNC | MS_INVALIDATE);

  if (res == -1)
    {
      perror ("synchronize with file failed");
    }
#else /* not HAVE_MMAP */
#endif /* not HAVE_MMAP */
}

void
nio_unmmap_file (int fd, jlong jaddress, int size)
{
#ifdef HAVE_MMAP
  int res = 0;
  char *address = *(void **) &jaddress;

  //  nio_msync(fd, jaddress, size);

  //  fprintf(stderr, "unmapping (%p -> %d bytes)\n", address, size);

  res = munmap (address, size);
  if (res == -1)
    {
      perror ("un-mapping file failed");
    }
#else /* not HAVE_MMAP */
#endif /* not HAVE_MMAP */
}


/***************************************
 *
 * Socket Channel implementation
 *
 *************/

/*************************************************************************/

/*
 * Returns a 32 bit Internet address for the passed in InetAddress object
 * Ronald: This is a verbatim copy from javanet.c.
 * It's a copy to avoid a link error in orp.
 */

static int
socket_channel_get_net_addr (JNIEnv * env, jobject addr)
{
#ifndef WITHOUT_NETWORK
  jclass cls = 0;
  jmethodID mid;
  jarray arr = 0;
  jbyte *octets;
  int netaddr, len;

  DBG
    ("socket_channel_get_net_addr(): Entered socket_channel_get_net_addr\n");

  /* Call the getAddress method on the object to retrieve the IP address */
  cls = (*env)->GetObjectClass (env, addr);
  if (cls == NULL)
    return (0);

  mid = (*env)->GetMethodID (env, cls, "getAddress", "()[B");
  if (mid == NULL)
    return (0);

  DBG ("socket_channel_get_net_addr(): Got getAddress method\n");

  arr = (*env)->CallObjectMethod (env, addr, mid);
  if (arr == NULL)
    return (0);

  DBG ("socket_channel_get_net_addr(): Got the address\n");

  /* Turn the IP address into a 32 bit Internet address in network byte order */
  len = (*env)->GetArrayLength (env, arr);
  if (len != 4)
    {
      JCL_ThrowException (env, "java/io/IOException",
			  "Internal Error: invalid byte array length");
      return (0);
    }
  DBG ("socket_channel_get_net_addr(): Length ok\n");

  octets = (*env)->GetByteArrayElements (env, arr, 0);
  if (octets == NULL)
    return (0);

  DBG ("socket_channel_get_net_addr(): Grabbed bytes\n");

  TARGET_NATIVE_NETWORK_IPADDRESS_BYTES_TO_INT (octets[0],
						octets[1],
						octets[2],
						octets[3], netaddr);

  (*env)->ReleaseByteArrayElements (env, arr, octets, 0);
  DBG ("socket_channel_get_net_addr(): Done getting addr\n");

  return netaddr;
#else /* not WITHOUT_NETWORK */
  return (0);
#endif /* not WITHOUT_NETWORK */
}
