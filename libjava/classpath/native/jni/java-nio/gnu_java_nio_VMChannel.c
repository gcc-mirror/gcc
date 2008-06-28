/* gnu_java_nio_VMChannel.c -
   Copyright (C) 2003, 2004, 2005, 2006, 2007  Free Software Foundation, Inc.

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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <config-int.h>

#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/uio.h>

#include <netinet/in.h>

#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include <jni.h>
#include <jcl.h>

#include "cpio.h"
#include "gnu_java_nio_VMChannel.h"
#include "javanio.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */

#if defined(HAVE_SYS_IOCTL_H)
#define BSD_COMP /* Get FIONREAD on Solaris2 */
#include <sys/ioctl.h>
#endif
#if defined(HAVE_SYS_FILIO_H) /* Get FIONREAD on Solaris 2.5 */
#include <sys/filio.h>
#endif

#define CONNECT_EXCEPTION "java/net/ConnectException"
#define IO_EXCEPTION "java/io/IOException"
#define SOCKET_EXCEPTION "java/net/SocketException"
#define INTERRUPTED_IO_EXCEPTION "java/io/InterruptedIOException"
#define NON_READABLE_CHANNEL_EXCEPTION "java/nio/channels/NonReadableChannelException"
#define NON_WRITABLE_CHANNEL_EXCEPTION "java/nio/channels/NonWritableChannelException"
#define SOCKET_TIMEOUT_EXCEPTION "java/net/SocketTimeoutException"

/* Align a value up or down to a multiple of the pagesize. */
#define ALIGN_DOWN(p,s) ((p) - ((p) % (s)))
#define ALIGN_UP(p,s) ((p) + ((s) - ((p) % (s))))

/*
 * Limit to maximum of 16 buffers
 */
#define JCL_IOV_MAX 16

#ifdef __cplusplus
extern "C"
{
#endif

enum JCL_buffer_type { DIRECT, HEAP, ARRAY, UNKNOWN };

struct JCL_buffer
{
  enum JCL_buffer_type type;
  jbyte *ptr;
  jint offset;
  jint position;
  jint limit;
  jint count;
};

jmethodID get_method_id(JNIEnv *, jclass, const char *, const char *);
void JCL_print_buffer(JNIEnv *, struct JCL_buffer *);
int JCL_init_buffer(JNIEnv *, struct JCL_buffer *, jobject);
void JCL_release_buffer(JNIEnv *, struct JCL_buffer *, jobject, jint);
void JCL_cleanup_buffers(JNIEnv *, struct JCL_buffer *, jint, jobjectArray, jint, jlong);
int JCL_thread_interrupted(JNIEnv *);

static jfieldID address_fid;
static jmethodID get_position_mid;
static jmethodID set_position_mid;
static jmethodID get_limit_mid;
static jmethodID set_limit_mid;
static jmethodID has_array_mid;
static jmethodID array_mid;
static jmethodID array_offset_mid;
static jmethodID thread_interrupted_mid;
static jclass vm_channel_class;

jmethodID
get_method_id(JNIEnv *env,  jclass clazz, const char *name, 
	          const char *sig)
{
  jmethodID mid = (*env)->GetMethodID(env, clazz, name, sig);
/*   NIODBG("name: %s; sig: %s", name, sig); */
  if (mid == NULL)
    {
      JCL_ThrowException(env, "java/lang/InternalError", name);
      return NULL;
    }
  
  return mid;
}

inline void
JCL_print_buffer(JNIEnv *env __attribute__((__unused__)), struct JCL_buffer *buf)
{
  fprintf (stderr, "Buffer - type: %d, ptr: %p\n", buf->type, buf->ptr);
}


int
JCL_init_buffer(JNIEnv *env, struct JCL_buffer *buf, jobject bbuf)
{
  void *addr = (*env)->GetDirectBufferAddress (env, bbuf);

/*   NIODBG("buf: %p; bbuf: %p; addr: %p", (void *) buf, bbuf, addr); */
  
  buf->position = (*env)->CallIntMethod(env, bbuf, get_position_mid);
  buf->limit = (*env)->CallIntMethod(env, bbuf, get_limit_mid);
  buf->offset = 0;
  buf->count = 0;
  buf->type = UNKNOWN;
    
  if (addr != NULL)
    {
      buf->ptr = (jbyte *) addr;
      buf->type = DIRECT;
    }
  else
    {
      jboolean has_array;
      has_array = (*env)->CallBooleanMethod(env, bbuf, has_array_mid);
      
      if (has_array == JNI_TRUE)
        {
          jbyteArray arr;
          buf->offset = (*env)->CallIntMethod(env, bbuf, array_offset_mid);
          arr = (*env)->CallObjectMethod(env, bbuf, array_mid);
          buf->ptr = (*env)->GetByteArrayElements(env, arr, 0);
          buf->type = ARRAY;
          (*env)->DeleteLocalRef(env, arr);
        }
      else
        {
          jobject address = (*env)->GetObjectField (env, bbuf, address_fid);
          if (address == NULL)
            return -1; /* XXX handle non-array, non-native buffers? */
          buf->ptr = (jbyte *) JCL_GetRawData(env, address);
          buf->type = HEAP;
          (*env)->DeleteLocalRef(env, address);
        }
    }
      
  return 0;
}

void
JCL_release_buffer(JNIEnv *env, struct JCL_buffer *buf, jobject bbuf, 
    jint action)
{
  jbyteArray arr;

/*   NIODBG("buf: %p; bbuf: %p; action: %x", (void *) buf, bbuf, action); */
  
  /* Set the position to the appropriate value */
  if (buf->count > 0)
    {
      jobject bbufTemp;
      bbufTemp = (*env)->CallObjectMethod(env, bbuf, set_position_mid, 
                                          buf->position + buf->count);
      (*env)->DeleteLocalRef(env, bbufTemp);
    }
    
  switch (buf->type)
    {
    case DIRECT:
    case HEAP:
      break;
    case ARRAY:
      arr = (*env)->CallObjectMethod(env, bbuf, array_mid);
      (*env)->ReleaseByteArrayElements(env, arr, buf->ptr, action);
      (*env)->DeleteLocalRef(env, arr);
      break;
    case UNKNOWN:
      /* TODO: Handle buffers that are not direct or array backed */
      break;
    }
}

void
JCL_cleanup_buffers(JNIEnv *env, 
                    struct JCL_buffer *bi_list, 
                    jint vec_len, 
                    jobjectArray bbufs, 
                    jint offset,
                    jlong num_bytes)
{
  jint i;

/*   NIODBG("bi_list: %p; vec_len: %d; bbufs: %p; offset: %d; num_bytes: %lld", */
/*       (void *) bi_list, vec_len, bbufs, offset, num_bytes); */
  
  /* Update all of the bbufs with the approriate information */
  for (i = 0; i < vec_len; i++)
    {
      struct JCL_buffer* buf;
      jobject bbuf;
      
      buf = &bi_list[i];
      bbuf = (*env)->GetObjectArrayElement(env, bbufs, offset + i);

      if (num_bytes > (buf->limit - buf->position))
        buf->count = (buf->limit - buf->position);
      else
        buf->count = num_bytes;
        
      num_bytes -= buf->count;
      
      JCL_release_buffer(env, buf, bbuf, JNI_ABORT);
      (*env)->DeleteLocalRef(env, bbuf);
    }
}


int
JCL_thread_interrupted(JNIEnv *env)
{
  return (int) (*env)->CallStaticBooleanMethod(env, vm_channel_class,
					       thread_interrupted_mid);
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    stdin_fd
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_stdin_1fd (JNIEnv *env __attribute__((unused)),
                                       jclass c __attribute__((unused)))
{
/*   NIODBG("%d", fileno (stdin)); */
  return fileno (stdin);
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    stdout_fd
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_stdout_1fd (JNIEnv *env __attribute__((unused)),
                                       jclass c __attribute__((unused)))
{
/*   NIODBG("%d", fileno (stdout)); */
  return fileno (stdout);
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    stderr_fd
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_stderr_1fd (JNIEnv *env __attribute__((unused)),
                                       jclass c __attribute__((unused)))
{
/*   NIODBG("%d", fileno (stderr)); */
  return fileno (stderr);
}


JNIEXPORT void JNICALL 
Java_gnu_java_nio_VMChannel_initIDs  (JNIEnv *env, 
	jclass clazz)
{
  jclass bufferClass = JCL_FindClass(env, "java/nio/Buffer");
  jclass byteBufferClass = JCL_FindClass(env, "java/nio/ByteBuffer");

/*   NIODBG("%s", "..."); */

  address_fid = (*env)->GetFieldID(env, bufferClass, "address", 
                                   "Lgnu/classpath/Pointer;");
  if (address_fid == NULL)
    {
  	  JCL_ThrowException(env, "java/lang/InternalError", 
  	  	"Unable to find internal field");
      return;
    }
  
  get_position_mid = get_method_id(env, bufferClass, "position", "()I");
  set_position_mid = get_method_id(env, bufferClass, "position", 
                                   "(I)Ljava/nio/Buffer;");
  get_limit_mid = get_method_id(env, bufferClass, "limit", "()I");
  set_limit_mid = get_method_id(env, bufferClass, "limit", 
                                "(I)Ljava/nio/Buffer;");
  has_array_mid = get_method_id(env, byteBufferClass, "hasArray", "()Z");
  array_mid = get_method_id(env, byteBufferClass, "array", "()[B");
  array_offset_mid = get_method_id(env, byteBufferClass, "arrayOffset", "()I");
  
  vm_channel_class = clazz;
  thread_interrupted_mid = (*env)->GetStaticMethodID(env, clazz,
                                                  "isThreadInterrupted",
                                                  "()Z");
}

JNIEXPORT void JNICALL 
Java_gnu_java_nio_VMChannel_setBlocking (JNIEnv *env, 
	jobject o __attribute__ ((__unused__)), 
	jint fd, 
	jboolean blocking)
{
  int opts;
  
/*   NIODBG("fd: %d; blocking: %d", fd, blocking); */

  opts = fcntl(fd, F_GETFL);
  if (opts < 0)
    {
      JCL_ThrowException(env, IO_EXCEPTION, 
        "Failed to get flags for file desriptor");
      return;
    }
  
  if (blocking == JNI_TRUE)
    opts &= ~(O_NONBLOCK);
  else
    opts |= O_NONBLOCK;
  
  opts = fcntl(fd, F_SETFL, opts);
  
  if (opts < 0)
    {
      JCL_ThrowException(env, IO_EXCEPTION, 
        "Failed to set flags for file desriptor");
      return;
    }  
}

/* Return true if fd is in non-blocking mode. */
static jboolean
is_non_blocking_fd(jint fd)
{
  int opts;
  opts = fcntl(fd, F_GETFL);
  if (opts == -1)
    {
      /* Assume blocking on error. */
      return 0;
    }
  return (opts & O_NONBLOCK) != 0;
}

JNIEXPORT jint JNICALL 
Java_gnu_java_nio_VMChannel_read__ILjava_nio_ByteBuffer_2 (JNIEnv *env,
                                                           jobject o __attribute__ ((__unused__)), 
                                                           jint fd, 
                                                           jobject bbuf)
{
#ifdef HAVE_READ
  jint len;
  ssize_t result;
  struct JCL_buffer buf;
  int tmp_errno;

/*   NIODBG("fd: %d; bbuf: %p", fd, bbuf); */
  
  if (JCL_init_buffer(env, &buf, bbuf) < 0)
    {
      /* TODO: Rethrown exception */
      JCL_ThrowException (env, IO_EXCEPTION, "Buffer initialisation failed");
      return -1;
    }

  len = buf.limit - buf.position;

  if (len == 0)
    {
      JCL_release_buffer (env, &buf, bbuf, JNI_ABORT);
      return 0;
    }
  
  do 
    {
      result = cpnio_read (fd, &(buf.ptr[buf.position + buf.offset]), len);
      tmp_errno = errno;
    }
  while (result == -1 && errno == EINTR && ! JCL_thread_interrupted(env));
  errno = tmp_errno;
  
  if (result == 0)
    {
      result = -1;
      buf.count = 0;
    }
  else if (result == -1)
    {
      buf.count = 0;
      if (errno == EAGAIN)
        {
          if (is_non_blocking_fd(fd))
            {
              /* Non-blocking */
              result = 0;
            }
          else
            {
              /* Read timeout on a socket with SO_RCVTIMEO != 0. */
              JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
              JCL_ThrowException(env, SOCKET_TIMEOUT_EXCEPTION, "read timed out");
              return -1;
            }
        }
      else if (errno == EBADF) /* Bad fd */
        {
          JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
          JCL_ThrowException (env, NON_READABLE_CHANNEL_EXCEPTION, 
                              strerror(errno));
          return -1;
        }
      else if (EINTR == errno) /* read interrupted */
        {
          JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
          JCL_ThrowException(env, INTERRUPTED_IO_EXCEPTION, strerror (errno));
          return -1;
        }
      else
        {
          JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
      	  JCL_ThrowException (env, IO_EXCEPTION, strerror(errno));
      	  return -1;
        }
    }
  else 
    buf.count = result;
      
  JCL_release_buffer(env, &buf, bbuf, 0);
  
  return result;
#else
  (void) fd;
  (void) bbuf;
  JCL_ThrowException (env, IO_EXCEPTION, "read not supported");
  return -1;
#endif /* HAVE_READ */
}

JNIEXPORT jint JNICALL 
Java_gnu_java_nio_VMChannel_write__ILjava_nio_ByteBuffer_2 (JNIEnv *env, 
                                                            jobject o __attribute__ ((__unused__)), 
                                                            jint fd, 
                                                            jobject bbuf)
{
#ifdef HAVE_WRITE
  jint len;
  ssize_t result;
  struct JCL_buffer buf;
  int tmp_errno;

/*   NIODBG("fd: %d; bbuf: %p", fd, bbuf); */
  
  if (JCL_init_buffer(env, &buf, bbuf) < 0)
    {
      /* TODO: Rethrown exception */
      JCL_ThrowException (env, IO_EXCEPTION, "Buffer initialisation failed");
      return -1;
    }

  len = buf.limit - buf.position;

  if (len == 0)
    {
      JCL_release_buffer (env, &buf, bbuf, JNI_ABORT);
      return 0;
    }
  
  do
    {
      result = cpnio_write (fd, &(buf.ptr[buf.position + buf.offset]), len);
      tmp_errno = errno;
    }
  while (result == -1 && errno == EINTR && ! JCL_thread_interrupted(env));
  errno = tmp_errno;

  buf.count = result;

  if (result == -1)
    {
      if (errno == EAGAIN) /* Non-blocking */
        {
          result = 0;
        }
      else
        {
          JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
          JCL_ThrowException(env, IO_EXCEPTION, strerror(errno));
          return -1;
        }
    }
    
  JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
  
  return result;
#else
  (void) fd;
  (void) bbuf;
  JCL_ThrowException (env, IO_EXCEPTION, "write not supported");
  return -1;
#endif /* HAVE_WRITE */
}


/*
 * Implementation of a scattering read.  Will use the appropriate
 * vector based read call (currently readv on Linux).
 * 
 * This has a limit to the number of buffers that will be read.  It
 * will not make muliple readv calls.  This is to ensure that operations 
 * are atomic.  Currently it is limited to 16 buffers.  This is for 
 * compatibiliy with Sun.
 */
JNIEXPORT jlong JNICALL 
Java_gnu_java_nio_VMChannel_readScattering (JNIEnv *env, 
	jobject o __attribute__ ((__unused__)), 
	jint fd, 
	jobjectArray bbufs, 
	jint offset, 
	jint length)
{
  jint i;
/*   jboolean is_error = JNI_FALSE; */
/*   char *error_msg; */
  struct iovec buffers[JCL_IOV_MAX];
  struct JCL_buffer bi_list[JCL_IOV_MAX];
  ssize_t result;
  jint vec_len = length < JCL_IOV_MAX ? length : JCL_IOV_MAX;
  jlong bytes_read = 0;
  int tmp_errno;

/*   NIODBG("fd: %d; bbufs: %p; offset: %d; length: %d", */
/*          fd, bbufs, offset, length); */
  
  /* Build the vector of buffers to read into */
  for (i = 0; i < vec_len; i++)
    {
      struct JCL_buffer* buf;
      jobject bbuf;
      
      buf = &bi_list[i];
      bbuf = (*env)->GetObjectArrayElement(env, bbufs, offset + i);
      
      JCL_init_buffer(env, buf, bbuf);

/*       JCL_print_buffer (env, buf); */
      
      buffers[i].iov_base = &(buf->ptr[buf->position + buf->offset]);
      buffers[i].iov_len = buf->limit - buf->position;
      (*env)->DeleteLocalRef(env, bbuf);
    }
    
  /* Work the scattering magic */
  do
    {
      result = cpnio_readv (fd, buffers, vec_len);
      tmp_errno = errno;
    }
  while (result == -1 && errno == EINTR && ! JCL_thread_interrupted(env));
  errno = tmp_errno;
  bytes_read = (jlong) result;
  
  /* Handle the response */
  if (result < 0)
    {
      if (errno == EAGAIN)
        {
          if (is_non_blocking_fd(fd))
            {
              /* Non-blocking */
              result = 0;
            }
          else
            {
              /* Read timeout on a socket with SO_RCVTIMEO != 0. */
              JCL_cleanup_buffers(env, bi_list, vec_len, bbufs, offset, bytes_read);
              JCL_ThrowException(env, SOCKET_TIMEOUT_EXCEPTION, "read timed out");
              return -1;
            }
        }
      else if (errno == EBADF) /* Bad fd */
        {
          JCL_cleanup_buffers(env, bi_list, vec_len, bbufs, offset, bytes_read);
          JCL_ThrowException (env, NON_READABLE_CHANNEL_EXCEPTION, 
                              strerror(errno));
          return -1;
        } 
      else
        {
          JCL_cleanup_buffers(env, bi_list, vec_len, bbufs, offset, bytes_read);
          JCL_ThrowException (env, IO_EXCEPTION, strerror(errno));
          return -1;
        }
      bytes_read = 0;
    }
  else if (result == 0) /* EOF */
    {
      result = -1;
    }
    
  JCL_cleanup_buffers(env, bi_list, vec_len, bbufs, offset, bytes_read);
                  
  return (jlong) result;
}


/*
 * Implementation of a gathering write.  Will use the appropriate
 * vector based read call (currently readv on Linux).
 * 
 * This has a limit to the number of buffers that will be read.  It
 * will not make muliple readv calls.  This is to ensure that operations 
 * are atomic.  Currently it is limited to 16 buffers.  This is for 
 * compatibiliy with Sun.
 */
JNIEXPORT jlong JNICALL 
Java_gnu_java_nio_VMChannel_writeGathering (JNIEnv *env, 
	jobject o __attribute__ ((__unused__)), 
	jint fd, 
	jobjectArray bbufs, 
	jint offset, 
	jint length)
{
  int i;
/*   jboolean is_error = JNI_FALSE; */
/*   char *error_msg; */
  struct iovec buffers[JCL_IOV_MAX];
  struct JCL_buffer bi_list[JCL_IOV_MAX];
  ssize_t result;
  jint vec_len = length < JCL_IOV_MAX ? length : JCL_IOV_MAX;
  jlong bytes_written;
  int tmp_errno;
  
/*   NIODBG("fd: %d; bbufs: %p; offset: %d; length: %d", */
/*          fd, bbufs, offset, length); */
  
  /* Build the vector of buffers to read into */
  for (i = 0; i < vec_len; i++)
    {
      struct JCL_buffer* buf;
      jobject bbuf;
      
      buf = &bi_list[i];
      bbuf = (*env)->GetObjectArrayElement(env, bbufs, offset + i);
      
      JCL_init_buffer(env, buf, bbuf); 
      
/*       JCL_print_buffer(env, buf); */

      buffers[i].iov_base = &(buf->ptr[buf->position + buf->offset]);
      buffers[i].iov_len = buf->limit - buf->position;
      (*env)->DeleteLocalRef(env, bbuf);
    }
    
  /* Work the gathering magic */
  do
    {
      result = cpnio_writev (fd, buffers, vec_len);
      tmp_errno = errno;
    }
  while (result == -1 && tmp_errno == EINTR && ! JCL_thread_interrupted(env));
  errno = tmp_errno;

  bytes_written = (jlong) result;

  if (result < 0)
    {
      bytes_written = 0;
      if (errno == EAGAIN) /* Non blocking */
        result = 0;
      else if (errno == EBADF) /* Bad fd */
        {
          JCL_cleanup_buffers(env, bi_list, vec_len, bbufs, offset, 
                              bytes_written);
          JCL_ThrowException (env, NON_WRITABLE_CHANNEL_EXCEPTION, 
                              strerror(errno));
          return -1;
        } 
      else
        {
          JCL_cleanup_buffers(env, bi_list, vec_len, bbufs, offset,
                              bytes_written);
          JCL_ThrowException (env, IO_EXCEPTION, strerror(errno));
          return -1;
        }
    }
  else if (result == 0) /* EOF??  Does this happen on a write */
    result = -1;
    
  JCL_cleanup_buffers(env, bi_list, vec_len, bbufs, offset, bytes_written);    
  return (jlong) result;
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    receive
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_receive (JNIEnv *env,
                                     jclass c __attribute__((unused)),
                                     jint fd, jobject dst, jobject addrPort)
{
#ifdef HAVE_RECVFROM
  char *addrPortPtr = (*env)->GetDirectBufferAddress (env, addrPort);
  struct JCL_buffer buf;
#ifdef HAVE_INET6
  struct sockaddr_in6 sock_storage;
  struct sockaddr_in6 *sock6;
  socklen_t slen = sizeof (struct sockaddr_in6);
#else
  struct sockaddr_in sock_storage;
  socklen_t slen = sizeof (struct sockaddr_in);
#endif /* HAVE_INET6 */
  struct sockaddr *sockaddr = (struct sockaddr *) &sock_storage;
  struct sockaddr_in *sock4;
  int ret;
  jint result = -1;

  if (JCL_init_buffer (env, &buf, dst) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, "loading buffer failed");

#ifndef HAVE_MSG_WAITALL
#define MSG_WAITALL       0
#endif

  ret = cpnio_recvfrom (fd, &(buf.ptr[buf.position + buf.offset]),
                        buf.limit - buf.position, MSG_WAITALL,
                        sockaddr, &slen);

  if (-1 == ret)
    {
      JCL_release_buffer (env, &buf, dst, JNI_ABORT);
      if (EINTR == errno)
        JCL_ThrowException (env, "java/io/InterruptedIOException", strerror (errno));
      else if (EAGAIN == errno)
        {
          /* If the socket is in blocking mode, our timeout expired. */
          int val = fcntl (fd, F_GETFL, 0);
          if (val == -1)
            JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
          else if ((val & O_NONBLOCK) == 0)
            JCL_ThrowException (env, "java/net/SocketTimeoutException",
                                "read timed out");
        }
      else
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return 0;
    }

  if (sockaddr->sa_family == AF_INET)
    {
      sock4 = (struct sockaddr_in *) sockaddr;
      memcpy (addrPortPtr, &(sock4->sin_addr.s_addr), 4);
      ;memcpy (addrPortPtr + 4, &(sock4->sin_port), 2);
      result = 4;
    }
#ifdef HAVE_INET6
  else if (sockaddr->sa_family == AF_INET6)
    {
      sock6 = (struct sockaddr_in6 *) sockaddr;
      memcpy (addrPortPtr, &(sock6->sin6_addr.s6_addr), 16);
      memcpy (addrPortPtr + 16, &(sock6->sin6_port), 2);
      result = 16;
    }
#endif /* HAVE_INET6 */
  else if (ret == 0)
    {
      result = 0;
    }
  else
    {
      JCL_ThrowException (env, "java/net/SocketException",
                          "unsupported address type returned");
    }

  buf.count += ret;
  JCL_release_buffer (env, &buf, dst, 0);
  return result;
#else
  (void) fd;
  (void) dst;
  (void) addrPort;
  JCL_ThrowException (env, IO_EXCEPTION, "recvfrom not supported");
#endif /* HAVE_RECVFROM */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    send
 * Signature: (Ljava/nio/ByteBuffer;[BI)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_send (JNIEnv *env,
                                  jclass c __attribute__((unused)),
                                  int fd, jobject src, jbyteArray addr, jint port)
{
#ifdef HAVE_SENDTO
  struct sockaddr_in sockaddr;
  jbyte *elems;
  struct JCL_buffer buf;
  int ret;

/*   NIODBG("fd: %d; src: %p; addr: %p; port: %d", */
/*          fd, src, addr, port); */

  if (JCL_init_buffer (env, &buf, src) == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "loading buffer failed");
      return -1;
    }

/*   JCL_print_buffer (env, &buf); */

  elems = (*env)->GetByteArrayElements (env, addr, NULL);

  sockaddr.sin_family = AF_INET;
  sockaddr.sin_addr.s_addr = *((uint32_t *) elems);
  sockaddr.sin_port = htons (port);

  do
    {
      ret = cpnio_sendto (fd, &(buf.ptr[buf.position + buf.offset]),
                          buf.limit - buf.position,
                          0, (const struct sockaddr *) &sockaddr,
                          sizeof (struct sockaddr_in));
    }
  while (-1 == ret && EINTR == errno);

  (*env)->ReleaseByteArrayElements (env, addr, elems, JNI_ABORT);

  if (-1 == ret)
    {
      if (errno != EAGAIN)
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      JCL_release_buffer (env, &buf, src, JNI_ABORT);
      return 0;
    }

  buf.count += ret;
  JCL_release_buffer (env, &buf, src, JNI_ABORT);
  return ret;
#else
  (void) fd;
  (void) src;
  (void) addr;
  (void) port;
#endif /* HAVE_SENDTO */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    send6
 * Signature: (Ljava/nio/ByteBuffer;[BI)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_send6 (JNIEnv *env,
                                   jclass c __attribute__((unused)),
                                   int fd, jobject src, jbyteArray addr, jint port)
{
#if defined(HAVE_SENDTO) && defined(HAVE_INET6)
  struct sockaddr_in6 sockaddr;
  jbyte *elems;
  struct JCL_buffer buf;
  int ret;

/*   NIODBG("fd: %d; src: %p; addr: %p; port: %d", */
/*          fd, src, addr, port); */

  if (JCL_init_buffer (env, &buf, src) == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "loading buffer failed");
      return -1;
    }

/*   JCL_print_buffer (env, &buf); */

  elems = (*env)->GetByteArrayElements (env, addr, NULL);

  sockaddr.sin6_family = AF_INET6;
  memcpy (&sockaddr.sin6_addr.s6_addr, elems, 16);
  sockaddr.sin6_port = htons (port);

  do
    {
      ret = cpnio_sendto (fd, (const void *) (buf.ptr + buf.offset),
                          buf.limit - buf.position,
                          0, (const struct sockaddr *) &sockaddr,
                          sizeof (struct sockaddr_in6));
    }
  while (-1 == ret && EINTR == errno);

  (*env)->ReleaseByteArrayElements (env, addr, elems, JNI_ABORT);

  if (-1 == ret)
    {
      if (errno != EAGAIN)
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      JCL_release_buffer (env, &buf, src, JNI_ABORT);
      return 0;
    }

  buf.count += ret;
  JCL_release_buffer (env, &buf, src, JNI_ABORT);
  return ret;
#else
  (void) fd;
  (void) src;
  (void) addr;
  (void) port;
  JCL_ThrowException (env, IO_EXCEPTION, "IPv6 sendto not supported");
  return -1;
#endif /* HAVE_SENDTO && HAVE_INET6 */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    read
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_read__I (JNIEnv *env,
                                     jclass c __attribute__((unused)),
                                     jint fd)
{
#ifdef HAVE_READ
  char in;
  int ret;
  int tmp_errno;

/*   NIODBG("fd: %d", fd); */

  do
    {
      ret = cpnio_read (fd, &in, 1);
      tmp_errno = errno;
    }
  while (ret == -1 && errno == EINTR && ! JCL_thread_interrupted(env));
  errno = tmp_errno;

  if (-1 == ret)
    {
      if (errno == EAGAIN && !is_non_blocking_fd(fd))
        {
          /* Read timeout on a socket with SO_RCVTIMEO != 0. */
          JCL_ThrowException(env, SOCKET_TIMEOUT_EXCEPTION, "read timed out");
        }
      else
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return -1;
    }
  
  if (0 == ret)
    return -1;

  return (in & 0xFF);
#else
  (void) fd;
  JCL_ThrowException (env, IO_EXCEPTION, "read not supported");
#endif /* HAVE_READ */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    write
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_VMChannel_write__II (JNIEnv *env,
                                       jclass c __attribute__((unused)),
                                       jint fd, jint data)
{
#ifdef HAVE_WRITE
  char out = (char) data;
  int ret;
  int tmp_errno;

/*   NIODBG("fd: %d; data: %d", fd, data); */

  do
    {
      ret = cpnio_write (fd, &out, 1);
      tmp_errno = errno;
    }
  while (ret == -1 && errno == EINTR && ! JCL_thread_interrupted(env));
  errno = tmp_errno;

  if (-1 == ret)
    JCL_ThrowException(env, IO_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) data;
  JCL_ThrowException (env, IO_EXCEPTION, "write not supported");
#endif /* HAVE_WRITE */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    socket
 * Signature: (Z)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_socket (JNIEnv *env, jclass clazz __attribute__((unused)),
                                    jboolean stream)
{
#ifdef HAVE_SOCKET
  int ret;

  do
    {
      ret = cpnio_socket (AF_INET, stream ? SOCK_STREAM : SOCK_DGRAM, 0);
    }
  while (-1 == ret && EINTR == errno);

  if (ret == -1)
    JCL_ThrowException (env, "java/net/SocketException", strerror (errno));
/*   NIODBG("created socket %d", ret); */

  return ret;
#else
  (void) stream;
  JCL_ThrowException (env, IO_EXCEPTION, "socket not supported");
  return -1;
#endif /* HAVE_SOCKET */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    connect
 * Signature: (I[BI)Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_nio_VMChannel_connect (JNIEnv *env, jclass clazz __attribute__((unused)),
                                     jint fd, jbyteArray addr, jint port, jint timeout)
{
#ifdef HAVE_CONNECT
  struct sockaddr_in sockaddr;
  struct timeval timeo;
  int origflags = 0, flags;
  jbyte *addr_elems;
  int ret;
  int tmpErrno;

  if ((*env)->GetArrayLength (env, addr) != 4)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
                          "expecting 4-byte address");
      return JNI_FALSE;
    }

  if (timeout > 0)
    {
      timeo.tv_sec = timeout / 1000;
      timeo.tv_usec = (timeout % 1000) * 1000;
      origflags = fcntl (fd, F_GETFL, 0);
      if (origflags == -1)
        {
          JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
          return JNI_FALSE;
        }
      /* Set nonblocking mode, if not already set. */
      if (!(origflags & O_NONBLOCK))
        {
          flags = origflags | O_NONBLOCK;
          if (fcntl (fd, F_SETFL, flags) == -1)
            {
              JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
              return JNI_FALSE;
            }
        }
    }

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);

  memset (&sockaddr, 0, sizeof (struct sockaddr_in));
  sockaddr.sin_family = AF_INET;
  sockaddr.sin_port = htons (port);
  sockaddr.sin_addr.s_addr = *((uint32_t *) addr_elems);


  do
    {
      ret = cpnio_connect (fd, (struct sockaddr *) &sockaddr,
                           sizeof (struct sockaddr_in));
      tmpErrno = errno;
    }
  while (ret == -1 && errno == EINTR && ! JCL_thread_interrupted(env));
  errno = tmpErrno;

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  /* If a timeout was specified, select on the file descriptor with
     the timeout. */
  if (timeout > 0 && ret == -1)
    {
      /* Reset the non-blocking flag, if needed. */
      if (!(origflags & O_NONBLOCK))
        {
          if (fcntl (fd, F_SETFL, origflags) == -1)
            {
              /* oops */
              JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
              return JNI_FALSE;
            }
        }
      if (EINPROGRESS == errno)
        {
          fd_set wrfds;
          FD_ZERO(&wrfds);
          FD_SET(fd, &wrfds);
          ret = cpnio_select (fd + 1, NULL, &wrfds, NULL, &timeo);
          if (ret == -1)
            {
              JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
              return JNI_FALSE;
            }
          if (ret == 0) /* connect timed out */
            {
              JCL_ThrowException (env, SOCKET_TIMEOUT_EXCEPTION,
                                  "connect timed out");
              return JNI_FALSE;
            }
          return JNI_TRUE; /* Connected! */
        }
      else if (ECONNREFUSED == errno)
        {
          JCL_ThrowException (env, CONNECT_EXCEPTION,
                              strerror (errno));
          return JNI_FALSE;
        }
      else
        {
          JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
          return JNI_FALSE;
        }
    }

  if (ret == -1)
    {
      if (EINPROGRESS == errno)
        return JNI_FALSE;
      else if (ECONNREFUSED == errno)
        {
          JCL_ThrowException (env, CONNECT_EXCEPTION,
                              strerror (errno));
          return JNI_FALSE;
        }
      else
        {
          JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
          return JNI_FALSE;
        }
    }

  return JNI_TRUE;
#else
  (void) fd;
  (void) addr;
  (void) port;
  (void) timeout;
  JCL_ThrowException (env, SOCKET_EXCEPTION, "connect not supported");
  return JNI_FALSE;
#endif /* HAVE_CONNECT */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    connect6
 * Signature: (I[BI)Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_nio_VMChannel_connect6 (JNIEnv *env, jclass clazz __attribute__((unused)),
                                      jint fd, jbyteArray addr, jint port, int timeout)
{
#if defined(HAVE_CONNECT) && defined(HAVE_INET6)
  struct sockaddr_in6 sockaddr;
  struct timeval timeo;
  int flags, origflags = 0;
  jbyte *addr_elems;
  int ret;

  if (timeout > 0)
    {
      timeo.tv_sec = timeout / 1000;
      timeo.tv_usec = (timeout % 1000) * 1000;
      origflags = fcntl (fd, F_GETFL, 0);
      if (origflags == -1)
        {
          JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
          return JNI_FALSE;
        }
      /* Set nonblocking mode, if not already set. */
      if (!(origflags & O_NONBLOCK))
        {
          flags = origflags | O_NONBLOCK;
          if (fcntl (fd, F_SETFL, flags) == -1)
            {
              JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
              return JNI_FALSE;
            }
        }
    }

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);

  memset (&sockaddr, 0, sizeof (struct sockaddr_in6));
  sockaddr.sin6_family = AF_INET6;
  sockaddr.sin6_port = htons (port);
  memcpy (&sockaddr.sin6_addr.s6_addr, addr_elems, 16);

  ret = cpnio_connect (fd, (struct sockaddr *) &sockaddr,
                       sizeof (struct sockaddr_in6));

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  /* If a timeout was specified, select on the file descriptor with
     the timeout. */
  if (timeout > 0 && ret == -1)
    {
      /* Reset the non-blocking flag, if needed. */
      if (!(origflags & O_NONBLOCK))
        {
          if (fcntl (fd, F_SETFL, origflags) == -1)
            {
              /* oops */
              JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
              return JNI_FALSE;
            }
        }
      if (EINPROGRESS == errno)
        {
          fd_set wrfds;
          FD_ZERO(&wrfds);
          FD_SET(fd, &wrfds);
          ret = cpnio_select (fd + 1, NULL, &wrfds, NULL, &timeo);
          if (ret == -1)
            {
              JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
              return JNI_FALSE;
            }
          if (ret == 0) /* connect timed out */
            {
              JCL_ThrowException (env, SOCKET_TIMEOUT_EXCEPTION,
                                  "connect timed out");
              return JNI_FALSE;
            }
          return JNI_TRUE; /* Connected! */
        }
      else if (ECONNREFUSED == errno)
        {
          JCL_ThrowException (env, CONNECT_EXCEPTION,
                              strerror (errno));
          return JNI_FALSE;
        }
      else
        {
          JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
          return JNI_FALSE;
        }
    }

  if (ret == -1)
    {
      if (EAGAIN == errno)
        return JNI_FALSE;
      else if (ECONNREFUSED == errno)
        {
          JCL_ThrowException (env, CONNECT_EXCEPTION,
                              strerror (errno));
          return JNI_FALSE;
        }
      else
        {
          JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
          return JNI_FALSE;
        }
    }

  return JNI_TRUE;
#else
  (void) fd;
  (void) addr;
  (void) port;
  (void) timeout;
  JCL_ThrowException (env, SOCKET_EXCEPTION, "IPv6 connect not supported");
  return JNI_FALSE;
#endif /* HAVE_CONNECT && HAVE_INET6 */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    getsockname
 * Signature: (ILjava/nio/ByteBuffer;)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_getsockname (JNIEnv *env, jclass clazz __attribute__((unused)),
                                         jint fd, jobject name)
{
#ifdef HAVE_GETSOCKNAME
#ifdef HAVE_INET6
  struct sockaddr_in6 *addr6;
  struct sockaddr_in6 sock_storage;
  socklen_t socklen = sizeof (struct sockaddr_in6);
#else
  struct sockaddr_in sock_storage;
  socklen_t socklen = sizeof (struct sockaddr_in);
#endif /* HAVE_INET6 */

  struct sockaddr *sockaddr = (struct sockaddr *) &sock_storage;
  struct sockaddr_in *addr4;
  int ret;
  char *nameptr = (*env)->GetDirectBufferAddress (env, name);

  ret = getsockname (fd, sockaddr, &socklen);
  if (ret == -1)
    {
      JCL_ThrowException (env, "java/net/SocketException", strerror (errno));
      return 0;
    }

  if (sockaddr->sa_family == AF_INET)
    {
      addr4 = (struct sockaddr_in *) sockaddr;
      memcpy (nameptr, &(addr4->sin_addr.s_addr), 4);
      memcpy (nameptr + 4, &(addr4->sin_port), 2);
      return 4;
    }

#ifdef HAVE_INET6
  /* IPv6 */
  if (sockaddr->sa_family == AF_INET6)
    {
      addr6 = (struct sockaddr_in6 *) sockaddr;
      memcpy (nameptr, &(addr6->sin6_addr.s6_addr), 16);
      memcpy (nameptr + 16, &(addr6->sin6_port), 2);
      return 16;
    }
#endif /* HAVE_INET6 */
  JCL_ThrowException (env, IO_EXCEPTION, "unsupported address format");
  return -1;
#else
  (void) fd;
  (void) name;
  JCL_ThrowException (env, IO_EXCEPTION, "getsockname not supported");
  return -1;
#endif /* HAVE_GETSOCKNAME */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    getpeername
 * Signature: (ILjava/nio/ByteBuffer;)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_getpeername (JNIEnv *env, jclass clazz __attribute__((unused)),
                                         jint fd, jobject name)
{
#ifdef HAVE_GETPEERNAME
#ifdef HAVE_INET6
  struct sockaddr_in6 *addr6;
  struct sockaddr_in6 sock_storage;
  socklen_t socklen = sizeof (struct sockaddr_in6);
#else
  struct sockaddr_in sock_storage;
  socklen_t socklen = sizeof (struct sockaddr_in);
#endif /* HAVE_INET6 */

  struct sockaddr *sockaddr = (struct sockaddr *) &sock_storage;
  struct sockaddr_in *addr4;
  int ret;
  char *nameptr = (*env)->GetDirectBufferAddress (env, name);
  
  ret = getpeername (fd, sockaddr, &socklen);
  if (ret == -1)
    {
      if (ENOTCONN != errno)
        JCL_ThrowException (env, "java/net/SocketException", strerror (errno));
      return 0;
    }

  if (sockaddr->sa_family == AF_INET)
    {
      addr4 = (struct sockaddr_in *) sockaddr;
      memcpy (nameptr, &(addr4->sin_addr.s_addr), 4);
      memcpy (nameptr + 4, &(addr4->sin_port), 2);
      return 4;
    }
#ifdef HAVE_INET6
  else if (sockaddr->sa_family == AF_INET6)
    {
      addr6 = (struct sockaddr_in6 *) sockaddr;
      memcpy (nameptr, &(addr6->sin6_addr.s6_addr), 16);
      memcpy (nameptr + 16, &(addr6->sin6_port), 2);
      return 16;
    }
#endif /* HAVE_INET6 */

  JCL_ThrowException (env, "java/net/SocketException",
                      "unsupported address type");
  return -1;
#else
  (void) fd;
  (void) name;
  JCL_ThrowException (env, IO_EXCEPTION, "getpeername not supported");
  return -1;
#endif /* HAVE_GETPEERNAME */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    accept
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_accept (JNIEnv *env,
                                    jclass c __attribute__((unused)),
                                    jint fd)
{
#ifdef HAVE_ACCEPT
  int ret;
  int tmp_errno = 0;

#ifdef HAVE_INET6
  struct sockaddr_in6 addr;
  socklen_t alen = sizeof (struct sockaddr_in6);
#else
  struct sockaddr_in addr;
  socklen_t alen = sizeof (struct sockaddr_in);
#endif /* HAVE_INET6 */

  do
    {
      ret = cpnio_accept (fd, (struct sockaddr *) &addr, &alen);
      tmp_errno = errno;
      
      if (ret == -1)
        switch (tmp_errno)
        {
          case EINTR:
            /* Check if interrupted by Thread.interrupt(). If not then some
             * other unrelated signal interrupted the system function and
             * we should start over again.
             */
            if (JCL_thread_interrupted(env))
              {
                JCL_ThrowException (env, "java/net/SocketException", strerror (tmp_errno));
                return -1;
              }
            break;
#if defined(EWOULDBLOCK) && defined(EAGAIN) && EWOULDBLOCK != EAGAIN
          case EWOULDBLOCK:
#endif
          case EAGAIN:
            if (!is_non_blocking_fd(fd))
              {
                JCL_ThrowException(env, SOCKET_TIMEOUT_EXCEPTION, "Accept timed out");
              }
            /* Socket in non-blocking mode and no pending connection. */
            return -1;
          default:
            JCL_ThrowException (env, "java/net/SocketException", strerror (tmp_errno));
            return -1;
        }
      else
        break;
    }
  while (1);

  cpio_closeOnExec(ret);

  return ret;
#else
  (void) fd;
  JCL_ThrowException (env, IO_EXCEPTION, "accept not supported");
  return -1;
#endif /* HAVE_ACCEPT */
}



/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    disconnect
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_VMChannel_disconnect (JNIEnv *env,
                                        jclass c __attribute__((unused)),
                                        jint fd)
{
  struct sockaddr sockaddr;

  sockaddr.sa_family = AF_UNSPEC;
  if (connect (fd, &sockaddr, sizeof (struct sockaddr)) == -1)
    {
      /* The expected error for a successful disconnect is EAFNOSUPPORT. */
      if (errno != EAFNOSUPPORT)
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    close
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_VMChannel_close (JNIEnv *env,
                                   jclass c __attribute__((unused)),
                                   jint fd)
{
  if (close (fd) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    available
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_available (JNIEnv *env,
                                       jclass c __attribute__((unused)),
                                       jint fd)
{
#if defined (FIONREAD)

  jint avail = 0;

#if defined(ENOTTY) && defined(HAVE_FSTAT)
  struct stat statBuffer;
  off_t n;
#endif

/*   NIODBG("fd: %d", fd); */
  if (ioctl (fd, FIONREAD, &avail) == -1)
    {
#if defined(ENOTTY) && defined(HAVE_FSTAT)
      if (errno == ENOTTY)
        {
          if ((fstat (fd, &statBuffer) == 0) && S_ISREG (statBuffer.st_mode))
            {
              n = lseek (fd, 0, SEEK_CUR);
              if (n != -1)
                {
                  avail = statBuffer.st_size - n;
                  return avail;
                }
            }
        }
#endif
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
/*   NIODBG("avail: %d", avail); */

  return avail;

#elif defined(HAVE_FSTAT)

  jint avail = 0;

  struct stat statBuffer;
  off_t n;

  if ((fstat (fd, &statBuffer) == 0) && S_ISREG (statBuffer.st_mode))
    {
      n = lseek (fd, 0, SEEK_CUR);
      if (n != -1) 
        { 
	  avail = statBuffer.st_size - n;
	  return avail;
        } 
    }
  JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));

#elif defined(HAVE_SELECT)

  jint avail = 0;
  fd_set filedescriptset;
  struct timeval tv;

  FD_ZERO (&filedescriptset);
  FD_SET (fd,&filedescriptset);
  memset (&tv, 0, sizeof(tv));

  switch (select (fd+1, &filedescriptset, NULL, NULL, &tv))
    {
      case -1:
        break;
      case  0:
        avail = 0;
	return avail;
      default:
        avail = 1;
	return avail;
    }
  JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));

#else

  JCL_ThrowException (env, IO_EXCEPTION, "No native method for available");

#endif
}


enum FileChannel_mode {
  CPNIO_READ   = 1,
  CPNIO_WRITE  = 2,
  CPNIO_APPEND = 4,
  CPNIO_EXCL   = 8,
  CPNIO_SYNC   = 16,
  CPNIO_DSYNC  = 32
};


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    open
 * Signature: (Ljava/lang/String;I)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMChannel_open (JNIEnv *env,
                                  jclass c __attribute__((unused)),
                                  jstring path, jint mode)
{
  int nmode = 0;
  int ret;
  const char *npath;

  if ((mode & CPNIO_READ) && (mode & CPNIO_WRITE))
    nmode = O_RDWR;
  else if (mode & CPNIO_WRITE)
    nmode = O_WRONLY;
  else
    nmode = O_RDONLY;

  nmode = (nmode
           | ((nmode == O_RDWR || nmode == O_WRONLY) ? O_CREAT : 0)
           | ((mode & CPNIO_APPEND) ? O_APPEND :
              ((nmode == O_WRONLY) ? O_TRUNC : 0))
           | ((mode & CPNIO_EXCL) ? O_EXCL : 0)
           | ((mode & CPNIO_SYNC) ? O_SYNC : 0));

  npath = JCL_jstring_to_cstring (env, path);

/*   NIODBG("path: %s; mode: %x", npath, nmode); */

  ret = open (npath, nmode, 0666);

/*   NIODBG("ret: %d\n", ret); */

  JCL_free_cstring (env, path, npath);

  if (-1 == ret)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));

  return ret;
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    position
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL
Java_gnu_java_nio_VMChannel_position (JNIEnv *env,
                                      jclass c __attribute__((unused)),
                                      jint fd)
{
#ifdef HAVE_LSEEK
  off_t ret;

  ret = lseek (fd, 0, SEEK_CUR);

  if (-1 == ret)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));

  return (jlong) ret;
#else
  JCL_ThrowException (env, IO_EXCEPTION, "position not supported");
  return -1;
#endif /* HAVE_LSEEK */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    seek
 * Signature: (IJ)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_VMChannel_seek (JNIEnv *env,
                                  jclass c __attribute__((unused)),
                                  jint fd, jlong pos)
{
#ifdef HAVE_LSEEK
  if (lseek (fd, (off_t) pos, SEEK_SET) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
#else
  JCL_ThrowException (env, IO_EXCEPTION, "seek not supported");
#endif /* HAVE_LSEEK */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    truncate
 * Signature: (IJ)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_VMChannel_truncate (JNIEnv *env,
                                      jclass c __attribute__((unused)),
                                      jint fd, jlong len)
{
#if defined(HAVE_FTRUNCATE) && defined(HAVE_LSEEK)
  off_t pos = lseek (fd, 0, SEEK_CUR);
  if (pos == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return;
    }
  if (ftruncate (fd, (off_t) len) == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return;
    }
  if (pos > len)
    {
      if (lseek (fd, len, SEEK_SET) == -1)
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
#else
  JCL_ThrowException (env, IO_EXCEPTION, "truncate not supported");
#endif /* HAVE_FTRUNCATE && HAVE_LSEEK */
}


/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    lock
 * Signature: (IJJZZ)Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_nio_VMChannel_lock (JNIEnv *env,
                                  jclass c __attribute__((unused)),
                                  jint fd, jlong pos, jlong len,
                                  jboolean shared, jboolean wait)
{
#if HAVE_FCNTL
  struct flock fl;

  fl.l_start  = (off_t) pos;
  /* Long.MAX_VALUE means lock everything possible starting at pos. */
  if (len == 9223372036854775807LL)
    fl.l_len = 0;
  else
    fl.l_len = (off_t) len;
  fl.l_pid    = getpid ();
  fl.l_type   = (shared ? F_RDLCK : F_WRLCK);
  fl.l_whence = SEEK_SET;

  if (cpnio_fcntl (fd, (wait ? F_SETLKW : F_SETLK), (long) &fl) == -1)
    {
      if (errno != EAGAIN)
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return JNI_FALSE;
    }

  return JNI_TRUE;
#else
  JCL_ThrowException (env, IO_EXCEPTION, "lock not supported");
  return JNI_FALSE;
#endif /* HAVE_FCNTL */
}

/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    unlock
 * Signature: (IJJ)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_VMChannel_unlock (JNIEnv *env,
                                    jclass c __attribute__((unused)),
                                    jint fd, jlong pos, jlong len)
{
#if HAVE_FCNTL
  struct flock fl;

  fl.l_start  = (off_t) pos;
  fl.l_len    = (off_t) len;
  fl.l_pid    = getpid ();
  fl.l_type   = F_UNLCK;
  fl.l_whence = SEEK_SET;

  if (cpnio_fcntl (fd, F_SETLK, (long) &fl) == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
#else
  JCL_ThrowException (env, IO_EXCEPTION, "unlock not supported");
#endif /* HAVE_FCNTL */
}

/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    size
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL
Java_gnu_java_nio_VMChannel_size (JNIEnv *env,
                                  jclass c __attribute__((unused)),
                                  jint fd)
{
#ifdef HAVE_FSTAT
  struct stat st;

  if (fstat (fd, &st) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));

  return (jlong) st.st_size;
#else
  JCL_ThrowException (env, IO_EXCEPTION, "size not supported");
  return 0;
#endif
}

/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    map
 * Signature: (ICJI)Lgnu/classpath/Pointer;
 */
JNIEXPORT jobject JNICALL
Java_gnu_java_nio_VMChannel_map (JNIEnv *env,
                                 jclass clazz __attribute__((unused)),
                                 jint fd, jchar mode, jlong position, jint size)
{
#ifdef HAVE_MMAP
  jclass MappedByteBufferImpl_class;
  jmethodID MappedByteBufferImpl_init = NULL;
  jobject Pointer_instance;
  volatile jobject buffer;
  long pagesize;
  int prot, flags;
  void *p;
  void *address;

/*   NIODBG("fd: %d; mode: %x; position: %lld; size: %d", */
/*          fd, mode, position, size); */

  /* FIXME: should we just assume we're on an OS modern enough to
     have 'sysconf'? And not check for 'getpagesize'? */
#if defined(HAVE_GETPAGESIZE)
  pagesize = getpagesize ();
#elif defined(HAVE_SYSCONF)
  pagesize = sysconf (_SC_PAGESIZE);
#else
  JCL_ThrowException (env, IO_EXCEPTION,
		      "can't determine memory page size");
  return NULL;
#endif /* HAVE_GETPAGESIZE/HAVE_SYSCONF */

  if ((*env)->ExceptionOccurred (env))
    {
      return NULL;
    }

  prot = PROT_READ;
  if (mode == '+' || mode == 'c')
    {
      /* When writing we need to make sure the file is big enough,
         otherwise the result of mmap is undefined. */
      struct stat st;
      if (fstat (fd, &st) == -1)
        {
          JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
          return NULL;
        }
      if (position + size > st.st_size)
        {
          if (ftruncate(fd, position + size) == -1)
            {
              JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
              return NULL;
            }
        }
      prot |= PROT_WRITE;
    }

  flags = (mode == 'c' ? MAP_PRIVATE : MAP_SHARED);
  p = mmap (NULL, (size_t) ALIGN_UP (size, pagesize), prot, flags,
	    fd, ALIGN_DOWN (position, pagesize));
  if (p == MAP_FAILED)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return NULL;
    }

  /* Unalign the mapped value back up, since we aligned offset
     down to a multiple of the page size. */
  address = (void *) ((char *) p + (position % pagesize));

  Pointer_instance = JCL_NewRawDataObject(env, address);

  MappedByteBufferImpl_class = (*env)->FindClass (env,
						  "java/nio/MappedByteBufferImpl");
  if (MappedByteBufferImpl_class != NULL)
    {
      MappedByteBufferImpl_init =
	(*env)->GetMethodID (env, MappedByteBufferImpl_class,
			     "<init>", "(Lgnu/classpath/Pointer;IZ)V");
    }

  if ((*env)->ExceptionOccurred (env))
    {
      munmap (p, ALIGN_UP (size, pagesize));
      return NULL;
    }
  if (MappedByteBufferImpl_init == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
                          "could not get MappedByteBufferImpl constructor");
      munmap (p, ALIGN_UP (size, pagesize));
      return NULL;
    }

  buffer = (*env)->NewObject (env, MappedByteBufferImpl_class,
                              MappedByteBufferImpl_init, Pointer_instance,
                              (jint) size, mode == 'r');
  return buffer;
#else
  (void) fd;
  (void) mode;
  (void) position;
  (void) size;
  JCL_ThrowException (env, IO_EXCEPTION,
		      "memory-mapped files not implemented");
  return 0;
#endif /* HAVE_MMAP */
}

/*
 * Class:     gnu_java_nio_VMChannel
 * Method:    flush
 * Signature: (IZ)Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_nio_VMChannel_flush (JNIEnv *env,
                                   jclass c __attribute__((unused)),
                                   jint fd, jboolean metadata __attribute__((unused)))
{
#ifdef HAVE_FSYNC
  /* XXX blocking? */
  if (fsync (fd) == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
      return JNI_FALSE;
    }
  return JNI_TRUE;
#else
  JCL_ThrowException (env, IO_EXCEPTION, "flush not implemented");
  return JNI_TRUE;
#endif /* HAVE_FSYNC */
}


#ifdef __cplusplus
}
#endif
