/* gnu_java_nio_VMChannel.c -
   Copyright (C) 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/uio.h>
#include <string.h>

#include <jni.h>
#include <jcl.h>

#include "gnu_java_nio_VMChannel.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */

#define IO_EXCEPTION "java/io/IOException"
#define NON_READABLE_CHANNEL_EXCEPTION "java/nio/channels/NonReadableChannelException"
#define NON_WRITABLE_CHANNEL_EXCEPTION "java/nio/channels/NonWritableChannelException"

/*
 * Limit to maximum of 16 buffers
 */
#define JCL_IOV_MAX 16

#ifdef __cplusplus
extern "C"
{
#endif

enum JCL_buffer_type { DIRECT, ARRAY, UNKNOWN };

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

static jfieldID address_fid;
static jmethodID get_position_mid;
static jmethodID set_position_mid;
static jmethodID get_limit_mid;
static jmethodID set_limit_mid;
static jmethodID has_array_mid;
static jmethodID array_mid;
static jmethodID array_offset_mid;

jmethodID
get_method_id(JNIEnv *env,  jclass clazz, const char *name, 
	          const char *sig)
{
  jmethodID mid = (*env)->GetMethodID(env, clazz, name, sig);
  if (mid == NULL)
    {
  	  JCL_ThrowException(env, "java/lang/InternalError", name);
      return NULL;
    }
  
  return mid;
}

void
JCL_print_buffer(JNIEnv *env __attribute__((__unused__)), struct JCL_buffer *buf)
{
  fprintf(stdout, "Buffer - type: %d, ptr: %p\n", buf->type, buf->ptr);
  fflush(stdout);
}


int
JCL_init_buffer(JNIEnv *env, struct JCL_buffer *buf, jobject bbuf)
{
  jobject address = (*env)->GetObjectField(env, bbuf, address_fid);
  
  buf->position = (*env)->CallIntMethod(env, bbuf, get_position_mid);
  buf->limit = (*env)->CallIntMethod(env, bbuf, get_limit_mid);
  buf->offset = 0;
  buf->count = 0;
  buf->type = UNKNOWN;
    
  if (address != NULL)
    {
      buf->ptr = (jbyte *) JCL_GetRawData(env, address);
      buf->type = DIRECT;
      (*env)->DeleteLocalRef(env, address);
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
          return -1;
        }
    }
      
  return 0;
}

void
JCL_release_buffer(JNIEnv *env, struct JCL_buffer *buf, jobject bbuf, 
    jint action)
{
  jbyteArray arr;
  
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


JNIEXPORT void JNICALL 
Java_gnu_java_nio_VMChannel_initIDs  (JNIEnv *env, 
	jclass clazz __attribute__ ((__unused__)))
{
  jclass bufferClass = JCL_FindClass(env, "java/nio/Buffer");
  jclass byteBufferClass = JCL_FindClass(env, "java/nio/ByteBuffer");
  
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
}

JNIEXPORT void JNICALL 
Java_gnu_java_nio_VMChannel_setBlocking (JNIEnv *env, 
	jobject o __attribute__ ((__unused__)), 
	jint fd, 
	jboolean blocking)
{
  int opts;
  
  opts = fcntl(fd, F_GETFL);
  if (opts < 0)
    {
      JCL_ThrowException(env, IO_EXCEPTION, 
        "Failed to get flags for file desriptor");
      return;
    }
  
  if (blocking)
    opts |= O_NONBLOCK;
  else
    opts &= ~(O_NONBLOCK);
  
  opts = fcntl(fd, F_SETFL, opts);
  
  if (opts < 0)
    {
      JCL_ThrowException(env, IO_EXCEPTION, 
        "Failed to set flags for file desriptor");
      return;
    }  
}


JNIEXPORT jint JNICALL 
Java_gnu_java_nio_VMChannel_read (JNIEnv *env, 
	jobject o __attribute__ ((__unused__)), 
	jint fd, 
	jobject bbuf)
{
  jint len;
  ssize_t result;
  struct JCL_buffer buf;
  
  if (JCL_init_buffer(env, &buf, bbuf) < 0)
    {
      /* TODO: Rethrown exception */
      JCL_ThrowException (env, IO_EXCEPTION, "Buffer initialisation failed");
      return -1;
    }
  
  len = buf.limit - buf.position;
  
  result = read(fd, &(buf.ptr[buf.position + buf.offset]), len);
  buf.count = result;
  
  if (result == 0)
    result = -1; /* End Of File */
  else if (result == -1)
    {
      buf.count = 0;
      if (errno == EAGAIN) /* Non-blocking */
        result = 0;
      else if (errno == EBADF) /* Bad fd */
        {
          JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
          JCL_ThrowException (env, NON_READABLE_CHANNEL_EXCEPTION, 
                              strerror(errno));
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
    
  JCL_release_buffer(env, &buf, bbuf, JNI_COMMIT);
  
  return result;
}

JNIEXPORT jint JNICALL 
Java_gnu_java_nio_VMChannel_write (JNIEnv *env, 
	jobject o __attribute__ ((__unused__)), 
	jint fd, 
	jobject bbuf)
{
  jint len;
  ssize_t result;
  struct JCL_buffer buf;
  
  if (JCL_init_buffer(env, &buf, bbuf) < 0)
    {
      /* TODO: Rethrown exception */
      JCL_ThrowException (env, IO_EXCEPTION, "Buffer initialisation failed");
      return -1;
    }
  
  len = buf.limit - buf.position;
  
  result = write(fd, &(buf.ptr[buf.position + buf.offset]), len);
  buf.count = result;
  
  if (result == -1)
    {
      if (errno == EAGAIN) /* Non-blocking */
          result = 0;
      else
        {
          JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
          JCL_ThrowException(env, IO_EXCEPTION, strerror(errno));
          return -1;
        }
    }
    
  JCL_release_buffer(env, &buf, bbuf, JNI_ABORT);
  
  return result;  
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
  
  /* Build the vector of buffers to read into */
  for (i = 0; i < vec_len; i++)
    {
      struct JCL_buffer* buf;
      jobject bbuf;
      
      buf = &bi_list[i];
      bbuf = (*env)->GetObjectArrayElement(env, bbufs, offset + i);
      
      JCL_init_buffer(env, buf, bbuf); 
      
      buffers[i].iov_base = &(buf->ptr[buf->position + buf->offset]);
      buffers[i].iov_len = buf->limit - buf->position;
      (*env)->DeleteLocalRef(env, bbuf);
    }
    
  /* Work the scattering magic */
  result = readv(fd, buffers, vec_len);
  bytes_read = (jlong) result;
  
  /* Handle the response */
  if (result < 0)
    {
      if (errno == EAGAIN) /* Non blocking */
        result = 0;
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
  
  
  /* Build the vector of buffers to read into */
  for (i = 0; i < vec_len; i++)
    {
      struct JCL_buffer* buf;
      jobject bbuf;
      
      buf = &bi_list[i];
      bbuf = (*env)->GetObjectArrayElement(env, bbufs, offset + i);
      
      JCL_init_buffer(env, buf, bbuf); 
      
      buffers[i].iov_base = &(buf->ptr[buf->position + buf->offset]);
      buffers[i].iov_len = buf->limit - buf->position;
      (*env)->DeleteLocalRef(env, bbuf);
    }
    
  /* Work the gathering magic */
  result = writev(fd, buffers, vec_len);
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




#ifdef __cplusplus
}
#endif
