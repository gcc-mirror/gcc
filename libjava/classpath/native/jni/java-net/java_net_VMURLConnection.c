/* VMURLConnection.c - native bits for URLConnection
   Copyright (C) 2006  Free Software Foundation, Inc.

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

#include <java_net_VMURLConnection.h>

#ifdef HAVE_MAGIC_H

#include <magic.h>

static magic_t cookie;

#endif /* HAVE_MAGIC_H */

void
Java_java_net_VMURLConnection_init (JNIEnv *env __attribute__ ((__unused__)),
				    jclass klass __attribute__ ((__unused__)))
{
#ifdef HAVE_MAGIC_H
  cookie = magic_open (MAGIC_MIME);
  if (cookie == (magic_t) NULL)
    return;
  if (magic_load (cookie, NULL) == -1)
    {
      magic_close (cookie);
      cookie = (magic_t) NULL;
    }
#endif /* HAVE_MAGIC_H */
}

#ifdef HAVE_MAGIC_H
jstring
Java_java_net_VMURLConnection_guessContentTypeFromBuffer (JNIEnv *env,
							  jclass klass
							  __attribute__ ((__unused__)),
							  jbyteArray bytes,
							  jint valid)
{
  jbyte *elements;
  const char *result;

  if (cookie == (magic_t) NULL)
    return NULL;

  elements = (*env)->GetByteArrayElements (env, bytes, NULL);
  result = magic_buffer (cookie, elements, valid);

  /* The mode we use doesn't matter, since we don't change the array. */
  (*env)->ReleaseByteArrayElements (env, bytes, elements, JNI_ABORT);

  if (result == NULL)
    return NULL;
  return (*env)->NewStringUTF (env, result);
#else
jstring
Java_java_net_VMURLConnection_guessContentTypeFromBuffer (JNIEnv *env
							  __attribute__ ((__unused__)),
							  jclass klass
							  __attribute__ ((__unused__)),
							  jbyteArray bytes
							  __attribute__ ((__unused__)),
							  jint valid
							  __attribute__ ((__unused__)))
{
  return NULL;
#endif /* HAVE_MAGIC_H */
}
