/* gnu_java_nio_charset_iconv_IconvDecoder.c --
   Copyright (C) 2005 Free Software Foundation, Inc.

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
#include <jcl.h>

#include <stdio.h>
#include <assert.h>
#include <errno.h>

#if defined(HAVE_ICONV)
#include <iconv.h>
#endif

#include "gnu_java_nio_charset_iconv_IconvDecoder.h"

#if defined(HAVE_ICONV)
static void createRawData (JNIEnv * env, jobject obj, void *ptr);
static void *getData (JNIEnv * env, jobject obj);

static jfieldID infid = NULL;
static jfieldID outfid = NULL;
#endif

/* Union used for type punning. */
union char_union
{
  jbyte **jb;
  jchar **jc;
  char **c;
};

JNIEXPORT void JNICALL
Java_gnu_java_nio_charset_iconv_IconvDecoder_openIconv (JNIEnv * env UNUSED,
							jobject obj UNUSED,
							jstring jname UNUSED)
{
#if defined(HAVE_ICONV)
  iconv_t iconv_object;
  jclass cls;

  const char *name = JCL_jstring_to_cstring (env, jname);
  if (name == NULL)
    return;

  /* Cache fieldIDs for use in decode function. */
  if (infid == NULL || outfid == NULL)
    {
      cls = (*env)->GetObjectClass (env, obj);
      infid = (*env)->GetFieldID (env, cls, "inremaining", "I");
      assert (infid != 0);
      outfid = (*env)->GetFieldID (env, cls, "outremaining", "I");
      assert (outfid != 0);
    }

  /* to java from "name", native java format depends on endianness */
#ifdef WORDS_BIGENDIAN
  iconv_object = iconv_open ("UTF-16BE", name);
#else
  iconv_object = iconv_open ("UTF-16LE", name);
#endif

  JCL_free_cstring (env, jname, name);
  if ((long) iconv_object == -1L)
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Charset not available");
      return;
    }
  createRawData (env, obj, (void *) iconv_object);
#else
  JCL_ThrowException (env, "java/lang/IllegalArgumentException",
		      "iconv not available");
#endif
}

JNIEXPORT jint JNICALL
Java_gnu_java_nio_charset_iconv_IconvDecoder_decode (JNIEnv * env UNUSED,
						     jobject obj UNUSED,
						     jbyteArray inArr UNUSED,
						     jcharArray outArr UNUSED,
						     jint posIn UNUSED,
                                                     jint remIn UNUSED,
						     jint posOut UNUSED,
                                                     jint remOut UNUSED)
{
#if defined(HAVE_ICONV)
  iconv_t iconv_object = getData (env, obj);
  size_t retval;
  union char_union in, out;
  jbyte *input, *inputcopy;
  jchar *output, *outputcopy;
  size_t lenIn = (size_t) remIn;
  size_t lenOut = (size_t) remOut * 2;

  inputcopy = input = (*env)->GetByteArrayElements (env, inArr, 0);
  outputcopy = output = (*env)->GetCharArrayElements (env, outArr, 0);

  input += posIn;
  output += posOut;

  in.jb = &input;
  out.jc = &output;
  retval = iconv (iconv_object, (ICONV_CONST char **) in.c, &lenIn,
		  out.c, &lenOut);

  /* XXX: Do we need to relase the input array? It's not modified. */
  (*env)->ReleaseByteArrayElements (env, inArr, inputcopy, 0);
  (*env)->ReleaseCharArrayElements (env, outArr, outputcopy, 0);

  if (retval == (size_t) (-1))
    {
      if (errno == EILSEQ)
	retval = 1;
      else
	retval = 0;
    }
  else
    retval = 0;

  (*env)->SetIntField (env, obj, infid, (jint) lenIn);
  (*env)->SetIntField (env, obj, outfid, (jint) (lenOut >> 1));

  return (jint) retval;
#else
  return -1;
#endif
}

JNIEXPORT void JNICALL
Java_gnu_java_nio_charset_iconv_IconvDecoder_closeIconv (JNIEnv * env UNUSED,
							 jobject obj UNUSED)
{
#if defined(HAVE_ICONV)
  iconv_t iconv_object;
  iconv_object = getData (env, obj);
  iconv_close (iconv_object);
#endif
}


#if defined(HAVE_ICONV)
static void
createRawData (JNIEnv * env, jobject obj, void *ptr)
{
  jclass cls;
  jobject data;
  jfieldID data_fid;

  cls = (*env)->GetObjectClass (env, obj);
  data_fid = (*env)->GetFieldID (env, cls, "data", "Lgnu/classpath/Pointer;");
  assert (data_fid != 0);

  data = JCL_NewRawDataObject(env, ptr);

  (*env)->SetObjectField (env, obj, data_fid, data);
}

static void *
getData (JNIEnv * env, jobject obj)
{
  jclass cls;
  jfieldID data_fid;
  jobject data;

  cls = (*env)->GetObjectClass (env, obj);
  data_fid = (*env)->GetFieldID (env, cls, "data", "Lgnu/classpath/Pointer;");
  assert (data_fid != 0);
  data = (*env)->GetObjectField (env, obj, data_fid);

  return JCL_GetRawData(env, data);
}
#endif

