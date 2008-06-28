/* java_io_VMObjectStreamClass.c -- Native methods for VMObjectStreamClass.java
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


#include <jni.h>
#include <jcl.h>

#include <stdlib.h>
#include <string.h>

#include "java_io_VMObjectStreamClass.h"

/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    hasClassInitializer
 * Signature: (Ljava/lang/Class;)Z
 */
JNIEXPORT jboolean JNICALL
Java_java_io_VMObjectStreamClass_hasClassInitializer (JNIEnv * env,
						      jclass vmosklass
						      __attribute__ ((__unused__)), jclass klass)
{
  jmethodID mid = (*env)->GetStaticMethodID (env, klass, "<clinit>", "()V");
  if (mid == NULL)
    {
      (*env)->ExceptionClear (env);
      return JNI_FALSE;
    }
  return JNI_TRUE;
}

static void
throwInternalError (JNIEnv * env)
{
  jclass internalErrorClass;
  jthrowable previousException, newException;
  jmethodID initException, getMessageID, initCauseID;
  jstring message;

  internalErrorClass = (*env)->FindClass (env, "java/lang/InternalError");
  /** Just give up if this also fails. */
  if (internalErrorClass == NULL)
    return;

  previousException = (*env)->ExceptionOccurred (env);

  if (previousException == NULL)
    {
      (*env)->ThrowNew (env, internalErrorClass,
			"Unknown error raised by the VM");
      return;
    }

  initException = (*env)->GetMethodID
    (env, internalErrorClass, "<init>", "(Ljava/lang/String;)V");
  getMessageID = (*env)->GetMethodID
    (env, (*env)->GetObjectClass (env, previousException),
     "getMessage", "()Ljava/lang/String;");
  initCauseID = (*env)->GetMethodID
    (env, internalErrorClass, "initCause", "(Ljava/lang/Throwable;)V");

  message = (*env)->CallObjectMethod (env, previousException, getMessageID);

  newException = (*env)->NewObject (env, internalErrorClass, initException,
				    message);
  (*env)->CallVoidMethod (env, newException, initCauseID, previousException);

  (*env)->ExceptionClear (env);
  (*env)->Throw (env, newException);
}

static jfieldID
getFieldReference (JNIEnv * env, jobject field, const char *type)
{
  jclass classClass;
  jclass fieldClass;
  jclass declaringClass;
  jclass typeClass;
  jfieldID fid;
  const char *field_name;
  const char *type_name;
  int type_len;
  jmethodID mid;
  jstring name;
  jstring tname;
  int i;

  fieldClass = (*env)->GetObjectClass (env, field);

  mid =
    (*env)->GetMethodID (env, fieldClass, "getName", "()Ljava/lang/String;");
  if (mid == NULL || (*env)->ExceptionOccurred (env) != NULL)
    {
      throwInternalError (env);
      return NULL;
    }

  name = (*env)->CallObjectMethod (env, field, mid);
  field_name = (*env)->GetStringUTFChars (env, name, NULL);

  mid = (*env)->GetMethodID (env, fieldClass,
			     "getDeclaringClass", "()Ljava/lang/Class;");
  if (mid == NULL || (*env)->ExceptionOccurred (env) != NULL)
    {
      throwInternalError (env);
      return NULL;
    }

  declaringClass = (*env)->CallObjectMethod (env, field, mid);

  /* Do we need to find out the exact type descriptor of the field? */
  if (type == NULL)
    {
      char *the_type;

      mid = (*env)->GetMethodID (env, fieldClass,
				 "getType", "()Ljava/lang/Class;");

      if (mid == NULL || (*env)->ExceptionOccurred (env) != NULL)
	{
	  throwInternalError (env);
	  return NULL;
	}

      typeClass = (*env)->CallObjectMethod (env, field, mid);
      classClass = (*env)->FindClass (env, "java/lang/Class");

      mid = (*env)->GetMethodID (env, classClass,
				 "getName", "()Ljava/lang/String;");

      if (mid == NULL || (*env)->ExceptionOccurred (env) != NULL)
	{
	  throwInternalError (env);
	  return NULL;
	}

      tname = (*env)->CallObjectMethod (env, typeClass, mid);
      type_name = (*env)->GetStringUTFChars (env, tname, NULL);

      /*
       * If it isn't an array class then the actual field type descriptor
       * starts with 'L', ends with ';' and has '/' instead of '.'.
       */
      type_len = strlen (type_name);
      if (type_name[0] != '[')
	{
	  /* XXX - FIXME - should not use dynamic allocation in core lib. */
	  the_type = (char *) malloc (type_len + 3);
	  the_type[0] = 'L';
	  the_type[type_len + 1] = ';';
	  the_type[type_len + 2] = '\0';
	  the_type++;
	}
      else
	{
	  /* XXX - FIXME - should not use dynamic allocation in core lib. */
	  the_type = (char *) malloc (type_len + 1);
	  the_type[type_len] = '\0';
	}

      for (i = 0; i < type_len; i++)
	if (type_name[i] == '.')
	  the_type[i] = '/';
	else
	  the_type[i] = type_name[i];

      if (type_name[0] != '[')
	the_type--;

      (*env)->ReleaseStringUTFChars (env, tname, type_name);
      fid = (*env)->GetFieldID (env, declaringClass, field_name, the_type);
      free (the_type);
    }
  else
    {
      type_len = -1;
      fid = (*env)->GetFieldID (env, declaringClass, field_name, type);
    }

  if (fid == NULL)
    {
      throwInternalError (env);
      return NULL;
    }
  (*env)->ReleaseStringUTFChars (env, name, field_name);

  return fid;
}

/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setBooleanNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;Z)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setBooleanNative (JNIEnv * env,
						   jclass vmosklass
						   __attribute__ ((__unused__)), jobject field, jobject object, jboolean value)
{
  jfieldID fid = getFieldReference (env, field, "Z");

  if (fid != NULL)
    (*env)->SetBooleanField (env, object, fid, value);
}

/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setCharNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;C)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setCharNative (JNIEnv * env,
						jclass vmosklass
						__attribute__ ((__unused__)),
						jobject field,
						jobject object, jchar value)
{
  jfieldID fid = getFieldReference (env, field, "C");

  if (fid != NULL)
    (*env)->SetCharField (env, object, fid, value);
}

/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setByteNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;B)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setByteNative (JNIEnv * env,
						jclass vmosklass
						__attribute__ ((__unused__)),
						jobject field,
						jobject object, jbyte value)
{
  jfieldID fid = getFieldReference (env, field, "B");

  if (fid != NULL)
    (*env)->SetByteField (env, object, fid, value);
}


/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setShortNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;S)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setShortNative (JNIEnv * env,
						 jclass vmosklass
						 __attribute__ ((__unused__)),
						 jobject field,
						 jobject object, jshort value)
{
  jfieldID fid = getFieldReference (env, field, "S");

  if (fid != NULL)
    (*env)->SetShortField (env, object, fid, value);
}

/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setIntNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;I)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setIntNative (JNIEnv * env,
					       jclass vmosklass
					       __attribute__ ((__unused__)),
					       jobject field,
					       jobject object, jint value)
{
  jfieldID fid = getFieldReference (env, field, "I");

  if (fid != NULL)
    (*env)->SetIntField (env, object, fid, value);
}


/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setLongNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;J)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setLongNative (JNIEnv * env,
						jclass vmosklass
						__attribute__ ((__unused__)),
						jobject field,
						jobject object, jlong value)
{
  jfieldID fid = getFieldReference (env, field, "J");

  if (fid != NULL)
    (*env)->SetLongField (env, object, fid, value);
}


/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setFloatNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;F)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setFloatNative (JNIEnv * env,
						 jclass vmosklass
						 __attribute__ ((__unused__)),
						 jobject field,
						 jobject object, jfloat value)
{
  jfieldID fid = getFieldReference (env, field, "F");

  if (fid != NULL)
    (*env)->SetFloatField (env, object, fid, value);
}

/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setDoubleNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;D)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setDoubleNative (JNIEnv * env,
						  jclass vmosklass
						  __attribute__ ((__unused__)), jobject field, jobject object, jdouble value)
{
  jfieldID fid = getFieldReference (env, field, "D");

  if (fid != NULL)
    (*env)->SetDoubleField (env, object, fid, value);
}

/*
 * Class:     java_io_VMObjectOutputStream
 * Method:    setObjectNative
 * Signature: (Ljava/lang/reflect/Field;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_java_io_VMObjectStreamClass_setObjectNative (JNIEnv * env,
						  jclass vmosklass
						  __attribute__ ((__unused__)), jobject field, jobject object, jobject value)
{
  jfieldID fid = getFieldReference (env, field, NULL);

  if (fid != NULL)
    (*env)->SetObjectField (env, object, fid, value);
}
