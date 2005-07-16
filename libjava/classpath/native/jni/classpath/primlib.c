/* primlib.c
   Copyright (C) 1998 Free Software Foundation, Inc.

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

#include <jnilink.h>
#include <primlib.h>
#include <jcl.h>

static jclass nativeWrapClass[PRIMLIB_NUMTYPES] =
  { NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL
};

static jclass nativeTypeClass[PRIMLIB_NUMTYPES] =
  { NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL
};

static jmethodID nativeWrapClassConstructor[PRIMLIB_NUMTYPES] =
  { NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL
};

static jmethodID nativeWrapClassAccessor[PRIMLIB_NUMTYPES] =
  { NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL
};

static const char *nativeWrapClassName[PRIMLIB_NUMTYPES] = {
  NULL,
  NULL,
  "java/lang/Boolean",
  "java/lang/Byte",
  "java/lang/Character",
  "java/lang/Short",
  "java/lang/Integer",
  "java/lang/Long",
  "java/lang/Float",
  "java/lang/Double",
  "java/lang/Void",
  NULL
};

static const char *nativeWrapClassConstructorSig[PRIMLIB_NUMTYPES] = {
  NULL,
  NULL,
  "(Z)V",
  "(B)V",
  "(C)V",
  "(S)V",
  "(I)V",
  "(J)V",
  "(F)V",
  "(D)V",
  "()V",
  NULL
};

static const char *nativeWrapClassAccessorName[PRIMLIB_NUMTYPES] = {
  NULL,
  NULL,
  "booleanValue",
  "byteValue",
  "charValue",
  "shortValue",
  "intValue",
  "longValue",
  "floatValue",
  "doubleValue",
  NULL,
  NULL
};

static const char *nativeWrapClassAccessorSig[PRIMLIB_NUMTYPES] = {
  NULL,
  NULL,
  "()Z",
  "()B",
  "()C",
  "()S",
  "()I",
  "()J",
  "()F",
  "()D",
  NULL,
  NULL
};


JNIEXPORT jclass JNICALL
PRIMLIB_GetNativeWrapClass (JNIEnv * env, int reflectType)
{
  return LINK_LinkClass (env, nativeWrapClass[reflectType],
			 nativeWrapClassName[reflectType]);
}

static jclass
ActuallyGetNativeTypeClass (JNIEnv * env, int reflectType)
{
  jclass wrapClass;
  jfieldID typeField;

  wrapClass = PRIMLIB_GetNativeWrapClass (env, reflectType);
  if (wrapClass == NULL)
    return NULL;
  typeField =
    (*env)->GetStaticFieldID (env, wrapClass, "TYPE", "Ljava/lang/Class");
  if (typeField == NULL)
    return NULL;
  return (*env)->GetStaticObjectField (env, wrapClass, typeField);
}

JNIEXPORT jclass JNICALL
PRIMLIB_GetNativeTypeClass (JNIEnv * env, int reflectType)
{
  return LINK_LinkKnownClass (env, nativeTypeClass[reflectType],
			      ActuallyGetNativeTypeClass (env, reflectType));
}

JNIEXPORT jmethodID JNICALL
PRIMLIB_GetNativeWrapClassConstructor (JNIEnv * env, int reflectType)
{
  PRIMLIB_GetNativeWrapClass (env, reflectType);
  return LINK_LinkConstructor (env, nativeWrapClassConstructor[reflectType],
			       nativeWrapClass[reflectType],
			       nativeWrapClassConstructorSig[reflectType]);
}

JNIEXPORT jmethodID JNICALL
PRIMLIB_GetNativeWrapClassAccessor (JNIEnv * env, int reflectType)
{
  PRIMLIB_GetNativeWrapClass (env, reflectType);
  return LINK_LinkMethod (env, nativeWrapClassAccessor[reflectType],
			  nativeWrapClass[reflectType],
			  nativeWrapClassAccessorName[reflectType],
			  nativeWrapClassAccessorSig[reflectType]);
}



JNIEXPORT jobject JNICALL
PRIMLIB_WrapBoolean (JNIEnv * env, jboolean b)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_BOOLEAN);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BOOLEAN),
			    construct, b);
}

JNIEXPORT jobject JNICALL
PRIMLIB_WrapByte (JNIEnv * env, jbyte b)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_BYTE);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE),
			    construct, b);
}

JNIEXPORT jobject JNICALL
PRIMLIB_WrapChar (JNIEnv * env, jchar c)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_CHAR);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_CHAR),
			    construct, c);
}

JNIEXPORT jobject JNICALL
PRIMLIB_WrapShort (JNIEnv * env, jshort s)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_SHORT);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_SHORT),
			    construct, s);
}

JNIEXPORT jobject JNICALL
PRIMLIB_WrapInt (JNIEnv * env, jint i)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_INT);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_INT),
			    construct, i);
}

JNIEXPORT jobject JNICALL
PRIMLIB_WrapLong (JNIEnv * env, jlong l)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_LONG);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_LONG),
			    construct, l);
}

JNIEXPORT jobject JNICALL
PRIMLIB_WrapFloat (JNIEnv * env, jfloat f)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_FLOAT);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_FLOAT),
			    construct, f);
}

JNIEXPORT jobject JNICALL
PRIMLIB_WrapDouble (JNIEnv * env, jdouble d)
{
  jmethodID construct =
    PRIMLIB_GetNativeWrapClassConstructor (env, PRIMLIB_DOUBLE);
  JCL_RETHROW_EXCEPTION (env);
  return (*env)->NewObject (env,
			    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_DOUBLE),
			    construct, d);
}


JNIEXPORT jboolean JNICALL
PRIMLIB_UnwrapBoolean (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj,
		    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BOOLEAN)))
    {
      return PRIMLIB_GetBooleanObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return JNI_FALSE;
    }
}

JNIEXPORT jbyte JNICALL
PRIMLIB_UnwrapByte (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj, PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE)))
    {
      return PRIMLIB_GetByteObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return 0;
    }
}

JNIEXPORT jshort JNICALL
PRIMLIB_UnwrapShort (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj,
		    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_SHORT)))
    {
      return PRIMLIB_GetShortObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE)))
    {
      return (jshort) PRIMLIB_GetByteObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return 0;
    }
}

JNIEXPORT jchar JNICALL
PRIMLIB_UnwrapChar (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj, PRIMLIB_GetNativeWrapClass (env, PRIMLIB_CHAR)))
    {
      return PRIMLIB_GetCharObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return 0;
    }
}

JNIEXPORT jint JNICALL
PRIMLIB_UnwrapInt (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj, PRIMLIB_GetNativeWrapClass (env, PRIMLIB_INT)))
    {
      return PRIMLIB_GetIntObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_SHORT)))
    {
      return (jint) PRIMLIB_GetShortObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_CHAR)))
    {
      return (jint) PRIMLIB_GetCharObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE)))
    {
      return (jint) PRIMLIB_GetByteObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return 0;
    }
}

JNIEXPORT jlong JNICALL
PRIMLIB_UnwrapLong (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj, PRIMLIB_GetNativeWrapClass (env, PRIMLIB_LONG)))
    {
      return PRIMLIB_GetLongObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_INT)))
    {
      return (jlong) PRIMLIB_GetIntObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_SHORT)))
    {
      return (jlong) PRIMLIB_GetShortObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_CHAR)))
    {
      return (jlong) PRIMLIB_GetCharObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE)))
    {
      return (jlong) PRIMLIB_GetByteObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return 0;
    }
}

JNIEXPORT jfloat JNICALL
PRIMLIB_UnwrapFloat (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj,
		    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_FLOAT)))
    {
      return PRIMLIB_GetFloatObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_LONG)))
    {
      return (jfloat) PRIMLIB_GetLongObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_INT)))
    {
      return (jfloat) PRIMLIB_GetIntObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_SHORT)))
    {
      return (jfloat) PRIMLIB_GetShortObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_CHAR)))
    {
      return (jfloat) PRIMLIB_GetCharObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE)))
    {
      return (jfloat) PRIMLIB_GetByteObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return 0;
    }
}

JNIEXPORT jdouble JNICALL
PRIMLIB_UnwrapDouble (JNIEnv * env, jobject obj)
{
  if ((*env)->
      IsInstanceOf (env, obj,
		    PRIMLIB_GetNativeWrapClass (env, PRIMLIB_DOUBLE)))
    {
      return PRIMLIB_GetDoubleObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_FLOAT)))
    {
      return (jdouble) PRIMLIB_GetFloatObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_LONG)))
    {
      return (jdouble) PRIMLIB_GetLongObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_INT)))
    {
      return (jdouble) PRIMLIB_GetIntObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_SHORT)))
    {
      return (jdouble) PRIMLIB_GetShortObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_CHAR)))
    {
      return (jdouble) PRIMLIB_GetCharObjectValue (env, obj);
    }
  else if ((*env)->
	   IsInstanceOf (env, obj,
			 PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE)))
    {
      return (jdouble) PRIMLIB_GetByteObjectValue (env, obj);
    }
  else
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "Argument not of correct type.");
      return 0;
    }
}

JNIEXPORT jint JNICALL
PRIMLIB_GetReflectiveWrapperType (JNIEnv * env, jobject obj)
{
  jclass typeClass;
  if (obj == NULL)
    {
      return PRIMLIB_NULL;
    }

  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_DOUBLE);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_DOUBLE;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_FLOAT);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_FLOAT;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_LONG);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_LONG;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_INT);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_INT;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_CHAR);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_CHAR;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_SHORT);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_SHORT;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BYTE);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_BYTE;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_BOOLEAN);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_BOOLEAN;
    }
  typeClass = PRIMLIB_GetNativeWrapClass (env, PRIMLIB_VOID);
  if ((*env)->IsInstanceOf (env, obj, typeClass))
    {
      return PRIMLIB_VOID;
    }
  return PRIMLIB_OBJECT;
}

JNIEXPORT jint JNICALL
PRIMLIB_GetReflectiveType (JNIEnv * env, jclass returnType)
{
  jclass typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_DOUBLE);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_DOUBLE;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_FLOAT);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_FLOAT;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_LONG);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_LONG;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_INT);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_INT;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_CHAR);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_CHAR;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_SHORT);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_SHORT;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_BYTE);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_BYTE;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_BOOLEAN);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_BOOLEAN;
    }
  typeClass = PRIMLIB_GetNativeTypeClass (env, PRIMLIB_VOID);
  if ((*env)->IsAssignableFrom (env, returnType, typeClass))
    {
      return PRIMLIB_VOID;
    }
  return PRIMLIB_OBJECT;
}


JNIEXPORT jboolean JNICALL
PRIMLIB_GetBooleanObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_BOOLEAN);
  return (*env)->CallBooleanMethod (env, obj, acc);
}

JNIEXPORT jbyte JNICALL
PRIMLIB_GetByteObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_BYTE);
  return (*env)->CallByteMethod (env, obj, acc);
}

JNIEXPORT jshort JNICALL
PRIMLIB_GetShortObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_SHORT);
  return (*env)->CallShortMethod (env, obj, acc);
}

JNIEXPORT jchar JNICALL
PRIMLIB_GetCharObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_CHAR);
  return (*env)->CallCharMethod (env, obj, acc);
}

JNIEXPORT jint JNICALL
PRIMLIB_GetIntObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_INT);
  return (*env)->CallIntMethod (env, obj, acc);
}

JNIEXPORT jlong JNICALL
PRIMLIB_GetLongObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_LONG);
  return (*env)->CallLongMethod (env, obj, acc);
}

JNIEXPORT jfloat JNICALL
PRIMLIB_GetFloatObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_FLOAT);
  return (*env)->CallFloatMethod (env, obj, acc);
}

JNIEXPORT jdouble JNICALL
PRIMLIB_GetDoubleObjectValue (JNIEnv * env, jobject obj)
{
  jmethodID acc = PRIMLIB_GetNativeWrapClassAccessor (env, PRIMLIB_DOUBLE);
  return (*env)->CallDoubleMethod (env, obj, acc);
}



JNIEXPORT jvalue JNICALL
PRIMLIB_UnwrapJValue (JNIEnv * env, jobject obj, jclass classType)
{
  jvalue retval;
  jint objType = PRIMLIB_GetReflectiveType (env, classType);
  if (objType == PRIMLIB_BOOLEAN)
    {
      retval.z = PRIMLIB_UnwrapBoolean (env, obj);
    }
  else if (objType == PRIMLIB_BYTE)
    {
      retval.b = PRIMLIB_UnwrapByte (env, obj);
    }
  else if (objType == PRIMLIB_CHAR)
    {
      retval.c = PRIMLIB_UnwrapChar (env, obj);
    }
  else if (objType == PRIMLIB_SHORT)
    {
      retval.s = PRIMLIB_UnwrapShort (env, obj);
    }
  else if (objType == PRIMLIB_INT)
    {
      retval.i = PRIMLIB_UnwrapInt (env, obj);
    }
  else if (objType == PRIMLIB_LONG)
    {
      retval.j = PRIMLIB_UnwrapLong (env, obj);
    }
  else if (objType == PRIMLIB_FLOAT)
    {
      retval.f = PRIMLIB_UnwrapFloat (env, obj);
    }
  else if (objType == PRIMLIB_DOUBLE)
    {
      retval.d = PRIMLIB_UnwrapDouble (env, obj);
    }
  else
    {
      if (obj != NULL && !(*env)->IsInstanceOf (env, obj, classType))
	{
	  JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			      "Argument not of correct object type.");
	  return retval;
	}
      retval.l = obj;
    }
  return retval;
}
