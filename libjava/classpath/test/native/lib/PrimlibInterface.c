#include "PrimlibInterface.h"
#include <primlib.h>

/*
 * Class:     PrimlibInterface
 * Method:    unwrapBoolean
 * Signature: (Ljava/lang/Object;)Z
 */
JNIEXPORT jboolean JNICALL Java_PrimlibInterface_unwrapBoolean
(JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapBoolean(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    unwrapByte
 * Signature: (Ljava/lang/Object;)B
 */
JNIEXPORT jbyte JNICALL Java_PrimlibInterface_unwrapByte
  (JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapByte(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    unwrapShort
 * Signature: (Ljava/lang/Object;)S
 */
JNIEXPORT jshort JNICALL Java_PrimlibInterface_unwrapShort
  (JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapShort(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    unwrapChar
 * Signature: (Ljava/lang/Object;)C
 */
JNIEXPORT jchar JNICALL Java_PrimlibInterface_unwrapChar
  (JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapChar(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    unwrapInt
 * Signature: (Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL Java_PrimlibInterface_unwrapInt
  (JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapInt(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    unwrapLong
 * Signature: (Ljava/lang/Object;)J
 */
JNIEXPORT jlong JNICALL Java_PrimlibInterface_unwrapLong
  (JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapLong(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    unwrapFloat
 * Signature: (Ljava/lang/Object;)F
 */
JNIEXPORT jfloat JNICALL Java_PrimlibInterface_unwrapFloat
  (JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapFloat(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    unwrapDouble
 * Signature: (Ljava/lang/Object;)D
 */
JNIEXPORT jdouble JNICALL Java_PrimlibInterface_unwrapDouble
  (JNIEnv * env, jclass thisClass, jobject o) {
	return PRIMLIB_UnwrapDouble(env, o);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapBoolean
 * Signature: (Z)Ljava/lang/Boolean;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapBoolean
(JNIEnv * env, jclass thisClass, jboolean val) {
	return PRIMLIB_WrapBoolean(env, val);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapByte
 * Signature: (B)Ljava/lang/Byte;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapByte
(JNIEnv * env, jclass thisClass, jbyte val) {
	return PRIMLIB_WrapByte(env, val);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapShort
 * Signature: (S)Ljava/lang/Short;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapShort
(JNIEnv * env, jclass thisClass, jshort val) {
	return PRIMLIB_WrapShort(env, val);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapChar
 * Signature: (C)Ljava/lang/Character;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapChar
(JNIEnv * env, jclass thisClass, jchar val) {
	return PRIMLIB_WrapChar(env, val);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapInt
 * Signature: (I)Ljava/lang/Integer;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapInt
(JNIEnv * env, jclass thisClass, jint val) {
	return PRIMLIB_WrapInt(env, val);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapLong
 * Signature: (J)Ljava/lang/Long;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapLong
(JNIEnv * env, jclass thisClass, jlong val) {
	return PRIMLIB_WrapLong(env, val);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapFloat
 * Signature: (F)Ljava/lang/Float;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapFloat
(JNIEnv * env, jclass thisClass, jfloat val) {
	return PRIMLIB_WrapFloat(env, val);
}

/*
 * Class:     PrimlibInterface
 * Method:    wrapDouble
 * Signature: (D)Ljava/lang/Double;
 */
JNIEXPORT jobject JNICALL Java_PrimlibInterface_wrapDouble
(JNIEnv * env, jclass thisClass, jdouble val) {
	return PRIMLIB_WrapDouble(env, val);
}
