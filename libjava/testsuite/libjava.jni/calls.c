#include <stdio.h>
#include <calls.h>

JNIEXPORT jint JNICALL
Java_calls_docall (JNIEnv *env, jobject _this)
{
  jmethodID method;
  jclass klass, super;

  jbyte b;
  jshort s;
  jchar c;
  jint i;
  jlong l;
  jfloat f;
  jdouble d;

  jvalue val;

  jint fails = 0;

  klass = (*env)->GetObjectClass (env, _this);
  super = (*env)->GetSuperclass (env, klass);

  method = (*env)->GetMethodID (env, klass, "byte_f", "()B");
  b = (*env)->CallByteMethod (env, _this, method);
  if (b != 23)
    ++fails;

  method = (*env)->GetMethodID (env, klass, "char_f", "(I)C");
  val.i = 10;
  c = (*env)->CallCharMethodA (env, _this, method, &val);
  if (c != ('a' + 10))
    ++fails;

  method = (*env)->GetMethodID (env, super, "int_f", "()I");
  i = (*env)->CallNonvirtualIntMethod (env, _this, super, method);
  if (i != 27)
    ++fails;

  i = (*env)->CallIntMethod (env, _this, method);
  if (i != 1023)
    ++fails;

  method = (*env)->GetStaticMethodID (env, klass, "long_f", "(J)J");
  l = (*env)->CallStaticLongMethod (env, klass, method, (jlong) 10);
  if (l != 2033)
    ++fails;

  method = (*env)->GetStaticMethodID (env, klass, "longpb_f", "(BJBJBJ)J");
  l = (*env)->CallStaticLongMethod (env, klass, method, (jbyte) 13, (jlong) 3,
		  		   (jbyte) 13, (jlong) 3, (jbyte) 13, (jlong) 4);
  if (l != 3033)
    ++fails;

  method = (*env)->GetMethodID (env, klass, "void_f", "()V");
  (*env)->CallVoidMethod (env, _this, method);

  method = (*env)->GetStaticMethodID (env, klass, "short_f", "()S");
  s = (*env)->CallStaticShortMethod (env, klass, method);
  if (s != 2)
    ++fails;

  method = (*env)->GetMethodID (env, klass, "double_f", "()D");
  d = (*env)->CallDoubleMethod (env, _this, method);
  if (d != -1.0)
    ++fails;

  method = (*env)->GetMethodID (env, klass, "float_f", "()F");
  f = (*env)->CallFloatMethod (env, _this, method);
  if (f != 1.0)
    ++fails;

  return fails;
}
