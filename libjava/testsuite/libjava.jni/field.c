#include <jni.h>
#include <field.h>

JNIEXPORT jobjectArray JNICALL
Java_field_fetch (JNIEnv *env, jobject this)
{
  jclass cls;
  jfieldID fid;
  jobjectArray obj;

  cls = (*env)->GetObjectClass (env, this);
  if (! cls)
    return 0;

  fid = (*env)->GetFieldID (env, cls, "F", "[Ljava/lang/Object;");
  if (! fid)
    return 0;

  obj = (*env)->GetObjectField (env, this, fid);

  return obj;
}


