#include <jni.h>
#include <cxxtest.h>

jobjectArray
Java_cxxtest_fetch (JNIEnv *env, jobject _this)
{
  jclass cls;
  jfieldID fid;
  jobjectArray obj;

  cls = env->GetObjectClass (_this);
  if (! cls)
    return 0;

  fid = env->GetFieldID (cls, "F", "[Ljava/lang/Object;");
  if (! fid)
    return 0;

  obj = reinterpret_cast<jobjectArray> (env->GetObjectField (_this, fid));

  return obj;
}


