#include <stdlib.h>
#include <throwit.h>

void
Java_throwit_throwit (JNIEnv *env, jclass klass, jstring name,
		      jboolean is_new)
{
  const char *buf = (*env)->GetStringUTFChars (env, name, NULL);
  jclass k = (*env)->FindClass (env, buf);
  (*env)->ReleaseStringUTFChars (env, name, buf);

  if (k == NULL || (*env)->ExceptionCheck (env))
    return;

  if (is_new)
    (*env)->ThrowNew (env, k, "the word is zardoz");
  else
    {
      jmethodID id = (*env)->GetMethodID (env, k, "<init>",
					  "(Ljava.lang.String;)V");
      jstring z = (*env)->NewStringUTF (env, "zardoz is the word");
      jobject obj = (*env)->NewObject (env, k, id, z);
      (*env)->Throw (env, obj);
    }
}
