#include <stdio.h>
#include <pr11951.h>

JNIEXPORT void JNICALL
Java_pr11951_nmethod (JNIEnv *env, jclass myclass)
{
  jmethodID method;
  jobject r;

  method = (*env)->GetStaticMethodID (env, myclass, "dosomething",
				      "()Ljava/lang/Object;");
  r = (*env)->CallStaticObjectMethod (env, myclass, method);
  printf ("%d\n", r == NULL);

  (*env)->ExceptionClear (env);
}
