#include <stdlib.h>
#include <stdio.h>
#include <iface.h>

void check (JNIEnv *);

void check(JNIEnv *env)
{
  if ((*env)->ExceptionCheck(env) != JNI_FALSE)
    {
      fprintf(stderr, "UNEXPECTED EXCEPTION\n");
      exit(-1);
    }
}

void
Java_iface_doCalls (JNIEnv *env, jobject self, jobject other)
{
  jclass iface_class, comparable_class;
  jmethodID iface_meth, comparable_meth;
  jvalue args[1];

  iface_class = (*env)->FindClass(env, "iface");
  check (env);
  comparable_class = (*env)->FindClass (env, "mycomp");
  check (env);

  iface_meth = (*env)->GetMethodID (env, iface_class, "compareTo",
				    "(Ljava/lang/Object;)I");
  check (env);
  comparable_meth = (*env)->GetMethodID (env, comparable_class, "compareTo",
					 "(Ljava/lang/Object;)I");
  check (env);

  args[0].l = other;
  (*env)->CallObjectMethodA (env, self, iface_meth, args);
  check (env);
  (*env)->CallObjectMethodA (env, self, comparable_meth, args);
  check (env);
}
