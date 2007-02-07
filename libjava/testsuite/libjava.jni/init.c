#include <jni.h>
#include <init$NativeClass.h>

JNIEXPORT void JNICALL
Java_init_00024NativeClass_printHello(JNIEnv *env, jclass cl)
{
	printf("hello\n");
}
