#include "MethodTester.h"
#include <jnilink.h>
#include <jcl.h>

static linkPtr t1_1=NULL;
static linkPtr t1_2=NULL;
static linkPtr t2=NULL;
static linkPtr t3=NULL;
static linkPtr t4=NULL;
static linkPtr t5=NULL;
static linkPtr t6=NULL;

/*
 * Class:     MethodTester
 * Method:    test1
 * Signature: (LData1;LData2;)Z
 */
JNIEXPORT jboolean JNICALL Java_MethodTester_test1
(JNIEnv * env, jobject thisObj, jobject d1, jobject d2) {
	if(LINK_LinkClass(env,&t1_1,"Data1") == NULL || LINK_LinkClass(env,&t1_2,"Data2") == NULL)
		return JNI_FALSE;
	if(!(*env)->IsAssignableFrom(env, LINK_ResolveClass(env,t1_1), LINK_ResolveClass(env,t1_2))
		&& (*env)->IsAssignableFrom(env, LINK_ResolveClass(env,t1_2), LINK_ResolveClass(env,t1_1))) {
		return JNI_TRUE;
	} else {
		return JNI_FALSE;
	}
}

/*
 * Class:     MethodTester
 * Method:    test2
 * Signature: (LData1;LData2;)Z
 */
JNIEXPORT jboolean JNICALL Java_MethodTester_test2
(JNIEnv * env, jobject thisObj, jobject d1, jobject d2) {
	jclass c1 = (*env)->GetObjectClass(env,d1);
	jclass c2 = (*env)->GetObjectClass(env,d2);
	if(LINK_LinkField(env,&t2,c1,"instanceVar","Z") == NULL)
		return JNI_FALSE;
	return (*env)->GetBooleanField(env,d1,LINK_ResolveField(env,t2))
		&& (*env)->GetBooleanField(env,d2,LINK_ResolveField(env,t2));
}

/*
 * Class:     MethodTester
 * Method:    test3
 * Signature: (LData1;LData2;)Z
 */
JNIEXPORT jboolean JNICALL Java_MethodTester_test3
  (JNIEnv * env, jobject thisObj, jobject d1, jobject d2) {
	jclass c1 = (*env)->GetObjectClass(env,d1);
	jclass c2 = (*env)->GetObjectClass(env,d2);
	if(LINK_LinkStaticField(env,&t3,c1,"staticVar","Z") == NULL)
		return JNI_FALSE;
	return (*env)->GetStaticBooleanField(env,d1,LINK_ResolveStaticField(env,t3))
		&& (*env)->GetStaticBooleanField(env,d2,LINK_ResolveStaticField(env,t3));
}

/*
 * Class:     MethodTester
 * Method:    test4
 * Signature: (LData1;LData2;)Z
 */
JNIEXPORT jboolean JNICALL Java_MethodTester_test4
  (JNIEnv * env, jobject thisObj, jobject d1, jobject d2) {
	jclass c1 = (*env)->GetObjectClass(env,d1);
	jclass c2 = (*env)->GetObjectClass(env,d2);
	jmethodID m1;
	jmethodID m2;
	if(LINK_LinkMethod(env,&t4,c1,"instanceMethod","()Z") == NULL)
		return JNI_FALSE;
	m1 = LINK_ResolveMethod(env,t4);
	m2 = LINK_ResolveMethod(env,t4);
	return (*env)->CallBooleanMethod(env,d1,m1)
		&& !(*env)->CallBooleanMethod(env,d2,m2);
}


/*
 * Class:     MethodTester
 * Method:    test5
 * Signature: (LData1;LData2;)Z
 */
JNIEXPORT jboolean JNICALL Java_MethodTester_test5
  (JNIEnv * env, jobject thisObj, jobject d1, jobject d2) {
	jclass c1 = (*env)->GetObjectClass(env,d1);
	jclass c2 = (*env)->GetObjectClass(env,d2);
	if(LINK_LinkStaticMethod(env,&t5,c1,"staticMethod","()Z") == NULL)
		return JNI_FALSE;
	return (*env)->CallStaticBooleanMethod(env,c1,LINK_ResolveStaticMethod(env,t5))
		&& (*env)->CallStaticBooleanMethod(env,c2,LINK_ResolveStaticMethod(env,t5));
}


/*
 * Class:     MethodTester
 * Method:    test6
 * Signature: (LData1;LData2;)Z
 */
JNIEXPORT jboolean JNICALL Java_MethodTester_test6
  (JNIEnv * env, jobject thisObj, jobject d1, jobject d2) {
	jclass c1 = (*env)->GetObjectClass(env,d1);
	jclass c2 = (*env)->GetObjectClass(env,d2);
	if(LINK_LinkMethod(env,&t6,c1,"finalMethod","()Z") == NULL)
		return JNI_FALSE;
	return (*env)->CallBooleanMethod(env,d1,LINK_ResolveMethod(env,t6))
		&& !(*env)->CallBooleanMethod(env,d2,LINK_ResolveMethod(env,t6));
}

