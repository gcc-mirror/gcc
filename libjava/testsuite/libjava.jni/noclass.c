#include <noclass.h>

void
Java_noclass_find_1it (JNIEnv *env, jclass k)
{
  /* We cause an exception by asking for a class we know does not
     exist.  */
  k = (*env)->FindClass (env, "java/lang/Sarcophagus");
}
