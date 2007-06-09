#include <jvmti.h>

JNIEXPORT jint JNICALL 
Agent_OnLoad (JavaVM *vm, char *options, void *reserved)
{
  // nothing -- this is just a stub to get JVMTI properly
  // initialized
  return 0;
}

