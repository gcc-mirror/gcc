#include <gcj/cni.h>

#include <jvm.h>
#include <jvmti.h>
#include <stdio.h>

#include <java/lang/Object.h>

#include "getmethodname.h"

static void
print_error (jvmtiEnv *env, const char *msg,  jvmtiError err)
{
  char *error_msg;
  env->GetErrorName (err, &error_msg);
  printf ("%s: %s\n", msg, error_msg);
  env->Deallocate (reinterpret_cast<unsigned char *> (error_msg));
}

#define NUM_METHODS 8
static const char *function_names[] = { "clone",
					"equals",
					"finalize",
					"getClass",
					"hashCode",
					"notify",
					"notifyAll",
					"toString" };
static int
function_index (const char *name)
{
  for (int i = 0; i < NUM_METHODS; ++i)
    {
      if (strcmp (function_names[i], name) == 0)
	return i;
    }

  return -1;
}

void
getmethodname::do_getmethodname_tests ()
{
  jvmtiEnv *env;
  JavaVM *vm = _Jv_GetJavaVM ();
  vm->GetEnv (reinterpret_cast<void **> (&env), JVMTI_VERSION_1_0);

  jvmtiError err;
  err = env->GetMethodName (reinterpret_cast<jmethodID> (NULL),
			    reinterpret_cast<char **> (NULL),
			    reinterpret_cast<char **> (NULL),
			    reinterpret_cast<char **> (NULL));
  print_error (env, "null jmethodID", err);

  jint count;
  jmethodID *methods;
  err = env->GetClassMethods (&java::lang::Object::class$, &count, &methods);
  print_error (env, "GetClassMethods", err);

  char *names[NUM_METHODS], *solo_names[NUM_METHODS];
  char *signatures[NUM_METHODS], *solo_signatures[NUM_METHODS];
  char *generics[NUM_METHODS], *solo_generics[NUM_METHODS];

  for (jint i = 0; i < count; ++i)
    {
      char *name, *n;
      char *signature, *s;
      char *generic, *g;
      err = env->GetMethodName (methods[i], &name, &signature, &generic);

      int idx = -1;
      if (err != JVMTI_ERROR_NONE)
	{
	  print_error (env, "GetMethodName - all fields", err);
	  continue;
	}

      idx = function_index (name);
      if (idx == -1)
	continue;

      names[idx] = name;
      signatures[idx] = signature;
      generics[idx] = generic;

      err = env->GetMethodName (methods[i], &n, NULL, NULL);
      print_error (env, "GetMethodName - name", err);
      solo_names[idx] = n;

      err = env->GetMethodName (methods[i], NULL, &s, NULL);
      print_error (env, "GetMethodName - signature", err);
      solo_signatures[idx] = s;

      err = env->GetMethodName (methods[i], NULL, NULL, &g);
      print_error (env, "GetMethodName - generic", err);
      solo_generics[idx] = g;
    }

#define WRAP(X) ((X) == NULL ? "null" : (X))
#define MATCH(X,Y) (strcmp ((X),(Y)) == 0 ? "match" : "do not match")
  for (int i = 0; i < NUM_METHODS; ++i)
    {
      printf ("name=%s, signature=%s, generic=%s\n",
	      WRAP (names[i]), WRAP (signatures[i]), WRAP (generics[i]));
      printf ("names %s\n", MATCH (solo_names[i], names[i]));
      printf ("signatures %s\n", MATCH (solo_signatures[i], signatures[i]));
      printf ("generic %s\n", "not yet");

      env->Deallocate (reinterpret_cast<unsigned char *> (names[i]));
      env->Deallocate (reinterpret_cast<unsigned char *> (solo_names[i]));
      env->Deallocate (reinterpret_cast<unsigned char *> (signatures[i]));
      env->Deallocate (reinterpret_cast<unsigned char *> (solo_signatures[i]));
      env->Deallocate (reinterpret_cast<unsigned char *> (generics[i]));
      env->Deallocate (reinterpret_cast<unsigned char *> (solo_generics[i]));
    }
}
