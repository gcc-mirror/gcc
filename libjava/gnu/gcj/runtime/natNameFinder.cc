// natNameFinder.cc - native helper methods for NameFinder.java

/* Copyright (C) 2002, 2003  Free Software Foundation, Inc

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Mark Wielaard (mark@klomp.org)
 * Based on the old name-finder.cc by Andrew Haley <aph@cygnus.com>.
 */

#include <config.h>

#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/String.h>
#include <java/lang/StackTraceElement.h>
#include <java/lang/StringBuffer.h>
#include <java-interp.h>

#include <gnu/gcj/runtime/NameFinder.h>

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

// On some systems, a prefix is attached to a method name before
// it is exported as a label. The GCC preprocessor predefines 
// this prefix as the macro __USER_LABEL_PREFIX__ which expands to
// a string (not string constant) representing the prefix, if any.
#undef LABEL_PREFIX
#ifdef __USER_LABEL_PREFIX__

#define USER_LABEL_PREFIX_STRING_0(s) #s
#define USER_LABEL_PREFIX_STRING(s) USER_LABEL_PREFIX_STRING_0(s)

#define LABEL_PREFIX USER_LABEL_PREFIX_STRING(__USER_LABEL_PREFIX__)

#else /* __USER_LABEL_PREFIX__ */

#define LABEL_PREFIX ""

#endif /* ! __USER_LABEL_PREFIX__ */

java::lang::String*
gnu::gcj::runtime::NameFinder::getExternalLabel (java::lang::String* name)
{
  jsize nameLen = JvGetStringUTFLength (name);
  jsize pfxLen = strlen (LABEL_PREFIX);
  char *newName = (char *) JvMalloc (pfxLen + nameLen + 1);
  *(newName + 0) = '\0';
  strcpy (newName, LABEL_PREFIX);
  JvGetStringUTFRegion (name, 0, name->length(), newName + pfxLen);
  *(newName + pfxLen + nameLen) = '\0';
  return JvNewStringLatin1 (newName);
}

java::lang::String*
gnu::gcj::runtime::NameFinder::getExecutable (void)
{
  return JvNewStringLatin1 (_Jv_ThisExecutable ());
}

java::lang::String*
gnu::gcj::runtime::NameFinder::getAddrAsString(RawData* addrs, jint n)
{
  _Jv_frame_info *p = (_Jv_frame_info *) addrs;
  typedef unsigned word_t __attribute ((mode (word)));
  word_t w = (word_t) p[n].addr;
  int digits = sizeof (void *) * 2;
  char hex[digits+5];

  strcpy (hex, "0x");
  for (int i = digits - 1; i >= 0; i--)
    {
      int digit = w % 16;

      w /= 16;
      hex[i+2] = digit > 9 ? 'a' + digit - 10 : '0' + digit;
    }
  hex [digits+2] = 0;

  return JvNewStringLatin1(hex);
}

java::lang::StackTraceElement*
gnu::gcj::runtime::NameFinder::dladdrLookup(RawData* addrs, jint n)
{
#if defined (HAVE_DLFCN_H) && defined (HAVE_DLADDR)
  extern char **_Jv_argv;
  char name[1024];
  char file_name[1024];
  _Jv_frame_info *stack = (_Jv_frame_info *) addrs;
  void* p = stack[n].addr;
  Dl_info dl_info;
   
  if (dladdr (p, &dl_info))
    {
      if (dl_info.dli_fname)
        strncpy (file_name, dl_info.dli_fname, sizeof file_name);
      if (dl_info.dli_sname)
        strncpy (name, dl_info.dli_sname, sizeof name);
     
     /* Don't trust dladdr() if the address is from the main program. */
     if (dl_info.dli_fname != NULL
         && dl_info.dli_sname != NULL
         && (_Jv_argv == NULL || strcmp (file_name, _Jv_argv[0]) != 0))
       return createStackTraceElement (JvNewStringLatin1 (name),
				       JvNewStringLatin1 (file_name));
    }
#endif
  return NULL;
}

java::lang::StackTraceElement *
gnu::gcj::runtime::NameFinder::lookupInterp(RawData* addrs, jint n)
{
#ifdef INTERPRETER
  _Jv_frame_info *stack = (_Jv_frame_info *) addrs;
  if (stack[n].interp == NULL)
    return NULL;

  _Jv_InterpMethod *meth
    = reinterpret_cast<_Jv_InterpMethod *> (stack[n].interp);
  java::lang::StringBuffer *sb = new java::lang::StringBuffer();
  sb->append(_Jv_NewStringUtf8Const(meth->self->name));
  sb->append(_Jv_NewStringUtf8Const(meth->self->signature));
  // FIXME: source file name and line number can be found from
  // bytecode debug information.  But currently we don't keep that
  // around.
  // FIXME: is using the defining class correct here?
  java::lang::String *className = meth->defining_class->getName();
  java::lang::String *methodName
	  = demangleInterpreterMethod(sb->toString(), className);
  return new java::lang::StackTraceElement(NULL, -1,
					   className, methodName, false);
#else // INTERPRETER
  return NULL;
#endif // INTERPRETER
}
