// natRuntime.cc - Implementation of native side of Runtime class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-props.h>
#include <java-stack.h>
#include <java/lang/Long.h>
#include <java/lang/Runtime.h>
#include <java/lang/UnknownError.h>
#include <java/lang/UnsatisfiedLinkError.h>
#include <gnu/gcj/runtime/FileDeleter.h>
#include <gnu/gcj/runtime/FinalizerThread.h>
#include <java/io/File.h>
#include <java/util/TimeZone.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/Process.h>
#include <java/lang/ConcreteProcess.h>
#include <java/lang/ClassLoader.h>

#include <jni.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <errno.h>

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif



#ifdef USE_LTDL
#include <ltdl.h>

/* FIXME: we don't always need this.  The next libtool will let us use
   AC_LTDL_PREOPEN to see if we do.  */
extern const lt_dlsymlist lt_preloaded_symbols[1] = { { 0, 0 } };

struct lookup_data
{
  const char *symname;
  void *result;
};

static int
find_symbol (lt_dlhandle handle, lt_ptr data)
{
  lookup_data *ld = (lookup_data *) data;
  ld->result = lt_dlsym (handle, ld->symname);
  return ld->result != NULL;
}

void *
_Jv_FindSymbolInExecutable (const char *symname)
{
  lookup_data data;
  data.symname = symname;
  data.result = NULL;
  lt_dlforeach (find_symbol, (lt_ptr) &data);
  return data.result;
}

#else

void *
_Jv_FindSymbolInExecutable (const char *)
{
  return NULL;
}

#endif /* USE_LTDL */



void
java::lang::Runtime::exitInternal (jint status)
{
  // Make status right for Unix.  This is perhaps strange.
  if (status < 0 || status > 255)
    status = 255;

  if (finalizeOnExit)
    _Jv_RunAllFinalizers ();

  // Delete all files registered with File.deleteOnExit()
  gnu::gcj::runtime::FileDeleter::deleteOnExitNow ();

  ::exit (status);
}

jlong
java::lang::Runtime::freeMemory (void)
{
  return _Jv_GCFreeMemory ();
}

void
java::lang::Runtime::gc (void)
{
  _Jv_RunGC ();
}

#ifdef USE_LTDL
// List of names for JNI_OnLoad.
static const char *onload_names[] = _Jv_platform_onload_names;
#endif

void
java::lang::Runtime::_load (jstring path, jboolean do_search)
{
  JvSynchronize sync (this);
  using namespace java::lang;
#ifdef USE_LTDL
  jint len = _Jv_GetStringUTFLength (path);
  char buf[len + 1 + strlen (_Jv_platform_solib_prefix)
	   + strlen (_Jv_platform_solib_suffix)];
  int offset = 0;
  if (do_search)
    {
      strcpy (buf, _Jv_platform_solib_prefix);
      offset = strlen (_Jv_platform_solib_prefix);
    }
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), &buf[offset]);
  buf[offset + total] = '\0';

  char *lib_name = buf;

  if (do_search)
    {
      ClassLoader *look = _Jv_StackTrace::GetFirstNonSystemClassLoader ();

      if (look != NULL)
	{
	  // Don't include solib prefix in string passed to
	  // findLibrary.
	  jstring name = look->findLibrary(JvNewStringUTF(&buf[offset]));
	  if (name != NULL)
	    {
	      len = _Jv_GetStringUTFLength (name);
	      lib_name = (char *) _Jv_AllocBytes(len + 1);
	      total = JvGetStringUTFRegion (name, 0,
					    name->length(), lib_name);
	      lib_name[total] = '\0';
	      // Don't append suffixes any more; we have the full file
	      // name.
	      do_search = false;
	    }
	}
    }

  lt_dlhandle h;
  // FIXME: make sure path is absolute.
  {
    // Synchronize on java.lang.Class. This is to protect the class chain from
    // concurrent modification by class registration calls which may be run
    // during the dlopen().
    JvSynchronize sync (&java::lang::Class::class$);
    h = do_search ? lt_dlopenext (lib_name) : lt_dlopen (lib_name);
  }
  if (h == NULL)
    {
      const char *msg = lt_dlerror ();
      jstring str = JvNewStringLatin1 (lib_name);
      str = str->concat (JvNewStringLatin1 (": "));
      str = str->concat (JvNewStringLatin1 (msg));
      throw new UnsatisfiedLinkError (str);
    }

  // Search for JNI_OnLoad function.
  void *onload = NULL;
  const char **name = onload_names;
  while (*name != NULL)
    {
      onload = lt_dlsym (h, *name);
      if (onload != NULL)
	break;
      ++name;
    }

  if (onload != NULL)
    {
      JavaVM *vm = _Jv_GetJavaVM ();
      if (vm == NULL)
	{
	  // FIXME: what?
	  return;
	}
      jint vers = ((jint (JNICALL *) (JavaVM *, void *)) onload) (vm, NULL);
      if (vers != JNI_VERSION_1_1 && vers != JNI_VERSION_1_2
	  && vers != JNI_VERSION_1_4)
	{
	  // FIXME: unload the library.
	  throw new UnsatisfiedLinkError (JvNewStringLatin1 ("unrecognized version from JNI_OnLoad"));
	}
    }
#else
  throw new UnknownError
    (JvNewStringLatin1 (do_search
			? "Runtime.loadLibrary not implemented"
			: "Runtime.load not implemented"));
#endif /* USE_LTDL */
}

jboolean
java::lang::Runtime::loadLibraryInternal (jstring lib)
{
  JvSynchronize sync (this);
  using namespace java::lang;
#ifdef USE_LTDL
  jint len = _Jv_GetStringUTFLength (lib);
  char buf[len + 1];
  jsize total = JvGetStringUTFRegion (lib, 0, lib->length(), buf);
  buf[total] = '\0';
  // FIXME: make sure path is absolute.
  lt_dlhandle h = lt_dlopenext (buf);
  return h != NULL;
#else
  return false;
#endif /* USE_LTDL */
}

void
java::lang::Runtime::init (void)
{
#ifdef USE_LTDL
  lt_dlinit ();
  // Make sure self is opened.
  lt_dlopen (NULL);
#endif
}

void
java::lang::Runtime::runFinalization (void)
{
  gnu::gcj::runtime::FinalizerThread::finalizerReady ();
}

jlong
java::lang::Runtime::totalMemory (void)
{
  return _Jv_GCTotalMemory ();
}

jlong
java::lang::Runtime::maxMemory (void)
{
  // We don't have a maximum.  FIXME: we might if we ask the GC for
  // one.
  return Long::MAX_VALUE;
}

void
java::lang::Runtime::traceInstructions (jboolean)
{
  // Do nothing.
}

void
java::lang::Runtime::traceMethodCalls (jboolean)
{
  // Do nothing.
}

java::lang::Process *
java::lang::Runtime::execInternal (jstringArray cmd,
				   jstringArray env,
				   java::io::File *dir)
{
  return new java::lang::ConcreteProcess (cmd, env, dir);
}

jint
java::lang::Runtime::availableProcessors (void)
{
  // FIXME: find the real value.
  return 1;
}

jstring
java::lang::Runtime::nativeGetLibname (jstring pathname, jstring libname)
{
  java::lang::StringBuffer *sb = new java::lang::StringBuffer ();
  sb->append(pathname);
  if (pathname->length() > 0)
    sb->append (_Jv_platform_file_separator);

  sb->append (JvNewStringLatin1 (_Jv_platform_solib_prefix));
  sb->append(libname);
  sb->append (JvNewStringLatin1 (_Jv_platform_solib_suffix));

  return sb->toString();
}
