// natRuntime.cc - Implementation of native side of Runtime class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software Foundation

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
#include <java/lang/Long.h>
#include <java/lang/Runtime.h>
#include <java/lang/UnknownError.h>
#include <java/lang/UnsatisfiedLinkError.h>
#include <gnu/gcj/runtime/FileDeleter.h>
#include <gnu/gcj/runtime/FinalizerThread.h>
#include <java/io/File.h>
#include <java/util/Properties.h>
#include <java/util/TimeZone.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/Process.h>
#include <java/lang/ConcreteProcess.h>
#include <java/lang/ClassLoader.h>
#include <gnu/gcj/runtime/StackTrace.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>

#include <jni.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <errno.h>

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif

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

void
_Jv_SetDLLSearchPath (const char *path)
{
  lt_dlsetsearchpath (path);
}

#else

void *
_Jv_FindSymbolInExecutable (const char *)
{
  return NULL;
}

void
_Jv_SetDLLSearchPath (const char *)
{
  // Nothing.
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
      ClassLoader *sys = ClassLoader::getSystemClassLoader();
      ClassLoader *look = NULL;
      gnu::gcj::runtime::StackTrace *t = new gnu::gcj::runtime::StackTrace(10);
      try
      	{
	  for (int i = 0; i < 10; ++i)
	    {
	      jclass klass = t->classAt(i);
	      if (klass != NULL)
		{
		  ClassLoader *loader = klass->getClassLoaderInternal();
		  if (loader != NULL && loader != sys)
		    {
		      look = loader;
		      break;
		    }
		}
	    }
	}
      catch (::java::lang::ArrayIndexOutOfBoundsException *e)
	{
	}

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

#if ! defined (DEFAULT_FILE_ENCODING) && defined (HAVE_ICONV) \
    && defined (HAVE_NL_LANGINFO)

static char *
file_encoding ()
{
  setlocale (LC_CTYPE, "");
  char *e = nl_langinfo (CODESET);
  if (e == NULL || *e == '\0')
    e = "8859_1";
  return e;
}

#define DEFAULT_FILE_ENCODING file_encoding ()

#endif

#ifndef DEFAULT_FILE_ENCODING
#define DEFAULT_FILE_ENCODING "8859_1"
#endif

static char *default_file_encoding = DEFAULT_FILE_ENCODING;

#if HAVE_GETPWUID_R
/* Use overload resolution to find out the signature of getpwuid_r.  */

  /* This is Posix getpwuid_r.  */
template <typename T_uid, typename T_passwd, typename T_buf, typename T_len>
static inline int
getpwuid_adaptor(int (*getpwuid_r)(T_uid user_id, T_passwd *pwd_r,
				   T_buf *buf_r, T_len len_r,
				   T_passwd **pwd_entry_ptr),
		 uid_t user_id, struct passwd *pwd_r,
		 char *buf_r, size_t len_r, struct passwd **pwd_entry)
{
  return getpwuid_r (user_id, pwd_r, buf_r, len_r, pwd_entry);
}

/* This is used on HPUX 10.20 */
template <typename T_uid, typename T_passwd, typename T_buf, typename T_len>
static inline int
getpwuid_adaptor(int (*getpwuid_r)(T_uid user_id, T_passwd *pwd_r,
				   T_buf *buf_r, T_len len_r),
		 uid_t user_id, struct passwd *pwd_r,
		 char *buf_r, size_t len_r, struct passwd **pwd_entry)
{
  return getpwuid_r (user_id, pwd_r, buf_r, len_r);
}

/* This is used on IRIX 5.2.  */
template <typename T_uid, typename T_passwd, typename T_buf, typename T_len>
static inline int
getpwuid_adaptor(T_passwd * (*getpwuid_r)(T_uid user_id, T_passwd *pwd_r,
					  T_buf *buf_r, T_len len_r),
		 uid_t user_id, struct passwd *pwd_r,
		 char *buf_r, size_t len_r, struct passwd **pwd_entry)
{
  *pwd_entry = getpwuid_r (user_id, pwd_r, buf_r, len_r);
  return (*pwd_entry == NULL) ? errno : 0;
}
#endif

void
java::lang::Runtime::insertSystemProperties (java::util::Properties *newprops)
{
  // A convenience define.
#define SET(Prop,Val) \
	newprops->put(JvNewStringLatin1 (Prop), JvNewStringLatin1 (Val))

  // A mixture of the Java Product Versioning Specification
  // (introduced in 1.2), and earlier versioning properties.
  SET ("java.version", GCJVERSION);
  SET ("java.vendor", "Free Software Foundation, Inc.");
  SET ("java.vendor.url", "http://gcc.gnu.org/java/");
  SET ("java.class.version", "46.0");
  SET ("java.vm.specification.version", "1.0");
  SET ("java.vm.specification.name", "Java(tm) Virtual Machine Specification");
  SET ("java.vm.specification.vendor", "Sun Microsystems Inc.");
  SET ("java.vm.version", __VERSION__);
  SET ("java.vm.vendor", "Free Software Foundation, Inc.");
  SET ("java.vm.name", "GNU libgcj");
  SET ("java.specification.version", "1.3");
  SET ("java.specification.name", "Java(tm) Platform API Specification");
  SET ("java.specification.vendor", "Sun Microsystems Inc.");

  char value[100];
#define NAME "GNU libgcj "
  strcpy (value, NAME);
  strncpy (value + sizeof (NAME) - 1, __VERSION__,
	   sizeof(value) - sizeof(NAME));
  value[sizeof (value) - 1] = '\0';
  jstring version = JvNewStringLatin1 (value);
  newprops->put (JvNewStringLatin1 ("java.fullversion"), version);
  newprops->put (JvNewStringLatin1 ("java.vm.info"), version);

  // This definition is rather arbitrary: we choose $(prefix).  In
  // part we do this because most people specify only --prefix and
  // nothing else when installing gcj.  Plus, people are free to
  // redefine `java.home' with `-D' if necessary.
  SET ("java.home", PREFIX);
  SET ("gnu.classpath.home", PREFIX);
  // This is set to $(libdir) because we use this to find .security
  // files at runtime.
  char val2[sizeof ("file://") + sizeof (LIBDIR) + 1];
  strcpy (val2, "file://");
  strcat (val2, LIBDIR);
  SET ("gnu.classpath.home.url", val2);

  SET ("file.encoding", default_file_encoding);

#ifdef HAVE_UNAME
  struct utsname u;
  if (! uname (&u))
    {
      SET ("os.name", u.sysname);
      SET ("os.arch", u.machine);
      SET ("os.version", u.release);
    }
  else
    {
      SET ("os.name", "unknown");
      SET ("os.arch", "unknown");
      SET ("os.version", "unknown");
    }
#endif /* HAVE_UNAME */

#ifndef NO_GETUID
#ifdef HAVE_PWD_H
  uid_t user_id = getuid ();
  struct passwd *pwd_entry;

#ifdef HAVE_GETPWUID_R
  struct passwd pwd_r;
  size_t len_r = 200;
  char *buf_r = (char *) _Jv_AllocBytes (len_r);

  while (buf_r != NULL)
    {
      int r = getpwuid_adaptor (getpwuid_r, user_id, &pwd_r,
				buf_r, len_r, &pwd_entry);
      if (r == 0)
	break;
      else if (r != ERANGE)
	{
	  pwd_entry = NULL;
	  break;
	}
      len_r *= 2;
      buf_r = (char *) _Jv_AllocBytes (len_r);
    }
#else
  pwd_entry = getpwuid (user_id);
#endif /* HAVE_GETPWUID_R */

  if (pwd_entry != NULL)
    {
      SET ("user.name", pwd_entry->pw_name);
      SET ("user.home", pwd_entry->pw_dir);
    }
#endif /* HAVE_PWD_H */
#endif /* NO_GETUID */

#ifdef HAVE_GETCWD
#ifdef HAVE_UNISTD_H
  /* Use getcwd to set "user.dir". */
  int buflen = 250;
  char *buffer = (char *) malloc (buflen);
  while (buffer != NULL)
    {
      if (getcwd (buffer, buflen) != NULL)
	{
	  SET ("user.dir", buffer);
	  break;
	}
      if (errno != ERANGE)
	break;
      buflen = 2 * buflen;
      buffer = (char *) realloc (buffer, buflen);
    }
  if (buffer != NULL)
    free (buffer);
#endif /* HAVE_UNISTD_H */
#endif /* HAVE_GETCWD */

  // Set user locale properties based on setlocale()
#if defined (HAVE_SETLOCALE) && defined (HAVE_LC_MESSAGES)
  // We let the user choose the locale.  However, since Java differs
  // from POSIX, we arbitrarily pick LC_MESSAGES as determining the
  // Java locale.  We can't use LC_ALL because it might return a full
  // list of all the settings.  If we don't have LC_MESSAGES then we
  // just default to `en_US'.
  setlocale (LC_ALL, "");
  char *locale = setlocale (LC_MESSAGES, "");
  if (locale && strlen (locale) >= 2)
    {
      char buf[3];
      buf[2] = '\0';
      // copy the first two chars to user.language
      strncpy (buf, locale, 2);
      SET ("user.language", buf);
      // if the next char is a '_', copy the two after that to user.region
      locale += 2;
      if (locale[0] == '_')
        {
	  locale++;
	  strncpy (buf, locale, 2);
	  SET ("user.region", buf);
        }
    }
  else
#endif /* HAVE_SETLOCALE and HAVE_LC_MESSAGES */
    {
      SET ("user.language", "en");
      SET ("user.region", "US");
    }  

  // Set some properties according to whatever was compiled in with
  // `-D'.
  for (int i = 0; _Jv_Compiler_Properties[i]; ++i)
    {
      const char *s, *p;
      // Find the `='.
      for (s = p = _Jv_Compiler_Properties[i]; *s && *s != '='; ++s)
	;
      jstring name = JvNewStringLatin1 (p, s - p);
      jstring val = JvNewStringLatin1 (*s == '=' ? s + 1 : s);
      newprops->put (name, val);
    }

  // Set the system properties from the user's environment.
#ifndef DISABLE_GETENV_PROPERTIES
  if (_Jv_Environment_Properties)
    {
      size_t i = 0;

      while (_Jv_Environment_Properties[i].key)
	{
	  SET (_Jv_Environment_Properties[i].key, 
	       _Jv_Environment_Properties[i].value);
	  i++;
	}
    }
#endif

  if (_Jv_Jar_Class_Path)
    newprops->put(JvNewStringLatin1 ("java.class.path"),
 		  JvNewStringLatin1 (_Jv_Jar_Class_Path));
  else
    {
      // FIXME: find libgcj.zip and append its path?
      char *classpath = ::getenv("CLASSPATH");
      jstring cp = newprops->getProperty (JvNewStringLatin1("java.class.path"));
      java::lang::StringBuffer *sb = new java::lang::StringBuffer ();
      
      if (classpath)
	{
	  sb->append (JvNewStringLatin1 (classpath));
	  sb->append (_Jv_platform_path_separator);
	}
      if (cp != NULL)
	sb->append (cp);
      else
	sb->append ((jchar) '.');
      
      newprops->put(JvNewStringLatin1 ("java.class.path"),
		      sb->toString ());
    }

  // The path to libgcj's boot classes
  SET ("sun.boot.class.path", BOOT_CLASS_PATH);

  // The name used to invoke this process (argv[0] in C).
  SET ("gnu.gcj.progname", _Jv_GetSafeArg (0));

  // Allow platform specific settings and overrides.
  _Jv_platform_initProperties (newprops);

  // If java.library.path is set, tell libltdl so we search the new
  // directories as well.  FIXME: does this work properly on Windows?
  String *path = newprops->getProperty(JvNewStringLatin1("java.library.path"));
  if (path)
    {
      char *val = (char *) _Jv_Malloc (JvGetStringUTFLength (path) + 1);
      jsize total = JvGetStringUTFRegion (path, 0, path->length(), val);
      val[total] = '\0';
      _Jv_SetDLLSearchPath (val);
      _Jv_Free (val);
    }
  else
    {
      // Set a value for user code to see.
      // FIXME: JDK sets this to the actual path used, including
      // LD_LIBRARY_PATH, etc.
      SET ("java.library.path", "");
    }
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
