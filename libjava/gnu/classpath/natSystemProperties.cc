// natSystemProperties.cc - Implementation of native side of
// SystemProperties class.

/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <stdlib.h>
#include <errno.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java-props.h>
#include <gnu/classpath/SystemProperties.h>
#include <java/lang/String.h>
#include <jni.h>

#ifdef USE_LTDL
#include <ltdl.h>

void
_Jv_SetDLLSearchPath (const char *path)
{
  lt_dlsetsearchpath (path);
}

#else

void
_Jv_SetDLLSearchPath (const char *)
{
  // Nothing.
}

#endif /* USE_LTDL */

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
gnu::classpath::SystemProperties::insertSystemProperties (java::util::Properties *newprops)
{
  // A convenience define.
#define SET(Prop,Val) \
	newprops->put(JvNewStringLatin1 (Prop), JvNewStringLatin1 (Val))

  // A mixture of the Java Product Versioning Specification
  // (introduced in 1.2), and earlier versioning properties.  Some
  // programs rely on seeing values that they expect, so we claim to
  // be a 1.4-ish VM for their sake.
  SET ("java.version", JV_VERSION);
  SET ("java.runtime.version", JV_VERSION);
  SET ("java.vendor", "Free Software Foundation, Inc.");
  SET ("java.vendor.url", "http://gcc.gnu.org/java/");
  SET ("java.class.version", "46.0");
  SET ("java.vm.specification.version", "1.0");
  SET ("java.vm.specification.name", "Java(tm) Virtual Machine Specification");
  SET ("java.vm.specification.vendor", "Sun Microsystems Inc.");
  SET ("java.vm.version", __VERSION__);
  SET ("java.vm.vendor", "Free Software Foundation, Inc.");
  SET ("java.vm.name", "GNU libgcj");
  SET ("java.specification.version", JV_API_VERSION);
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
  SET ("java.home", JAVA_HOME);
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
      SET ("os.version", u.release);

      // Normalize x86 architecture names to "i386" (except on Windows, which 
      // is handled in win32.cc).
      if (u.machine[0] == 'i'
	  && u.machine[1] != 0
	  && u.machine[2] == '8'
	  && u.machine[3] == '6'
	  && u.machine[4] == 0)
	SET ("os.arch", "i386");
      else
	SET ("os.arch", u.machine);
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

  // The java extensions directory.
  SET ("java.ext.dirs", JAVA_EXT_DIRS);

  // The endorsed directories that libgcj knows about by default.
  // This is a way to get other jars into the boot class loader
  // without overriding java.endorsed.dirs.
  SET ("gnu.gcj.runtime.endorsed.dirs", GCJ_ENDORSED_DIRS);

  // The path to libgcj's boot classes
  SET ("sun.boot.class.path", BOOT_CLASS_PATH);

  // If there is a default system database, set it.
  SET ("gnu.gcj.precompiled.db.path", LIBGCJ_DEFAULT_DATABASE);

  // Set some properties according to whatever was compiled in with
  // `-D'.  Important: after this point, the only properties that
  // should be set are those which either the user cannot meaningfully
  // override, or which augment whatever value the user has provided.
  for (int i = 0; i < _Jv_Properties_Count; ++i)
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

  // The name used to invoke this process (argv[0] in C).
  SET ("gnu.gcj.progname", _Jv_GetSafeArg (0));

  // Allow platform specific settings and overrides.
  _Jv_platform_initProperties (newprops);

  // If java.library.path is set, tell libltdl so we search the new
  // directories as well.  FIXME: does this work properly on Windows?
  ::java::lang::String *path = newprops->getProperty(JvNewStringLatin1("java.library.path"));
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

  // If java.class.path is still not set then set it according to the
  // CLASSPATH environment variable if given.  See gij.cc main () and
  // prims.cc _Jv_CreateJavaVM () for all the ways this could have
  // been set much earlier.
  // If CLASSPATH isn't set or if the path is empty fall back to "."
  path = newprops->getProperty(JvNewStringLatin1("java.class.path"));
  if (!path)
    {
      char *classpath = getenv("CLASSPATH");
      if (classpath && classpath[0] != 0)
	{
	  path = JvNewStringLatin1 (classpath);
	  newprops->put(JvNewStringLatin1 ("java.class.path"), path);
	}
    }

  if (!path || path->length() == 0)
    SET ("java.class.path", ".");
}

jboolean
gnu::classpath::SystemProperties::isWordsBigEndian (void)
{
  union
  {
    long lval;
    char cval;
  } u;

  u.lval = 1;
  return u.cval == 0;
}

