// natSystem.cc - Native code implementing System class.

/* Copyright (C) 1998, 1999, 2000, 2001 , 2002 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "platform.h"

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

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java-props.h>
#include <java/lang/System.h>
#include <java/lang/Class.h>
#include <java/lang/ArrayStoreException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/StringBuffer.h>
#include <java/util/Properties.h>
#include <java/util/TimeZone.h>
#include <java/io/PrintStream.h>
#include <java/io/InputStream.h>



void
java::lang::System::setErr (java::io::PrintStream *newErr)
{
  checkSetIO ();
  // This violates `final' semantics.  Oh well.
  err = newErr;
}

void
java::lang::System::setIn (java::io::InputStream *newIn)
{
  checkSetIO ();
  // This violates `final' semantics.  Oh well.
  in = newIn;
}

void
java::lang::System::setOut (java::io::PrintStream *newOut)
{
  checkSetIO ();
  // This violates `final' semantics.  Oh well.
  out = newOut;
}

void
java::lang::System::arraycopy (jobject src, jint src_offset,
			       jobject dst, jint dst_offset,
			       jint count)
{
  if (! src || ! dst)
    throw new NullPointerException;

  jclass src_c = src->getClass();
  jclass dst_c = dst->getClass();
  jclass src_comp = src_c->getComponentType();
  jclass dst_comp = dst_c->getComponentType();

  if (! src_c->isArray() || ! dst_c->isArray()
      || src_comp->isPrimitive() != dst_comp->isPrimitive()
      || (src_comp->isPrimitive() && src_comp != dst_comp))
    throw new ArrayStoreException;

  __JArray *src_a = (__JArray *) src;
  __JArray *dst_a = (__JArray *) dst;
  if (src_offset < 0 || dst_offset < 0 || count < 0
      || src_offset + count > src_a->length
      || dst_offset + count > dst_a->length)
    throw new ArrayIndexOutOfBoundsException;

  // Do-nothing cases.
  if ((src == dst && src_offset == dst_offset)
      || ! count)
    return;

  // If both are primitive, we can optimize trivially.  If DST
  // components are always assignable from SRC components, then we
  // will never need to raise an error, and thus can do the
  // optimization.  If source and destinations are the same, then we
  // know that the assignability premise always holds.
  const bool prim = src_comp->isPrimitive();
  if (prim || dst_comp->isAssignableFrom(src_comp) || src == dst)
    {
      const size_t size = (prim ? src_comp->size()
			   : sizeof elements((jobjectArray)src)[0]);

      char *src_elts = _Jv_GetArrayElementFromElementType (src, src_comp);
      src_elts += size * src_offset;

      char *dst_elts = _Jv_GetArrayElementFromElementType (dst, dst_comp);
      dst_elts += size * dst_offset;

#if HAVE_MEMMOVE
      // We don't bother trying memcpy.  It can't be worth the cost of
      // the check.
      // Don't cast to (void*), as memmove may expect (char*)
      memmove (dst_elts, src_elts, count * size);
#else
      bcopy (src_elts, dst_elts, count * size);
#endif
    }
  else
    {
      jobject *src_elts = elements ((jobjectArray) src_a) + src_offset;
      jobject *dst_elts = elements ((jobjectArray) dst_a) + dst_offset;

      for (int i = 0; i < count; ++i)
	{
	  if (*src_elts
	      && ! dst_comp->isAssignableFrom((*src_elts)->getClass()))
	    throw new ArrayStoreException;
	  *dst_elts++ = *src_elts++;
	}
    }
}

jlong
java::lang::System::currentTimeMillis (void)
{
  return _Jv_platform_gettimeofday ();
}

jint
java::lang::System::identityHashCode (jobject obj)
{
  return _Jv_HashCode (obj);
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

/*
 * This method returns a time zone string that is used by init_properties
 * to set the default timezone property 'user.timezone'.  That value is
 * used by default as a key into the timezone table used by the
 * java::util::TimeZone class.
 */
jstring
java::lang::System::getSystemTimeZone (void)
{
  struct tm *tim;
  time_t current_time;
  long tzoffset;
  const char *tz1, *tz2;
  char *tzid;

  current_time = time(0);

  mktime(tim = localtime(&current_time));
#ifdef STRUCT_TM_HAS_GMTOFF
  // tm_gmtoff is secs EAST of UTC.
  tzoffset = -(tim->tm_gmtoff) + tim->tm_isdst * 3600L;
#elif HAVE_TIMEZONE
  // timezone is secs WEST of UTC.
  tzoffset = timezone;	
#else
  // FIXME: there must be another global if neither tm_gmtoff nor timezone
  // is available, esp. if tzname is valid.
  // Richard Earnshaw <rearnsha@arm.com> has suggested using difftime to
  // calculate between gmtime and localtime (and accounting for possible
  // daylight savings time) as an alternative.
  tzoffset = 0L;
#endif

#ifdef HAVE_TM_ZONE
  tz1 = tim->tm_zone;
  tz2 = "";
#elif defined (HAVE_TZNAME)
  tz1 = tzname[0];
  tz2 = strcmp (tzname[0], tzname[1]) ? tzname[1] : "";
#else
  // Some targets have no concept of timezones.
  tz1 = "???";
  tz2 = tz1;
#endif

  if ((tzoffset % 3600) == 0)
    tzoffset = tzoffset / 3600;

  tzid = (char*) _Jv_Malloc (strlen(tz1) + strlen(tz2) + 6);
  sprintf(tzid, "%s%ld%s", tz1, tzoffset, tz2);
  jstring retval = JvNewStringUTF (tzid);
  _Jv_Free (tzid);

  return retval;
}

void
java::lang::System::init_properties (void)
{
  JvSynchronize sync (&java::lang::System::class$);
  
  if (properties != NULL)
    return;

  java::util::Properties* newprops = new java::util::Properties ();
  
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

  // Set the "user.timezone" property.
  jstring timezone = getDefaultTimeZoneId ();
  if (timezone != NULL)
    newprops->put (JvNewStringLatin1 ("user.timezone"), timezone);

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
#ifdef WIN32
	  sb->append ((jchar) ';');
#else
	  sb->append ((jchar) ':');
#endif
	}
      if (cp != NULL)
	sb->append (cp);
      else
	sb->append ((jchar) '.');
      
      newprops->put(JvNewStringLatin1 ("java.class.path"),
		      sb->toString ());
    }

  // Allow platform specific settings and overrides.
  _Jv_platform_initProperties (newprops);

  // Finally, set the field. This ensures that concurrent getProperty() 
  // calls will return initialized values without requiring them to be 
  // synchronized in the common case.
  properties = newprops;
}
