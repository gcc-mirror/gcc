// natSystem.cc - Native code implementing System class.

/* Copyright (C) 1998, 1999, 2000  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#ifdef HAVE_GETPWUID_R
#define _POSIX_PTHREAD_SEMANTICS
#ifndef _REENTRANT
#define _REENTRANT
#endif
#endif

#include <string.h>
#include <time.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <errno.h>

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java-props.h>
#include <java/lang/System.h>
#include <java/lang/Class.h>
#include <java/lang/ArrayStoreException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/util/Properties.h>
#include <java/io/PrintStream.h>
#include <java/io/InputStream.h>

#define SystemClass _CL_Q34java4lang6System
extern java::lang::Class SystemClass;



#if defined (ECOS)
extern "C" unsigned long long _clock (void);
#endif

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
    _Jv_Throw (new NullPointerException);

  jclass src_c = src->getClass();
  jclass dst_c = dst->getClass();
  jclass src_comp = src_c->getComponentType();
  jclass dst_comp = dst_c->getComponentType();

  if (! src_c->isArray() || ! dst_c->isArray()
      || src_comp->isPrimitive() != dst_comp->isPrimitive()
      || (src_comp->isPrimitive() && src_comp != dst_comp))
    _Jv_Throw (new ArrayStoreException);

  __JArray *src_a = (__JArray *) src;
  __JArray *dst_a = (__JArray *) dst;
  if (src_offset < 0 || dst_offset < 0 || count < 0
      || src_offset + count > src_a->length
      || dst_offset + count > dst_a->length)
    _Jv_Throw (new ArrayIndexOutOfBoundsException);

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
	    _Jv_Throw (new ArrayStoreException);
	  *dst_elts++ = *src_elts++;
	}
    }
}

jlong
java::lang::System::currentTimeMillis (void)
{
  jlong r;

#if defined (HAVE_GETTIMEOFDAY)
  struct timeval tv;
  gettimeofday (&tv, NULL);
  r = (jlong) tv.tv_sec * 1000 + tv.tv_usec / 1000;
#elif defined (HAVE_TIME)
  r = time (NULL) * 1000;
#elif defined (HAVE_FTIME)
  struct timeb t;
  ftime (&t);
  r = t.time * 1000 + t.millitm;
#elif defined (ECOS)
  r = _clock();
#else
  // In the absence of any function, time remains forever fixed.
  r = 23;
#endif

  return r;
}

jint
java::lang::System::identityHashCode (jobject obj)
{
  return _Jv_HashCode (obj);
}

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
java::lang::System::init_properties (void)
{
  {
    // We only need to synchronize around this gatekeeper.
    JvSynchronize sync (&SystemClass);
    if (prop_init)
      return;
    prop_init = true;
  }

  properties = new java::util::Properties ();
  // A convenience define.
#define SET(Prop,Val) \
	properties->put(JvNewStringLatin1 (Prop), JvNewStringLatin1 (Val))

  // A mixture of the Java Product Versioning Specification
  // (introduced in 1.2), and earlier versioning properties.
  SET ("java.version", VERSION);
  SET ("java.vendor", "Cygnus Solutions");
  SET ("java.vendor.url", "http://sourceware.cygnus.com/java/");
  SET ("java.class.version", GCJVERSION);
  SET ("java.vm.specification.version", "1.1");
  SET ("java.vm.specification.name", "Java(tm) Virtual Machine Specification");
  SET ("java.vm.specification.vendor", "Sun Microsystems Inc.");
  SET ("java.vm.version", GCJVERSION);
  SET ("java.vm.vendor", "Cygnus Solutions");
  SET ("java.vm.name", "libgcj");
  SET ("java.specification.version", "1.1");
  SET ("java.specification.name", "Java(tm) Language Specification");
  SET ("java.specification.vendor", "Sun Microsystems Inc.");

  // FIXME: how to set these given location-independence?
  // SET ("java.home", "FIXME");
  // SET ("java.class.path", "FIXME");
  SET ("file.encoding", default_file_encoding);

#ifdef WIN32
  SET ("file.separator", "\\");
  SET ("path.separator", ";");
  SET ("line.separator", "\r\n");
#else
  // Unix.
  SET ("file.separator", "/");
  SET ("path.separator", ":");
  SET ("line.separator", "\n");
#endif

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
      properties->put (name, val);
    }

  // Set the system properties from the user's environment.
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
}
