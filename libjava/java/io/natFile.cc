// natFile.cc - Native part of File class.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#ifdef HAVE_DIRENT_H
#define _POSIX_PTHREAD_SEMANTICS
#ifndef _REENTRANT
#  define _REENTRANT
#endif
#include <dirent.h>
#endif
#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/io/File.h>
#include <java/io/IOException.h>
#include <java/util/Vector.h>
#include <java/lang/String.h>
#include <java/io/FilenameFilter.h>
#include <java/lang/System.h>

jboolean
java::io::File::access (jstring canon, jint query)
{
  if (! canon)
    return false;
  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (canon, 0, canon->length(), buf);
  // FIXME?
  buf[total] = '\0';
  JvAssert (query == READ || query == WRITE || query == EXISTS);
#ifdef HAVE_ACCESS
  int mode;
  if (query == READ)
    mode = R_OK;
  else if (query == WRITE)
    mode = W_OK;
  else
    mode = F_OK;
  return ::access (buf, mode) == 0;
#else
  return false;
#endif
}

jboolean
java::io::File::stat (jstring canon, jint query)
{
  if (! canon)
    return false;
  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (canon, 0, canon->length(), buf);
  // FIXME?
  buf[total] = '\0';

#ifdef HAVE_STAT
  struct stat sb;
  if (::stat (buf, &sb))
    return false;
  
  JvAssert (query == DIRECTORY || query == ISFILE);
  jboolean r = S_ISDIR (sb.st_mode);
  return query == DIRECTORY ? r : ! r;
#else
  return false;
#endif
}

jlong
java::io::File::attr (jstring canon, jint query)
{
  if (! canon)
    return false;

  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (canon, 0, canon->length(), buf);
  // FIXME?
  buf[total] = '\0';

#ifdef HAVE_STAT
  struct stat sb;
  // FIXME: not sure about return value here.
  if (::stat (buf, &sb))
    return 0;

  JvAssert (query == MODIFIED || query == LENGTH);
  // FIXME: time computation is very POSIX-specific -- POSIX and Java
  // have the same Epoch.
  return query == MODIFIED ? (jlong)sb.st_mtime * 1000 : sb.st_size;
#else
  // There's no good choice here.
  return 23;
#endif
}

jstring
java::io::File::getCanonicalPath (void)
{
  char buf[MAXPATHLEN], buf2[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  // FIXME?
  buf[total] = '\0';

#ifdef HAVE_REALPATH
  if (realpath (buf, buf2) == NULL)
    _Jv_Throw (new IOException (JvNewStringLatin1 (strerror (errno))));

  // FIXME: what encoding to assume for file names?  This affects many
  // calls.
  return JvNewStringUTF (buf2);
#else
  return JvNewStringUTF (buf);
#endif
}

jboolean
java::io::File::isAbsolute (void)
{
  // FIXME: cpp define name.
  // FIXME: cygwin.
#ifdef WIN32
  if (path->charAt(0) == '/' || path->charAt(0) == '\\')
    return true;
  if (path->length() < 3)
    return false;
  // Hard-code A-Za-z because Windows (I think) can't use non-ASCII
  // letters as drive names.
  if ((path->charAt(0) < 'a' || path->charAt(0) > 'z')
      && (path->charAt(0) < 'A' || path->charAt(0) > 'Z'))
    return false;
  return (path->charAt(1) == ':'
	  && (path->charAt(2) == '/' || path->charAt(2) == '\\'));
#else
  return path->charAt(0) == '/';
#endif
}

#ifdef HAVE_DIRENT_H
#if defined(__JV_POSIX_THREADS__) && defined(HAVE_READDIR_R)

static struct dirent *
get_entry (DIR *dir, struct dirent *e)
{
  struct dirent *r;
  if (readdir_r (dir, e, &r) || r == NULL)
    return NULL;
  return e;
}

#else /* defined(__JV_POSIX_THREADS__) && defined(HAVE_READDIR_R) */

static struct dirent *
get_entry (DIR *dir, struct dirent *)
{
  return readdir (dir);
}

#endif /* defined(__JV_POSIX_THREADS__) && defined(HAVE_READDIR_R) */
#endif /* HAVE_DIRENT_H */

jstringArray
java::io::File::performList (jstring canon, FilenameFilter *filter)
{
  if (! canon)
    return NULL;

#ifdef HAVE_DIRENT_H
  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (canon, 0, canon->length(), buf);
  // FIXME?
  buf[total] = '\0';

  DIR *dir = opendir (buf);
  if (! dir)
    return NULL;

  java::util::Vector *vec = new java::util::Vector ();
  struct dirent *d, d2;
  while ((d = get_entry (dir, &d2)) != NULL)
    {
      if (! strcmp (d->d_name, ".") || ! strcmp (d->d_name, ".."))
	continue;

      jstring name = JvNewStringUTF (d->d_name);
      if (filter && ! filter->accept(this, name))
	continue;

      vec->addElement(name);
    }

  closedir (dir);

  jobjectArray ret = JvNewObjectArray (vec->size(), canon->getClass(),
				       NULL);
  vec->copyInto(ret);
  return reinterpret_cast<jstringArray> (ret);
#else /* HAVE_DIRENT_H */
  return NULL;
#endif /* HAVE_DIRENT_H */
}

jboolean
java::io::File::performMkdir (void)
{
  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  // FIXME?
  buf[total] = '\0';

#ifdef HAVE_MKDIR
  return ::mkdir (buf, 0755) == 0;
#else
  return false;
#endif
}

jboolean
java::io::File::performRenameTo (File *dest)
{
  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  // FIXME?
  buf[total] = '\0';
  char buf2[MAXPATHLEN];
  total = JvGetStringUTFRegion (dest->path, 0, dest->path->length(), buf2);
  // FIXME?
  buf2[total] = '\0';

#ifdef HAVE_RENAME
  return ::rename (buf, buf2) == 0;
#else
  return false;
#endif
}

jboolean
java::io::File::performDelete (jstring canon)
{
  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (canon, 0, canon->length(), buf);
  // FIXME?
  buf[total] = '\0';

#ifdef HAVE_UNLINK
#ifdef HAVE_RMDIR
  if (! ::rmdir (buf))
    return true;
#endif // HAVE_RMDIR
  if (errno == ENOTDIR)
    return ::unlink (buf) == 0;
#endif // HAVE_UNLINK
  return false;
}
