// natFile.cc - Native part of File class for POSIX.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002  Free Software Foundation

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
#include <dirent.h>
#endif
#include <string.h>
#include <utime.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/io/File.h>
#include <java/io/IOException.h>
#include <java/util/ArrayList.h>
#include <java/lang/String.h>
#include <java/io/FilenameFilter.h>
#include <java/io/FileFilter.h>
#include <java/lang/System.h>

jboolean
java::io::File::_access (jint query)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
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
java::io::File::_stat (jint query)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

  if (query == ISHIDDEN)
    return (getName()->charAt(0) == '.');

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
java::io::File::attr (jint query)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

#ifdef HAVE_STAT
  struct stat sb;
  // FIXME: not sure about return value here.
  if (::stat (buf, &sb))
    return 0;

  JvAssert (query == MODIFIED || query == LENGTH);
  return query == MODIFIED ? (jlong)sb.st_mtime * 1000 : sb.st_size;
#else
  // There's no good choice here.
  return 23;
#endif
}

jstring
java::io::File::getCanonicalPath (void)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  char buf2[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

#ifdef HAVE_REALPATH
  if (realpath (buf, buf2) == NULL)
    throw new IOException (JvNewStringLatin1 (strerror (errno)));

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
  return path->charAt(0) == '/';
}

jobjectArray
java::io::File::performList (java::io::FilenameFilter *filter, 
			     java::io::FileFilter *fileFilter, 
			     java::lang::Class *result_type)
{
  /* Some systems have dirent.h, but no directory reading functions like
     opendir.  */
#if defined(HAVE_DIRENT_H) && defined(HAVE_OPENDIR)
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

  DIR *dir = opendir (buf);
  if (! dir)
    return NULL;

  java::util::ArrayList *list = new java::util::ArrayList ();
  struct dirent *d;
#ifdef HAVE_READDIR_R
  int name_max = pathconf (buf, _PC_NAME_MAX);
  char dbuf[sizeof (struct dirent) + name_max + 1];
  while (readdir_r (dir, (struct dirent *) dbuf, &d) == 0 && d != NULL)
#else /* HAVE_READDIR_R */
  while ((d = readdir (dir)) != NULL)
#endif /* HAVE_READDIR_R */
    {
      // Omit "." and "..".
      if (d->d_name[0] == '.'
	  && (d->d_name[1] == '\0'
	      || (d->d_name[1] == '.' && d->d_name[2] == '\0')))
	continue;

      jstring name = JvNewStringUTF (d->d_name);
      if (filter && ! filter->accept(this, name))
	continue;

      if (result_type == &java::io::File::class$)
        {
	  java::io::File *file = new java::io::File (this, name);
	  if (fileFilter && ! fileFilter->accept(file))
	    continue;

	  list->add(file);
	}
      else
	list->add(name);
    }

  closedir (dir);

  jobjectArray ret = JvNewObjectArray (list->size(), result_type, NULL);
  list->toArray(ret);
  return ret;
#else /* HAVE_DIRENT_H && HAVE_OPENDIR */
  return NULL;
#endif /* HAVE_DIRENT_H && HAVE_OPENDIR */
}

jboolean
java::io::File::performMkdir (void)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

#ifdef HAVE_MKDIR
  return ::mkdir (buf, 0755) == 0;
#else
  return false;
#endif
}

jboolean
java::io::File::performSetReadOnly (void)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

#if defined (HAVE_STAT) && defined (HAVE_CHMOD)
  struct stat sb;
  if (::stat (buf, &sb))
    return false;

  if (::chmod(buf, sb.st_mode & 0555))
    return false;  
  return true;
#else
  return false;
#endif
}

JArray< ::java::io::File *>*
java::io::File::performListRoots ()
{
  ::java::io::File *f = new ::java::io::File (JvNewStringLatin1 ("/"));
  JArray<java::io::File *> *unixroot
    = reinterpret_cast <JArray<java::io::File *>*> 
          (JvNewObjectArray (1, &java::io::File::class$, f));
  elements (unixroot) [0] = f;
  return unixroot;
}

jboolean
java::io::File::performRenameTo (File *dest)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';
  char *buf2
    = (char *) __builtin_alloca (JvGetStringUTFLength (dest->path) + 1);
  total = JvGetStringUTFRegion (dest->path, 0, dest->path->length(), buf2);
  buf2[total] = '\0';

#ifdef HAVE_RENAME
  return ::rename (buf, buf2) == 0;
#else
  return false;
#endif
}

jboolean
java::io::File::performSetLastModified (jlong time)
{
#ifdef HAVE_UTIME
  utimbuf tb;

  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';
  
  tb.actime = time / 1000;
  tb.modtime = time / 1000;
  return ::utime (buf, &tb);
#else
  return false;
#endif
}

jboolean
java::io::File::performCreate (void)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

  int fd = ::open (buf, O_CREAT | O_EXCL, 0644);

  if (fd < 0)
    {
      if (errno == EEXIST)
        return false;
      throw new IOException (JvNewStringLatin1 (strerror (errno)));
    }
  else
    {
      ::close (fd);
      return true;
    }
}

jboolean
java::io::File::performDelete (void)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

#ifdef HAVE_UNLINK
#ifdef HAVE_RMDIR
  if (! ::rmdir (buf))
    return true;
  if (errno == ENOTDIR)
#endif // HAVE_RMDIR
    return ::unlink (buf) == 0;
#endif // HAVE_UNLINK
  return false;
}

void
java::io::File::init_native ()
{
  maxPathLen = MAXPATHLEN;
  caseSensitive = true;
}
