// natFile.cc - Native part of File class for POSIX.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2006, 2008, 2012
   Free Software Foundation

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
java::io::File::access (jint query)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';
  JvAssert (query == READ || query == WRITE || query == EXISTS
	    || query == EXEC);
#ifdef HAVE_ACCESS
  int mode;
  if (query == READ)
    mode = R_OK;
  else if (query == WRITE)
    mode = W_OK;
  else if (query == EXISTS)
    mode = F_OK;
  else
    mode = X_OK;
  return ::access (buf, mode) == 0;
#else
  return false;
#endif
}

jboolean
java::io::File::stat (jint query)
{
  if (query == ISHIDDEN)
    return getName()->charAt(0) == '.';

#ifdef HAVE_STAT
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';

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

// These two methods are used to maintain dynamically allocated
// buffers for getCanonicalPath without the overhead of calling
// realloc every time a buffer is modified.  Buffers are sized
// at the smallest multiple of CHUNKSIZ that is greater than or
// equal to the desired length.  The default CHUNKSIZ is 256,
// longer than most paths, so in most cases a getCanonicalPath
// will require only one malloc per buffer.

#define CHUNKLOG 8
#define CHUNKSIZ (1 << CHUNKLOG)

static int
nextChunkSize (int size)
{
  return ((size >> CHUNKLOG) + ((size & (CHUNKSIZ - 1)) ? 1 : 0)) << CHUNKLOG;
}

static char *
maybeGrowBuf (char *buf, int *size, int required)
{
  if (required > *size)
    {
      *size = nextChunkSize (required);
      buf = (char *) _Jv_Realloc (buf, *size);
    }
  return buf;
}

// Return a canonical representation of the pathname of this file.  On
// the GNU system this involves the removal of redundant separators,
// references to "." and "..", and symbolic links.
//
// The conversion proceeds on a component-by-component basis: symbolic
// links and references to ".."  are resolved as and when they occur.
// This means that if "/foo/bar" is a symbolic link to "/baz" then the
// canonical form of "/foo/bar/.." is "/" and not "/foo".
//
// In order to mimic the behaviour of proprietary JVMs, non-existant
// path components are allowed (a departure from the normal GNU system
// convention).  This means that if "/foo/bar" is a symbolic link to
// "/baz", the canonical form of "/non-existant-directory/../foo/bar"
// is "/baz".

jstring
java::io::File::getCanonicalPath (void)
{
  jstring path = getAbsolutePath ();

  int len = JvGetStringUTFLength (path);
  int srcl = nextChunkSize (len + 1);
  char *src = (char *) _Jv_Malloc (srcl);
  JvGetStringUTFRegion (path, 0, path->length(), src);
  src[len] = '\0';
  int srci = 1;

  int dstl = nextChunkSize (2);  
  char *dst = (char *) _Jv_Malloc (dstl);
  dst[0] = '/';
  int dsti = 1;

  bool fschecks = true;

  while (src[srci] != '\0')
    {
      // Skip slashes.
      while (src[srci] == '/')
	srci++;
      int tmpi = srci;
      // Find next slash.
      while (src[srci] != '/' && src[srci] != '\0')
	srci++;
      if (srci == tmpi)
	// We hit the end.
	break;
      len = srci - tmpi;

      // Handle "." and "..".
      if (len == 1 && src[tmpi] == '.')
	continue;
      if (len == 2 && src[tmpi] == '.' && src[tmpi + 1] == '.')
	{
	  while (dsti > 1 && dst[dsti - 1] != '/')
	    dsti--;
	  if (dsti != 1)
	    dsti--;
	  // Reenable filesystem checking if disabled, as we might
	  // have reversed over whatever caused the problem before.
	  // At least one proprietary JVM has inconsistencies because
	  // it does not do this.
	  fschecks = true;
	  continue;
	}

      // Handle real path components.
      dst = maybeGrowBuf (dst, &dstl, dsti + (dsti > 1 ? 1 : 0) + len + 1);
      int dsti_save = dsti;
      if (dsti > 1)
	dst[dsti++] = '/';
      strncpy (&dst[dsti], &src[tmpi], len);
      dsti += len;
      if (fschecks == false)
	continue;

#if defined (HAVE_LSTAT) && defined (HAVE_READLINK)
      struct stat sb;
      dst[dsti] = '\0';
      if (::lstat (dst, &sb) == 0)
	{
	  if (S_ISLNK (sb.st_mode))
	    {
	      int tmpl = CHUNKSIZ;
	      char *tmp = (char *) _Jv_Malloc (tmpl);

	      while (1)
		{
		  tmpi = ::readlink (dst, tmp, tmpl);
		  if (tmpi < 1)
		    {
		      _Jv_Free (src);
		      _Jv_Free (dst);
		      _Jv_Free (tmp);
		      throw new IOException (
			JvNewStringLatin1 ("readlink failed"));
		    }
		  if (tmpi < tmpl)
		    break;
		  tmpl += CHUNKSIZ;
		  tmp = (char *) _Jv_Realloc (tmp, tmpl);
		}

	      // Prepend the link's path to src.
	      tmp = maybeGrowBuf (tmp, &tmpl, tmpi + strlen (&src[srci]) + 1);
	      strcpy(&tmp[tmpi], &src[srci]);
	      _Jv_Free (src);
	      src = tmp;
	      srcl = tmpl;
	      srci = 0;

	      // Either replace or append dst depending on whether the
	      // link is relative or absolute.
	      dsti = src[0] == '/' ? 1 : dsti_save;
	    }
	}
      else
	{
	  // Something doesn't exist, or we don't have permission to
	  // read it, or a previous path component is a directory, or
	  // a symlink is looped.  Whatever, we can't check the
	  // filesystem any more.
	  fschecks = false;
	}
#endif // HAVE_LSTAT && HAVE_READLINK
    }
  dst[dsti] = '\0';

  // FIXME: what encoding to assume for file names?  This affects many
  // calls.
  path = JvNewStringUTF (dst);
  _Jv_Free (src);
  _Jv_Free (dst);
  return path;
}

jboolean
java::io::File::isAbsolute (void)
{
  return path->length() > 0 && path->charAt(0) == '/';
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
  while ((d = readdir (dir)) != NULL)
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
java::io::File::setFilePermissions (jboolean enable,
				    jboolean ownerOnly,
				    jint permissions)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';
  JvAssert (permissions == READ || permissions == WRITE || permissions == EXEC);
#if defined (HAVE_STAT) && defined (HAVE_CHMOD)
  mode_t mode = 0;

  struct stat sb;
  if (::stat (buf, &sb))
    return false;

  if (ownerOnly)
    {
      if (permissions == READ)
        mode |= S_IRUSR;
      else if (permissions == WRITE)
        mode |= S_IWUSR;
      else if (permissions == EXEC)
        mode |= S_IXUSR;
    }
  else
    {
      if (permissions == READ)
        mode |= (S_IRUSR | S_IRGRP | S_IROTH);
      else if (permissions == WRITE)
        mode |= (S_IWUSR | S_IWGRP | S_IWOTH);
      else if (permissions == EXEC)
        mode |= (S_IXUSR | S_IXGRP | S_IXOTH);
    }
  
  if (enable)
    mode = sb.st_mode | mode;
  else
    mode = sb.st_mode & ~mode;
  
  if (::chmod(buf, mode) < 0)
    return false;
  return true;
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
  return (::utime (buf, &tb) == 0);
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
#ifdef MAXPATHLEN
  maxPathLen = MAXPATHLEN;
#else
  /* Some systems do not have a limit on the length of a file name,
     the GNU system is one such example.  */
  maxPathLen = 0;
#endif
  caseSensitive = true;
}
