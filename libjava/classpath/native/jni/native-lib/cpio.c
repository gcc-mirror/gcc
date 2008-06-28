/* cpio.c - Common java file IO native functions
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

/* do not move; needed here because of some macro definitions */
#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>

#include <jni.h>

#if defined(HAVE_SYS_IOCTL_H)
#define BSD_COMP /* Get FIONREAD on Solaris2 */
#include <sys/ioctl.h>
#endif
#if defined(HAVE_SYS_FILIO_H) /* Get FIONREAD on Solaris 2.5 */
#include <sys/filio.h>
#endif

#if defined(HAVE_SYS_STAT_H)
#include <sys/stat.h>
#endif

#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(HAVE_SYS_SELECT_H)
#include <sys/select.h>
#endif

#if defined(HAVE_STATVFS)
#include <sys/statvfs.h>
#endif

#include <utime.h>

#include "cpnative.h"
#include "cpio.h"

/* Some POSIX systems don't have O_SYNC and O_DYSNC so we define them here.  */
#if !defined (O_SYNC) && defined (O_FSYNC)
#define O_SYNC O_FSYNC
#endif
#if !defined (O_DSYNC) && defined (O_FSYNC)
#define O_DSYNC O_FSYNC
#endif
/* If O_DSYNC is still not defined, use O_SYNC (needed for newlib).  */
#if !defined (O_DSYNC)
#define O_DSYNC O_SYNC
#endif

JNIEXPORT int cpio_openFile (const char *filename, int *fd, int flags, int permissions)
{
  int sflags = 0;
  int rwflags = flags & CPFILE_FLAG_READWRITE;
  int perms;

  if (flags & CPFILE_FLAG_CREATE)
    sflags |= O_CREAT;
  if (flags & CPFILE_FLAG_APPEND)
    sflags |= O_APPEND;
  if (flags & CPFILE_FLAG_TRUNCATE)
    sflags |= O_TRUNC;
  if (flags & CPFILE_FLAG_SYNC)
    sflags |= O_SYNC;
  if (flags & CPFILE_FLAG_DSYNC)
    sflags |= O_DSYNC;
#if defined(O_BINARY)
  if (flags & CPFILE_FLAG_BINARY)
    sflags |= O_BINARY;
#endif

  switch (rwflags)
    {
    case CPFILE_FLAG_READ:
      sflags |= O_RDONLY;
      break;
    case CPFILE_FLAG_WRITE:
      sflags |= O_WRONLY;
      break;
    case CPFILE_FLAG_READWRITE:
      sflags |= O_RDWR;
      break;
    }

  if (permissions == CPFILE_PERMISSION_NORMAL)
	  perms = (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  else
	  perms = 0;

  *fd = open (filename, sflags, perms);
  
  if (*fd < 0)
    return errno;

  return CPNATIVE_OK;
}

JNIEXPORT int cpio_closeFile (int fd)
{
  if (close (fd) < 0)
    return errno;
  
  return CPNATIVE_OK;
}

JNIEXPORT int cpio_availableBytes (int fd, jlong *bytes_available)
{
#if defined (FIONREAD)
  ssize_t n;

  if (ioctl (fd, FIONREAD, (char *)&n) != 0)
    return errno;

  *bytes_available = n;
  return CPNATIVE_OK;  
#elif defined(HAVE_FSTAT)
  struct stat statBuffer;
  off_t n;
  int result;

  *bytes_available = 0;
  if ((fstat (fd, &statBuffer) == 0) && S_ISREG (statBuffer.st_mode))
    {
      n = lseek (fd, 0, SEEK_CUR);
      if (n != -1) 
       { 
         *bytes_available = statBuffer.st_size - n; 
         result = CPNATIVE_OK;
       } 
      else 
       { 
         result = errno;
       } 
    } 
  else 
    { 
      result = errno;
    } 
  
  return result;
#elif defined(HAVE_SELECT)
  fd_set filedescriptset;
  struct timeval tv;
  int result;

  *bytes_available = 0;
  
  FD_ZERO (&filedescriptset);
  FD_SET (fd,&filedescriptset);
  memset (&tv, 0, sizeof(tv));

  switch (select (fd+1, &filedescriptset, NULL, NULL, &tv))
    {
    case -1: 
      result=errno; 
      break;
    case  0:
      *bytes_available = 0;
      result = CPNATIVE_OK;
      break;      
    default: 
      *bytes_available = 1;
      result = CPNATIVE_OK;
      break;
    }
  return result;

#else
  *bytes_available = 0;
  return ENOTSUP;
#endif
}

JNIEXPORT int cpio_getFileSize (int fd, jlong *filesize)
{
  struct stat statBuffer;

  if (fstat(fd, &statBuffer) < 0)
    return errno;
  
  *filesize = statBuffer.st_size;
  return CPNATIVE_OK;
}

JNIEXPORT int cpio_getFilePosition (int fd, jlong *offset)
{
  *offset = lseek (fd, 0, SEEK_CUR);
  if (*offset < 0)
    return errno;
  
  return CPNATIVE_OK;
}

JNIEXPORT int cpio_setFilePosition (int fd, jlong position)
{
  if (lseek (fd, position, SEEK_SET) < 0)
    return errno;

  return CPNATIVE_OK;
}

JNIEXPORT int cpio_read (int fd, void *buffer, jint length, jint *bytes_read)
{
  *bytes_read = read (fd, buffer, length);
  
  if (*bytes_read < 0)
  {
    return errno;
  }

  return CPNATIVE_OK;
}

JNIEXPORT int cpio_write (int fd, const void *buffer, jint length, jint *bytes_written)
{
  *bytes_written = write (fd, buffer, length);
  
  if (*bytes_written < 0)
    return errno;

  return CPNATIVE_OK;
}

JNIEXPORT int cpio_fsync (int fd)
{
  if (fsync (fd) < 0)
    return errno;

  return CPNATIVE_OK;
}

JNIEXPORT int cpio_truncate (int fd, jlong size)
{
  if (ftruncate (fd, size) < 0)
    return errno;

  return CPNATIVE_OK;
}

JNIEXPORT int cpio_setFileSize (int native_fd, jlong new_size)
{
  jlong file_size;
  jlong save_offset;
  int result;
  char data;
  jint bytes_written;
  
  result = cpio_getFileSize (native_fd, &file_size);
  if (result != CPNATIVE_OK)
    return result;

  /* Save off current position */
  result = cpio_getFilePosition (native_fd, &save_offset);
  if (result != CPNATIVE_OK)
    return result;

  if (file_size < new_size)
    {
      /* File is too short -- seek to one byte short of where we want,
       * then write a byte */

      /* move to position n-1 */
      result = cpio_setFilePosition (native_fd, new_size-1);
      if (result != CPNATIVE_OK)
	return result;

      /* write a byte
         Note: This will fail if we somehow get here in read only mode
         * That shouldn't happen */
      data = '\0';
      result = cpio_write (native_fd, &data, 1, &bytes_written);
      if (result != CPNATIVE_OK)
	return result;

      /* Reposition file pointer to where we started if not beyond new len. */
      if (save_offset < new_size)
	{
	  result = cpio_setFilePosition (native_fd, save_offset);
	  if (result != CPNATIVE_OK)
	    return result;
	}
    }
  else if (new_size < file_size)
    {
      /* File is too long - use ftruncate if available */
      result = cpio_truncate (native_fd, new_size);
      if (result != CPNATIVE_OK)
	  return result;

      /* Reposition file pointer when it now is beyond the end of file. */
      if (new_size < save_offset)
	{
	  result = cpio_setFilePosition (native_fd, new_size);
	  if (result != CPNATIVE_OK)
	    return result;
	}
    }

  return CPNATIVE_OK;
}

int cpio_setFileReadonly (const char *filename)
{
  struct stat statbuf;

  if (stat(filename, &statbuf) < 0)
    return errno;

#ifdef S_IWRITE 
  if (chmod(filename, statbuf.st_mode & ~(S_IWRITE | S_IWGRP | S_IWOTH)) < 0)
    return errno;
#endif

  return 0;
}

int cpio_chmod (const char *filename, int permissions)
{
  struct stat statbuf;
  int perms = 0;

  if (stat(filename, &statbuf) < 0)
    return errno;
  
  /* check for permission flags */
  if (permissions & CPFILE_FLAG_USR)
    {
      if (permissions & CPFILE_FLAG_READ)
        perms |= S_IRUSR;
  
      if (permissions & CPFILE_FLAG_WRITE)
        perms |= S_IWUSR;
        
      if (permissions & CPFILE_FLAG_EXEC)
        perms |= S_IXUSR;
    }
  else
    {
      if (permissions & CPFILE_FLAG_READ)
        perms |= (S_IRUSR | S_IRGRP | S_IROTH);
        
      if (permissions & CPFILE_FLAG_WRITE)
        perms |= (S_IWUSR | S_IWGRP | S_IWOTH);
        
      if (permissions & CPFILE_FLAG_EXEC)
        perms |= (S_IXUSR | S_IXGRP | S_IXOTH);
    }
  
  if (permissions & CPFILE_FLAG_OFF)
    perms = statbuf.st_mode & ~perms;
  else
    perms = statbuf.st_mode | perms;
  
  if (chmod(filename, perms) < 0)
    return errno;
  
  return 0;
}

JNIEXPORT long long
cpio_df (__attribute__((unused)) const char *path,
         __attribute__((unused)) CPFILE_DF_TYPE type)
{
  long long result = 0L;
  
#if defined(HAVE_STATVFS)

  long long scale_factor = 0L;
  struct statvfs buf;
  
  if (statvfs (path, &buf) < 0)
    return 0L;
  
  /* f_blocks, f_bfree and f_bavail are defined in terms of f_frsize */
  scale_factor = (long long) (buf.f_frsize);

  switch (type)
    {
      case TOTAL:
        result = (long long) (buf.f_blocks * scale_factor);
        break;
      case FREE:
        result = (long long) (buf.f_bfree * scale_factor);
        break;
      case USABLE:
        result = (long long) (buf.f_bavail * scale_factor);
        break;
      default:
        result = 0L;
        break;  
    }
    
#endif

  return result;
}

int cpio_checkAccess (const char *filename, unsigned int flag)
{
  struct stat statbuf;
  unsigned int perms = 0;
 
  if (stat(filename, &statbuf) < 0)
    return errno;
  
  switch (flag)
    {
    case CPFILE_FLAG_READ:
      perms = R_OK;
      break;
      
    case CPFILE_FLAG_WRITE:
      perms = W_OK;
      break;
      
    case CPFILE_FLAG_EXEC:
    default:
      perms = X_OK;
      break;
    }
  
  return (access (filename, perms));
}

int cpio_isFileExists (const char *filename)
{
  struct stat statbuf;

  if (stat(filename, &statbuf) < 0)
    {
      return errno;
    }

  return 0;
}

int cpio_checkType (const char *filename, jint *entryType)
{
  struct stat statbuf;

  if (stat(filename, &statbuf) < 0)
    return errno;

  if (S_ISDIR(statbuf.st_mode))
    *entryType = CPFILE_DIRECTORY;
  else
    *entryType = CPFILE_FILE;

  return 0;
}

int cpio_getModificationTime (const char *filename, jlong *mtime)
{
  struct stat statbuf;

  if (stat(filename, &statbuf) < 0)
    return errno;

  *mtime = (jlong)statbuf.st_mtime * (jlong)1000;

  return 0;
}

int cpio_setModificationTime (const char *filename, jlong mtime)
{
  struct stat statbuf;
  struct utimbuf buf;

  if (stat(filename, &statbuf) < 0)
    return errno;

  buf.actime = statbuf.st_atime;
  buf.modtime = mtime / 1000;

  if (utime(filename, &buf) < 0)
    return errno;

  return 0;
}

int cpio_removeFile (const char *filename)
{
  if (unlink(filename) < 0 && rmdir(filename) < 0)
    return errno;

  return 0;
}

int cpio_mkdir (const char *path)
{
  if (mkdir(path, S_IRWXU | S_IRWXG | S_IRWXO) < 0)
    return errno;
  
  return 0;
}

int cpio_rename (const char *old_name, const char *new_name)
{
  if (rename(old_name, new_name) < 0)
    return errno;

  return 0;
}

int cpio_openDir (const char *dirname, void **handle)
{
  *handle = (void *)opendir(dirname);
  if (*handle == NULL)
    return errno;
  
  return 0;
}

int cpio_closeDir (void *handle)
{
  closedir((DIR *)handle);
  return 0;
}


int cpio_readDir (void *handle, char *filename)
{
  struct dirent *dBuf;

  errno = 0;
  dBuf = readdir((DIR *)handle);

  if (dBuf == NULL)
    {
      /* Some OS's (OS X) return NULL on end-of-dir, but
         don't set errno to anything. */
      if (errno == 0)
        return ENOENT; /* Whatever. */
      return errno;
    }

  strncpy (filename, dBuf->d_name, FILENAME_MAX - 1);
  return 0;
}

int
cpio_closeOnExec(int fd)
{
	if (fcntl (fd, F_SETFD, FD_CLOEXEC) == -1)
	  return errno;
	  
	return 0;
}
