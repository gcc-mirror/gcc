/* cpio.h -
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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

#ifndef _CLASSPATH_IO_H_INCLUDED
#define _CLASSPATH_IO_H_INCLUDED

#include <jni.h>

#define CPFILE_FLAG_CREATE   0x0001
#define CPFILE_FLAG_APPEND   0x0002
#define CPFILE_FLAG_TRUNCATE 0x0004
#define CPFILE_FLAG_SYNC     0x0008
#define CPFILE_FLAG_DSYNC    0x0010
#define CPFILE_FLAG_BINARY   0x0020
#define CPFILE_FLAG_READ     0x0040
#define CPFILE_FLAG_WRITE    0x0080
#define CPFILE_FLAG_EXEC     0x0100
#define CPFILE_FLAG_USR      0x0400
#define CPFILE_FLAG_OFF      0x0800

#define CPFILE_PERMISSION_NORMAL 1

#define CPFILE_FLAG_READWRITE (CPFILE_FLAG_READ|CPFILE_FLAG_WRITE)

JNIEXPORT int cpio_openFile (const char *filename, int *fd, int flags, int permissions);
JNIEXPORT int cpio_closeFile (int fd);
JNIEXPORT int cpio_availableBytes (int fd, jlong *avail);
JNIEXPORT int cpio_getFileSize (int fd, jlong *filesize);
JNIEXPORT int cpio_setFileSize (int fd, jlong filesize);
JNIEXPORT int cpio_getFilePosition (int fd, jlong *position);
JNIEXPORT int cpio_setFilePosition (int fd, jlong position);
JNIEXPORT int cpio_read (int fd, void *data, jint len, jint *bytes_read);
JNIEXPORT int cpio_write (int fd, const void *data, jint len, jint *bytes_written);
JNIEXPORT int cpio_fsync (int fd);
JNIEXPORT int cpio_truncate (int fd, jlong size);
JNIEXPORT int cpio_closeOnExec(int fd);

#define CPFILE_FILE 0
#define CPFILE_DIRECTORY 1

JNIEXPORT int cpio_setFileReadonly (const char *filename);
JNIEXPORT int cpio_chmod (const char *filename, int permissions);
JNIEXPORT int cpio_checkAccess (const char *filename, unsigned int flag);
JNIEXPORT int cpio_isFileExists (const char *filename);
JNIEXPORT int cpio_checkType (const char *filename, jint *entryType);
JNIEXPORT int cpio_getModificationTime (const char *filename, jlong *mtime);
JNIEXPORT int cpio_setModificationTime (const char *filename, jlong mtime);
JNIEXPORT int cpio_removeFile (const char *filename);
JNIEXPORT int cpio_mkdir (const char *filename);
JNIEXPORT int cpio_rename (const char *old_name, const char *new_name);

/* to be used with cpio_df */
typedef enum {
  TOTAL = 0,
  FREE,
  USABLE
} CPFILE_DF_TYPE;

JNIEXPORT long long cpio_df (const char *path, CPFILE_DF_TYPE type);

JNIEXPORT int cpio_openDir (const char *dirname, void **handle);
JNIEXPORT int cpio_closeDir (void *handle);
JNIEXPORT int cpio_readDir (void *handle, char *filename);

#endif
