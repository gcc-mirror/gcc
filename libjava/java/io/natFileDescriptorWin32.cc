// natFileDescriptorWin32.cc - Native part of FileDescriptor class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software 
   Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// FIXME: In order to support interrupting of IO operations, we
// need to change to use the windows asynchronous IO functions

#include <config.h>

#include <stdio.h>
#include <string.h>

#include <windows.h>
#undef STRICT

#include <gcj/cni.h>
#include <jvm.h>
#include <java/io/FileDescriptor.h>
#include <java/io/SyncFailedException.h>
#include <java/io/IOException.h>
#include <java/io/InterruptedIOException.h>
#include <java/io/EOFException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/String.h>
#include <java/lang/Thread.h>
#include <java/io/FileNotFoundException.h>

// FIXME: casting a FILE (pointer) to a jint will not work on Win64 --
//        we should be using gnu.gcj.RawData's.

void
java::io::FileDescriptor::init(void)
{
  in = new java::io::FileDescriptor((jint)(GetStdHandle (STD_INPUT_HANDLE)));
  out = new java::io::FileDescriptor((jint)(GetStdHandle (STD_OUTPUT_HANDLE)));
  err = new java::io::FileDescriptor((jint)(GetStdHandle (STD_ERROR_HANDLE)));
}

static char *
winerr (void)
{
  static LPVOID last = NULL;
  LPVOID old = NULL;

  if (last)
    old = last;

  FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER |
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    GetLastError(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPTSTR) &last,
    0,
    NULL);

  if (old)
    LocalFree (old);

  return (char *)last;
}

jboolean
java::io::FileDescriptor::valid (void) {
  BY_HANDLE_FILE_INFORMATION info;
  return GetFileInformationByHandle ((HANDLE)fd, &info) != 0;
}

void
java::io::FileDescriptor::sync (void) {
  if (! FlushFileBuffers ((HANDLE)fd))
    throw new SyncFailedException (JvNewStringLatin1 (winerr ()));
}

jint
java::io::FileDescriptor::open (jstring path, jint jflags) {

  HANDLE handle = NULL;
  DWORD access = 0;
  DWORD create = OPEN_EXISTING;
  char buf[MAX_PATH] = "";

  jsize total = JvGetStringUTFRegion(path, 0, path->length(), buf);
  buf[total] = '\0';

  JvAssert((jflags & READ) || (jflags & WRITE));

  if ((jflags & READ) && (jflags & WRITE))
    {
      access = GENERIC_READ | GENERIC_WRITE;
      if (jflags & APPEND)
	create = OPEN_ALWAYS;
      else
	create = CREATE_ALWAYS;
    }
  else if(jflags & READ)
    access = GENERIC_READ;
  else
    {
      access = GENERIC_WRITE;
      if (jflags & APPEND)
	create = OPEN_ALWAYS;
      else
        create = CREATE_ALWAYS;
    }

  handle = CreateFile(buf, access, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, create, 0, NULL);

  if (handle == INVALID_HANDLE_VALUE)
    {
      char msg[MAX_PATH + 1000];
      sprintf (msg, "%s: %s", buf, winerr ());
      throw new FileNotFoundException (JvNewStringLatin1 (msg));
    }

  // For APPEND mode, move the file pointer to the end of the file.
  if (jflags & APPEND)
    {
      DWORD low = SetFilePointer (handle, 0, NULL, FILE_END);
      if ((low == 0xffffffff) && (GetLastError () != NO_ERROR)) 
        throw new FileNotFoundException (JvNewStringLatin1 (winerr ()));
    }
  return (jint)handle;
}

void
java::io::FileDescriptor::write (jint b)
{
  DWORD bytesWritten;
  jbyte buf = (jbyte)b;

  if (WriteFile ((HANDLE)fd, &buf, 1, &bytesWritten, NULL))
    {
      if (java::lang::Thread::interrupted())
        {
          InterruptedIOException *iioe = new InterruptedIOException (JvNewStringLatin1 ("write interrupted"));
          iioe->bytesTransferred = bytesWritten;
	  throw iioe;
        }
      if (bytesWritten != 1)
	throw new IOException (JvNewStringLatin1 (winerr ()));
    }
  else
    throw new IOException (JvNewStringLatin1 (winerr ()));
  // FIXME: loop until bytesWritten == 1
}

void
java::io::FileDescriptor::write(jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new java::lang::NullPointerException;
  if(offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new java::lang::ArrayIndexOutOfBoundsException;

  jbyte *buf = elements (b) + offset;
  DWORD bytesWritten;
  if (WriteFile ((HANDLE)fd, buf, len, &bytesWritten, NULL))
    {
      if (java::lang::Thread::interrupted())
        {
          InterruptedIOException *iioe = new InterruptedIOException (JvNewStringLatin1 ("write interrupted"));
          iioe->bytesTransferred = bytesWritten;
	  throw iioe;
        }
    }
  else
    throw new IOException (JvNewStringLatin1 (winerr ()));
  // FIXME: loop until bytesWritten == len
}

void
java::io::FileDescriptor::close (void)
{
  HANDLE save = (HANDLE)fd;
  fd = (jint)INVALID_HANDLE_VALUE;
  if (! CloseHandle (save))
    throw new IOException (JvNewStringLatin1 (winerr ()));
}

void
java::io::FileDescriptor::setLength(jlong pos)
{
  LONG liOrigFilePointer;
  LONG liNewFilePointer;
  LONG liEndFilePointer;

  // Get the original file pointer.
  if (SetFilePointer((HANDLE) fd, (LONG) 0, &liOrigFilePointer,
		     FILE_CURRENT) != (BOOL) 0
      && (GetLastError() != NO_ERROR))
    throw new IOException (JvNewStringLatin1 (winerr ()));

  // Get the length of the file.
  if (SetFilePointer((HANDLE) fd, (LONG) 0, &liEndFilePointer,
		     FILE_END) != (BOOL) 0
      && (GetLastError() != NO_ERROR))
    throw new IOException (JvNewStringLatin1 (winerr ()));

  if ((jlong)liEndFilePointer == pos)
    {
      // Restore the file pointer.
      if (liOrigFilePointer != liEndFilePointer)
	{
	  if (SetFilePointer((HANDLE) fd, liOrigFilePointer, &liNewFilePointer,
			     FILE_BEGIN) != (BOOL) 0
	      && (GetLastError() != NO_ERROR))
	    throw new IOException (JvNewStringLatin1 (winerr ()));
	}
      return;
    }

  // Seek to the new end of file.
  if (SetFilePointer((HANDLE) fd, (LONG) pos, &liNewFilePointer,
		     FILE_BEGIN) != (BOOL) 0
      && (GetLastError() != NO_ERROR))
    throw new IOException (JvNewStringLatin1 (winerr ()));

  // Truncate the file at this point.
  if (SetEndOfFile((HANDLE) fd) != (BOOL) 0 && (GetLastError() != NO_ERROR))
    throw new IOException (JvNewStringLatin1 (winerr ()));

  if (liOrigFilePointer < liNewFilePointer)
    {
      // Restore the file pointer.
      if (SetFilePointer((HANDLE) fd, liOrigFilePointer, &liNewFilePointer,
			 FILE_BEGIN) != (BOOL) 0
	  && (GetLastError() != NO_ERROR))
	throw new IOException (JvNewStringLatin1 (winerr ()));
    }
}

jint
java::io::FileDescriptor::seek (jlong pos, jint whence, jboolean eof_trunc)
{
  JvAssert (whence == SET || whence == CUR);

  jlong len = length();
  jlong here = getFilePointer();

  if (eof_trunc
      && ((whence == SET && pos > len) || (whence == CUR && here + pos > len)))
    {
      whence = SET;
      pos = len;
    }

  LONG high = pos >> 32;
  DWORD low = SetFilePointer ((HANDLE)fd, (DWORD)(0xffffffff & pos), &high, whence == SET ? FILE_BEGIN : FILE_CURRENT);
  if ((low == 0xffffffff) && (GetLastError () != NO_ERROR))
    throw new IOException (JvNewStringLatin1 (winerr ()));
  return low;
}

jlong
java::io::FileDescriptor::getFilePointer(void)
{
  LONG high = 0;
  DWORD low = SetFilePointer ((HANDLE)fd, 0, &high, FILE_CURRENT);
  if ((low == 0xffffffff) && (GetLastError() != NO_ERROR))
    throw new IOException (JvNewStringLatin1 (winerr ()));
  return (((jlong)high) << 32L) | (jlong)low;
}

jlong
java::io::FileDescriptor::length(void)
{
  DWORD high;
  DWORD low;

  low = GetFileSize ((HANDLE)fd, &high);
  // FIXME: Error checking
  return (((jlong)high) << 32L) | (jlong)low;
}

jint
java::io::FileDescriptor::read(void)
{
  CHAR buf;
  DWORD read;

  if (! ReadFile ((HANDLE)fd, &buf, 1, &read, NULL))
    {
      if (GetLastError () == ERROR_BROKEN_PIPE)
        return -1;
      else
        throw new IOException (JvNewStringLatin1 (winerr ()));
    }

  if (! read)
    return -1;
  else
    return (jint)(buf & 0xff);
}

jint
java::io::FileDescriptor::read(jbyteArray buffer, jint offset, jint count)
{
  if (! buffer)
    throw new java::lang::NullPointerException;

  jsize bsize = JvGetArrayLength (buffer);
  if (offset < 0 || count < 0 || offset + count > bsize)
    throw new java::lang::ArrayIndexOutOfBoundsException;

  // Must return 0 if an attempt is made to read 0 bytes.
  if (count == 0)
    return 0;

  jbyte *bytes = elements (buffer) + offset;

  DWORD read;
  if (! ReadFile((HANDLE)fd, bytes, count, &read, NULL))
    {
      if (GetLastError () == ERROR_BROKEN_PIPE)
        return -1;
      else
        throw new IOException (JvNewStringLatin1 (winerr ()));
    }

  if (read == 0) return -1;

  return (jint)read;
}

jint
java::io::FileDescriptor::available(void)
{
  // FIXME:
  return length() - getFilePointer();
}
