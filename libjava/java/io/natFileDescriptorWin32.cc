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
#include <platform.h>

#include <stdio.h>
#include <string.h>

#undef STRICT

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

static bool testCanUseGetHandleInfo()
{
  /* Test to see whether GetHandleInformation can be used
     for console input or screen buffers. This is better
     a kludgy OS version check. */
  DWORD dwFlags;
  return GetHandleInformation (GetStdHandle (STD_INPUT_HANDLE),
    &dwFlags) != 0;
}

// FIXME: casting a FILE (pointer) to a jint will not work on Win64 --
//        we should be using gnu.gcj.RawData's.

void
java::io::FileDescriptor::init(void)
{
  in = new java::io::FileDescriptor((jint)(GetStdHandle (STD_INPUT_HANDLE)));
  out = new java::io::FileDescriptor((jint)(GetStdHandle (STD_OUTPUT_HANDLE)));
  err = new java::io::FileDescriptor((jint)(GetStdHandle (STD_ERROR_HANDLE)));
}

jboolean
java::io::FileDescriptor::valid (void) {
  static bool bCanUseGetHandleInfo = testCanUseGetHandleInfo();
  if (bCanUseGetHandleInfo)
  {
    /* As with UNIX, a "file" descriptor can be one of
       a gazillion possible underlying things like a pipe
       or socket, so we can't get too fancy here. */
    DWORD dwFlags;
    HANDLE h = (HANDLE) fd;
    return GetHandleInformation (h, &dwFlags) != 0;
  }
  else
  {
    /* Can't use GetHandleInformation() for console handles on < WinNT 5. */
    return true;
  }
}

void
java::io::FileDescriptor::sync (void) {
  if (! FlushFileBuffers ((HANDLE)fd))
  {
    DWORD dwErrorCode = GetLastError ();
    throw new SyncFailedException (_Jv_WinStrError (dwErrorCode));
  }
}

jint
java::io::FileDescriptor::open (jstring path, jint jflags) {

  HANDLE handle = NULL;
  DWORD access = 0;
  DWORD create = OPEN_EXISTING;
  
  JV_TEMP_STRING_WIN32(cpath, path)

  JvAssert((jflags & READ) || (jflags & WRITE));

  if ((jflags & READ) && (jflags & WRITE))
    {
      access = GENERIC_READ | GENERIC_WRITE;
      if (jflags & EXCL)
        create = CREATE_NEW; // this will raise error if file exists.
      else
        create = OPEN_ALWAYS; // equivalent to O_CREAT
    }
  else if (jflags & READ)
    {
      access = GENERIC_READ;
      create = OPEN_EXISTING; // ignore EXCL
    }
  else
    { 
      access = GENERIC_WRITE;
      if (jflags & EXCL)
        create = CREATE_NEW;
      else if (jflags & APPEND)
        create = OPEN_ALWAYS;
      else
        create = CREATE_ALWAYS;
    }

  handle = CreateFile(cpath, access, FILE_SHARE_READ | FILE_SHARE_WRITE,
    NULL, create, 0, NULL);

  if (handle == INVALID_HANDLE_VALUE)
    {
       DWORD dwErrorCode = GetLastError ();
       throw new FileNotFoundException (_Jv_WinStrError (cpath, dwErrorCode));
    }

  // For APPEND mode, move the file pointer to the end of the file.
  if (jflags & APPEND)
    {
      DWORD low = SetFilePointer (handle, 0, NULL, FILE_END);
      if ((low == 0xffffffff) && (GetLastError () != NO_ERROR)) 
      {
        DWORD dwErrorCode = GetLastError ();
        throw new FileNotFoundException (_Jv_WinStrError (cpath, dwErrorCode));
      }
    }
    
  // Make this handle non-inheritable so that child
  // processes don't inadvertently prevent us from
  // closing this file.
  _Jv_platform_close_on_exec (handle);

  return (jint) handle;
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
        _Jv_ThrowIOException ();
    }
  else
    _Jv_ThrowIOException ();
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
    _Jv_ThrowIOException ();
  // FIXME: loop until bytesWritten == len
}

void
java::io::FileDescriptor::close (void)
{
  HANDLE save = (HANDLE)fd;
  fd = (jint)INVALID_HANDLE_VALUE;
  if (! CloseHandle (save))
    _Jv_ThrowIOException ();
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
    _Jv_ThrowIOException ();

  // Get the length of the file.
  if (SetFilePointer((HANDLE) fd, (LONG) 0, &liEndFilePointer,
         FILE_END) != (BOOL) 0
      && (GetLastError() != NO_ERROR))
    _Jv_ThrowIOException ();

  if ((jlong)liEndFilePointer == pos)
    {
      // Restore the file pointer.
      if (liOrigFilePointer != liEndFilePointer)
  {
    if (SetFilePointer((HANDLE) fd, liOrigFilePointer, &liNewFilePointer,
           FILE_BEGIN) != (BOOL) 0
        && (GetLastError() != NO_ERROR))
      _Jv_ThrowIOException ();
  }
      return;
    }

  // Seek to the new end of file.
  if (SetFilePointer((HANDLE) fd, (LONG) pos, &liNewFilePointer,
         FILE_BEGIN) != (BOOL) 0
      && (GetLastError() != NO_ERROR))
    _Jv_ThrowIOException ();

  // Truncate the file at this point.
  if (SetEndOfFile((HANDLE) fd) != (BOOL) 0 && (GetLastError() != NO_ERROR))
    _Jv_ThrowIOException ();

  if (liOrigFilePointer < liNewFilePointer)
    {
      // Restore the file pointer.
      if (SetFilePointer((HANDLE) fd, liOrigFilePointer, &liNewFilePointer,
        FILE_BEGIN) != (BOOL) 0
        && (GetLastError() != NO_ERROR))
        _Jv_ThrowIOException ();
    }
}

jint
java::io::FileDescriptor::seek (jlong pos, jint whence, jboolean eof_trunc)
{
  JvAssert (whence == SET || whence == CUR);

  jlong len = getLength();
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
    _Jv_ThrowIOException ();
  return low;
}

jlong
java::io::FileDescriptor::getFilePointer(void)
{
  LONG high = 0;
  DWORD low = SetFilePointer ((HANDLE)fd, 0, &high, FILE_CURRENT);
  if ((low == 0xffffffff) && (GetLastError() != NO_ERROR))
    _Jv_ThrowIOException ();
  return (((jlong)high) << 32L) | (jlong)low;
}

jlong
java::io::FileDescriptor::getLength(void)
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
        _Jv_ThrowIOException ();
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
        _Jv_ThrowIOException ();
    }

  if (read == 0) return -1;

  return (jint)read;
}

jint
java::io::FileDescriptor::available(void)
{
  // FIXME:
  return getLength() - getFilePointer();
}
