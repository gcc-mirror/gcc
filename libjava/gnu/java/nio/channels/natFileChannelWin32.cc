// natFileChannelImplWin32.cc - Native part of FileChannelImpl class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004  Free Software 
   Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// FIXME: In order to support interrupting of IO operations, we
// need to change to use the windows asynchronous IO functions

#include <config.h>
#include <platform.h>

#include <gcj/cni.h>
#include <gcj/javaprims.h>
#include <jvm.h>

#include <stdio.h>

#include <gnu/gcj/RawData.h>
#include <gnu/java/nio/FileLockImpl.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>
#include <java/io/FileNotFoundException.h>
#include <java/io/IOException.h>
#include <java/io/SyncFailedException.h>
#include <java/io/InterruptedIOException.h>
#include <java/io/EOFException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/System.h>
#include <java/lang/String.h>
#include <java/lang/Thread.h>
#include <java/nio/ByteBuffer.h>
#include <java/nio/MappedByteBufferImpl.h>
#include <java/nio/channels/FileChannel.h>
#include <java/nio/channels/FileLock.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>

using gnu::gcj::RawData;
using java::io::IOException;
using java::nio::MappedByteBufferImpl;
using java::io::InterruptedIOException;
using java::io::FileNotFoundException;
using java::lang::ArrayIndexOutOfBoundsException;
using gnu::java::nio::channels::FileChannelImpl;

#undef STRICT

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
FileChannelImpl::init(void)
{
  in = new FileChannelImpl((jint)(GetStdHandle (STD_INPUT_HANDLE)),
			   FileChannelImpl::READ);
  out = new FileChannelImpl((jint)(GetStdHandle (STD_OUTPUT_HANDLE)),
			    FileChannelImpl::WRITE);
  err = new FileChannelImpl((jint)(GetStdHandle (STD_ERROR_HANDLE)),
			    FileChannelImpl::WRITE);
}

#if 0
FileChannelImpl::sync (void) {
  if (! FlushFileBuffers ((HANDLE)fd))
  {
    DWORD dwErrorCode = GetLastError ();
    throw new SyncFailedException (_Jv_WinStrError (dwErrorCode));
  }
}
#endif

jint
FileChannelImpl::open (jstring path, jint jflags) {

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
      if ((low == (DWORD) 0xffffffff) && (GetLastError () != NO_ERROR)) 
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
FileChannelImpl::write (jint b)
{
  DWORD bytesWritten;
  jbyte buf = (jbyte)b;

  if (WriteFile ((HANDLE)fd, &buf, 1, &bytesWritten, NULL))
    {
      if (::java::lang::Thread::interrupted())
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
FileChannelImpl::write(jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new ::java::lang::NullPointerException;
  if(offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new ArrayIndexOutOfBoundsException;

  jbyte *buf = elements (b) + offset;
  DWORD bytesWritten;

  if (WriteFile ((HANDLE)fd, buf, len, &bytesWritten, NULL))
    {
      if (::java::lang::Thread::interrupted())
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
FileChannelImpl::implCloseChannel (void)
{
  HANDLE save = (HANDLE)fd;
  fd = (jint)INVALID_HANDLE_VALUE;
  if (! CloseHandle (save))
    _Jv_ThrowIOException ();
}

void
FileChannelImpl::implTruncate (jlong size)
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

  if ((jlong)liEndFilePointer == size)
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
  if (SetFilePointer((HANDLE) fd, (LONG) size, &liNewFilePointer,
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

void
FileChannelImpl::seek (jlong newPos)
{
  LONG high = pos >> 32;
  DWORD low = SetFilePointer ((HANDLE)fd, (DWORD)(0xffffffff & newPos), &high, FILE_BEGIN);
  if ((low == 0xffffffff) && (GetLastError () != NO_ERROR))
    _Jv_ThrowIOException ();
}

jlong
FileChannelImpl::implPosition (void)
{
  LONG high = 0;
  DWORD low = SetFilePointer ((HANDLE)fd, 0, &high, FILE_CURRENT);
  if ((low == 0xffffffff) && (GetLastError() != NO_ERROR))
    _Jv_ThrowIOException ();
  return (((jlong)high) << 32L) | (jlong)low;
}

jlong
FileChannelImpl::size (void)
{
  DWORD high;
  DWORD low;

  low = GetFileSize ((HANDLE)fd, &high);
  // FIXME: Error checking
  return (((jlong)high) << 32L) | (jlong)low;
}

jint
FileChannelImpl::read (void)
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
FileChannelImpl::read (jbyteArray buffer, jint offset, jint count)
{
  if (! buffer)
    throw new ::java::lang::NullPointerException;

  jsize bsize = JvGetArrayLength (buffer);
  if (offset < 0 || count < 0 || offset + count > bsize)
    throw new ArrayIndexOutOfBoundsException;

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
FileChannelImpl::available (void)
{
  // FIXME:
  return size() - position();
}

jboolean
FileChannelImpl::lock (jlong pos, jlong len, jboolean shared, jboolean wait)
{
  DWORD flags = 0;
  OVERLAPPED ovlpd;

  ZeroMemory(&ovlpd,sizeof(OVERLAPPED));

  if(!shared)
    flags |= LOCKFILE_EXCLUSIVE_LOCK;
  if(!wait)
    flags |= LOCKFILE_FAIL_IMMEDIATELY;

  ovlpd.Offset = (DWORD)pos;
  ovlpd.OffsetHigh = pos>>32;

  DWORD lenlow = (DWORD)len;
  DWORD lenhigh = len>>32;

  BOOL ret = LockFileEx((HANDLE)fd,flags,0,lenlow,lenhigh,&ovlpd);

  if(ret==ERROR_IO_PENDING && !shared && wait)
    ret = GetOverlappedResult((HANDLE)fd,&ovlpd,NULL,wait);

  if(!ret)
    _Jv_ThrowIOException(GetLastError());

  return true;
}

void
FileChannelImpl::unlock (jlong pos, jlong len)
{
  OVERLAPPED ovlpd;

  ZeroMemory(&ovlpd,sizeof(OVERLAPPED));

  ovlpd.Offset = (DWORD)pos;
  ovlpd.OffsetHigh = pos>>32;

  DWORD lenlow = (DWORD)len;
  DWORD lenhigh = len>>32;

  BOOL ret = UnlockFileEx((HANDLE)fd,0,lenlow,lenhigh,&ovlpd);

  if(!ret)
    _Jv_ThrowIOException(GetLastError());
}

java::nio::MappedByteBuffer *
FileChannelImpl::mapImpl (jchar mmode, jlong position, jint size)
{
  SYSTEM_INFO siSysInfo;
  GetSystemInfo(&siSysInfo); 
  DWORD page_size = siSysInfo.dwPageSize;
  jlong offset = position & ~(page_size-1);
  jint align = position - offset;
  jlong high = position + size;
  jlong max_size;
  if (mmode == '+')
    max_size = high - offset;
  else
    max_size = 0;
  DWORD access, protect;
  if (mmode == 'r')
    {
      access = FILE_MAP_READ;
      protect = PAGE_READONLY;
    }
  else if (mmode == '+')
    {
      access = FILE_MAP_WRITE;
      protect = PAGE_READWRITE;
    }
  else
    {
      access = FILE_MAP_COPY;
      protect = PAGE_WRITECOPY;
    }
  HANDLE hFileMapping = CreateFileMapping((HANDLE) fd,
					  (LPSECURITY_ATTRIBUTES) NULL,
					  protect,
					  (DWORD) (max_size >> 32),
					  (DWORD) max_size,
					  (LPCTSTR) NULL);
  if (hFileMapping == NULL)
    throw new IOException();
  void *ptr = MapViewOfFile(hFileMapping, access,
			    (DWORD) (offset >> 32), (DWORD) offset,
			    (SIZE_T) (high - offset));
  if (ptr == NULL)
    throw new IOException();
  MappedByteBufferImpl *buf
    = new MappedByteBufferImpl((RawData *) ((char *) ptr + align),
			       size, mode == 'r');
  buf->implPtr = reinterpret_cast<RawData*> (ptr);
  buf->implLen = (jlong) (size_t) hFileMapping;
  return buf;
}

void
MappedByteBufferImpl::unmapImpl ()
{
  UnmapViewOfFile((void*)implPtr);
  CloseHandle((HANDLE) (size_t) implLen);
}

void
MappedByteBufferImpl::loadImpl ()
{
}

jboolean
MappedByteBufferImpl::isLoadedImpl ()
{
  return true;
}

void
MappedByteBufferImpl::forceImpl ()
{
}
