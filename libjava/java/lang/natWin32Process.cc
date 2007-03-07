// natWin32Process.cc - Native side of Win32 process code.

/* Copyright (C) 2003, 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

// Conflicts with the definition in "java/lang/reflect/Modifier.h"
#undef STRICT

#include <java/lang/Win32Process.h>
#include <java/lang/IllegalThreadStateException.h>
#include <java/lang/InterruptedException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/Thread.h>
#include <java/io/File.h>
#include <java/io/FileDescriptor.h>
#include <java/io/FileInputStream.h>
#include <java/io/FileOutputStream.h>
#include <java/io/IOException.h>
#include <java/lang/OutOfMemoryError.h>
#include <java/lang/Win32Process$EOFInputStream.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>

using gnu::java::nio::channels::FileChannelImpl;

void
java::lang::Win32Process::cleanup (void)
{
  // FIXME:
  // We used to close the input, output and
  // error streams here, but we can't do that
  // because the caller also has the right
  // to close these and FileInputStream and FileOutputStream
  // scream if you attempt to close() them twice. Presently,
  // we use _Jv_platform_close_on_exec, which is similar
  // to the POSIX approach.
  //
  // What I wanted to do is have private nested
  // classes in Win32Process which extend FileInputStream
  // and FileOutputStream, respectively, but override
  // close() to permit multiple calls to close(). This
  // led to class header and platform configury issues
  // that I didn't feel like dealing with. However,
  // this approach could conceivably be a good multiplatform
  // one since delaying the pipe close until process
  // termination could be wasteful if many child processes
  // are spawned within the parent process' lifetime.
  inputStream = NULL;
  outputStream = NULL;
  errorStream = NULL;
  
  if (procHandle)
    {
      CloseHandle((HANDLE) procHandle);
      procHandle = (jint) INVALID_HANDLE_VALUE;
    }
}

void
java::lang::Win32Process::destroy (void)
{
  if (! hasExited ())
    {
      // Kill it forcibly and assign an (arbitrary) exit code of 0.
      TerminateProcess ((HANDLE) procHandle, 0);
      exitCode = 0;

      cleanup ();
    }
}

jboolean
java::lang::Win32Process::hasExited (void)
{
  DWORD exitStatus;

  if (GetExitCodeProcess ((HANDLE) procHandle, &exitStatus) != 0)
    {
      // NOTE: STILL_ACTIVE is defined as "259" by Win32 - if the
      // child actually exits with this return code, we have a
      // problem here. See MSDN documentation on GetExitCodeProcess( ).

      if (exitStatus == STILL_ACTIVE)
        return false;
      else
        {
          cleanup ();
          exitCode = exitStatus;
          return true;
        }
    }
  else
    return true;
}

jint
java::lang::Win32Process::waitFor (void)
{
  if (! hasExited ())
    {
      DWORD exitStatus = 0UL;

      // Set up our waitable objects array
      // - 0: the handle to the process we just launched
      // - 1: our thread's interrupt event
      HANDLE arh[2];
      arh[0] = (HANDLE) procHandle;
      arh[1] = _Jv_Win32GetInterruptEvent ();
      DWORD rval = WaitForMultipleObjects (2, arh, 0, INFINITE);

      // Use the returned value from WaitForMultipleObjects
      // instead of our thread's interrupt_flag to test for
      // thread interruption. See the comment for
      // _Jv_Win32GetInterruptEvent().
      bool bInterrupted = rval == (WAIT_OBJECT_0 + 1);
      
      if (bInterrupted)
        {
          // Querying this forces a reset our thread's interrupt flag.
          Thread::interrupted();
          
          cleanup ();
          throw new InterruptedException ();
        }

      GetExitCodeProcess ((HANDLE) procHandle, &exitStatus);
      exitCode = exitStatus;

      cleanup ();
    }

  return exitCode;
}


// Helper class for creating and managing the pipes
// used for I/O redirection for child processes.
class ChildProcessPipe
{
public:
  // Indicates from the child process' point of view
  // whether the pipe is for reading or writing.
  enum EType {INPUT, OUTPUT, DUMMY};

  ChildProcessPipe(EType eType);
  ~ChildProcessPipe();
  
  // Returns a pipe handle suitable for use by the parent process
  HANDLE getParentHandle();
  
  // Returns a pipe handle suitable for use by the child process.
  HANDLE getChildHandle();
  
private:
  EType m_eType;
  HANDLE m_hRead, m_hWrite;
};

ChildProcessPipe::ChildProcessPipe(EType eType):
  m_eType(eType), m_hRead(0), m_hWrite(0)
{
  if (eType == DUMMY)
    return;
  
  SECURITY_ATTRIBUTES sAttrs;

  // Explicitly allow the handles to the pipes to be inherited.
  sAttrs.nLength = sizeof (SECURITY_ATTRIBUTES);
  sAttrs.bInheritHandle = 1;
  sAttrs.lpSecurityDescriptor = NULL;

  if (CreatePipe (&m_hRead, &m_hWrite, &sAttrs, 0) == 0)
    {
      DWORD dwErrorCode = GetLastError ();
      throw new java::io::IOException (
        _Jv_WinStrError (_T("Error creating pipe"), dwErrorCode));
    }

  // If this is the read end of the child, we need
  // to make the parent write end non-inheritable. Similarly,
  // if this is the write end of the child, we need to make
  // the parent read end non-inheritable. If we didn't
  // do this, the child would inherit these ends and we wouldn't
  // be able to close them from our end. For full details,
  // do a Google search on "Q190351".
  HANDLE& rhStd = m_eType==INPUT ? m_hWrite : m_hRead;
  _Jv_platform_close_on_exec (rhStd);
}

ChildProcessPipe::~ChildProcessPipe()
{
  // Close the parent end of the pipe. This
  // destructor is called after the child process
  // has been spawned.
  if (m_eType != DUMMY)
    CloseHandle(getChildHandle());
}

HANDLE ChildProcessPipe::getParentHandle()
{
  return m_eType==INPUT ? m_hWrite : m_hRead;
}

HANDLE ChildProcessPipe::getChildHandle()
{
  return m_eType==INPUT ? m_hRead : m_hWrite;
}

void
java::lang::Win32Process::startProcess (jstringArray progarray,
					jstringArray envp,
					java::io::File *dir,
					jboolean redirect)
{
  using namespace java::io;

  procHandle = (jint) INVALID_HANDLE_VALUE;

  // Reconstruct the command line.
  jstring *elts = elements (progarray);

  int cmdLineLen = 0;

  for (int i = 0; i < progarray->length; ++i)
    cmdLineLen += (elts[i]->length() + 1);

  LPTSTR cmdLine = (LPTSTR) _Jv_Malloc ((cmdLineLen + 1) * sizeof(TCHAR));
  LPTSTR cmdLineCurPos = cmdLine;

  for (int i = 0; i < progarray->length; ++i)
    {
      if (i > 0)
        *cmdLineCurPos++ = _T(' ');
        
      jint len = elts[i]->length();
      JV_TEMP_STRING_WIN32(thiselt, elts[i]);
      _tcscpy(cmdLineCurPos, thiselt);
      cmdLineCurPos += len;
    }
  *cmdLineCurPos = _T('\0');

  // Get the environment, if any.
  LPTSTR env = NULL;
  if (envp)
    {
      elts = elements (envp);

      int envLen = 0;
      for (int i = 0; i < envp->length; ++i)
        envLen += (elts[i]->length() + 1);

      env = (LPTSTR) _Jv_Malloc ((envLen + 1) * sizeof(TCHAR));

      int j = 0;
      for (int i = 0; i < envp->length; ++i)
        {
          jint len = elts[i]->length();
          
          JV_TEMP_STRING_WIN32(thiselt, elts[i]);
          _tcscpy(env + j, thiselt);
          
          j += len;
          
          // Skip past the null terminator that _tcscpy just inserted.
          j++;
        }
      *(env + j) = _T('\0');
    }

  // Get the working directory path, if specified.
  JV_TEMP_STRING_WIN32 (wdir, dir ? dir->getPath () : 0);

  errorStream = NULL;
  inputStream = NULL;
  outputStream = NULL;

  java::lang::Throwable *exc = NULL;

  try
    {
      // We create anonymous pipes to communicate with the child
      // on each of standard streams.
      ChildProcessPipe aChildStdIn(ChildProcessPipe::INPUT);
      ChildProcessPipe aChildStdOut(ChildProcessPipe::OUTPUT);
      ChildProcessPipe aChildStdErr(redirect ? ChildProcessPipe::DUMMY
				    : ChildProcessPipe::OUTPUT);

      outputStream = new FileOutputStream (new FileChannelImpl (
                           (jint) aChildStdIn.getParentHandle (),
			   FileChannelImpl::WRITE));
      inputStream = new FileInputStream (new FileChannelImpl (
                           (jint) aChildStdOut.getParentHandle (),
			   FileChannelImpl::READ));
      if (redirect)
        errorStream = Win32Process$EOFInputStream::instance;
      else
        errorStream = new FileInputStream (new FileChannelImpl (
                           (jint) aChildStdErr.getParentHandle (),
			   FileChannelImpl::READ));

      // Now create the child process.
      PROCESS_INFORMATION pi;
      STARTUPINFO si;

      ZeroMemory (&pi, sizeof (PROCESS_INFORMATION));

      ZeroMemory (&si, sizeof (STARTUPINFO));
      si.cb = sizeof (STARTUPINFO);

      // Explicitly specify the handles to the standard streams.
      si.dwFlags |= STARTF_USESTDHANDLES;

      si.hStdInput = aChildStdIn.getChildHandle();
      si.hStdOutput = aChildStdOut.getChildHandle();
      si.hStdError = redirect ? aChildStdOut.getChildHandle()
                              : aChildStdErr.getChildHandle();

      // Spawn the process. CREATE_NO_WINDOW only applies when
      // starting a console application; it suppresses the
      // creation of a console window. This flag is ignored on
      // Win9X.
      
      if (CreateProcess (NULL,
                         cmdLine,
                         NULL,
                         NULL,
                         1,
                         CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT,
                         env,
                         wdir,
                         &si,
                         &pi) == 0)
        {
          DWORD dwErrorCode = GetLastError ();
          throw new IOException (
            _Jv_WinStrError (_T("Error creating child process"), dwErrorCode));
        }

      procHandle = (jint ) pi.hProcess;

      _Jv_Free (cmdLine);
      if (env != NULL)
        _Jv_Free (env);
    }
  catch (java::lang::Throwable *thrown)
    {
      cleanup ();
      exc = thrown;
    }

  if (exc != NULL)
    throw exc;
}
