// natWin32Process.cc - Native side of Win32 process code.

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

// Conflicts with the definition in "java/lang/reflect/Modifier.h"
#undef STRICT

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/ConcreteProcess.h>
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

void
java::lang::ConcreteProcess::cleanup (void)
{
  if (inputStream != NULL)
    {
      inputStream->close ();
      inputStream = NULL;
    }

  if (outputStream != NULL)
    {
      outputStream->close ();
      outputStream = NULL;
    }

  if (errorStream != NULL)
    {
      errorStream->close ();
      errorStream = NULL;
    }
}

void
java::lang::ConcreteProcess::destroy (void)
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
java::lang::ConcreteProcess::hasExited (void)
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
java::lang::ConcreteProcess::waitFor (void)
{
  if (! hasExited ())
    {
      DWORD exitStatus = 0UL;

      // FIXME: The wait should be interruptible.
      WaitForSingleObject ((HANDLE) procHandle, INFINITE);

      GetExitCodeProcess ((HANDLE) procHandle, &exitStatus);
      exitCode = exitStatus;

      cleanup ();
    }

  return exitCode;
}

static char *
new_string (jstring string)
{
  jsize s = _Jv_GetStringUTFLength (string);
  char *buf = (char *) _Jv_Malloc (s + 1);
  _Jv_GetStringUTFRegion (string, 0, s, buf);
  buf[s] = '\0';
  return buf;
}

void
java::lang::ConcreteProcess::startProcess (jstringArray progarray,
                                           jstringArray envp,
                                           java::io::File *dir)
{
  using namespace java::io;

  procHandle = (jint) INVALID_HANDLE_VALUE;

  // Reconstruct the command line.
  jstring *elts = elements (progarray);

  int cmdLineLen = 0;

  for (int i = 0; i < progarray->length; ++i)
    cmdLineLen += (_Jv_GetStringUTFLength (elts[i]) + 3);

  char *cmdLine = (char *) _Jv_Malloc (cmdLineLen + 1);
  char *cmdLineCurPos = cmdLine;

  for (int i = 0; i < progarray->length; ++i)
    {
      if (i > 0)
        *cmdLineCurPos++ = ' ';
      *cmdLineCurPos++ = '\"';
      jsize s = _Jv_GetStringUTFLength (elts[i]);
      _Jv_GetStringUTFRegion (elts[i], 0, s, cmdLineCurPos);
      cmdLineCurPos += s;
      *cmdLineCurPos++ = '\"';
    }
  *cmdLineCurPos = '\0';

  // Get the environment, if any.
  char *env = NULL;
  if (envp)
    {
      elts = elements (envp);

      int envLen = 0;
      for (int i = 0; i < envp->length; ++i)
        envLen += (_Jv_GetStringUTFLength (elts[i]) + 1);

      env = (char *) _Jv_Malloc (envLen + 1);

      int j = 0;
      for (int i = 0; i < envp->length; ++i)
        {
          jsize s = _Jv_GetStringUTFLength (elts[i]);
          _Jv_GetStringUTFRegion (elts[i], 0, s, (env + j));

          j += s;
          *(env + j) = '\0';
          j++;
        }
      *(env + j) = '\0';
    }

  // Get the working directory path, if specified.
  char *wdir = NULL;
  if (dir != NULL)
    wdir = new_string (dir->getPath ());

  errorStream = NULL;
  inputStream = NULL;
  outputStream = NULL;

  java::lang::Throwable *exc = NULL;

  try
    {
      // We create anonymous pipes to communicate with the child
      // on each of standard streams.

      HANDLE cldStdInRd, cldStdInWr;
      HANDLE cldStdOutRd, cldStdOutWr;
      HANDLE cldStdErrRd, cldStdErrWr;

      SECURITY_ATTRIBUTES sAttrs;

      // Explicitly allow the handles to the pipes to be inherited.
      sAttrs.nLength = sizeof (SECURITY_ATTRIBUTES);
      sAttrs.bInheritHandle = 1;
      sAttrs.lpSecurityDescriptor = NULL;


      char tmpBuff[64];
      if (CreatePipe (&cldStdInRd, &cldStdInWr, &sAttrs, 0) == 0)
        {
          sprintf (tmpBuff,
                   "Error creating stdin pipe (Win32 Error Code: %lu)",
                   GetLastError ());
          throw new IOException (JvNewStringLatin1 (tmpBuff));
        }

      if (CreatePipe (&cldStdOutRd, &cldStdOutWr, &sAttrs, 0) == 0)
        {
          sprintf (tmpBuff,
                   "Error creating stdout pipe (Win32 Error Code: %lu)",
                   GetLastError ());
          throw new IOException (JvNewStringLatin1 (tmpBuff));
        }

      if (CreatePipe (&cldStdErrRd, &cldStdErrWr, &sAttrs, 0) == 0)
        {
          sprintf (tmpBuff,
                   "Error creating stderr pipe (Win32 Error Code: %lu)",
                   GetLastError ());
          throw new IOException (JvNewStringLatin1 (tmpBuff));
        }

      outputStream = new FileOutputStream
                         (new FileDescriptor ((jint) cldStdInWr));
      inputStream = new FileInputStream
                        (new FileDescriptor ((jint) cldStdOutRd));
      errorStream = new FileInputStream
                        (new FileDescriptor ((jint) cldStdErrRd));

      // Now create the child process.
      PROCESS_INFORMATION pi;
      STARTUPINFO si;

      ZeroMemory (&pi, sizeof (PROCESS_INFORMATION));

      ZeroMemory (&si, sizeof (STARTUPINFO));
      si.cb = sizeof (STARTUPINFO);

      // Explicitly specify the handles to the standard streams.
      si.dwFlags |= STARTF_USESTDHANDLES;

      si.hStdInput = cldStdInRd;
      si.hStdOutput = cldStdOutWr;
      si.hStdError = cldStdErrWr;

      if (CreateProcess (NULL,
                         cmdLine,
                         NULL,
                         NULL,
                         1,
                         0,
                         env,
                         wdir,
                         &si,
                         &pi) == 0)
        {
          sprintf (tmpBuff,
                   "Error creating child process (Win32 Error Code: %lu)",
                   GetLastError ());
          throw new IOException (JvNewStringLatin1 (tmpBuff));
        }

      procHandle = (jint ) pi.hProcess;

      // Close the wrong ends (for the parent) of the pipes.
      CloseHandle (cldStdInRd);
      CloseHandle (cldStdOutWr);
      CloseHandle (cldStdErrWr);

      _Jv_Free (cmdLine);
      if (env != NULL)
        _Jv_Free (env);
      if (wdir != NULL)
        _Jv_Free (wdir);
    }
  catch (java::lang::Throwable *thrown)
    {
      cleanup ();
      exc = thrown;
    }

  if (exc != NULL)
    throw exc;
}
