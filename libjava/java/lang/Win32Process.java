// Win32Process.java - Subclass of Process for Win32 systems.

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

/**
 * @author Adam Megacz
 * @date Feb 24, 2002
 */

// This is entirely internal to our implementation.

// This file is copied to `ConcreteProcess.java' before compilation.
// Hence the class name apparently does not match the file name.
final class ConcreteProcess extends Process
{
  public native void destroy ();

  public int exitValue ()
  {
    if (! hasExited ())
      throw new IllegalThreadStateException ("Process has not exited");

    return exitCode;
  }

  public InputStream getErrorStream ()
  {
    return errorStream;
  }

  public InputStream getInputStream ()
  {
    return inputStream;
  }

  public OutputStream getOutputStream ()
  {
    return outputStream;
  }

  public native int waitFor () throws InterruptedException;

  public ConcreteProcess (String[] progarray,
                          String[] envp,
                          File dir)
    throws IOException
  {
    for (int i = 0; i < progarray.length; i++)
      {
        String s = progarray[i];

        if ( (s.indexOf (' ') >= 0) || (s.indexOf ('\t') >= 0))
          progarray[i] = "\"" + s + "\"";
      }

    startProcess (progarray, envp, dir);
  }

  // The standard streams (stdin, stdout and stderr, respectively)
  // of the child as seen by the parent process.
  private OutputStream outputStream;
  private InputStream inputStream;
  private InputStream errorStream;

  // Handle to the child process - cast to HANDLE before use.
  private int procHandle;

  // Exit code of the child if it has exited.
  private int exitCode;

  private native boolean hasExited ();
  private native void startProcess (String[] progarray,
           String[] envp,
           File dir)
    throws IOException;
  private native void cleanup ();
}
