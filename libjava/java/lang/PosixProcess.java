// PosixProcess.java - Subclass of Process for POSIX systems.

/* Copyright (C) 1998, 1999  Free Software Foundation

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
 * @author Tom Tromey <tromey@cygnus.com>
 * @date May 3, 1999
 */

// This is entirely internal to our implementation.

// This file is copied to `ConcreteProcess.java' before compilation.
// Hence the class name apparently does not match the file name.
final class ConcreteProcess extends Process
{
  public native void destroy ();

  public int exitValue ()
  {
    if (! hasExited)
      throw new IllegalThreadStateException("Process has not exited");
    return status;
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

  // This is used for actual initialization, as we can't write a
  // native constructor.
  public native void startProcess (String[] progarray,
                                   String[] envp,
                                   File dir)
    throws IOException;

  // This file is copied to `ConcreteProcess.java' before
  // compilation.  Hence the constructor name apparently does not
  // match the file name.
  public ConcreteProcess (String[] progarray,
                          String[] envp,
                          File dir)
    throws IOException
  {
    startProcess (progarray, envp, dir);
  }

  // The process id.  This is cast to a pid_t on the native side.
  private long pid;

  // True when child has exited.
  private boolean hasExited;

  // The exit status, if the child has exited.
  private int status;

  // The streams.
  private InputStream errorStream;
  private InputStream inputStream;
  private OutputStream outputStream;
}
