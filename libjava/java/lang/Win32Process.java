// Win32Process.java - Subclass of Process for Win32 systems.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

/**
 * @author Adam Megacz
 * @date Feb 24, 2002
 */

// This is entirely internal to our implementation.

// NOTE: when this is implemented, we'll need to add
// HANDLE_FLAG_INHERIT in FileDescriptor and other places, to make
// sure that file descriptors aren't inherited by the child process.
// See _Jv_platform_close_on_exec.

// This file is copied to `ConcreteProcess.java' before compilation.
// Hence the class name apparently does not match the file name.
final class ConcreteProcess extends Process
{
  public void destroy ()
  {
    throw new Error("not implemented");
  }
  
  public int exitValue ()
  {
    throw new Error("not implemented");
  }

  public InputStream getErrorStream ()
  {
    throw new Error("not implemented");
  }

  public InputStream getInputStream ()
  {
    throw new Error("not implemented");
  }

  public OutputStream getOutputStream ()
  {
    throw new Error("not implemented");
  }

  public int waitFor () throws InterruptedException
  {
    throw new Error("not implemented");
  }

  public ConcreteProcess (String[] progarray, String[] envp) throws IOException
  {
    throw new IOException("not implemented");
  }

}
