// EcosProcess.java - Subclass of Process for eCos systems.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date May 11, 1999
 */

// This is entirely internal to our implementation.

// This file is copied to `ConcreteProcess.java' before compilation.
// Hence the class name apparently does not match the file name.
final class ConcreteProcess extends Process
{
  // See natEcosProcess.cc to understand why this is native.
  public native void destroy ();

  public int exitValue ()
  {
    return 0;
  }
  public InputStream getErrorStream ()
  {
    return null;
  }

  public InputStream getInputStream ()
  {
    return null;
  }

  public OutputStream getOutputStream ()
  {
    return null;
  }

  public int waitFor () throws InterruptedException
  {
    return 0;
  }

  public ConcreteProcess (String[] progarray, String[] envp) throws IOException
  {
    throw new IOException ("eCos processes unimplemented");
  }
}
