// Process.java - Represent spawned system process.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.*;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 23, 1998.  
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 */

public abstract class Process
{
  abstract public void destroy ();
  abstract public int exitValue ();
  abstract public InputStream getErrorStream ();
  abstract public InputStream getInputStream ();
  abstract public OutputStream getOutputStream ();
  abstract public int waitFor () throws InterruptedException;
}
