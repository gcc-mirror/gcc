// Runtime.java - Runtime class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.StringTokenizer;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date August 27, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  All 1.1 methods exist.  exec() is not fully implemented.
 */

public class Runtime
{
  public Process exec (String prog) throws IOException
  {
    return exec (prog, null);
  }

  public Process exec (String prog, String[] envp) throws IOException
  {
    StringTokenizer st = new StringTokenizer(prog);
    String[] a = new String[st.countTokens ()];
    for (int i = 0; i < a.length; i++)
      a[i] = st.nextToken ();
    return exec (a, envp);
  }

  public Process exec (String[] progarray) throws IOException
  {
    return exec (progarray, null);
  }

  public Process exec (String[] progarray, String[] envp) throws IOException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkExec(progarray[0]);
    return new ConcreteProcess (progarray, envp);
  }

  private final static void checkExit (int status)
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkExit(status);
  }

  public native void exit (int status);

  public native long freeMemory ();
  public native void gc ();

  // Deprecated in 1.1.  We implement what the JCL book says.
  public InputStream getLocalizedInputStream (InputStream in)
  {
    return in;
  }

  // Deprecated in 1.1.  We implement what the JCL book says.
  public OutputStream getLocalizedOutputStream (OutputStream out)
  {
    return out;
  }

  public static Runtime getRuntime ()
  {
    return self;
  }

  private final void checkLink (String lib)
  {
    if (lib == null)
      throw new NullPointerException ();
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkLink(lib);
  }

  private native void _load (String pathname, boolean do_search);

  public void load (String pathname)
  {
    _load (pathname, false);
  }

  public void loadLibrary (String libname)
  {
    _load (libname, true);
  }

  // This is a helper function for the ClassLoader which can load
  // compiled libraries.  Returns true if library (which is just the
  // base name -- path searching is done by this function) was loaded,
  // false otherwise.
  native boolean loadLibraryInternal (String libname);

  public native void runFinalization ();

  // This method is static in JDK 1.1, but isn't listed as static in
  // the books.  It is marked as static in the 1.2 docs.
  public static void runFinalizersOnExit (boolean run)
  {
    // The status we pass to the security check is unspecified.
    checkExit (0);
    self.finalize_on_exit = run;
  }

  public native long totalMemory ();
  public native void traceInstructions (boolean on);
  public native void traceMethodCalls (boolean on);

  // A helper for the constructor.
  private final native void init ();

  // The sole constructor.
  private Runtime ()
  {
    init ();
  }

  // Private data.
  private static Runtime self = new Runtime ();
  // FIXME: for now this can't be static.  If it is, our compiler will
  // mark it as local, and it will be inaccessible to natRuntime.cc.
  private boolean finalize_on_exit;
}
