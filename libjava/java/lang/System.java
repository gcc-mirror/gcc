// System.java - System-specific info.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.util.Properties;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date August 27, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status: 1.1.  Some 1.2 methods missing.  Properties code not fully
 * implemented.
 */

public final class System
{
  public static native void arraycopy (Object src, int srcOffset,
				       Object dst, int dstOffset,
				       int count);

  public static native long currentTimeMillis ();

  public static void exit (int status)
  {
    Runtime.getRuntime().exit(status);
  }

  public static void gc ()
  {
    Runtime.getRuntime().gc();
  }

  // Marked deprecated in 1.1.  We implement what the JCL book says.
  public static String getenv (String name)
  {
    throw new Error ();
  }

  private static native void init_properties ();

  public static Properties getProperties ()
  {
    if (secman != null)
      secman.checkPropertiesAccess();
    init_properties ();
    return properties;
  }

  public static String getProperty (String property)
  {
    if (secman != null)
      secman.checkPropertyAccess(property);
    init_properties ();
    return properties.getProperty(property);
  }

  public static String getProperty (String property, String defval)
  {
    if (secman != null)
      secman.checkPropertyAccess(property, defval);
    init_properties ();
    return properties.getProperty(property, defval);
  }

  public static SecurityManager getSecurityManager ()
  {
    return secman;
  }

  public static native int identityHashCode (Object obj);

  public static void load (String pathname)
  {
    Runtime.getRuntime().load(pathname);
  }

  public static void loadLibrary (String libname)
  {
    Runtime.getRuntime().loadLibrary(libname);
  }

  public static void runFinalization ()
  {
    Runtime.getRuntime().runFinalization();
  }

  // Marked as deprecated in 1.2.
  public static void runFinalizersOnExit (boolean run)
  {
    Runtime.getRuntime().runFinalizersOnExit(run);
  }

  private static void checkSetIO ()
  {
    // In 1.1, we are supposed to call checkExec, but the argument is
    // not specified.  In 1.2, we are supposed to use checkPermission,
    // which doesn't exist in 1.1.
    if (secman != null)
      secman.checkExec("");
  }

  public static native void setErr (PrintStream newErr);
  public static native void setIn (InputStream newIn);
  public static native void setOut (PrintStream newOut);

  public static void setProperties (Properties props)
  {
    if (secman != null)
      secman.checkPropertiesAccess();
    // We might not have initialized yet.
    prop_init = true;
    properties = props;
  }

  // TODO 1.2.
  // public static String setProperty (String key, String value);

  // TODO 1.2.
  // public static String mapLibraryName (String libname);

  public static void setSecurityManager (SecurityManager s)
  {
    if (secman != null)
      throw new SecurityException ();
    secman = s;
  }

  // Public data.
  public static final InputStream in = new BufferedInputStream (new FileInputStream (FileDescriptor.in));

  public static final PrintStream out = new PrintStream (new BufferedOutputStream (new FileOutputStream (FileDescriptor.out)), true);

  public static final PrintStream err = new PrintStream (new BufferedOutputStream (new FileOutputStream (FileDescriptor.err)), true);

  // Don't allow System objects to be made.
  private System ()
  {
  }

  // Private data.
  private static SecurityManager secman = null;
  private static Properties properties = null;
  // This boolean is only required for 1.1 and earlier.  After 1.1, a
  // null properties should always be re-initialized.
  private static boolean prop_init = false;
}
