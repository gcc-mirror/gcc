// FirstThread.java - Implementation of very first thread.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import java.util.jar.*;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date August 24, 1998 
 */

// This is entirely internal to our implementation.

final class FirstThread extends Thread
{
  public native void run ();

  public FirstThread (Class k, Object o)
  {
    super (null, null, "main");
    klass = k;
    klass_name = null;
    args = o;
  }

  public FirstThread (String class_name, Object o)
  {
    super (null, null, "main");
    klass = null;
    klass_name = class_name;
    args = o;
  }

  private static void die (String s)
  {
    System.err.println(s);
    System.exit(1);
  }

  public static void main (String[] args)
  {
    try {

      JarFile j = new JarFile (args[0]);

      Attributes a = j.getManifest().getMainAttributes();

      jarMainClassName = a.getValue(Attributes.Name.MAIN_CLASS);

      if (jarMainClassName != null)
	return;

    } catch (Exception e) {
      // empty
    }

    System.err.println ("Failed to load Main-Class manifest attribute from\n"
			+ args[0]);
  }

  // If interpreter is invoked with -jar, the main class name is recorded
  // here.
  public static String jarMainClassName;

  // Private data.
  private Class klass;
  private String klass_name;
  private Object args;
}
