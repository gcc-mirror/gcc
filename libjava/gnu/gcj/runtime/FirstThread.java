// FirstThread.java - Implementation of very first thread.

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

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

final class FirstThread extends Thread
{
  public FirstThread (Class k, String[] args)
  {
    super (null, null, "main");
    klass = k;
    this.args = args;
  }

  public FirstThread (String class_name, String[] args, boolean is_jar)
  {
    super (null, null, "main");
    klass_name = class_name;
    this.args = args;
    this.is_jar = is_jar;
  }

  public void run()
  {
    if (is_jar)
      klass_name = getMain(klass_name);

    if (klass == null)
      {
        try
	  {
	    klass = Class.forName(klass_name);
	  }
	catch (ClassNotFoundException x)
	  {
	    throw new NoClassDefFoundError(klass_name);
	  }
      }

    call_main();
  }

  private String getMain (String name)
  {
    String mainName = null;
    try {

      JarFile j = new JarFile (name);

      Attributes a = j.getManifest().getMainAttributes();

      mainName = a.getValue(Attributes.Name.MAIN_CLASS);

    } catch (Exception e) {
      // empty
    }

    if (mainName == null)
      {
	System.err.println ("Failed to load Main-Class manifest attribute from\n"
			    + name);
	System.exit(1);
      }
    return mainName;
  }

  private native void call_main ();

  // Private data.
  private Class klass;
  private String klass_name;
  private Object args;
  private boolean is_jar;

  // If the user links statically then we need to ensure that these
  // classes are linked in.  Otherwise bootstrapping fails.  These
  // classes are only referred to via Class.forName(), so we add an
  // explicit mention of them here.
  static final Class Kcert = java.security.cert.Certificate.class;
  static final Class Kfile = gnu.gcj.protocol.file.Handler.class;
  static final Class Khttp = gnu.gcj.protocol.http.Handler.class;
  static final Class Kjar  = gnu.gcj.protocol.jar.Handler.class;
}
