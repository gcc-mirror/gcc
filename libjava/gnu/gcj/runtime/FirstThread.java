// FirstThread.java - Implementation of very first thread.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

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

final class FirstThread
{
  public static String getMain (String name)
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
      System.err.println ("Failed to load Main-Class manifest attribute from\n"
			  + name);
    return mainName;
  }

  // Private data.
  private Class klass;
  private String klass_name;
  private Object args;

  // If the user links statically then we need to ensure that these
  // classes are linked in.  Otherwise bootstrapping fails.  These
  // classes are only referred to via Class.forName(), so we add an
  // explicit mention of them here.
  static final Class Kcert = java.security.cert.Certificate.class;
  static final Class Kfile = gnu.gcj.protocol.file.Handler.class;
  static final Class Khttp = gnu.gcj.protocol.http.Handler.class;
  static final Class Kjar  = gnu.gcj.protocol.jar.Handler.class;
}
