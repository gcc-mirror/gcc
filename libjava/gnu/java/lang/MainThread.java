/* gnu.java.lang.MainThread
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.java.lang;

import java.util.jar.Attributes;
import java.util.jar.JarFile;

/**
 * MainThread is a Thread which uses the main() method of some class.
 *
 * @author John Keiser
 * @author Tom Tromey (tromey@redhat.com)
 */
final class MainThread extends Thread
{
  // If the user links statically then we need to ensure that these
  // classes are linked in.  Otherwise bootstrapping fails.  These
  // classes are only referred to via Class.forName(), so we add an
  // explicit mention of them here.
  static final Class Kcert     = java.security.cert.Certificate.class;
  static final Class Kfile     = gnu.java.net.protocol.file.Handler.class;
  static final Class Khttp     = gnu.java.net.protocol.http.Handler.class;
  static final Class Kjar      = gnu.java.net.protocol.jar.Handler.class;
  static final Class Klocale   = gnu.java.locale.LocaleInformation.class;
  static final Class Kcalendar = gnu.java.locale.Calendar.class;

  // Private data.
  private Class klass;
  private String klass_name;
  private String[] args;
  private boolean is_jar;

  public MainThread(Class k, String[] args)
  {
    super(null, null, "main");
    klass = k;
    this.args = args;
  }

  public MainThread(String classname, String[] args, boolean is_jar)
  {
    super (null, null, "main");
    klass_name = classname;
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
	    klass = Class.forName(klass_name, true,
				  ClassLoader.getSystemClassLoader());
	  }
	catch (ClassNotFoundException x)
	  {
	    NoClassDefFoundError ncdfe = new NoClassDefFoundError(klass_name);
	    ncdfe.initCause(x);
	    throw ncdfe;
	  }
      }

    call_main();
  }

  private String getMain(String name)
  {
    String mainName = null;
    try
      {
	JarFile j = new JarFile(name);
	Attributes a = j.getManifest().getMainAttributes();
	mainName = a.getValue(Attributes.Name.MAIN_CLASS);
      }
    catch (Exception e)
      {
	// Ignore.
      }

    if (mainName == null)
      {
	System.err.println("Failed to load Main-Class manifest attribute from "
			   + name);
	System.exit(1);
      }
    return mainName;
  }

  private native void call_main();
}
