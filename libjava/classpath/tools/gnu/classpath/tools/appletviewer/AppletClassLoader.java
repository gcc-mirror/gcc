/* AppletClassLoader -- a loader for applet classes
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

package gnu.classpath.tools.appletviewer;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;

public class AppletClassLoader extends URLClassLoader
{
  /**
   * Constructs a new <code>AppletLoader</code> object.
   *
   * @param codebase the codebase of the applet
   * @param archives the urls to add to the search path
   */
  public AppletClassLoader(URL codebase, ArrayList archives)
  {
    super(new URL[0]);

    for (int count = 0; count < archives.size(); count++)
      addURL((URL) archives.get(count));

    addURL(codebase);
  }

  /**
   * Finds the specified class. This method should be overridden by
   * class loader implementations that follow the delegation model for
   * loading classes, and will be invoked by the loadClass method after
   * checking the parent class loader for the requested class. The default
   * implementation throws a ClassNotFoundException.
   *
   * (description copied from java.lang.ClassLoader.findClass(String))
   *
   * @param name The name of the class.
   *
   * @return the resulting <code>Class</code> object.
   *
   * @exception ClassNotFoundException if the class is not found.
   */
  protected Class findClass(String name) throws ClassNotFoundException
  {
    return super.findClass(name);
  }
}
