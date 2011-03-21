/* ClassPersistenceDelegate.java - A PersistenceDelegate for the Class type.
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package gnu.java.beans.encoder;

import java.beans.Encoder;
import java.beans.Expression;
import java.beans.PersistenceDelegate;

/** <p>The <code>ClassPersistenceDelegate</code> creates
 * <code>Expression</code> instances which denote class resolutions.</p>
 *
 * <p>The class resolution is always the last step when serializing a tree
 * of objects. Due to the recursive nature of the algorithm we need a way
 * to end the recursion. This is achieved by the implementation of this
 * {@link instantiate} method. Arbitrary classes are described with a call
 * to <code>Class.forName</code>. However for the <code>Class</code> class
 * we call <code>getClass()</code> on a <code>String.class</code> instance.
 * This in turn lead to the resolution of the String class which is always
 * encoded as <code>"".getClass()</code>. Finally the <code>Encoder</code>
 * treats strings in a special way so that the recursion ends here.
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class ClassPersistenceDelegate extends PersistenceDelegate
{

  protected Expression instantiate(Object oldInstance, Encoder out)
  {
    Class oldClass = (Class) oldInstance;

    // Due to the special handling of String instances in the Encoder
    // this Expression does not lead to further class resolutions.
    if (oldClass == String.class)
      return new Expression(oldClass, "", "getClass", null);

    // This Expression will lead to the class resolution of String.class.
    if (oldClass == Class.class)
      return new Expression(oldClass, String.class, "getClass", null);

    // This Expression will lead to the class resolution of Class.class.
    return new Expression(oldClass, Class.class, "forName",
                          new Object[] { oldClass.getName() });
  }

}
