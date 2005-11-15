/* VMStackWalker.java -- Reference implementation of VM hooks for stack access
   Copyright (C) 2005 Free Software Foundation

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

package gnu.classpath;

/**
 * This class provides access to the classes on the Java stack
 * for reflection and security purposes.
 *
 * <p>
 * This class is only available to privileged code (i.e., code loaded
 * by the bootstrap loader).
 *
 * @author John Keiser
 * @author Eric Blake <ebb9@email.byu.edu>
 * @author Archie Cobbs
 */
public final class VMStackWalker
{
  /**
   * Get a list of all the classes currently executing methods on the
   * Java stack. <code>getClassContext()[0]</code> is the class associated
   * with the currently executing method, i.e., the method that called
   * <code>VMStackWalker.getClassContext()</code> (possibly through
   * reflection). So you may need to pop off these stack frames from
   * the top of the stack:
   * <ul>
   * <li><code>VMStackWalker.getClassContext()</code>
   * <li><code>Method.invoke()</code>
   * </ul>
   *
   * @return an array of the declaring classes of each stack frame
   */
  public static native Class[] getClassContext();

  /**
   * Get the class associated with the method invoking the method
   * invoking this method, or <code>null</code> if the stack is not
   * that deep (e.g., invoked via JNI invocation API). This method
   * is an optimization for the expression <code>getClassContext()[1]</code>
   * and should return the same result.
   *
   * <p>
   * VM implementers are encouraged to provide a more efficient
   * version of this method.
   */
  public static Class getCallingClass()
  {
    Class[] ctx = getClassContext();
    if (ctx.length < 3)
      return null;
    return ctx[2];
  }

  /**
   * Get the class loader associated with the Class returned by
   * <code>getCallingClass()</code>, or <code>null</code> if no such class
   * exists or it is the boot loader. This method is an optimization for the
   * expression <code>VMStackWalker.getClassLoader(getClassContext()[1])</code>
   * and should return the same result.
   *
   * <p>
   * VM implementers are encouraged to provide a more efficient
   * version of this method.
   */
  public static ClassLoader getCallingClassLoader()
  {
    Class[] ctx = getClassContext();
    if (ctx.length < 3)
      return null;
    return getClassLoader(ctx[2]);
  }

  /**
   * Retrieve the class's ClassLoader, or <code>null</code> if loaded
   * by the bootstrap loader. I.e., this should return the same thing
   * as {@link java.lang.VMClass#getClassLoader}. This duplicate version
   * is here to work around access permissions.
   */
  public static native ClassLoader getClassLoader(Class cl);
}

