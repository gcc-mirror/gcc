/* VMClassLoader.java -- Reference implementation of compiler interface
   Copyright (C) 2004, 2010  Free Software Foundation, Inc.

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

package java.lang;

/**
 * This class is just a per-VM reflection of java.lang.Compiler.
 * All methods are defined identically.
 */
final class VMCompiler
{
  /**
   * Don't allow new `Compiler's to be made.
   */
  private VMCompiler()
  {
  }

  /**
   * Compile the class named by <code>oneClass</code>.
   *
   * @param oneClass the class to compile
   * @return <code>false</code> if no compiler is available or
   *         compilation failed, <code>true</code> if compilation succeeded
   * @throws NullPointerException if oneClass is null
   */
  static boolean compileClass(Class oneClass)
  {
    // Never succeed.
    return false;
  }

  /**
   * Compile the classes whose name matches <code>classNames</code>.
   *
   * @param classNames the name of classes to compile
   * @return <code>false</code> if no compiler is available or
   *         compilation failed, <code>true</code> if compilation succeeded
   * @throws NullPointerException if classNames is null
   */
  static boolean compileClasses(String classNames)
  {
    // Note the incredibly lame interface.  Always fail.
    return false;
  }

  /**
   * This method examines the argument and performs an operation
   * according to the compilers documentation.  No specific operation
   * is required.
   *
   * @param arg a compiler-specific argument
   * @return a compiler-specific value, including null
   * @throws NullPointerException if the compiler doesn't like a null arg
   */
  static Object command(Object arg)
  {
    // Our implementation defines this to a no-op.
    return null;
  }

  /**
   * Calling <code>Compiler.enable()</code> will cause the compiler
   * to resume operation if it was previously disabled; provided that a
   * compiler even exists.
   */
  static void enable()
  {
  }

  /**
   * Calling <code>Compiler.disable()</code> will cause the compiler
   * to be suspended; provided that a compiler even exists.
   */
  static void disable()
  {
  }
}
