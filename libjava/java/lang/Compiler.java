/* Compiler.java -- placeholder for Java-to-native runtime compilers
   Copyright (C) 1998, 1999, 2001, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.lang;

/**
 * The <code>Compiler</code> class is a placeholder for a JIT compiler
 * implementation, and does nothing unless there is such a compiler.
 *
 * <p>The system property <code>java.compiler</code> may contain the name
 * of a library to load with <code>System.loadLibrary</code> when the
 * virtual machine first starts.  If so, and loading the library succeeds,
 * then a function by the name of <code>java_lang_Compiler_start()</code>
 * in that library is called.
 *
 * <p>Note that a VM might not have implemented any of this.
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @see System#getProperty(String)
 * @see System#getProperty(String, String)
 * @see System#loadLibrary(String)
 * @since JDK 1.0
 * @status updated to 1.4
 */
public final class Compiler
{
  /**
   * Don't allow new `Compiler's to be made.
   */
  private Compiler()
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
  public static boolean compileClass(Class oneClass)
  {
    return VMCompiler.compileClass(oneClass);
  }

  /**
   * Compile the classes whose name matches <code>classNames</code>.
   *
   * @param classNames the name of classes to compile
   * @return <code>false</code> if no compiler is available or
   *         compilation failed, <code>true</code> if compilation succeeded
   * @throws NullPointerException if classNames is null
   */
  public static boolean compileClasses(String classNames)
  {
    return VMCompiler.compileClasses(classNames);
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
  public static Object command(Object arg)
  {
    return VMCompiler.command(arg);
  }

  /**
   * Calling <code>Compiler.enable()</code> will cause the compiler
   * to resume operation if it was previously disabled; provided that a
   * compiler even exists.
   */
  public static void enable()
  {
    VMCompiler.enable();
  }

  /**
   * Calling <code>Compiler.disable()</code> will cause the compiler
   * to be suspended; provided that a compiler even exists.
   */
  public static void disable()
  {
    VMCompiler.disable();
  }
}
