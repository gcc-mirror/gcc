/*
  Copyright (c) 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License.
 */

package gnu.java.rmi.rmic;

/** A Compiler object can be used to compile a .java file into a
 * .class file.  This is an abstract class; the
 * <code>getInstance()</code> method is used to obtain the actual
 * compiler object.  */
public abstract class Compiler
{
  // Can't directly instantiate.
  protected Compiler ()
  {
  }

  /** Create a new compiler instance.  */
  public static Compiler getInstance ()
  {
    return getInstance (System.getProperty ("classpath.compiler", "gcj"));
  }

  /** Create a new compiler instance given the name of the compiler.  */
  public static Compiler getInstance (String name)
  {
    try
      {
	Class k = Class.forName (classPrefix + name);
	return (Compiler) k.newInstance ();
      }
    catch (Throwable _)
      {
      }
    return null;
  }

  /** Set the directory where output files will be put.  */
  public void setDestination (String dest)
  {
    this.dest = dest;
  }

  /** Compile the given file.  Throws exception on error.  */
  public abstract void compile (String name) throws Exception;

  /** The destination directory, or null if none set.  */
  protected String dest;

  /** Class prefix used when trying to find instance.  */
  private static final String classPrefix = "gnu.java.rmi.rmic.Compile_";
}
