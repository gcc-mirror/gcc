/*
  Copyright (c) 2001, 2003 Free Software Foundation, Inc.

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

package gnu.java.rmi.rmic;

/**
 * A Compiler object can be used to compile a .java file into a
 * .class file.  This is an abstract class; the
 * <code>getInstance()</code> method is used to obtain the actual
 * compiler object.
 */
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

   /** Get the directory where output files will be put.  */
   public String getDestination ()
   {
     return dest;
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
