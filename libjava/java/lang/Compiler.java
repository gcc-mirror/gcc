/* Compiler.java -- Interface for implementing a method to override 
   Object.clone()comparaing objects to obtain an ordering
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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
executable file might be covered by the GNU General Public License. */


package java.lang;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 */

/**
 * The <code>Compiler</code> class is a place holder for a JIT
 * compiler implementation does nothing unless there is such a 
 * compiler by default.
 * <p>
 * The system property <code>java.compiler</code> may contain the name
 * of a library to load with <code>System.loadLibrary</code> when the
 * virtual machine first starts.  If so and loading the library succeeds,
 * then a function by the name of <code>java_lang_Compiler_start()</code> 
 * in that library is called.
 *
 * Note that a VM might not have implemented any of this.
 *
 * @author Tom Tromey <tromey@cygnus.com>
 *
 * @since JDK 1.0
 * @see System#getProperty(java.lang.String)
 * @see System#getProperty(java.lang.String,java.lang.String)
 * @see System#loadLibrary(java.lang.String)
 */
public final class Compiler 
{
    /**
     * Don't allow new `Compiler's to be made.
     */
    private Compiler ()
    {
    }

    /**
     * Compile the class named by <code>oneClass</code>.
     * 
     * @param oneClass the class to compile
     * @return <code>false</code> if no compiler is available or 
     * compilation failed and <code>true</code> if compilation succeeded.
     */
    public static boolean compileClass (Class oneClass) 
    {
 	// Never succeed.
	return false;
    }
    
    /**
     * Compile the classes whose name matches <code>classNames/code>.
     *
     * @param classNames the name of classes to compile
     * @return <code>false</code> if no compiler is available or 
     * compilation failed and <code>true</code> if compilation succeeded.
     */
    public static boolean compileClasses (String classNames) 
    {
	// Note the incredibly lame interface.  Always fail.
	return false;
    }

    /**
     * This method examines the argument and performs an operation 
     * according to the compilers documentation.  No specific operation
     * is required.
     */
    public static Object command (Object arg) 
    {
	// Our implementation defines this to a no-op.
	return null;
    }

    /**
     * Calling <code>Compiler.enable()</code> will cause the compiler
     * to resume operation if it was previously disabled.  
     */
    public static void enable () { }

    /**
     * Calling <code>Compiler.disable()</code> will cause the compiler
     * to be suspended.
     */
    public static void disable () { }
}
