/* Cloneable.java -- Interface for marking objects cloneable by Object.clone()
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
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

/**
 * This interface should be implemented by classes wishing to
 * support of override <code>Object.clone()</code>.  The default
 * behaviour of <code>clone()</code> performs a shallow copy, but
 * subclasses often change this to perform a deep copy.  Therefore,
 * it is a good idea to document how deep your clone will go.
 * If <code>clone()</code> is called on an object which does not
 * implement this interface, a <code>CloneNotSupportedException</code>
 * will be thrown.
 * <p>
 *
 * This interface is simply a tagging interface; it carries no
 * requirements on methods to implement.  However, it is typical for
 * a Cloneable class to implement at least <code>equals</code>,
 * <code>hashCode</code>, and <code>clone</code>, sometimes
 * increasing the accessibility of clone to be public. The typical
 * implementation of <code>clone</code> invokes <code>super.clone()</code>
 * rather than a constructor, but this is not a requirement.
 * <p>
 *
 * If an object that implement Cloneable should not be cloned,
 * simply override the <code>clone</code> method to throw a
 * <code>CloneNotSupportedException</code>.
 * <p>
 *
 * All array types implement Cloneable, and have a public
 * <code>clone</code> method that will never fail with a
 * <code>CloneNotSupportedException</code>.
 *
 * @since 1.0
 * @author Paul Fisher
 * @author Eric Blake <ebb9@email.byu.edu>
 * @author Warren Levy <warrenl@cygnus.com>
 *
 * @see Object#clone()
 * @see CloneNotSupportedException
 */
public interface Cloneable
{
  // Tagging interface only.
}
