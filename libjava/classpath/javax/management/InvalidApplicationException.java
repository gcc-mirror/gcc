/* InvalidApplicationException.java -- Thrown by invalid query attributes.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

/**
 * Thrown when a query or attribute is applied to a management bean
 * which is of the wrong class.  This exception is only used
 * internally by the Java management API and is not exposed to user
 * code.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class InvalidApplicationException
  extends Exception
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -3048022274675537269L;

  /**
   * The attribute value that caused the exception.
   */
  private Object val;

  /**
   * Constructs a new <code>InvalidApplicationException</code>
   * using the specified object to represent the invalid value.
   *
   * @param val the inappropriate expression.
   */
  public InvalidApplicationException(Object val)
  {
    super();
    this.val = val;
  }

  /**
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.InvalidApplicationException</code>)
   * and the invalid expression.
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  public String toString()
  {
    return getClass().getName()
      + "[val=" + val 
      + "]";
  }

}

