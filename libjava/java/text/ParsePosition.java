/* ParsePosition.java -- Keep track of position while parsing.
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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


package java.text;

/**
 * This class is used to keep track of the current position during parsing
 * operations.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 */
public class ParsePosition
{
  /**
   * This is the index of the current parse position.
   */
  private int index;

  /**
   * This is the index of the position where an error occurred during parsing.
   */
  private int error_index;

  /**
   * This method initializes a new instance of <code>ParsePosition</code> to
   * have the specified initial index value.
   *
   * @param index The initial parsing index.
   */
  public ParsePosition (int index)
  {
    this.index = index;
    error_index = -1;
  }

  /**
   * This method returns the current parsing index.
   *
   * @return The current parsing index
   */
  public int getIndex ()
  {
    return index;
  }

  /**
   * This method sets the current parsing index to the specified value.
   *
   * @param index The new parsing index.
   */
  public void setIndex (int index)
  {
    this.index = index;
  }

  /**
   * This method returns the error index value.  This value defaults to -1
   * unless explicitly set to another value.
   *
   * @return The error index.
   */
  public int getErrorIndex ()
  {
    return error_index;
  }

  /**
   * This method sets the error index to the specified value.
   *
   * @param error_index The new error index
   */
  public void setErrorIndex (int error_index)
  {
    this.error_index = error_index;
  }

  /**
   * This method tests the specified object for equality with this
   * object.  The two objects will be considered equal if and only if
   * all of the following conditions are met.
   * <p>
   * <ul>
   * <li>The specified object is not <code>null</code>.</li>
   * <li>The specified object is an instance of <code>ParsePosition</code>.</li>
   * <li>The specified object has the same index and error index as
   *     this object.</li>
   * </ul>
   *
   * @param obj The <code>Object</code> to test for equality against
   *            this object. 
   *
   * @return <code>true</code> if the specified object is equal to
   * this object, <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof ParsePosition))
      return false;

    ParsePosition other = (ParsePosition) obj;
    return index == other.index && error_index == other.error_index;
  }

  /**
   * This method returns a <code>String</code> representation of this
   * object.
   *
   * @return A <code>String</code> that represents this object.
   */
  public String toString ()
  {
    return (getClass ().getName () + "[index=" + getIndex ()
	    + ",errorIndex=" + getErrorIndex () + "]");
  }
}
