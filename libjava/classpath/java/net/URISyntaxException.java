/* URISyntaxException.java -- a string could not be parsed as a URI
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.net;


/**
 * This exception is thrown when a String cannot be parsed as a URI.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see URI
 * @since 1.4
 * @status updated to 1.4
 */
public class URISyntaxException extends Exception
{
  /**
   * Compatible with JDK 1.4+.
   */
  private static final long serialVersionUID = 2137979680897488891L;

  /**
   * The failed input.
   *
   * @serial the bad URI
   */
  private final String input;

  /**
   * The index of failure.
   *
   * @serial the location of the problem
   */
  private final int index;

  /**
   * Create an exception from the invalid string, with the index set to -1.
   *
   * @param input the bad URI
   * @param msg the descriptive error message
   * @throws NullPointerException if input or msg are null
   */
  public URISyntaxException(String input, String msg)
  {
    this(input, msg, -1);
  }

  /**
   * Create an exception from the invalid string, with the index of the
   * point of failure.
   *
   * @param input the bad URI
   * @param msg the descriptive error message
   * @param index the index of the parse error, or -1
   * @throws NullPointerException if input or msg are null
   * @throws IllegalArgumentException if index &lt; -1
   */
  public URISyntaxException(String input, String msg, int index)
  {
    // The toString() hack checks for null.
    super(msg.toString());
    this.input = input.toString();
    this.index = index;
    if (index < -1)
      throw new IllegalArgumentException();
  }

  /**
   * Returns the bad input string.
   *
   * @return the bad URI, guaranteed non-null
   */
  public String getInput()
  {
    return input;
  }

  /**
   * Returns the reason for the failure.
   *
   * @return the message, guaranteed non-null
   */
  public String getReason()
  {
    return super.getMessage();
  }

  /**
   * Returns the index of the failure, or -1.
   *
   * @return the index of failure
   */
  public int getIndex()
  {
    return index;
  }

  /**
   * Returns a message describing the parse error, as if by
   * <code>getReason() + (getIndex() &gt;= 0 ? " at index " + getIndex() : "")
   * + ": " + getInput()</code>.
   *
   * @return the message string
   */
  public String getMessage()
  {
    return (super.getMessage() + (index >= 0 ? " at index " + index : "")
           + ": " + input);
  }
}
