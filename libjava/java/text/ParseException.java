/* ParseException.java -- an error occurred while parsing
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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
 * This exception is thrown when an unexpected error occurs during parsing.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 * @see Format
 * @see FieldPosition
 * @status updated to 1.4
 */
public class ParseException extends Exception
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 2703218443322787634L;

  /**
   * This is the position where the error was encountered.
   *
   * @serial the zero-based offset in the string where the error occurred
   */
  private final int errorOffset;

  /**
   * This method initializes a new instance of <code>ParseException</code>
   * with a detailed error message and a error position.
   *
   * @param msg the descriptive message describing the error
   * @param offset the position where the error was encountered
   */
  public ParseException(String s, int offset)
  {
    super(s);
    errorOffset = offset;
  }

  /**
   * This method returns the position where the error occurred.
   *
   * @return the position where the error occurred
   */
  public int getErrorOffset()
  {
    return errorOffset;
  }
} // class ParseException
