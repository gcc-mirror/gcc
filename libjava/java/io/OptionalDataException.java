/* OptionalDataException.java -- indicates unexpected data in serialized stream
   Copyright (C) 1998, 2000, 2001, 2002, 2005  Free Software Foundation, Inc.

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


package java.io;

/**
 * This exception is thrown when unexpected data appears in the input
 * stream from which a serialized object is being read. There are two
 * cases:<br><ul>
 * <li>The next stream element is primitive data. <code>eof</code> will
 *     be false, and <code>count</code> is the number of bytes of primitive
 *     data available.</li>
 * <li>The data consumable by readObject or readExternal has been exhausted.
 *     <code>eof</code> is true, and <code>count</code> is 0.</li>
 * </ul>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 * @since 1.1
 * @status updated to 1.4
 */
public class OptionalDataException extends ObjectStreamException
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -8011121865681257820L;

  /**
   * Whether or not the end of the stream has been reached.
   *
   * @serial the end of the buffer was reached
   */
  public boolean eof;

  /**
   * The number of valid bytes that can be read.
   *
   * @serial the bytes of the buffer remaining
   */
  public int length;

  /**
   * Create a new OptionalDataException with an eof parameter indicating
   * whether or not the end of stream is reached and the number of valid
   * bytes that may be read.
   *
   * @param eof 'true' if end of stream reached, 'false' otherwise
   * @param count The number of valid bytes to be read
   */
  OptionalDataException(boolean eof, int count)
  {
    this.eof = eof;
    this.length = count;
  }
} // class OptionalDataException
