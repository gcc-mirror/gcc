/* StringWriter.java -- Writes bytes to a StringBuffer
   Copyright (C) 1998, 1999, 2000, 2001, 2003 Free Software Foundation, Inc.

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

// Wow is this a dumb class.  CharArrayWriter can do all this and
// more.  I would redirect all calls to one in fact, but the javadocs say
// use a StringBuffer so I will comply.

/**
  * This class writes chars to an internal <code>StringBuffer</code> that
  * can then be used to retrieve a <code>String</code>.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey <tromey@cygnus.com>
  */
public class StringWriter extends Writer
{
  /**
   * This is the default size of the buffer if the user doesn't specify it.
   * @specnote The JCL Volume 1 says that 16 is the default size.
   */
  private static final int DEFAULT_BUFFER_SIZE = 16;

  /**
   * This method closes the stream.  The contents of the internal buffer
   * can still be retrieved, but future writes are not guaranteed to work.
   */
  public void close () throws IOException
  {
    // JCL says this does nothing.  This seems to violate the Writer
    // contract, in that other methods should still throw an
    // IOException after a close.  Still, we just follow JCL.
  }

  /**
   * This method flushes any buffered characters to the underlying output.
   * It does nothing in this class.
   */
  public void flush ()
  {
  }

  /**
   * This method returns the <code>StringBuffer</code> object that this
   * object is writing to.  Note that this is the actual internal buffer, so
   * any operations performed on it will affect this stream object.
   *
   * @return The <code>StringBuffer</code> object being written to
   */
  public StringBuffer getBuffer ()
  {
    return buffer;
  }

  /**
   * This method initializes a new <code>StringWriter</code> to write to a
   * <code>StringBuffer</code> initially sized to a default size of 16
   * chars.
   */
  public StringWriter ()
  {
    this (DEFAULT_BUFFER_SIZE);
  }

  /**
   * This method initializes a new <code>StringWriter</code> to write to a
   * <code>StringBuffer</code> with the specified initial size.
   *
   * @param size The initial size to make the <code>StringBuffer</code>
   */
  public StringWriter (int size)
  {
    super ();
    buffer = new StringBuffer (size);
    lock = buffer;
  }

  /**
   * This method returns the contents of the internal <code>StringBuffer</code>
   * as a <code>String</code>.
   *
   * @return A <code>String</code> representing the chars written to
   * this stream. 
   */
  public String toString ()
  {
    return buffer.toString();
  }

  /**
   * This method writes a single character to the output, storing it in
   * the internal buffer.
   *
   * @param oneChar The <code>char</code> to write, passed as an int.
   */
  public void write (int oneChar)
  {
    buffer.append((char) (oneChar & 0xFFFF));
  }

  /**
   * This method writes <code>len</code> chars from the specified
   * array starting at index <code>offset</code> in that array to this
   * stream by appending the chars to the end of the internal buffer.
   *
   * @param chars The array of chars to write
   * @param offset The index into the array to start writing from
   * @param len The number of chars to write
   */
  public void write (char[] chars, int offset, int len)
  {
    buffer.append(chars, offset, len);
  }

  /**
   * This method writes the characters in the specified <code>String</code>
   * to the stream by appending them to the end of the internal buffer.
   *
   * @param str The <code>String</code> to write to the stream.
   */
  public void write (String str)
  {
    buffer.append(str);
  }

  /**
   * This method writes out <code>len</code> characters of the specified
   * <code>String</code> to the stream starting at character position
   * <code>offset</code> into the stream.  This is done by appending the
   * characters to the internal buffer.
   *
   * @param str The <code>String</code> to write characters from
   * @param offset The character position to start writing from
   * @param len The number of characters to write.
   */ 
  public void write (String str, int offset, int len)
  {
//      char[] tmpbuf = new char[len];
//      str.getChars(offset, offset+len, tmpbuf, 0);
//      buf.append(tmpbuf, 0, tmpbuf.length);
    // This implementation assumes that String.substring is more
    // efficient than using String.getChars and copying the data
    // twice.  For libgcj, this is true.  For Classpath, it is not.
    // FIXME.
    buffer.append(str.substring(offset, offset + len));
  }

  /**
   * This is the <code>StringBuffer</code> that we use to store bytes that
   * are written.
   */
  private StringBuffer buffer;
}
