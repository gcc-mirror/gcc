/* CharArrayWriter.java -- Write chars to a buffer
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


package java.io;

/**
  * This class allows data to be written to a char array buffer and
  * and then retrieved by an application.   The internal char array
  * buffer is dynamically resized to hold all the data written.  Please
  * be aware that writing large amounts to data to this stream will
  * cause large amounts of memory to be allocated.
  * <p>
  * The size of the internal buffer defaults to 32 and it is resized
  * in increments of 1024 chars.  This behavior can be over-ridden by using the
  * following two properties:
  * <p>
  * <ul>
  * <li><xmp>gnu.java.io.CharArrayWriter.initialBufferSize</xmp></li>
  * <li><xmp>gnu.java.io.CharArrayWriter.bufferIncrementSize</xmp></li>
  * </ul>
  * <p>
  * There is a constructor that specified the initial buffer size and
  * that is the preferred way to set that value because it it portable
  * across all Java class library implementations.
  * <p>
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@cygnus.com)
  */
public class CharArrayWriter extends Writer
{
  /**
   * The default initial buffer size
   */
  private static final int DEFAULT_INITIAL_BUFFER_SIZE = 32;

  /**
   * This method initializes a new <code>CharArrayWriter</code> with
   * the default buffer size of 32 chars.  If a different initial
   * buffer size is desired, see the constructor
   * <code>CharArrayWriter(int size)</code>.
   */
  public CharArrayWriter ()
  {
    this (DEFAULT_INITIAL_BUFFER_SIZE);
  }

  /**
   * This method initializes a new <code>CharArrayWriter</code> with
   * a specified initial buffer size.
   *
   * @param size The initial buffer size in chars
   */
  public CharArrayWriter (int size)
  {
    super ();
    buf = new char[size];
  }

  /**
   * Closes the stream.  This method is guaranteed not to free the contents
   * of the internal buffer, which can still be retrieved.
   */
  public void close ()
  {
  }

  /**
   * This method flushes all buffered chars to the stream.
   */
  public void flush ()
  {
  }

  /**
   * This method discards all of the chars that have been written to the
   * internal buffer so far by setting the <code>count</code> variable to
   * 0.  The internal buffer remains at its currently allocated size.
   */
  public void reset ()
  {
    synchronized (lock)
      {
        count = 0;
      }
  }

  /**
   * This method returns the number of chars that have been written to
   * the buffer so far.  This is the same as the value of the protected
   * <code>count</code> variable.  If the <code>reset</code> method is
   * called, then this value is reset as well.  Note that this method does
   * not return the length of the internal buffer, but only the number
   * of chars that have been written to it.
   *
   * @return The number of chars in the internal buffer
   *
   * @see #reset()
   */
  public int size ()
  {
    return count;
  }

  /**
   * This method returns a char array containing the chars that have been
   * written to this stream so far.  This array is a copy of the valid
   * chars in the internal buffer and its length is equal to the number of
   * valid chars, not necessarily to the the length of the current
   * internal buffer.  Note that since this method allocates a new array,
   * it should be used with caution when the internal buffer is very large.
   */
  public char[] toCharArray ()
  {
    synchronized (lock)
      {
        char[] nc = new char[count];
        System.arraycopy(buf, 0, nc, 0, count);
        return nc;
      }
  }

  /**
   * Returns the chars in the internal array as a <code>String</code>.  The
   * chars in the buffer are converted to characters using the system default
   * encoding.  There is an overloaded <code>toString()</code> method that
   * allows an application specified character encoding to be used.
   *
   * @return A <code>String</code> containing the data written to this
   *         stream so far
   */
  public String toString ()
  {
    synchronized (lock)
      {
        return new String (buf, 0, count);
      }
  }

  /**
   * This method writes the writes the specified char into the internal
   * buffer.
   *
   * @param oneChar The char to be read passed as an int
   */
  public void write (int oneChar)
  {
    synchronized (lock)
      {
        resize (1);
        buf[count++] = (char) oneChar;
      }
  }

  /**
   * This method writes <code>len</code> chars from the passed in array
   * <code>buf</code> starting at index <code>offset</code> into that buffer
   *
   * @param buffer The char array to write data from
   * @param offset The index into the buffer to start writing data from
   * @param len The number of chars to write
   */
  public void write (char[] buffer, int offset, int len)
  {
    synchronized (lock)
      {
        if (len >= 0)
          resize (len);
        System.arraycopy(buffer, offset, buf, count, len);
        count += len;
      }
  }

  /**
   * This method writes <code>len</code> chars from the passed in
   * <code>String</code> <code>buf</code> starting at index
   * <code>offset</code> into the internal buffer.
   *
   * @param str The <code>String</code> to write data from
   * @param offset The index into the string to start writing data from
   * @param len The number of chars to write
   */
  public void write (String str, int offset, int len)
  {
    synchronized (lock)
      {
        if (len >= 0)
          resize (len);
        str.getChars(offset, offset + len, buf, count);
        count += len;
      }
  }

  /**
   * This method writes all the chars that have been written to this stream
   * from the internal buffer to the specified <code>Writer</code>.
   *
   * @param out The <code>Writer</code> to write to
   *
   * @exception IOException If an error occurs
   */
  public void writeTo (Writer out) throws IOException
  {
    synchronized (lock)
      {
        out.write(buf, 0, count);
      }
  }

  /**
   * Appends the Unicode character, <code>c</code>, to the output stream
   * underlying this writer.  This is equivalent to <code>write(c)</code>.
   *
   * @param c the character to append.
   * @return a reference to this object.
   * @since 1.5
   */
  public CharArrayWriter append(char c)
  {
    write(c);
    return this;
  }

  /**
   * Appends the specified sequence of Unicode characters to the
   * output stream underlying this writer.  This is equivalent to
   * appending the results of calling <code>toString()</code> on the
   * character sequence.  As a result, the entire sequence may not be
   * appended, as it depends on the implementation of
   * <code>toString()</code> provided by the
   * <code>CharSequence</code>.  For example, if the character
   * sequence is wrapped around an input buffer, the results will
   * depend on the current position and length of that buffer.
   *
   * @param cs the character sequence to append.  If seq is null,
   *        then the string "null" (the string representation of null)
   *        is appended.
   * @return a reference to this object.
   * @since 1.5
   */
  public CharArrayWriter append(CharSequence cs)
  {
    try
      {
        write(cs == null ? "null" : cs.toString());
      }
    catch (IOException _)
      {
        // Can't happen.
      }
    return this;
  }

  /**
   * Appends the specified subsequence of Unicode characters to the
   * output stream underlying this writer, starting and ending at the
   * specified positions within the sequence.  The behaviour of this
   * method matches the behaviour of writing the result of
   * <code>append(seq.subSequence(start,end))</code> when the sequence
   * is not null.
   *
   * @param cs the character sequence to append.  If seq is null,
   *        then the string "null" (the string representation of null)
   *        is appended.
   * @param start the index of the first Unicode character to use from
   *        the sequence.
   * @param end the index of the last Unicode character to use from the
   *        sequence.
   * @return a reference to this object.
   * @throws IndexOutOfBoundsException if either of the indices are negative,
   *         the start index occurs after the end index, or the end index is
   *         beyond the end of the sequence.
   * @since 1.5
   */
  public CharArrayWriter append(CharSequence cs, int start, int end)
  {
    try
      {
        write(cs == null ? "null" : cs.subSequence(start, end).toString());
      }
    catch (IOException _)
      {
        // Can't happen.
      }
    return this;
  }

  /**
   * This private method makes the buffer bigger when we run out of room
   * by allocating a larger buffer and copying the valid chars from the
   * old array into it.  This is obviously slow and should be avoided by
   * application programmers by setting their initial buffer size big
   * enough to hold everything if possible.
   */
  private void resize (int len)
  {
    if (count + len >= buf.length)
      {
        int newlen = buf.length * 2;
        if (count + len > newlen)
          newlen = count + len;
        char[] newbuf = new char[newlen];
        System.arraycopy(buf, 0, newbuf, 0, count);
        buf = newbuf;
      }
  }

  /**
   * The internal buffer where the data written is stored
   */
  protected char[] buf;

  /**
   * The number of chars that have been written to the buffer
   */
  protected int count;
}
