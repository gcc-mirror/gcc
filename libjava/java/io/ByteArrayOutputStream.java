/* BufferedReader.java
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005  Free Software Foundation, Inc.

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

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

/**
  * This class allows data to be written to a byte array buffer and
  * and then retrieved by an application.   The internal byte array
  * buffer is dynamically resized to hold all the data written.  Please
  * be aware that writing large amounts to data to this stream will
  * cause large amounts of memory to be allocated.
  * <p>
  * The size of the internal buffer defaults to 32 and it is resized
  * by doubling the size of the buffer.  This default size can be
  * overridden by using the
  * <code>gnu.java.io.ByteArrayOutputStream.initialBufferSize</code>
  * property.
  * <p>
  * There is a constructor that specified the initial buffer size and
  * that is the preferred way to set that value because it it portable
  * across all Java class library implementations.
  * <p>
  * Note that this class also has methods that convert the byte array
  * buffer to a <code>String</code> using either the system default or an
  * application specified character encoding.  Thus it can handle 
  * multibyte character encodings.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@cygnus.com)
  * @date September 24, 1998 
  */
public class ByteArrayOutputStream extends OutputStream
{
  /**
   * This method initializes a new <code>ByteArrayOutputStream</code>
   * with the default buffer size of 32 bytes.  If a different initial
   * buffer size is desired, see the constructor
   * <code>ByteArrayOutputStream(int size)</code>.  For applications
   * where the source code is not available, the default buffer size
   * can be set using the system property
   * <code>gnu.java.io.ByteArrayOutputStream.initialBufferSize</code>
   */
  public ByteArrayOutputStream ()
  {
    this (initial_buffer_size);
  }

  /**
   * This method initializes a new <code>ByteArrayOutputStream</code> with
   * a specified initial buffer size.
   *
   * @param size The initial buffer size in bytes
   */
  public ByteArrayOutputStream (int size)
  {
    buf = new byte[size];
    count = 0;
  }

  /**
   * This method discards all of the bytes that have been written to
   * the internal buffer so far by setting the <code>count</code>
   * variable to 0.  The internal buffer remains at its currently
   * allocated size.
   */
  public synchronized void reset ()
  {
    count = 0;
  }

  /**
   * This method returns the number of bytes that have been written to
   * the buffer so far.  This is the same as the value of the protected
   * <code>count</code> variable.  If the <code>reset</code> method is
   * called, then this value is reset as well.  Note that this method does
   * not return the length of the internal buffer, but only the number
   * of bytes that have been written to it.
   *
   * @return The number of bytes in the internal buffer
   *
   * @see #reset()
   */
  public int size ()
  {
    return count;
  }

  /**
   * This method returns a byte array containing the bytes that have been
   * written to this stream so far.  This array is a copy of the valid
   * bytes in the internal buffer and its length is equal to the number of
   * valid bytes, not necessarily to the the length of the current 
   * internal buffer.  Note that since this method allocates a new array,
   * it should be used with caution when the internal buffer is very large.
   */
  public synchronized byte[] toByteArray ()
  {
    byte[] ret = new byte[count];
    System.arraycopy(buf, 0, ret, 0, count);
    return ret;
  }

  /**
   * Returns the bytes in the internal array as a <code>String</code>.  The
   * bytes in the buffer are converted to characters using the system default
   * encoding.  There is an overloaded <code>toString()</code> method that
   * allows an application specified character encoding to be used.
   *
   * @return A <code>String</code> containing the data written to this
   * stream so far
   */
  public String toString ()
  {
    return new String (buf, 0, count);
  }

  /**
   * Returns the bytes in the internal array as a <code>String</code>.  The
   * bytes in the buffer are converted to characters using the specified
   * encoding. 
   *
   * @param enc The name of the character encoding to use
   *
   * @return A <code>String</code> containing the data written to this
   * stream so far
   *
   * @exception UnsupportedEncodingException If the named encoding is
   * not available
   */
  public String toString (String enc) throws UnsupportedEncodingException
  {
    return new String (buf, 0, count, enc);
  }

  /**
   * This method returns the bytes in the internal array as a
   * <code>String</code>.  It uses each byte in the array as the low
   * order eight bits of the Unicode character value and the passed in
   * parameter as the high eight bits.
   * <p>
   * This method does not convert bytes to characters in the proper way and
   * so is deprecated in favor of the other overloaded <code>toString</code>
   * methods which use a true character encoding.
   *
   * @param hibyte The high eight bits to use for each character in
   * the <code>String</code> 
   *
   * @return A <code>String</code> containing the data written to this
   * stream so far
   *
   * @deprecated
   */
  public String toString (int hibyte)
  {
    return new String (buf, 0, count, hibyte);
  }

  // Resize buffer to accommodate new bytes.
  private void resize (int add)
  {
    if (count + add > buf.length)
      {
	int newlen = buf.length * 2;
	if (count + add > newlen)
	  newlen = count + add;
	byte[] newbuf = new byte[newlen];
	System.arraycopy(buf, 0, newbuf, 0, count);
	buf = newbuf;
      }
  }

  /**
   * This method writes the writes the specified byte into the internal
   * buffer.
   *
   * @param oneByte The byte to be read passed as an int
   */
  public synchronized void write (int oneByte)
  {
    resize (1);
    buf[count++] = (byte) oneByte;
  }

  /**
   * This method writes <code>len</code> bytes from the passed in array 
   * <code>buf</code> starting at index <code>offset</code> into the
   * internal buffer.
   *
   * @param buffer The byte array to write data from
   * @param offset The index into the buffer to start writing data from
   * @param add The number of bytes to write
   */
  public synchronized void write (byte[] buffer, int offset, int add)
  {
    // If ADD < 0 then arraycopy will throw the appropriate error for
    // us.
    if (add >= 0)
      resize (add);
    System.arraycopy(buffer, offset, buf, count, add);
    count += add;
  }

  /**
   * This method writes all the bytes that have been written to this stream
   * from the internal buffer to the specified <code>OutputStream</code>.
   *
   * @param out The <code>OutputStream</code> to write to
   *
   * @exception IOException If an error occurs
   */
  public synchronized void writeTo (OutputStream out) throws IOException
  {
    out.write(buf, 0, count);
  }

  /**
   * The internal buffer where the data written is stored
   */
  protected byte[] buf;

  /**
   * The number of bytes that have been written to the buffer
   */
  protected int count;

  /**
   * The default initial buffer size.  Specified by the JCL.
   */
  private static final int DEFAULT_INITIAL_BUFFER_SIZE = 32;

  // The default buffer size which can be overridden by the user.
  private static final int initial_buffer_size;

  static
  {
    int r
      = Integer.getInteger ("gnu.java.io.ByteArrayOutputStream.initialBufferSize",
			    DEFAULT_INITIAL_BUFFER_SIZE).intValue ();
    if (r <= 0)
      r = DEFAULT_INITIAL_BUFFER_SIZE;
    initial_buffer_size = r;
  }
}
