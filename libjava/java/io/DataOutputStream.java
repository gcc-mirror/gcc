/* DataOutputStream.java -- Writes primitive Java datatypes to streams
   Copyright (C) 1998, 2001, 2003 Free Software Foundation, Inc.

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
 * This class provides a mechanism for writing primitive Java datatypes
 * to an <code>OutputStream</code> in a portable way.  Data written to
 * a stream using this class can be read back in using the
 * <code>DataInputStream</code> class on any platform.
 *
 * @see DataInputStream
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Tom Tromey <tromey@cygnus.com>
 */
public class DataOutputStream extends FilterOutputStream implements DataOutput
{
  /**
   * This is the total number of bytes that have been written to the
   * stream by this object instance.
   */
  protected int written;

  /**
   * This method initializes an instance of <code>DataOutputStream</code> to
   * write its data to the specified underlying <code>OutputStream</code>
   *
   * @param out The subordinate <code>OutputStream</code> to which this 
   * object will write
   */
  public DataOutputStream (OutputStream out)
  {
    super (out);
    written = 0;
  }

  /**
   * This method flushes any unwritten bytes to the underlying stream.
   *
   * @exception IOException If an error occurs.
   */
  public void flush () throws IOException
  {
    out.flush();
  }

  /**
   * This method returns the total number of bytes that have been written to
   * the underlying output stream so far.  This is the value of the
   * <code>written</code> instance variable
   *
   * @return The number of bytes written to the stream.
   */
  public final int size ()
  {
    return written;
  }

  /**
   * This method writes the specified byte (passed as an <code>int</code>)
   * to the underlying output stream.
   *
   * @param b The byte to write, passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs.
   */
  public synchronized void write (int b) throws IOException
  {
    out.write(b);
    ++written;
  }

  /**
   * This method writes <code>len</code> bytes from the specified byte array
   * <code>buf</code> starting at position <code>offset</code> into the
   * buffer to the underlying output stream.
   *
   * @param buf The byte array to write from.
   * @param offset The index into the byte array to start writing from.
   * @param len The number of bytes to write.
   *
   * @exception IOException If an error occurs.
   */
  public synchronized void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException
  {
    out.write(b, off, len);
    written += len;
  }

  /**
   * This method writes a Java <code>boolean</code> to the underlying output 
   * stream. For a value of <code>true</code>, 1 is written to the stream.
   * For a value of <code>false</code>, 0 is written.
   *
   * @param b The <code>boolean</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   */
  public final void writeBoolean (boolean v) throws IOException
  {
    write (v ? 1 : 0);
  }

  /**
   * This method writes a Java <code>byte</code> value to the underlying
   * output stream.
   *
   * @param b The <code>byte</code> to write to the stream, passed as 
   * the low eight bits of an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  public final void writeByte (int v) throws IOException
  {
    write (v & 0xff);
  }

  /**
   * This method writes a Java <code>short</code> to the stream, high byte
   * first.  This method requires two bytes to encode the value.
   *
   * @param s The <code>short</code> value to write to the stream,
   * passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  public final void writeShort (int v) throws IOException
  {
    write ((byte) (0xff & (v >> 8)));
    write ((byte) (0xff & v));
  }

  /**
   * This method writes a single <code>char</code> value to the stream,
   * high byte first.
   *
   * @param c The <code>char</code> value to write, 
   * passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  public final void writeChar (int v) throws IOException
  {
    write ((byte) (0xff & (v >> 8)));
    write ((byte) (0xff & v));
  }

  /**
   * This method writes a Java <code>int</code> to the stream, high bytes
   * first.  This method requires four bytes to encode the value.
   *
   * @param i The <code>int</code> value to write to the stream.
   *
   * @exception IOException If an error occurs
   */
  public final void writeInt (int v) throws IOException
  {
    write ((byte) (0xff & (v >> 24)));
    write ((byte) (0xff & (v >> 16)));
    write ((byte) (0xff & (v >>  8)));
    write ((byte) (0xff & v));
  }

  /**
   * This method writes a Java <code>long</code> to the stream, high bytes
   * first.  This method requires eight bytes to encode the value.
   *
   * @param l The <code>long</code> value to write to the stream.
   *
   * @exception IOException If an error occurs
   */
  public final void writeLong (long v) throws IOException
  {
    write ((byte) (0xff & (v >> 56)));
    write ((byte) (0xff & (v >> 48)));
    write ((byte) (0xff & (v >> 40)));
    write ((byte) (0xff & (v >> 32)));
    write ((byte) (0xff & (v >> 24)));
    write ((byte) (0xff & (v >> 16)));
    write ((byte) (0xff & (v >>  8)));
    write ((byte) (0xff & v));
  }

  /**
   * This method writes a Java <code>float</code> value to the stream.  This
   * value is written by first calling the method 
   * <code>Float.floatToIntBits</code>
   * to retrieve an <code>int</code> representing the floating point number,
   * then writing this <code>int</code> value to the stream exactly the same
   * as the <code>writeInt()</code> method does.
   *
   * @param f The floating point number to write to the stream.
   *
   * @exception IOException If an error occurs
   *
   * @see writeInt
   */
  public final void writeFloat (float v) throws IOException
  {
    writeInt (Float.floatToIntBits(v));
  }

  /**
   * This method writes a Java <code>double</code> value to the stream.  This
   * value is written by first calling the method
   * <code>Double.doubleToLongBits</code>
   * to retrieve an <code>long</code> representing the floating point number,
   * then writing this <code>long</code> value to the stream exactly the same
   * as the <code>writeLong()</code> method does.
   *
   * @param d The double precision floating point number to write to 
   * the stream.
   *
   * @exception IOException If an error occurs
   *
   * @see writeLong
   */
  public final void writeDouble (double v) throws IOException
  {
    writeLong (Double.doubleToLongBits(v));
  }

  /**
   * This method writes all the bytes in a <code>String</code> out to the
   * stream.  One byte is written for each character in the
   * <code>String</code>.
   * The high eight bits of each character are discarded.
   *
   * @param s The <code>String</code> to write to the stream
   *
   * @exception IOException If an error occurs
   */
  public final void writeBytes (String s) throws IOException
  {
    int len = s.length();
    for (int i = 0; i < len; ++i)
      writeByte (s.charAt(i));
  }

  /**
   * This method writes all the characters in a <code>String</code> to the
   * stream.  There will be two bytes for each character value.  The high
   * byte of the character will be written first.
   *
   * @param s The <code>String</code> to write to the stream.
   *
   * @exception IOException If an error occurs
   */
  public final void writeChars (String s) throws IOException
  {
    int len = s.length();
    for (int i = 0; i < len; ++i)
      writeChar (s.charAt(i));
  }

  /**
   * This method writes a Java <code>String</code> to the stream in a modified
   * UTF-8 format.  First, two bytes are written to the stream indicating the
   * number of bytes to follow.  Note that this is the number of bytes in the
   * encoded <code>String</code> not the <code>String</code> length.  Next
   * come the encoded characters.  Each character in the <code>String</code>
   * is encoded as either one, two or three bytes.  For characters in the
   * range of <code>\u0001</code> to <\u007F>, one byte is used.  The character
   * value goes into bits 0-7 and bit eight is 0.  For characters in the range
   * of <code>\u0080</code> to <code>\u007FF</code>, two bytes are used.  Bits
   * 6-10 of the character value are encoded bits 0-4 of the first byte, with
   * the high bytes having a value of "110".  Bits 0-5 of the character value
   * are stored in bits 0-5 of the second byte, with the high bits set to
   * "10".  This type of encoding is also done for the null character
   * <code>\u0000</code>.  This eliminates any C style NUL character values
   * in the output.  All remaining characters are stored as three bytes.
   * Bits 12-15 of the character value are stored in bits 0-3 of the first
   * byte.  The high bits of the first bytes are set to "1110".  Bits 6-11
   * of the character value are stored in bits 0-5 of the second byte.  The
   * high bits of the second byte are set to "10".  And bits 0-5 of the
   * character value are stored in bits 0-5 of byte three, with the high bits
   * of that byte set to "10".
   *
   * @param s The <code>String</code> to write to the output in UTF format
   *
   * @exception IOException If an error occurs
   */
  public final void writeUTF (String s) throws IOException
  {
    int len = s.length();
    int sum = 0;

    for (int i = 0; i < len && sum <= 65535; ++i)
      {
	char c = s.charAt(i);
	if (c >= '\u0001' && c <= '\u007f')
	  sum += 1;
	else if (c == '\u0000' || (c >= '\u0080' && c <= '\u07ff'))
	  sum += 2;
	else
	  sum += 3;
      }

    if (sum > 65535)
      throw new UTFDataFormatException ();

    writeShort (sum);

    for (int i = 0; i < len; ++i)
      {
	char c = s.charAt(i);
	if (c >= '\u0001' && c <= '\u007f')
	  write (c);
	else if (c == '\u0000' || (c >= '\u0080' && c <= '\u07ff'))
	  {
	    write (0xc0 | (0x1f & (c >> 6)));
	    write (0x80 | (0x3f & c));
	  }
	else
	  {
	    // JSL says the first byte should be or'd with 0xc0, but
	    // that is a typo.  Unicode says 0xe0, and that is what is
	    // consistent with DataInputStream.
	    write (0xe0 | (0x0f & (c >> 12)));
	    write (0x80 | (0x3f & (c >> 6)));
	    write (0x80 | (0x3f & c));
	  }
      }
  }

} // class DataOutputStream

