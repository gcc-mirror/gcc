/* DataOutputStream.java -- Writes primitive Java datatypes to streams
   Copyright (C) 1998, 2001, 2003, 2005  Free Software Foundation, Inc.

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
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
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
   * @param value The <code>byte</code> to write, passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs.
   */
  public synchronized void write (int value) throws IOException
  {
    out.write (value);
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
  public synchronized void write (byte[] buf, int offset, int len) 
     throws IOException
  {
    out.write(buf, offset, len);
    written += len;
  }

  /**
   * This method writes a Java boolean value to an output stream.  If
   * <code>value</code> is <code>true</code>, a byte with the value of
   * 1 will be written, otherwise a byte with the value of 0 will be
   * written.
   *
   * The value written can be read using the <code>readBoolean</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>boolean</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see DataInput#readBoolean
   */
  public final void writeBoolean (boolean value) throws IOException
  {
    write (value ? 1 : 0);
  }

  /**
   * This method writes a Java byte value to an output stream.  The
   * byte to be written will be in the lowest 8 bits of the
   * <code>int</code> value passed.
   *
   * The value written can be read using the <code>readByte</code> or
   * <code>readUnsignedByte</code> methods in <code>DataInput</code>.
   *
   * @param value The <code>byte</code> to write to the stream, passed as 
   * the low eight bits of an <code>int</code>.
   *
   * @exception IOException If an error occurs
   *
   * @see DataInput#readByte
   * @see DataInput#readUnsignedByte
   */
  public final void writeByte (int value) throws IOException
  {
    write (value & 0xff);
  }

  /**
   * This method writes a Java short value to an output stream.  The
   * char to be written will be in the lowest 16 bits of the <code>int</code>
   * value passed.  These bytes will be written "big endian".  That is,
   * with the high byte written first in the following manner:
   * <p>
   * <code>byte0 = (byte)((value & 0xFF00) >> 8);<br>
   * byte1 = (byte)(value & 0x00FF);</code>
   * <p>
   *
   * The value written can be read using the <code>readShort</code> and
   * <code>readUnsignedShort</code> methods in <code>DataInput</code>.
   *
   * @param value The <code>short</code> value to write to the stream,
   * passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs
   *
   * @see DataInput#readShort
   * @see DataInput#readUnsignedShort
   */
  public final synchronized void writeShort (int value) throws IOException
  {
    write ((byte) (0xff & (value >> 8)));
    write ((byte) (0xff & value));
  }

  /**
   * This method writes a Java char value to an output stream.  The
   * char to be written will be in the lowest 16 bits of the <code>int</code>
   * value passed.  These bytes will be written "big endian".  That is,
   * with the high byte written first in the following manner:
   * <p>
   * <code>byte0 = (byte)((value & 0xFF00) >> 8);<br>
   * byte1 = (byte)(value & 0x00FF);</code>
   * <p>
   *
   * The value written can be read using the <code>readChar</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>char</code> value to write, 
   * passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs
   *
   * @see DataInput#readChar
   */
  public final synchronized void writeChar (int value) throws IOException
  {
    write ((byte) (0xff & (value >> 8)));
    write ((byte) (0xff & value));
  }

  /**
   * This method writes a Java int value to an output stream.  The 4 bytes
   * of the passed value will be written "big endian".  That is, with
   * the high byte written first in the following manner:
   * <p>
   * <code>byte0 = (byte)((value & 0xFF000000) >> 24);<br>
   * byte1 = (byte)((value & 0x00FF0000) >> 16);<br>
   * byte2 = (byte)((value & 0x0000FF00) >> 8);<br>
   * byte3 = (byte)(value & 0x000000FF);</code>
   * <p>
   *
   * The value written can be read using the <code>readInt</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>int</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see DataInput#readInt
   */
  public final synchronized void writeInt (int value) throws IOException
  {
    write ((byte) (0xff & (value >> 24)));
    write ((byte) (0xff & (value >> 16)));
    write ((byte) (0xff & (value >>  8)));
    write ((byte) (0xff & value));
  }

  /**
   * This method writes a Java long value to an output stream.  The 8 bytes
   * of the passed value will be written "big endian".  That is, with
   * the high byte written first in the following manner:
   * <p>
   * <code>byte0 = (byte)((value & 0xFF00000000000000L) >> 56);<br>
   * byte1 = (byte)((value & 0x00FF000000000000L) >> 48);<br>
   * byte2 = (byte)((value & 0x0000FF0000000000L) >> 40);<br>
   * byte3 = (byte)((value & 0x000000FF00000000L) >> 32);<br>
   * byte4 = (byte)((value & 0x00000000FF000000L) >> 24);<br>
   * byte5 = (byte)((value & 0x0000000000FF0000L) >> 16);<br>
   * byte6 = (byte)((value & 0x000000000000FF00L) >> 8);<br>
   * byte7 = (byte)(value & 0x00000000000000FFL);</code>
   * <p>
   *
   * The value written can be read using the <code>readLong</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>long</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see DataInput#readLong
   */
  public final synchronized void writeLong (long value) throws IOException
  {
    write ((byte) (0xff & (value >> 56)));
    write ((byte) (0xff & (value>> 48)));
    write ((byte) (0xff & (value>> 40)));
    write ((byte) (0xff & (value>> 32)));
    write ((byte) (0xff & (value>> 24)));
    write ((byte) (0xff & (value>> 16)));
    write ((byte) (0xff & (value>>  8)));
    write ((byte) (0xff & value));
  }

  /**
   * This method writes a Java <code>float</code> value to the stream.  This
   * value is written by first calling the method
   * <code>Float.floatToIntBits</code>
   * to retrieve an <code>int</code> representing the floating point number,
   * then writing this <code>int</code> value to the stream exactly the same
   * as the <code>writeInt()</code> method does.
   *
   * The value written can be read using the <code>readFloat</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>float</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see writeInt
   * @see DataInput#readFloat
   * @see Float#floatToIntBits
   */
  public final void writeFloat (float value) throws IOException
  {
    writeInt (Float.floatToIntBits (value));
  }

  /**
   * This method writes a Java <code>double</code> value to the stream.  This
   * value is written by first calling the method
   * <code>Double.doubleToLongBits</code>
   * to retrieve an <code>long</code> representing the floating point number,
   * then writing this <code>long</code> value to the stream exactly the same
   * as the <code>writeLong()</code> method does.
   *
   * The value written can be read using the <code>readDouble</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>double</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see writeLong
   * @see DataInput#readDouble
   * @see Double#doubleToLongBits
   */
  public final void writeDouble (double value) throws IOException
  {
    writeLong (Double.doubleToLongBits (value));
  }

  /**
   * This method writes all the bytes in a <code>String</code> out to the
   * stream.  One byte is written for each character in the
   * <code>String</code>.
   * The high eight bits of each character are discarded, thus this
   * method is inappropriate for completely representing Unicode characters.
   *
   * @param value The <code>String</code> to write to the stream
   *
   * @exception IOException If an error occurs
   */
  public final void writeBytes (String value) throws IOException
  {
    int len = value.length();
    for (int i = 0; i < len; ++i)
      writeByte (value.charAt(i));
  }

  /**
   * This method writes all the characters of a <code>String</code> to an
   * output stream as an array of <code>char</code>'s. Each character
   * is written using the method specified in the <code>writeChar</code>
   * method.
   *
   * @param value The <code>String</code> to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see writeChar
   */
  public final void writeChars (String value) throws IOException
  {
    int len = value.length();
    for (int i = 0; i < len; ++i)
      writeChar (value.charAt(i));
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
   * The value written can be read using the <code>readUTF</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>String</code> to write to the output in UTF format
   *
   * @exception IOException If an error occurs
   *
   * @see DataInput#readUTF
   */
  public final synchronized void writeUTF(String value) throws IOException
  {
    int len = value.length();
    int sum = 0;

    for (int i = 0; i < len && sum <= 65535; ++i)
      {
	char c = value.charAt(i);
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
	char c = value.charAt(i);
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

