/* LittleEndianInputStream.java --
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005  Free Software Foundation

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


package gnu.CORBA.CDR;

import java.io.DataInput;
import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;

/**
 * This class reads data in the Little Endian format. It reuses
 * code from GNU Classpath DataInputStream.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class LittleEndianInputStream
  extends FilterInputStream
  implements abstractDataInputStream
{
  // Byte buffer, used to make primitive read calls more efficient.
  byte[] buf = new byte[ 8 ];

  /**
   * This constructor initializes a new <code>DataInputStream</code>
   * to read from the specified subordinate stream.
   *
   * @param in The subordinate <code>InputStream</code> to read from
   */
  public LittleEndianInputStream(InputStream in)
  {
    super(in);
  }

  /**
   * This method reads bytes from the underlying stream into the specified
   * byte array buffer.  It will attempt to fill the buffer completely, but
   * may return a short count if there is insufficient data remaining to be
   * read to fill the buffer.
   *
   * @param b The buffer into which bytes will be read.
   *
   * @return The actual number of bytes read, or -1 if end of stream reached
   * before reading any bytes.
   *
   * @exception IOException If an error occurs.
   */
  public int read(byte[] b)
           throws IOException
  {
    return in.read(b, 0, b.length);
  }

  /**
   * This method reads bytes from the underlying stream into the specified
   * byte array buffer.  It will attempt to read <code>len</code> bytes and
   * will start storing them at position <code>off</code> into the buffer.
   * This method can return a short count if there is insufficient data
   * remaining to be read to complete the desired read length.
   *
   * @param b The buffer into which bytes will be read.
   * @param off The offset into the buffer to start storing bytes.
   * @param len The requested number of bytes to read.
   *
   * @return The actual number of bytes read, or -1 if end of stream reached
   * before reading any bytes.
   *
   * @exception IOException If an error occurs.
   */
  public int read(byte[] b, int off, int len)
           throws IOException
  {
    return in.read(b, off, len);
  }

  /**
   * This method reads a Java boolean value from an input stream.  It does
   * so by reading a single byte of data.  If that byte is zero, then the
   * value returned is <code>false</code>.  If the byte is non-zero, then
   * the value returned is <code>true</code>.
   * <p>
   * This method can read a <code>boolean</code> written by an object
   * implementing the <code>writeBoolean()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>boolean</code> value read
   *
   * @exception EOFException If end of file is reached before reading
   * the boolean
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeBoolean
   */
  public boolean readBoolean()
                      throws IOException
  {
    return convertToBoolean(in.read());
  }

  /**
   * This method reads a Java byte value from an input stream.  The value
   * is in the range of -128 to 127.
   * <p>
   * This method can read a <code>byte</code> written by an object
   * implementing the <code>writeByte()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>byte</code> value read
   *
   * @exception EOFException If end of file is reached before reading the byte
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeByte
   */
  public byte readByte()
                throws IOException
  {
    return convertToByte(in.read());
  }

  /**
   * This method reads a Java <code>char</code> value from an input stream.
   * It operates by reading two bytes from the stream and converting them to
   * a single 16-bit Java <code>char</code>.  The two bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code>
   * represent the first and second byte read from the stream
   * respectively, they will be transformed to a <code>char</code> in
   * the following manner:
   * <p>
   * <code>(char)(((byte1 &amp; 0xFF) &lt;&lt; 8) | (byte2 &amp; 0xFF)</code>
   * <p>
   * This method can read a <code>char</code> written by an object
   * implementing the <code>writeChar()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>char</code> value read
   *
   * @exception EOFException If end of file is reached before reading the char
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeChar
   */
  public char readChar()
                throws IOException
  {
    readFully(buf, 0, 2);
    return convertToChar(buf);
  }

  /**
   * This method reads a Java double value from an input stream.  It operates
   * by first reading a <code>long</code> value from the stream by calling the
   * <code>readLong()</code> method in this interface, then converts
   * that <code>long</code> to a <code>double</code> using the
   * <code>longBitsToDouble</code> method in the class
   * <code>java.lang.Double</code>
   * <p>
   * This method can read a <code>double</code> written by an object
   * implementing the <code>writeDouble()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>double</code> value read
   *
   * @exception EOFException If end of file is reached before reading
   * the double
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeDouble
   * @see java.lang.Double#longBitsToDouble
   */
  public double readDouble()
                    throws IOException
  {
    return Double.longBitsToDouble(readLong());
  }

  /**
   * This method reads a Java float value from an input stream.  It
   * operates by first reading an <code>int</code> value from the
   * stream by calling the <code>readInt()</code> method in this
   * interface, then converts that <code>int</code> to a
   * <code>float</code> using the <code>intBitsToFloat</code> method
   * in the class <code>java.lang.Float</code>
   * <p>
   * This method can read a <code>float</code> written by an object
   * implementing the <code>writeFloat()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>float</code> value read
   *
   * @exception EOFException If end of file is reached before reading the float
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeFloat
   * @see java.lang.Float#intBitsToFloat
   */
  public float readFloat()
                  throws IOException
  {
    return Float.intBitsToFloat(readInt());
  }

  /**
   * This method reads raw bytes into the passed array until the array is
   * full.  Note that this method blocks until the data is available and
   * throws an exception if there is not enough data left in the stream to
   * fill the buffer.  Note also that zero length buffers are permitted.
   * In this case, the method will return immediately without reading any
   * bytes from the stream.
   *
   * @param b The buffer into which to read the data
   *
   * @exception EOFException If end of file is reached before filling the
   * buffer
   * @exception IOException If any other error occurs
   */
  public void readFully(byte[] b)
                 throws IOException
  {
    readFully(b, 0, b.length);
  }

  /**
   * This method reads raw bytes into the passed array <code>buf</code>
   * starting
   * <code>offset</code> bytes into the buffer.  The number of bytes read
   * will be
   * exactly <code>len</code>.  Note that this method blocks until the data is
   * available and throws an exception if there is not enough data left in
   * the stream to read <code>len</code> bytes.  Note also that zero length
   * buffers are permitted.  In this case, the method will return immediately
   * without reading any bytes from the stream.
   *
   * @param buf The buffer into which to read the data
   * @param offset The offset into the buffer to start storing data
   * @param len The number of bytes to read into the buffer
   *
   * @exception EOFException If end of file is reached before filling the
   * buffer
   * @exception IOException If any other error occurs
   */
  public void readFully(byte[] buf, int offset, int len)
                 throws IOException
  {
    if (len < 0)
      throw new IndexOutOfBoundsException("Negative length: " + len);

    while (len > 0)
      {
        // in.read will block until some data is available.
        int numread = in.read(buf, offset, len);
        if (numread < 0)
          throw new EOFException();
        len -= numread;
        offset += numread;
      }
  }

  /**
   * This method reads a Java <code>int</code> value from an input stream
   * It operates by reading four bytes from the stream and converting them to
   * a single Java <code>int</code>.  The bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> through <code>byte4</code> represent
   * the first four bytes read from the stream, they will be
   * transformed to an <code>int</code> in the following manner:
   * <p>
   * <code>(int)(((byte1 &amp; 0xFF) &lt;&lt; 24) + ((byte2 &amp; 0xFF) &lt;&lt; 16) +
   * ((byte3 &amp; 0xFF)&lt;&lt; 8) + (byte4 &amp; 0xFF)))</code>
   * <p>
   * The value returned is in the range of -2147483648 to 2147483647.
   * <p>
   * This method can read an <code>int</code> written by an object
   * implementing the <code>writeInt()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>int</code> value read
   *
   * @exception EOFException If end of file is reached before reading the int
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeInt
   */
  public int readInt()
              throws IOException
  {
    readFully(buf, 0, 4);
    return convertToInt(buf);
  }

  /**
   * This method reads the next line of text data from an input
   * stream.  It operates by reading bytes and converting those bytes
   * to <code>char</code> values by treating the byte read as the low
   * eight bits of the <code>char</code> and using 0 as the high eight
   * bits.  Because of this, it does not support the full 16-bit
   * Unicode character set.
   * <p>
   * The reading of bytes ends when either the end of file or a line
   * terminator is encountered.  The bytes read are then returned as a
   * <code>String</code> A line terminator is a byte sequence
   * consisting of either <code>\r</code>, <code>\n</code> or
   * <code>\r\n</code>.  These termination charaters are discarded and
   * are not returned as part of the string.
   * <p>
   * This method can read data that was written by an object implementing the
   * <code>writeLine()</code> method in <code>DataOutput</code>.
   *
   * @return The line read as a <code>String</code>
   *
   * @exception IOException If an error occurs
   *
   * @see DataOutput
   *
   * @deprecated
   */
  public String readLine()
                  throws IOException
  {
    StringBuffer strb = new StringBuffer();

    while (true)
      {
        int c = in.read();
        if (c == -1) // got an EOF
          return strb.length() > 0 ? strb.toString() : null;
        if (c == '\r')
          {
            int next_c = in.read();
            if (next_c != '\n' && next_c != -1)
              {
                if (!(in instanceof PushbackInputStream))
                  in = new PushbackInputStream(in);
                ((PushbackInputStream) in).unread(next_c);
              }
            break;
          }
        if (c == '\n')
          break;
        strb.append((char) c);
      }

    return strb.length() > 0 ? strb.toString() : "";
  }

  /**
   * This method reads a Java <code>long</code> value from an input stream
   * It operates by reading eight bytes from the stream and converting them to
   * a single Java <code>long</code>.  The bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> through <code>byte8</code> represent
   * the first eight bytes read from the stream, they will be
   * transformed to an <code>long</code> in the following manner:
   * <p>
   * <code>(long)(((byte1 &amp; 0xFF) &lt;&lt; 56) + ((byte2 &amp; 0xFF) &lt;&lt; 48) +
   * ((byte3 &amp; 0xFF) &lt;&lt; 40) + ((byte4 &amp; 0xFF) &lt;&lt; 32) +
   * ((byte5 &amp; 0xFF) &lt;&lt; 24) + ((byte6 &amp; 0xFF) &lt;&lt; 16) +
   * ((byte7 &amp; 0xFF) &lt;&lt; 8) + (byte8 &amp; 0xFF)))
   * </code>
   * <p>
   * The value returned is in the range of -9223372036854775808 to
   * 9223372036854775807.
   * <p>
   * This method can read an <code>long</code> written by an object
   * implementing the <code>writeLong()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>long</code> value read
   *
   * @exception EOFException If end of file is reached before reading the long
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeLong
   */
  public long readLong()
                throws IOException
  {
    readFully(buf, 0, 8);
    return convertToLong(buf);
  }

  /**
   * This method reads a signed 16-bit value into a Java in from the
   * stream.  It operates by reading two bytes from the stream and
   * converting them to a single 16-bit Java <code>short</code>.  The
   * two bytes are stored most significant byte first (i.e., "big
   * endian") regardless of the native host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code>
   * represent the first and second byte read from the stream
   * respectively, they will be transformed to a <code>short</code>. in
   * the following manner:
   * <p>
   * <code>(short)(((byte1 &amp; 0xFF) &lt;&lt; 8) | (byte2 &amp; 0xFF))</code>
   * <p>
   * The value returned is in the range of -32768 to 32767.
   * <p>
   * This method can read a <code>short</code> written by an object
   * implementing the <code>writeShort()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>short</code> value read
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeShort
   */
  public short readShort()
                  throws IOException
  {
    readFully(buf, 0, 2);
    return convertToShort(buf);
  }

  /**
   * This method reads 8 unsigned bits into a Java <code>int</code>
   * value from the stream. The value returned is in the range of 0 to
   * 255.
   * <p>
   * This method can read an unsigned byte written by an object
   * implementing the <code>writeUnsignedByte()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The unsigned bytes value read as a Java <code>int</code>.
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeByte
   */
  public int readUnsignedByte()
                       throws IOException
  {
    return convertToUnsignedByte(in.read());
  }

  /**
   * This method reads 16 unsigned bits into a Java int value from the stream.
   * It operates by reading two bytes from the stream and converting them to
   * a single Java <code>int</code>  The two bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code>
   * represent the first and second byte read from the stream
   * respectively, they will be transformed to an <code>int</code> in
   * the following manner:
   * <p>
   * <code>(int)(((byte1 &amp; 0xFF) &lt;&lt; 8) + (byte2 &amp; 0xFF))</code>
   * <p>
   * The value returned is in the range of 0 to 65535.
   * <p>
   * This method can read an unsigned short written by an object
   * implementing the <code>writeUnsignedShort()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The unsigned short value read as a Java <code>int</code>
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeShort
   */
  public int readUnsignedShort()
                        throws IOException
  {
    readFully(buf, 0, 2);
    return convertToUnsignedShort(buf);
  }

  /**
   * This method attempts to skip and discard the specified number of bytes
   * in the input stream.  It may actually skip fewer bytes than requested.
   * This method will not skip any bytes if passed a negative number of bytes
   * to skip.
   *
   * @param n The requested number of bytes to skip.
   *
   * @return The requested number of bytes to skip.
   *
   * @exception IOException If an error occurs.
   * @specnote The JDK docs claim that this returns the number of bytes
   *  actually skipped. The JCL claims that this method can throw an
   *  EOFException. Neither of these appear to be true in the JDK 1.3's
   *  implementation. This tries to implement the actual JDK behaviour.
   */
  public int skipBytes(int n)
                throws IOException
  {
    if (n <= 0)
      return 0;
    try
      {
        return (int) in.skip(n);
      }
    catch (EOFException x)
      {
        // do nothing.
      }
    return n;
  }

  protected boolean convertToBoolean(int b)
                              throws EOFException
  {
    if (b < 0)
      throw new EOFException();

    return (b != 0);
  }

  protected byte convertToByte(int i)
                        throws EOFException
  {
    if (i < 0)
      throw new EOFException();

    return (byte) i;
  }

  protected int convertToUnsignedByte(int i)
                               throws EOFException
  {
    if (i < 0)
      throw new EOFException();

    return (i & 0xFF);
  }

  /**
   * Less significant byte first.
   */
  protected char convertToChar(byte[] buf)
  {
    return (char) ((buf [ 1 ] << 8) | (buf [ 0 ] & 0xff));
  }

  /**
   * Less significant byte first.
   */
  protected short convertToShort(byte[] buf)
  {
    return (short) ((buf [ 1 ] << 8) | (buf [ 0 ] & 0xff));
  }

  /**
   * Less significant byte first.
   */
  protected int convertToUnsignedShort(byte[] buf)
  {
    return (((buf [ 1 ] & 0xff) << 8) | (buf [ 0 ] & 0xff));
  }

  /**
   * Less significant byte first.
   */
  protected int convertToInt(byte[] buf)
  {
    return (((buf [ 3 ] & 0xff) << 24) | ((buf [ 2 ] & 0xff) << 16) |
           ((buf [ 1 ] & 0xff) << 8) | (buf [ 0 ] & 0xff));
  }

  /**
   * Less significant byte first.
   */
  protected long convertToLong(byte[] buf)
  {
    return (((long) (buf [ 7 ] & 0xff) << 56) |
           ((long) (buf [ 6 ] & 0xff) << 48) |
           ((long) (buf [ 5 ] & 0xff) << 40) |
           ((long) (buf [ 4 ] & 0xff) << 32) |
           ((long) (buf [ 3 ] & 0xff) << 24) |
           ((long) (buf [ 2 ] & 0xff) << 16) |
           ((long) (buf [ 1 ] & 0xff) << 8) | ((long) (buf [ 0 ] & 0xff)));
  }

  /**
   * This should never be called.
   *
   * @throws InternalError, always.
   */
  public String readUTF()
  {
    throw new InternalError();
  }
}