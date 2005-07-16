/* LittleEndianOutputStream.java --
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

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * This stream writes data in the Little Endian format
 * (less significant byte first). This is opposite to the
 * usual data presentation in java platform.
 *
 * This class reuses code from DataOutputStream.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
 */
public class LittleEndianOutputStream
  extends FilterOutputStream
  implements abstractDataOutputStream
{
  /**
   * This method initializes an instance of <code>DataOutputStream</code> to
   * write its data to the specified underlying <code>OutputStream</code>
   *
   * @param out The subordinate <code>OutputStream</code> to which this
   * object will write
   */
  public LittleEndianOutputStream(OutputStream out)
  {
    super(out);
  }

  /**
   * This method flushes any unwritten bytes to the underlying stream.
   *
   * @exception IOException If an error occurs.
   */
  public void flush()
             throws IOException
  {
    out.flush();
  }

  /**
   * This method writes the specified byte (passed as an <code>int</code>)
   * to the underlying output stream.
   *
   * @param value The <code>byte</code> to write, passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs.
   */
  public synchronized void write(int value)
                          throws IOException
  {
    out.write(value);
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
  public synchronized void write(byte[] buf, int offset, int len)
                          throws IOException
  {
    out.write(buf, offset, len);
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
  public void writeBoolean(boolean value)
                    throws IOException
  {
    write(value ? 1 : 0);
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
  public void writeByte(int value)
                 throws IOException
  {
    write(value & 0xff);
  }

  /**
   * This method writes a Java short value to an output stream.
   *
   * @param value The <code>short</code> value to write to the stream,
   * passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  public synchronized void writeShort(int value)
                               throws IOException
  {
    write((byte) (0xff & value));
    write((byte) (0xff & (value >> 8)));
  }

  /**
   * Writes char in Little Endian.
   */
  public synchronized void writeChar(int value)
                              throws IOException
  {
    write((byte) (0xff & value));
    write((byte) (0xff & (value >> 8)));
  }

  /**
   * Writes int in Little Endian.
   */
  public synchronized void writeInt(int value)
                             throws IOException
  {
    write((byte) (0xff & value));
    write((byte) (0xff & (value >> 8)));
    write((byte) (0xff & (value >> 16)));
    write((byte) (0xff & (value >> 24)));
  }

  /**
   * Writes long in Little Endian.
   */
  public synchronized void writeLong(long value)
                              throws IOException
  {
    write((byte) (0xff & value));
    write((byte) (0xff & (value >> 8)));
    write((byte) (0xff & (value >> 16)));
    write((byte) (0xff & (value >> 24)));
    write((byte) (0xff & (value >> 32)));
    write((byte) (0xff & (value >> 40)));
    write((byte) (0xff & (value >> 48)));
    write((byte) (0xff & (value >> 56)));
  }

  /**
   * This method writes a Java <code>float</code> value to the stream.  This
   * value is written by first calling the method
   * <code>Float.floatToIntBits</code>
   * to retrieve an <code>int</code> representing the floating point number,
   * then writing this <code>int</code> value to the stream exactly the same
   * as the <code>writeInt()</code> method does.
   *
   * @param value The <code>float</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see writeInt
   * @see DataInput#readFloat
   * @see Float#floatToIntBits
   */
  public void writeFloat(float value)
                  throws IOException
  {
    writeInt(Float.floatToIntBits(value));
  }

  /**
   * This method writes a Java <code>double</code> value to the stream.  This
   * value is written by first calling the method
   * <code>Double.doubleToLongBits</code>
   * to retrieve an <code>long</code> representing the floating point number,
   * then writing this <code>long</code> value to the stream exactly the same
   * as the <code>writeLong()</code> method does.
   *
   * @param value The <code>double</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   *
   * @see writeLong
   * @see DataInput#readDouble
   * @see Double#doubleToLongBits
   */
  public void writeDouble(double value)
                   throws IOException
  {
    writeLong(Double.doubleToLongBits(value));
  }
}