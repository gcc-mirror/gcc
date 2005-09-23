/* ImageInputStream.java
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.imageio.stream;

import java.io.DataInput;
import java.io.EOFException;
import java.io.IOException;
import java.nio.ByteOrder;


/**
 * An input stream for use by {@link javax.imageio.ImageReader
 * ImageReaders}.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public interface ImageInputStream
  extends DataInput
{
  void setByteOrder(ByteOrder order);

  ByteOrder getByteOrder();
  
  int read()
    throws IOException;

  int read(byte[] b)
    throws IOException;

  int read(byte[] b, int offset, int length)
    throws IOException;


  /**
   * Reads up to a specified number of bytes, and modifies a
   * {@link IIOByteBuffer} to hold the read data.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   *
   * @param buf an <code>IIOByteBuffer</code> that will hold the read
   * data.
   *
   * @param numBytes the maximum number of bytes to read.
   *
   * @throws IndexOutOfBoundsException if <code>numBytes</code> is
   * negative.
   *
   * @throws NullPointerException if <code>buf</code> is
   * <code>null</code>.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   */
  void readBytes(IIOByteBuffer buf, int numBytes)
    throws IOException;


  /**
   * Reads a byte and checks whether or not its value is zero.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before the byte is read.
   *
   * @throws EOFException if the input stream is at its end.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readBit()
   * @see #readByte()
   * @see #readFully(byte[], int, int)
   */
  boolean readBoolean()
    throws IOException;


  /**
   * Reads a signed byte.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   *
   * @throws EOFException if the input stream is at its end.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readUnsignedByte()
   * @see #readFully(byte[], int, int)
   */
  byte readByte()
    throws IOException;


  /**
   * Reads an unsigned byte.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   *
   * @throws EOFException if the input stream is at its end.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readByte()
   * @see #readFully(byte[], int, int)
   */
  int readUnsignedByte()
    throws IOException;


  /**
   * Reads an signed 16-bit integer. If necessary, the value gets
   * converted from the stream&#x2019;s {@linkplain #getByteOrder()
   * current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @throws EOFException if the input stream ends before all two
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readUnsignedShort()
   * @see #readChar()
   * @see #readFully(short[], int, int)
   */
  short readShort()
    throws IOException;


  /**
   * Reads an unsigned 16-bit integer. If necessary, the value gets
   * converted from the stream&#x2019;s {@linkplain #getByteOrder()
   * current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * <p>This method does the same as {@link #readChar()}.
   *
   * @throws EOFException if the input stream ends before all two
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readShort()
   * @see #readChar()
   * @see #readFully(char[], int, int)
   */
  int readUnsignedShort()
    throws IOException;


  /**
   * Reads an unsigned 16-bit integer. If necessary, the value gets
   * converted from the stream&#x2019;s {@linkplain #getByteOrder()
   * current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * <p>This method does the same as {@link #readUnsignedShort()}.
   *
   * @throws EOFException if the input stream ends before all two
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readFully(char[], int, int)
   */
  char readChar()
    throws IOException;


  /**
   * Reads a signed 32-bit integer. If necessary, the value gets
   * converted from the stream&#x2019;s {@linkplain #getByteOrder()
   * current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @throws EOFException if the input stream ends before all four
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readUnsignedInt()
   * @see #readFully(int[], int, int)
   */
  int readInt()
    throws IOException;


  /**
   * Reads an unsigned 32-bit integer. If necessary, the value gets
   * converted from the stream&#x2019;s {@linkplain #getByteOrder()
   * current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @throws EOFException if the input stream ends before all four
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readInt()
   * @see #readFully(int[], int, int)
   */
  long readUnsignedInt()
    throws IOException;


  /**
   * Reads a signed 64-bit integer. If necessary, the value gets
   * converted from the stream&#x2019;s {@linkplain #getByteOrder()
   * current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @throws EOFException if the input stream ends before all eight
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readFully(long[], int, int)
   */
  long readLong()
    throws IOException;


  /**
   * Reads an IEEE 32-bit single-precision floating point number. If
   * necessary, the value gets converted from the stream&#x2019;s
   * {@linkplain #getByteOrder() current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @throws EOFException if the input stream ends before all four
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readFully(float[], int, int)
   */
  float readFloat()
    throws IOException;


  /**
   * Reads an IEEE 64-bit double-precision floating point number. If
   * necessary, the value gets converted from the stream&#x2019;s
   * {@linkplain #getByteOrder() current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @throws EOFException if the input stream ends before all eight
   * bytes were read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readFully(double[], int, int)
   */
  double readDouble()
    throws IOException;

  String readLine()
    throws IOException;

  String readUTF()
    throws IOException;


  /**
   * Reads a sequence of signed 8-bit integers into a
   * <code>byte[]</code> array.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param b an array for storing the read values.
   *
   * @param offset the index of the first element in <code>b</code>
   * that will hold read data.
   *
   * @param numBytes the number of bytes to read.
   *
   * @throws IndexOutOfBoundsException if <code>offset</code> or
   * <code>numBytes</code> is negative, or if <code>offset +
   * numBytes</code> exceeds <code>b.length</code>.
   *
   * @throws NullPointerException if <code>b</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readByte()
   */
  void readFully(byte[] b, int offset, int numBytes)
    throws IOException;


  /**
   * Reads a sequence of signed 8-bit integers into a
   * <code>byte[]</code> array.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param b an array for storing the read values.
   *
   * @throws NullPointerException if <code>b</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readByte()
   * @see #readFully(byte[], int, int)
   */
  void readFully(byte[] b)
    throws IOException;


  /**
   * Reads a sequence of signed 16-bit integers into a
   * <code>short[]</code> array.  If necessary, values are converted
   * from the stream&#x2019;s {@linkplain #getByteOrder() current byte
   * order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param s an array for storing the read values.
   *
   * @param offset the index of the first element in <code>s</code>
   * that will hold read data.
   *
   * @param numShorts the number of signed 16-bit integers to read
   * (which is one half of the number of bytes).
   *
   * @throws IndexOutOfBoundsException if <code>offset</code> or
   * <code>numShorts</code> is negative, or if <code>offset +
   * numShorts</code> exceeds <code>s.length</code>.
   *
   * @throws NullPointerException if <code>s</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readShort()
   */
  void readFully(short[] s, int offset, int numShorts)
    throws IOException;


  /**
   * Reads a sequence of unsigned 16-bit integers into a
   * <code>char[]</code> array.  If necessary, values are converted
   * from the stream&#x2019;s {@linkplain #getByteOrder() current byte
   * order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param c an array for storing the read values.
   *
   * @param offset the index of the first element in <code>c</code>
   * that will hold read data.
   *
   * @param numChars the number of unsigned 16-bit integers to read
   * (which is one half of the number of bytes).
   *
   * @throws IndexOutOfBoundsException if <code>offset</code> or
   * <code>numChars</code> is negative, or if <code>offset +
   * numChars</code> exceeds <code>c.length</code>.
   *
   * @throws NullPointerException if <code>c</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readChar()
   */
  void readFully(char[] c, int offset, int numChars)
    throws IOException;


  /**
   * Reads a sequence of signed 32-bit integers into a
   * <code>long[]</code> array.  If necessary, values are converted
   * from the stream&#x2019;s {@linkplain #getByteOrder() current byte
   * order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param i an array for storing the read values.
   *
   * @param offset the index of the first element in <code>i</code>
   * that will hold read data.
   *
   * @param numLongs the number of signed 32-bit integers to read
   * (which is one fourth of the number of bytes).
   *
   * @throws IndexOutOfBoundsException if <code>offset</code> or
   * <code>numInts</code> is negative, or if <code>offset +
   * numInts</code> exceeds <code>i.length</code>.
   *
   * @throws NullPointerException if <code>i</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readInt()
   */
  void readFully(int[] i, int offset, int numInts)
    throws IOException;


  /**
   * Reads a sequence of signed 64-bit integers into a
   * <code>long[]</code> array.  If necessary, values are converted
   * from the stream&#x2019;s {@linkplain #getByteOrder() current byte
   * order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param l an array for storing the read values.
   *
   * @param offset the index of the first element in <code>l</code>
   * that will hold read data.
   *
   * @param numLongs the number of signed 64-bit integers to read
   * (which is one eight of the number of bytes).
   *
   * @throws IndexOutOfBoundsException if <code>offset</code> or
   * <code>numLongs</code> is negative, or if <code>offset +
   * numLongs</code> exceeds <code>l.length</code>.
   *
   * @throws NullPointerException if <code>l</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readLong()
   */
  void readFully(long[] l, int offset, int numLongs)
    throws IOException;


  /**
   * Reads a sequence of IEEE 32-bit single-precision floating point
   * numbers into a <code>float[]</code> array.  If necessary, values
   * are converted from the stream&#x2019;s {@linkplain
   * #getByteOrder() current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param d an array for storing the read values.
   *
   * @param offset the index of the first element in <code>d</code>
   * that will hold read data.
   *
   * @param numFloats the number of IEEE 32-bit single-precision
   * floating point numbers to read (which is one fourth of the number
   * of bytes).
   *
   * @throws IndexOutOfBoundsException if <code>offset</code> or
   * <code>numFloats</code> is negative, or if <code>offset +
   * numFloats</code> exceeds <code>f.length</code>.
   *
   * @throws NullPointerException if <code>f</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readFloat()
   */
  void readFully(float[] f, int offset, int numFloats)
    throws IOException;


  /**
   * Reads a sequence of IEEE 64-bit double-precision floating point
   * numbers into a <code>double[]</code> array.  If necessary, values
   * are converted from the stream&#x2019;s {@linkplain
   * #getByteOrder() current byte order}.
   *
   * <p>The {@linkplain #getBitOffset() bit offset} is set to zero
   * before any data is read.
   * 
   * @param d an array for storing the read values.
   *
   * @param offset the index of the first element in <code>d</code>
   * that will hold read data.
   *
   * @param numDoubles the number of IEEE 64-bit double-precision
   * floating point numbers to read (which is one eight of the number
   * of bytes).
   *
   * @throws IndexOutOfBoundsException if <code>offset</code> or
   * <code>numDoubles</code> is negative, or if <code>offset +
   * numDoubles</code> exceeds <code>d.length</code>.
   *
   * @throws NullPointerException if <code>d</code> is
   * <code>null</code>.
   *
   * @throws EOFException if the input stream ends before all content
   * was read.
   *
   * @throws IOException if some general problem happens with
   * accessing data.
   *
   * @see #readDouble()
   */
  void readFully(double[] d, int offset, int numDoubles)
    throws IOException;

  long getStreamPosition()
    throws IOException;

  int getBitOffset()
    throws IOException;

  void setBitOffset(int bitOffset)
    throws IOException;

  int readBit()
    throws IOException;

  long readBits(int numBits)
    throws IOException;

  long length()
    throws IOException;

  int skipBytes(int numBytes)
    throws IOException;

  long skipBytes(long numBytes)
    throws IOException;

  void seek(long pos)
    throws IOException;

  void mark();

  void reset()
    throws IOException;

  void flushBefore(long pos)
    throws IOException;

  void flush()
    throws IOException;

  long getFlushedPosition();

  boolean isCached();

  boolean isCachedMemory();

  boolean isCachedFile();

  void close()
    throws IOException;
}
