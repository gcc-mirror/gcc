/* ImageOutputStream.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.UTFDataFormatException;
import java.nio.ByteOrder;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class ImageOutputStreamImpl extends ImageInputStreamImpl
  implements ImageOutputStream
{
  public ImageOutputStreamImpl()
  {
    // Do nothing here.
  }

  protected final void flushBits()
    throws IOException
  {
    checkClosed();
    if (bitOffset != 0)
      {
        int offset = bitOffset;
        int partial = read();
        if (partial < 0)
          {
            partial = 0;
            bitOffset = 0;
          }
        else
          {
            seek(getStreamPosition() - 1);
            partial &= -1 << (8 - offset);
          }
        write(partial);
      }
  }

  public void write(byte[] data)
    throws IOException
  {
    write(data, 0, data.length);
  }

  public abstract void write(byte[] data, int offset, int len)
    throws IOException;

  public abstract void write(int value)
    throws IOException;

  public void writeBit(int bit)
    throws IOException
  {
    writeBits(1L & bit, 1);
  }

  public void writeBits(long bits, int numBits)
    throws IOException
  {
    checkClosed();
    // Append chunk of bits to any preexisting bits, if any.
    if (getStreamPosition() > 0 || bitOffset > 0)
      {
        int offs = bitOffset;
        int partial = read();
        if (partial != -1)
          seek(getStreamPosition() - 1);
        else
          partial = 0;
        if (numBits + offs < 8)
          {
            // Append complete bits to partial byte.
            int shift = 8 - (offs + numBits);
            int mask = -1 >>> (32 - numBits);
            partial &= ~(mask << shift);
            partial |= (bits & mask) << shift;
            write(partial);
            seek(getStreamPosition() - 1);
            bitOffset = offs + numBits;
            numBits = 0;
          }
        else
          {
            // Append bits and decrease numBits accordingly.
            int num = 8 - offs;
            int mask = -1 >>> (32 - num);
            partial &= ~mask;
            partial |= (bits >> (numBits - num)) & mask;
            write(partial);
            numBits -= num;
          }
      }

    // Write out whole chunks, if any.
    if (numBits > 7)
      {
        int remaining = numBits % 8;
        for (int numBytes = numBits / 8; numBytes > 0; numBytes--)
          {
            int shift = (numBytes - 1) * 8 + remaining;
            int value = (int) ((shift == 0) ? bits & 0xff
                                            : (bits >> shift) & 0xff);
            write(value);
          }
        numBits = remaining;
      }

    // Write remaing partial bytes.
    if (numBits != 0)
      {
        int partial = read();
        if (partial == -1)
          {
            seek(getStreamPosition() - 1);
          }
        else
          {
            partial = 0;
          }
        int shift = 8 - numBits;
        int mask = -1 >>> (32 - numBits);
        partial &= ~(mask << shift);
        partial |= (bits & mask) << shift;
        write(partial);
        seek(getStreamPosition() - 1);
        bitOffset = numBits;
      }
  }

  public void writeBoolean(boolean value)
    throws IOException
  {
    writeByte(value ? 1 : 0);
  }

  public void writeByte(int value)
    throws IOException
  {
    write(value & 0xff);
  }

  public void writeBytes(String data)
    throws IOException
  {
    // This is bogus, but it is how the method is specified.
    // Sun ought to deprecate this method.
    int len = data.length();
    for (int i = 0; i < len; ++i)
      writeByte(data.charAt(i));
  }

  public void writeChar(int value)
    throws IOException
  {
    writeShort(value);
  }

  public void writeChars(char[] data, int offset, int len)
    throws IOException
  {
    for(int i = 0; i < len; ++len)
      writeChar(data[offset + i]);
  }

  public void writeChars(String data)
    throws IOException
  {
    int len = data.length();
    for (int i = 0; i < len; ++i)
      writeChar(data.charAt(i));
  }

  public void writeDouble(double value)
    throws IOException
  {
    writeLong(Double.doubleToLongBits(value));
  }

  public void writeDoubles(double[] data, int offset, int len)
    throws IOException
  {
    for(int i = 0; i < len; ++len)
      writeDouble(data[offset + i]);
  }
  
  public void writeFloat(float value)
    throws IOException
  {
    writeInt(Float.floatToIntBits(value));
  }
  
  public void writeFloats(float[] data, int offset, int len)
    throws IOException
  {
    for(int i = 0; i < len; ++len)
      writeFloat(data[offset + i]);
  }
  
  public void writeInt(int value)
    throws IOException
  {
    if (getByteOrder() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer[0] = ((byte) value);
        buffer[1] = ((byte) (value >> 8));
        buffer[2] = ((byte) (value >> 16));
        buffer[3] = ((byte) (value >> 24));
      }
    else
      {
        buffer[0] = ((byte) (value >> 24));
        buffer[1] = ((byte) (value >> 16));
        buffer[2] = ((byte) (value >> 8));
        buffer[3] = ((byte) value);
      }
    
    write(buffer, 0, 4);
  }
  
  public void writeInts(int[] data, int offset, int len)
    throws IOException
  {
    for(int i = 0; i < len; ++len)
      writeInt(data[offset + i]);
  }
  
  public void writeLong(long value)
    throws IOException
  {
    if (getByteOrder() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer[0] = ((byte) value);
        buffer[1] = ((byte) (value >> 8));
        buffer[2] = ((byte) (value >> 16));
        buffer[3] = ((byte) (value >> 24));
        buffer[4] = ((byte) (value >> 32));
        buffer[5] = ((byte) (value >> 40));
        buffer[6] = ((byte) (value >> 48));
        buffer[7] = ((byte) (value >> 56));
      }
    else
      {
        buffer[0] = ((byte) (value >> 56));
        buffer[1] = ((byte) (value >> 48));
        buffer[2] = ((byte) (value >> 40));
        buffer[3] = ((byte) (value >> 32));
        buffer[4] = ((byte) (value >> 24));
        buffer[5] = ((byte) (value >> 16));
        buffer[6] = ((byte) (value >> 8));
        buffer[7] = ((byte) value);
      }
    
    write(buffer, 0, 8);
  }
  
  public void writeLongs(long[] data, int offset, int len)
    throws IOException
  {
    for(int i = 0; i < len; ++len)
      writeLong(data[offset + i]);
  }
  
  public void writeShort(int value)
    throws IOException
  {
    if (getByteOrder() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer[0] = ((byte) value);
        buffer[1] = ((byte) (value >> 8));
      }
    else
      {
        buffer[0] = ((byte) (value >> 8));
        buffer[1] = ((byte) value);
      }
    
    write(buffer, 0, 2);
  }
  
  public void writeShorts(short[] data, int offset, int len)
    throws IOException
  {
    for(int i = 0; i < len; ++len)
      writeShort(data[offset + i]);
  }
  
  public void writeUTF(String value)
    throws IOException
  {
    // NOTE: this code comes directly from DataOutputStream.
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

    int pos = 0;
    byte[] buf = new byte[sum];

    for (int i = 0; i < len; ++i)
      {
        char c = value.charAt(i);
        if (c >= '\u0001' && c <= '\u007f')
          buf[pos++] = (byte) c;
        else if (c == '\u0000' || (c >= '\u0080' && c <= '\u07ff'))
          {
            buf[pos++] = (byte) (0xc0 | (0x1f & (c >> 6)));
            buf[pos++] = (byte) (0x80 | (0x3f & c));
          }
        else
          {
            // JSL says the first byte should be or'd with 0xc0, but
            // that is a typo.  Unicode says 0xe0, and that is what is
            // consistent with DataInputStream.
            buf[pos++] = (byte) (0xe0 | (0x0f & (c >> 12)));
            buf[pos++] = (byte) (0x80 | (0x3f & (c >> 6)));
            buf[pos++] = (byte) (0x80 | (0x3f & c));
          }
      }
    
    writeShort (sum);
    write(buf, 0, sum);
  }
}
