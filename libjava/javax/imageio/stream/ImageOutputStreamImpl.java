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


package javax.imageio.stream;

import java.io.IOException;
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

  protected void flushBits()
    throws IOException
  {
    // FIXME: Implement me.
    throw new Error("not implemented");
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
    // FIXME: Implement me.
    throw new Error("not implemented");
  }

  public void writeBits(long bits, int numBits)
    throws IOException
  {
    // FIXME: Implement me.
    throw new Error("not implemented");
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
    write(data.getBytes());
  }

  public void writeChar(int value)
    throws IOException
  {
    writeShort((short) value);
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
    // FIXME: Implement me.
    throw new Error("not implemented");
  }

  public void writeDouble(double value)
    throws IOException
  {
    writeLong((long) value);
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
    writeInt((int) value);
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
  
  public void writeUTF(String data)
    throws IOException
  {
    throw new Error("not implemented");
  }
}
