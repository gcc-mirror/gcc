/* ByteBufferImpl.java -- 
   Copyright (C) 2003 Free Software Foundation, Inc.

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

package java.nio;

/**
 * @author Michael Koch <konqueror@gmx.de>
 */
final class ByteBufferHelper
{
  private static final void checkRemainingForRead (ByteBuffer buffer, int bytes)
  {
    if (buffer.remaining() < bytes)
      throw new BufferUnderflowException();
  }
  
  private static final void checkRemainingForWrite (ByteBuffer buffer, int bytes)
  {
    if (buffer.remaining() < bytes)
      throw new BufferOverflowException();
  }

  private static final void checkAvailableForRead (ByteBuffer buffer,
                                                   int index, int bytes)
  {
    if (buffer.limit() < (index + bytes))
      throw new BufferUnderflowException();
  }
  
  private static final void checkAvailableForWrite (ByteBuffer buffer,
                                                    int index, int bytes)
  {
    if (buffer.limit() < (index + bytes))
      throw new BufferOverflowException();
  }
  
  public static final char getChar (ByteBuffer buffer)
  {
    return (char) getShort (buffer);
  }
  
  public static final ByteBuffer putChar (ByteBuffer buffer, char value)
  {
    return putShort (buffer, (short) value);
  }
  
  public static final char getChar (ByteBuffer buffer, int index)
  {
    return (char) getShort (buffer, index);
  }
  
  public static final ByteBuffer putChar (ByteBuffer buffer, int index,
                                          char value)
  {
    return putShort (buffer, index, (short) value);
  }

  public static final short getShort (ByteBuffer buffer)
  {
    checkRemainingForRead (buffer, 2);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        return (short) ((buffer.get() & 0xff)
                        + (buffer.get() << 8));
      }

    return (short) ((buffer.get() << 8)
                    + (buffer.get() & 0xff));
  }
  
  public static final ByteBuffer putShort (ByteBuffer buffer, short value)
  {
    checkRemainingForWrite (buffer, 2);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put ((byte) value);
        buffer.put ((byte) (value >> 8));
      }
    else
      {
        buffer.put ((byte) (value >> 8));
        buffer.put ((byte) value);
      }

    return buffer;
  }
  
  public static final short getShort (ByteBuffer buffer, int index)
  {
    checkAvailableForRead (buffer, index, 2);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        return (short) ((buffer.get (index) & 0xff)
                        + (buffer.get (++index) << 8));
      }

    return (short) ((buffer.get (index) << 8)
                    + (buffer.get (++index) & 0xff));
  }
  
  public static final ByteBuffer putShort (ByteBuffer buffer, int index,
                                           short value)
  {
    checkAvailableForWrite (buffer, index, 2);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put (index, (byte) value);
        buffer.put (++index, (byte) (value >> 8));
      }
    else
      {
        buffer.put (index, (byte) (value >> 8));
        buffer.put (++index, (byte) value);
      }
    
    return buffer;
  }

  public static final int getInt (ByteBuffer buffer)
  {
    checkRemainingForRead (buffer, 4);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        return ((buffer.get() & 0xff)
                + ((buffer.get() & 0xff) << 8)
                + ((buffer.get() & 0xff) << 16)
                + (buffer.get() << 24));
      }

    return (int) ((buffer.get() << 24)
                  + ((buffer.get() & 0xff) << 16)
                  + ((buffer.get() & 0xff) << 8)
                  + (buffer.get() & 0xff));
  }
  
  public static final ByteBuffer putInt (ByteBuffer buffer, int value)
  {
    checkRemainingForWrite (buffer, 4);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put ((byte) value);
        buffer.put ((byte) (value >> 8));
        buffer.put ((byte) (value >> 16));
        buffer.put ((byte) (value >> 24));
      }
    else
      {
        buffer.put ((byte) (value >> 24));
        buffer.put ((byte) (value >> 16));
        buffer.put ((byte) (value >> 8));
        buffer.put ((byte) value);
      }
    
    return buffer;
  }
  
  public static final int getInt (ByteBuffer buffer, int index)
  {
    checkAvailableForRead (buffer, index, 4);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        return ((buffer.get (index) & 0xff)
                + ((buffer.get (++index) & 0xff) << 8)
                + ((buffer.get (++index) & 0xff) << 16)
                + (buffer.get (++index) << 24));
      }

    return ((buffer.get (index) << 24)
            + ((buffer.get (++index) & 0xff) << 16)
            + ((buffer.get (++index) & 0xff) << 8)
            + (buffer.get (++index) & 0xff));
  }
  
  public static final ByteBuffer putInt (ByteBuffer buffer, int index,
                                         int value)
  {
    checkAvailableForWrite (buffer, index, 4);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put (index, (byte) value);
        buffer.put (++index, (byte) (value >> 8));
        buffer.put (++index, (byte) (value >> 16));
        buffer.put (++index, (byte) (value >> 24));
      }
    else
      {
        buffer.put (index, (byte) (value >> 24));
        buffer.put (++index, (byte) (value >> 16));
        buffer.put (++index, (byte) (value >> 8));
        buffer.put (++index, (byte) value);
      }
  
    return buffer;
  }

  public static final long getLong (ByteBuffer buffer)
  {
    checkRemainingForRead (buffer, 8);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        return ((buffer.get() & 0xff)
                + (((buffer.get() & 0xff)) << 8)
                + (((buffer.get() & 0xff)) << 16)
                + (((buffer.get() & 0xffL)) << 24)
                + (((buffer.get() & 0xffL)) << 32)
                + (((buffer.get() & 0xffL)) << 40)
                + (((buffer.get() & 0xffL)) << 48)
                + (((long) buffer.get()) << 56));
      }

    return ((((long) buffer.get()) << 56)
            + ((buffer.get() & 0xffL) << 48)
            + ((buffer.get() & 0xffL) << 40)
            + ((buffer.get() & 0xffL) << 32)
            + ((buffer.get() & 0xffL) << 24)
            + ((buffer.get() & 0xff) << 16)
            + ((buffer.get() & 0xff) << 8)
            + (buffer.get() & 0xff));
  }
  
  public static final ByteBuffer putLong (ByteBuffer buffer, long value)
  {
    checkRemainingForWrite (buffer, 8);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put ((byte) value);
        buffer.put ((byte) (value >> 8));
        buffer.put ((byte) (value >> 16));
        buffer.put ((byte) (value >> 24));
        buffer.put ((byte) (value >> 32));
        buffer.put ((byte) (value >> 40));
        buffer.put ((byte) (value >> 48));
        buffer.put ((byte) (value >> 56));
      }
    else
      {
        buffer.put ((byte) (value >> 56));
        buffer.put ((byte) (value >> 48));
        buffer.put ((byte) (value >> 40));
        buffer.put ((byte) (value >> 32));
        buffer.put ((byte) (value >> 24));
        buffer.put ((byte) (value >> 16));
        buffer.put ((byte) (value >> 8));
        buffer.put ((byte) value);
      }
    
    return buffer;
  }
  
  public static final long getLong (ByteBuffer buffer, int index)
  {
    checkAvailableForRead (buffer, index, 8);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        return ((buffer.get (index) & 0xff)
                + ((buffer.get (++index) & 0xff) << 8)
                + ((buffer.get (++index) & 0xff) << 16)
                + ((buffer.get (++index) & 0xffL) << 24)
                + ((buffer.get (++index) & 0xffL) << 32)
                + ((buffer.get (++index) & 0xffL) << 40)
                + ((buffer.get (++index) & 0xffL) << 48)
                + (((long) buffer.get (++index)) << 56));
      }

    return ((((long) buffer.get (index)) << 56)
            + ((buffer.get (++index) & 0xffL) << 48)
            + ((buffer.get (++index) & 0xffL) << 40)
            + ((buffer.get (++index) & 0xffL) << 32)
            + ((buffer.get (++index) & 0xffL) << 24)
            + ((buffer.get (++index) & 0xff) << 16)
            + ((buffer.get (++index) & 0xff) << 8)
            + (buffer.get (++index) & 0xff));
  }
  
  public static final ByteBuffer putLong (ByteBuffer buffer, int index,
                                          long value)
  {
    checkAvailableForWrite (buffer, index, 8);

    if (buffer.order() == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put (index, (byte) value);
        buffer.put (++index, (byte) (value >> 8));
        buffer.put (++index, (byte) (value >> 16));
        buffer.put (++index, (byte) (value >> 24));
        buffer.put (++index, (byte) (value >> 32));
        buffer.put (++index, (byte) (value >> 40));
        buffer.put (++index, (byte) (value >> 48));
        buffer.put (++index, (byte) (value >> 56));
      }
    else
      {
        buffer.put (index, (byte) (value >> 56));
        buffer.put (++index, (byte) (value >> 48));
        buffer.put (++index, (byte) (value >> 40));
        buffer.put (++index, (byte) (value >> 32));
        buffer.put (++index, (byte) (value >> 24));
        buffer.put (++index, (byte) (value >> 16));
        buffer.put (++index, (byte) (value >> 8));
        buffer.put (++index, (byte) value);
      }
    
    return buffer;
  }

  public static final float getFloat (ByteBuffer buffer)
  {
    return Float.intBitsToFloat (getInt (buffer));
  }
  
  public static final ByteBuffer putFloat (ByteBuffer buffer, float value)
  {
    return putInt (buffer, Float.floatToRawIntBits (value));
  }
  
  public static final float getFloat (ByteBuffer buffer, int index)
  {
    return Float.intBitsToFloat (getInt (buffer, index));
  }

  public static final ByteBuffer putFloat (ByteBuffer buffer, int index,
                                           float value)
  {
    return putInt (buffer, index, Float.floatToRawIntBits (value));
  }

  public static final double getDouble (ByteBuffer buffer)
  {
    return Double.longBitsToDouble (getLong (buffer));
  }

  public static final ByteBuffer putDouble (ByteBuffer buffer, double value)
  {
    return putLong (buffer, Double.doubleToLongBits (value));
  }
  
  public static final double getDouble (ByteBuffer buffer, int index)
  {
    return Double.longBitsToDouble (getLong (buffer, index));
  }
  
  public static final ByteBuffer putDouble (ByteBuffer buffer, int index,
                                            double value)
  {
    return putLong (buffer, index, Double.doubleToLongBits (value));
  }

} // ByteBufferHelper
