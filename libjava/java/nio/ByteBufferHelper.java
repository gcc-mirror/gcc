/* ByteBufferImpl.java -- 
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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
  private static void checkRemainingForRead (ByteBuffer buffer, int bytes)
  {
    if (buffer.remaining() < bytes)
      throw new BufferUnderflowException();
  }
  
  private static void checkRemainingForWrite (ByteBuffer buffer, int bytes)
  {
    if (buffer.remaining() < bytes)
      throw new BufferOverflowException();
  }

  private static void checkAvailableForRead (ByteBuffer buffer,
					     int index, int bytes)
  {
    if (buffer.limit() < (index + bytes))
      throw new BufferUnderflowException();
  }
  
  private static void checkAvailableForWrite (ByteBuffer buffer,
					      int index, int bytes)
  {
    if (buffer.limit() < (index + bytes))
      throw new BufferOverflowException();
  }
  
  public static char getChar (ByteBuffer buffer, ByteOrder order)
  {
    return (char) getShort (buffer, order);
  }
  
  public static void putChar (ByteBuffer buffer, char value, ByteOrder order)
  {
    putShort (buffer, (short) value, order);
  }
  
  public static char getChar (ByteBuffer buffer, int index, ByteOrder order)
  {
    return (char) getShort (buffer, index, order);
  }
  
  public static void putChar (ByteBuffer buffer, int index,
			      char value, ByteOrder order)
  {
    putShort (buffer, index, (short) value, order);
  }

  public static short getShort (ByteBuffer buffer, ByteOrder order)
  {
    checkRemainingForRead (buffer, 2);

    if (order == ByteOrder.LITTLE_ENDIAN)
      {
        return (short) ((buffer.get() & 0xff)
                        + (buffer.get() << 8));
      }

    return (short) ((buffer.get() << 8)
                    + (buffer.get() & 0xff));
  }
  
  public static void putShort (ByteBuffer buffer, short value, ByteOrder order)
  {
    checkRemainingForWrite (buffer, 2);

    if (order == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put ((byte) value);
        buffer.put ((byte) (value >> 8));
      }
    else
      {
        buffer.put ((byte) (value >> 8));
        buffer.put ((byte) value);
      }
  }
  
  public static short getShort (ByteBuffer buffer,
				      int index, ByteOrder order)
  {
    checkAvailableForRead (buffer, index, 2);

    if (order == ByteOrder.LITTLE_ENDIAN)
      {
        return (short) ((buffer.get (index) & 0xff)
                        + (buffer.get (++index) << 8));
      }

    return (short) ((buffer.get (index) << 8)
                    + (buffer.get (++index) & 0xff));
  }
  
  public static void putShort (ByteBuffer buffer, int index,
			       short value, ByteOrder order)
  {
    checkAvailableForWrite (buffer, index, 2);

    if (order == ByteOrder.LITTLE_ENDIAN)
      {
        buffer.put (index, (byte) value);
        buffer.put (++index, (byte) (value >> 8));
      }
    else
      {
        buffer.put (index, (byte) (value >> 8));
        buffer.put (++index, (byte) value);
      }
  }

  public static int getInt (ByteBuffer buffer, ByteOrder order)
  {
    checkRemainingForRead (buffer, 4);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  
  public static void putInt (ByteBuffer buffer, int value, ByteOrder order)
  {
    checkRemainingForWrite (buffer, 4);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  }
  
  public static int getInt (ByteBuffer buffer, int index, ByteOrder order)
  {
    checkAvailableForRead (buffer, index, 4);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  
  public static void putInt (ByteBuffer buffer, int index,
				   int value, ByteOrder order)
  {
    checkAvailableForWrite (buffer, index, 4);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  }

  public static long getLong (ByteBuffer buffer, ByteOrder order)
  {
    checkRemainingForRead (buffer, 8);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  
  public static void putLong (ByteBuffer buffer, long value, ByteOrder order)
  {
    checkRemainingForWrite (buffer, 8);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  }
  
  public static long getLong (ByteBuffer buffer, int index, ByteOrder order)
  {
    checkAvailableForRead (buffer, index, 8);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  
  public static void putLong (ByteBuffer buffer, int index,
				    long value, ByteOrder order)
  {
    checkAvailableForWrite (buffer, index, 8);

    if (order == ByteOrder.LITTLE_ENDIAN)
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
  }

  public static float getFloat (ByteBuffer buffer, ByteOrder order)
  {
    return Float.intBitsToFloat (getInt (buffer, order));
  }
  
  public static void putFloat (ByteBuffer buffer, float value, ByteOrder order)
  {
    putInt (buffer, Float.floatToRawIntBits (value), order);
  }
  
  public static float getFloat (ByteBuffer buffer, int index, ByteOrder order)
  {
    return Float.intBitsToFloat (getInt (buffer, index, order));
  }

  public static void putFloat (ByteBuffer buffer, int index,
				     float value, ByteOrder order)
  {
    putInt (buffer, index, Float.floatToRawIntBits (value), order);
  }

  public static double getDouble (ByteBuffer buffer, ByteOrder order)
  {
    return Double.longBitsToDouble (getLong (buffer, order));
  }

  public static void putDouble (ByteBuffer buffer, double value, ByteOrder order)
  {
    putLong (buffer, Double.doubleToLongBits (value), order);
  }
  
  public static double getDouble (ByteBuffer buffer, int index, ByteOrder order)
  {
    return Double.longBitsToDouble (getLong (buffer, index, order));
  }
  
  public static void putDouble (ByteBuffer buffer, int index,
				double value, ByteOrder order)
  {
    putLong (buffer, index, Double.doubleToLongBits (value), order);
  }
} // ByteBufferHelper
