/* ShortBuffer.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import gnu.java.nio.ShortBufferImpl;

public abstract class ShortBuffer extends Buffer
{
  private ByteOrder endian = ByteOrder.BIG_ENDIAN;
  protected short [] backing_buffer;

  public static ShortBuffer allocateDirect(int capacity)
  {
    return new ShortBufferImpl(capacity, 0, capacity);
  }

  public static ShortBuffer allocate(int capacity)
  {
    return new ShortBufferImpl(capacity, 0, capacity);
  }

  final public static ShortBuffer wrap(short[] array, int offset, int length)
  {
    return new ShortBufferImpl(array, offset, length);
  }

  final public static ShortBuffer wrap(String a)
  {
    int len = a.length();
    short[] buffer = new short[len];

    for (int i=0;i<len;i++)
      {
        buffer[i] = (short) a.charAt(i);
      }

    return wrap(buffer, 0, len);
  }

  final public static ShortBuffer wrap(short[] array)
  {
    return wrap(array, 0, array.length);
  }

  final public ShortBuffer get(short[] dst, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      {
        dst[i] = get();
      }

    return this;
  }

  final public ShortBuffer get(short[] dst)
  {
    return get(dst, 0, dst.length);
  }

  final public ShortBuffer put(ShortBuffer src)
  {
    while (src.hasRemaining())
      put(src.get());

    return this;
  }

  final public ShortBuffer put(short[] src, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      put(src[i]);

    return this;
  }

  public final ShortBuffer put(short[] src)
  {
    return put(src, 0, src.length);
  }

  public final boolean hasArray()
  {
    return (backing_buffer != null);
  }

  public final short[] array()
  {
    return backing_buffer;
  }

  public final int arrayOffset()
  {
    return 0;
  }

  public int hashCode()
  {
    return super.hashCode();
  }

  public boolean equals(Object obj)
  {
    if (obj instanceof ShortBuffer)
      {
        return compareTo(obj) == 0;
      }

    return false;
  }

  public int compareTo(Object ob)
  {
    ShortBuffer a = (ShortBuffer) ob;

    if (a.remaining() != remaining())
      return 1;

    if (! hasArray() ||
        ! a.hasArray())
      {
        return 1;
      }

    int r = remaining();
    int i1 = position ();
    int i2 = a.position ();

    for (int i=0;i<r;i++)
      {
        int t = (int) (get(i1)- a.get(i2));

        if (t != 0)
          {
            return (int) t;
          }
      }

    return 0;
  }

  public final ByteOrder order()
  {
    return endian;
  }

  public final ShortBuffer order(ByteOrder bo)
  {
    endian = bo;
    return this;
  }

  public abstract short get();
  public abstract java.nio. ShortBuffer put(short b);
  public abstract short get(int index);
  public abstract java.nio. ShortBuffer put(int index, short b);
  public abstract ShortBuffer compact();
  public abstract boolean isDirect();
  public abstract ShortBuffer slice();
  public abstract ShortBuffer duplicate();
  public abstract ShortBuffer asReadOnlyBuffer();
  public abstract ShortBuffer asShortBuffer();
  public abstract CharBuffer asCharBuffer();
  public abstract IntBuffer asIntBuffer();
  public abstract LongBuffer asLongBuffer();
  public abstract FloatBuffer asFloatBuffer();
  public abstract DoubleBuffer asDoubleBuffer();
  public abstract char getChar();
  public abstract ShortBuffer putChar(char value);
  public abstract char getChar(int index);
  public abstract ShortBuffer putChar(int index, char value);
  public abstract short getShort();
  public abstract ShortBuffer putShort(short value);
  public abstract short getShort(int index);
  public abstract ShortBuffer putShort(int index, short value);
  public abstract int getInt();
  public abstract ShortBuffer putInt(int value);
  public abstract int getInt(int index);
  public abstract ShortBuffer putInt(int index, int value);
  public abstract long getLong();
  public abstract ShortBuffer putLong(long value);
  public abstract long getLong(int index);
  public abstract ShortBuffer putLong(int index, long value);
  public abstract float getFloat();
  public abstract ShortBuffer putFloat(float value);
  public abstract float getFloat(int index);
  public abstract ShortBuffer putFloat(int index, float value);
  public abstract double getDouble();
  public abstract ShortBuffer putDouble(double value);
  public abstract double getDouble(int index);
  public abstract ShortBuffer putDouble(int index, double value);
}
