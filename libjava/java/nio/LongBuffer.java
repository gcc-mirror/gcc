/* LongBuffer.java -- 
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

import gnu.java.nio.LongBufferImpl;

public abstract class LongBuffer extends Buffer
{
  private ByteOrder endian = ByteOrder.BIG_ENDIAN;
  protected long [] backing_buffer;

  public static LongBuffer allocateDirect(int capacity)
  {
    return new LongBufferImpl(capacity, 0, capacity);
  }

  public static LongBuffer allocate(int capacity)
  {
    return new LongBufferImpl(capacity, 0, capacity);
  }

  final public static LongBuffer wrap(long[] array, int offset, int length)
  {
    return new LongBufferImpl (array, offset, length);
  }

  final public static LongBuffer wrap(String a)
  {
    int len = a.length();
    long[] buffer = new long[len];

    for (int i=0;i<len;i++)
      {
        buffer[i] = (long) a.charAt(i);
      }

    return wrap(buffer, 0, len);
  }

  final public static LongBuffer wrap(long[] array)
  {
    return wrap(array, 0, array.length);
  }

  final public LongBuffer get(long[] dst, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      {
        dst[i] = get();
      }

    return this;
  }

  final public LongBuffer get(long[] dst)
  {
    return get(dst, 0, dst.length);
  }

  final public LongBuffer put(LongBuffer src)
  {
    while (src.hasRemaining())
      put(src.get());

    return this;
  }

  final public LongBuffer put(long[] src, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      put(src[i]);

    return this;
  }

  public final LongBuffer put(long[] src)
  {
    return put(src, 0, src.length);
  }

  public final boolean hasArray()
  {
    return (backing_buffer != null);
  }

  public final long[] array()
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
    if (obj instanceof LongBuffer)
      {
        return compareTo(obj) == 0;
      }

    return false;
  }

  public int compareTo(Object ob)
  {
    LongBuffer a = (LongBuffer) ob;

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

  public final LongBuffer order(ByteOrder bo)
  {
    endian = bo;
    return this;
  }

  public abstract long get();
  public abstract java.nio. LongBuffer put(long b);
  public abstract long get(int index);
  public abstract java.nio. LongBuffer put(int index, long b);
  public abstract LongBuffer compact();
  public abstract boolean isDirect();
  public abstract LongBuffer slice();
  public abstract LongBuffer duplicate();
  public abstract LongBuffer asReadOnlyBuffer();
  public abstract ShortBuffer asShortBuffer();
  public abstract CharBuffer asCharBuffer();
  public abstract IntBuffer asIntBuffer();
  public abstract LongBuffer asLongBuffer();
  public abstract FloatBuffer asFloatBuffer();
  public abstract DoubleBuffer asDoubleBuffer();
  public abstract char getChar();
  public abstract LongBuffer putChar(char value);
  public abstract char getChar(int index);
  public abstract LongBuffer putChar(int index, char value);
  public abstract short getShort();
  public abstract LongBuffer putShort(short value);
  public abstract short getShort(int index);
  public abstract LongBuffer putShort(int index, short value);
  public abstract int getInt();
  public abstract LongBuffer putInt(int value);
  public abstract int getInt(int index);
  public abstract LongBuffer putInt(int index, int value);
  public abstract long getLong();
  public abstract LongBuffer putLong(long value);
  public abstract long getLong(int index);
  public abstract LongBuffer putLong(int index, long value);
  public abstract float getFloat();
  public abstract LongBuffer putFloat(float value);
  public abstract float getFloat(int index);
  public abstract LongBuffer putFloat(int index, float value);
  public abstract double getDouble();
  public abstract LongBuffer putDouble(double value);
  public abstract double getDouble(int index);
  public abstract LongBuffer putDouble(int index, double value);
}
