/* CharBufferImpl.java -- 
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

package gnu.java.nio;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.CharBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;

public final class CharBufferImpl extends CharBuffer
{
  private boolean ro;

  private ByteOrder endian = ByteOrder.BIG_ENDIAN;
  
  public CharBufferImpl(int cap, int off, int lim)
  {
    this.backing_buffer = new char[cap];
    this.cap = cap;
    this.limit(lim);
    this.position(off);
  }
  
  public CharBufferImpl(char[] array, int off, int lim)
  {
    this.backing_buffer = array;
    this.cap = array.length;
    this.limit(lim);
    this.position(off);
  }
  
  public CharBufferImpl (CharBufferImpl copy)
  {
    backing_buffer = copy.backing_buffer;
    ro = copy.ro;
    limit (copy.limit());
    position (copy.position ());
  }
  
  void inc_pos (int a)
  {
    position (position () + a);
  }

  CharBufferImpl(byte[] copy) { this.backing_buffer = copy != null ? nio_cast(copy) : null; }
  private static native byte nio_get_Byte(CharBufferImpl b, int index, int limit);
  private static native void nio_put_Byte(CharBufferImpl b, int index, int limit, byte value);
  public ByteBuffer asByteBuffer() { ByteBufferImpl res = new ByteBufferImpl(backing_buffer); res.limit((limit()*1)/2); return res; }

  CharBufferImpl(char[] copy) { this.backing_buffer = copy != null ? nio_cast(copy) : null; }
  private static native char nio_get_Char(CharBufferImpl b, int index, int limit);
  private static native void nio_put_Char(CharBufferImpl b, int index, int limit, char value);
  public CharBuffer asCharBuffer() { CharBufferImpl res = new CharBufferImpl(backing_buffer); res.limit((limit()*2)/2); return res; }

  CharBufferImpl(short[] copy) { this.backing_buffer = copy != null ? nio_cast(copy) : null; }
  private static native short nio_get_Short(CharBufferImpl b, int index, int limit);
  private static native void nio_put_Short(CharBufferImpl b, int index, int limit, short value);
  public ShortBuffer asShortBuffer() { ShortBufferImpl res = new ShortBufferImpl(backing_buffer); res.limit((limit()*2)/2); return res; }

  CharBufferImpl(int[] copy) { this.backing_buffer = copy != null ? nio_cast(copy) : null; }
  private static native int nio_get_Int(CharBufferImpl b, int index, int limit);
  private static native void nio_put_Int(CharBufferImpl b, int index, int limit, int value);
  public IntBuffer asIntBuffer() { IntBufferImpl res = new IntBufferImpl(backing_buffer); res.limit((limit()*4)/2); return res; }

  CharBufferImpl(long[] copy) { this.backing_buffer = copy != null ? nio_cast(copy) : null; }
  private static native long nio_get_Long(CharBufferImpl b, int index, int limit);
  private static native void nio_put_Long(CharBufferImpl b, int index, int limit, long value);
  public LongBuffer asLongBuffer() { LongBufferImpl res = new LongBufferImpl(backing_buffer); res.limit((limit()*8)/2); return res; }

  CharBufferImpl(float[] copy) { this.backing_buffer = copy != null ? nio_cast(copy) : null; }
  private static native float nio_get_Float(CharBufferImpl b, int index, int limit);
  private static native void nio_put_Float(CharBufferImpl b, int index, int limit, float value);
  public FloatBuffer asFloatBuffer() { FloatBufferImpl res = new FloatBufferImpl(backing_buffer); res.limit((limit()*4)/2); return res; }

  CharBufferImpl(double[] copy) { this.backing_buffer = copy != null ? nio_cast(copy) : null; }
  private static native double nio_get_Double(CharBufferImpl b, int index, int limit);
  private static native void nio_put_Double(CharBufferImpl b, int index, int limit, double value);
  public DoubleBuffer asDoubleBuffer() { DoubleBufferImpl res = new DoubleBufferImpl(backing_buffer); res.limit((limit()*8)/2); return res; }
  
  private static native char[] nio_cast(byte[]copy);
  private static native char[] nio_cast(char[]copy);
  private static native char[] nio_cast(short[]copy);
  private static native char[] nio_cast(long[]copy);
  private static native char[] nio_cast(int[]copy);
  private static native char[] nio_cast(float[]copy);
  private static native char[] nio_cast(double[]copy);

  
  public boolean isReadOnly()
  {
    return ro;
  }
  
  public CharBuffer slice()
  {
    CharBufferImpl buffer = new CharBufferImpl (this);
    buffer.array_offset = position ();
    return buffer;
  }
  
  public CharBuffer duplicate()
  {
    return new CharBufferImpl(this);
  }
  
  public CharBuffer asReadOnlyBuffer()
  {
    CharBufferImpl a = new CharBufferImpl(this);
    a.ro = true;
    return a;
  }
  
  public CharBuffer compact()
  {
    return this;
  }
  
  public boolean isDirect()
  {
    return backing_buffer != null;
  }

  final public CharSequence subSequence (int start, int end)
  {
    if (start < 0 ||
        end > length () ||
        start > end)
      throw new IndexOutOfBoundsException ();

    // No support for direct buffers yet.
    // assert array () != null;
    return new CharBufferImpl (array (), position () + start,
                               position () + end);
  }
  
  final public char get()
  {
    char e = backing_buffer[position()];
    position(position()+1);
    return e;
  }
  
  final public CharBuffer put(char b)
  {
    backing_buffer[position()] = b;
    position(position()+1);
    return this;
  }
  
  final public char getChar() { return get(); } final public CharBuffer putChar(char value) { return put(value); } final public char getChar(int index) { return get(index); } final public CharBuffer putChar(int index, char value) { return put(index, value); };
  final public short getShort() { short a = nio_get_Short(this, position(), limit()); inc_pos(2); return a; } final public CharBuffer putShort(short value) { nio_put_Short(this, position(), limit(), value); inc_pos(2); return this; } final public short getShort(int index) { short a = nio_get_Short(this, index, limit()); return a; } final public CharBuffer putShort(int index, short value) { nio_put_Short(this, index, limit(), value); return this; };
  final public int getInt() { int a = nio_get_Int(this, position(), limit()); inc_pos(4); return a; } final public CharBuffer putInt(int value) { nio_put_Int(this, position(), limit(), value); inc_pos(4); return this; } final public int getInt(int index) { int a = nio_get_Int(this, index, limit()); return a; } final public CharBuffer putInt(int index, int value) { nio_put_Int(this, index, limit(), value); return this; };
  final public long getLong() { long a = nio_get_Long(this, position(), limit()); inc_pos(8); return a; } final public CharBuffer putLong(long value) { nio_put_Long(this, position(), limit(), value); inc_pos(8); return this; } final public long getLong(int index) { long a = nio_get_Long(this, index, limit()); return a; } final public CharBuffer putLong(int index, long value) { nio_put_Long(this, index, limit(), value); return this; };
  final public float getFloat() { float a = nio_get_Float(this, position(), limit()); inc_pos(4); return a; } final public CharBuffer putFloat(float value) { nio_put_Float(this, position(), limit(), value); inc_pos(4); return this; } final public float getFloat(int index) { float a = nio_get_Float(this, index, limit()); return a; } final public CharBuffer putFloat(int index, float value) { nio_put_Float(this, index, limit(), value); return this; };
  final public double getDouble() { double a = nio_get_Double(this, position(), limit()); inc_pos(8); return a; } final public CharBuffer putDouble(double value) { nio_put_Double(this, position(), limit(), value); inc_pos(8); return this; } final public double getDouble(int index) { double a = nio_get_Double(this, index, limit()); return a; } final public CharBuffer putDouble(int index, double value) { nio_put_Double(this, index, limit(), value); return this; };

  final public char get(int index)
  {
    return backing_buffer[index];
  }
  
  final public CharBuffer put(int index, char b)
  {
    backing_buffer[index] = b;
    return this;
  }


  public final ByteOrder order()
  {
    return endian;
  }
}
