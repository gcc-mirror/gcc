/* ByteBufferImpl.java -- 
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
import java.nio.CharBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ReadOnlyBufferException;
import java.nio.ShortBuffer;

/**
 * This is a Heap memory implementation
 */
public final class ByteBufferImpl extends ByteBuffer
{
  private boolean readOnly;
  
  public ByteBufferImpl (int cap, int off, int lim)
  {
    super (cap, lim, off, 0);
    this.backing_buffer = new byte [cap];
    readOnly = false;
  }

  public ByteBufferImpl (byte[] array, int offset, int length)
  {
    super (array.length, length, offset, 0);
    this.backing_buffer = array;
    readOnly = false;
  }

  public ByteBufferImpl (ByteBufferImpl copy)
  {
    super (copy.capacity (), copy.limit (), copy.position (), 0);
    backing_buffer = copy.backing_buffer;
    readOnly = copy.isReadOnly ();
  }

  void inc_pos (int toAdd)
  {
    position (position () + toAdd);
  }

  private static native byte[] nio_cast(byte[]copy);
  private static native byte[] nio_cast(char[]copy);
  private static native byte[] nio_cast(short[]copy);
  private static native byte[] nio_cast(long[]copy);
  private static native byte[] nio_cast(int[]copy);
  private static native byte[] nio_cast(float[]copy);
  private static native byte[] nio_cast(double[]copy);

  ByteBufferImpl (byte[] copy)
  {
    super (copy.length, copy.length, 0, 0);
    this.backing_buffer = copy != null ? nio_cast (copy) : null;
    readOnly = false;
  }

  private static native byte nio_get_Byte (ByteBufferImpl b, int index, int limit);
  
  private static native void nio_put_Byte (ByteBufferImpl b, int index, int limit, byte value);
  
  public ByteBuffer asByteBuffer ()
  {
    ByteBufferImpl res = new ByteBufferImpl (backing_buffer);
    res.limit ((limit () * 1) / 1);
    return res;
  }

  ByteBufferImpl (char[] copy)
  {
    super (copy.length * 2, copy.length * 2, 0, 0);
    this.backing_buffer = copy != null ? nio_cast (copy) : null;
    readOnly = false;
  }

  private static native char nio_get_Char (ByteBufferImpl b, int index, int limit);

  private static native void nio_put_Char (ByteBufferImpl b, int index, int limit, char value);

  public CharBuffer asCharBuffer ()
  {
    CharBufferImpl res = new CharBufferImpl (backing_buffer);
    res.limit ((limit () * 2) / 1);
    return res;
  }

  ByteBufferImpl (short[] copy)
  {
    super (copy.length, copy.length, 0, 0);
    this.backing_buffer = copy != null ? nio_cast (copy) : null;
    readOnly = false;
  }
  
  private static native short nio_get_Short (ByteBufferImpl b, int index, int limit);
  
  private static native void nio_put_Short (ByteBufferImpl b, int index, int limit, short value);
  
  public ShortBuffer asShortBuffer ()
  {
    ShortBufferImpl res = new ShortBufferImpl (backing_buffer);
    res.limit ((limit () * 2) / 1);
    return res;
  }

  ByteBufferImpl (int[] copy)
  {
    super (copy.length * 4, copy.length * 4, 0, 0);
    this.backing_buffer = copy != null ? nio_cast(copy) : null;
    readOnly = false;
  }
  
  private static native int nio_get_Int (ByteBufferImpl b, int index, int limit);
  
  private static native void nio_put_Int (ByteBufferImpl b, int index, int limit, int value);
  
  public IntBuffer asIntBuffer ()
  {
    IntBufferImpl res = new IntBufferImpl (backing_buffer);
    res.limit ((limit() * 4) / 1);
    return res;
  }

  ByteBufferImpl (long[] copy)
  {
    super (copy.length * 8, copy.length * 8, 0, 0);
    this.backing_buffer = copy != null ? nio_cast (copy) : null;
    readOnly = false;
  }
  
  private static native long nio_get_Long (ByteBufferImpl b, int index, int limit);
  
  private static native void nio_put_Long (ByteBufferImpl b, int index, int limit, long value);
  
  public LongBuffer asLongBuffer ()
  {
    LongBufferImpl res = new LongBufferImpl (backing_buffer);
    res.limit ((limit() * 8) / 1);
    return res;
  }

  ByteBufferImpl (float[] copy)
  {
    super (copy.length * 4, copy.length * 4, 0, 0);
    this.backing_buffer = copy != null ? nio_cast (copy) : null;
    readOnly = false;
  }
  
  private static native float nio_get_Float (ByteBufferImpl b, int index, int limit);
  
  private static native void nio_put_Float (ByteBufferImpl b, int index, int limit, float value);
  
  public FloatBuffer asFloatBuffer ()
  {
    FloatBufferImpl res = new FloatBufferImpl (backing_buffer);
    res.limit ((limit() * 4) / 1);
    return res;
  }

  ByteBufferImpl (double[] copy)
  {
    super (copy.length * 8, copy.length * 8, 0, 0);
    this.backing_buffer = copy != null ? nio_cast (copy) : null;
    readOnly = false;
  }
  
  private static native double nio_get_Double (ByteBufferImpl b, int index, int limit);
  
  private static native void nio_put_Double (ByteBufferImpl b, int index, int limit, double value);
  
  public DoubleBuffer asDoubleBuffer ()
  {
    DoubleBufferImpl res = new DoubleBufferImpl (backing_buffer);
    res.limit ((limit () * 8) / 1);
    return res;
  }

  public boolean isReadOnly()
  {
    return readOnly;
  }
  
  public ByteBuffer slice()
  {
    return new ByteBufferImpl(this);
  }

  public ByteBuffer duplicate()
  {
    return new ByteBufferImpl(this);
  }

  public ByteBuffer asReadOnlyBuffer()
  {
    ByteBufferImpl a = new ByteBufferImpl(this);
    a.readOnly = true;
    return a;
  }

  public ByteBuffer compact()
  {
    return this;
  }

  public boolean isDirect()
  {
    return false;
  }
  
  final public byte get()
  {
    byte e = backing_buffer[position()];
    position(position()+1);
    return e;
  }
  
  final public ByteBuffer put(byte b)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    backing_buffer[position()] = b;
    position(position()+1);
    return this;
  }
  
  final public byte get(int index)
  {
    return backing_buffer[index];
  }
  
  final public ByteBuffer put(int index, byte b)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    backing_buffer[index] = b;
    return this;
  }
  
  final public char getChar ()
  {
    char a = nio_get_Char (this, position (), limit ());
    inc_pos (2);
    return a;
  }
  
  final public ByteBuffer putChar (char value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Char (this, position (), limit (), value);
    inc_pos (2);
    return this;
  }
  
  final public char getChar (int index)
  {
    char a = nio_get_Char (this, index, limit ());
    return a;
  }
  
  final public ByteBuffer putChar (int index, char value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Char (this, index, limit (), value);
    return this;
  }

  final public short getShort ()
  {
    short a = nio_get_Short (this, position (), limit ());
    inc_pos (2);
    return a;
  }
  
  final public ByteBuffer putShort (short value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Short (this, position (), limit(), value);
    inc_pos (2);
    return this;
  }
  
  final public short getShort (int index)
  {
    short a = nio_get_Short (this, index, limit ());
    return a;
  }
  
  final public ByteBuffer putShort (int index, short value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Short (this, index, limit (), value);
    return this;
  }

  final public int getInt ()
  {
    int a = nio_get_Int (this, position (), limit ());
    inc_pos (4);
    return a;
  }
  
  final public ByteBuffer putInt (int value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Int (this, position (), limit , value);
    inc_pos (4);
    return this;
  }
  
  final public int getInt (int index)
  {
    int a = nio_get_Int (this, index, limit ());
    return a;
  }
  
  final public ByteBuffer putInt (int index, int value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Int(this, index, limit (), value);
    return this;
  }

  final public long getLong ()
  {
    long a = nio_get_Long (this, position (), limit ());
    inc_pos (8);
    return a;
  }
  
  final public ByteBuffer putLong (long value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Long (this, position (), limit (), value);
    inc_pos (8);
    return this;
  }
  
  final public long getLong (int index)
  {
    long a = nio_get_Long (this, index, limit ());
    return a;
  }
  
  final public ByteBuffer putLong (int index, long value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Long (this, index, limit (), value);
    return this;
  }

  final public float getFloat ()
  {
    float a = nio_get_Float (this, position (), limit ());
    inc_pos (4);
    return a;
  }
  
  final public ByteBuffer putFloat (float value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Float (this, position (), limit (), value);
    inc_pos (4);
    return this;
  }
  
  final public float getFloat (int index)
  {
    float a = nio_get_Float (this, index, limit ());
    return a;
  }

  final public ByteBuffer putFloat (int index, float value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Float (this, index, limit(), value);
    return this;
  }

  final public double getDouble ()
  {
    double a = nio_get_Double (this, position (), limit ());
    inc_pos (8);
    return a;
  }

  final public ByteBuffer putDouble (double value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Double (this, position(), limit (), value);
    inc_pos (8);
    return this;
  }
  
  final public double getDouble (int index)
  {
    return nio_get_Double (this, index, limit ());
  }
  
  final public ByteBuffer putDouble (int index, double value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    nio_put_Double (this, index, limit (), value);
    return this;
  }
}
