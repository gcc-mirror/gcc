/* ByteBufferImpl.java -- 
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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
 * This is a Heap memory implementation
 */
final class ByteBufferImpl extends ByteBuffer
{
  private boolean readOnly;

  ByteBufferImpl (int capacity)
  {
    this (new byte [capacity], 0, capacity, capacity, 0, -1, false);
  }
  
  ByteBufferImpl (byte[] buffer, int offset, int capacity, int limit, int position, int mark, boolean readOnly)
  {
    super (buffer, offset, capacity, limit, position, mark);
    this.readOnly = readOnly;
  }
  
  public CharBuffer asCharBuffer ()
  {
    return new CharViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public ShortBuffer asShortBuffer ()
  {
    return new ShortViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public IntBuffer asIntBuffer ()
  {
    return new IntViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public LongBuffer asLongBuffer ()
  {
    return new LongViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public FloatBuffer asFloatBuffer ()
  {
    return new FloatViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public DoubleBuffer asDoubleBuffer ()
  {
    return new DoubleViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public boolean isReadOnly ()
  {
    return readOnly;
  }
  
  public ByteBuffer slice ()
  {
    return new ByteBufferImpl (backing_buffer, array_offset + position (), remaining (), remaining (), 0, -1, isReadOnly ());
  }
  
  public ByteBuffer duplicate ()
  {
    return new ByteBufferImpl (backing_buffer, array_offset, capacity (), limit (), position (), mark, isReadOnly ());
  }
  
  public ByteBuffer asReadOnlyBuffer ()
  {
    return new ByteBufferImpl (backing_buffer, array_offset, capacity (), limit (), position (), mark, true);
  }
  
  public ByteBuffer compact ()
  {
    int copied = 0;
    
    while (remaining () > 0)
      {
	put (copied, get ());
	copied++;
      }

    position (copied);
    return this;
  }
  
  public boolean isDirect ()
  {
    return false;
  }

  /**
   * Relative get method. Reads the next <code>byte</code> from the buffer.
   */
  final public byte get ()
  {
    byte result = backing_buffer [position ()];
    position (position () + 1);
    return result;
  }
  
  /**
   * Relative put method. Writes <code>value</code> to the next position
   * in the buffer.
   * 
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  final public ByteBuffer put (byte value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
	  	    
    backing_buffer [position ()] = value;
    position (position () + 1);
    return this;
  }
  
  /**
   * Absolute get method. Reads the <code>byte</code> at position
   * <code>index</code>.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  final public byte get (int index)
  {
    return backing_buffer [index];
  }
  
  /**
   * Absolute put method. Writes <code>value</value> to position
   * <code>index</code> in the buffer.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  final public ByteBuffer put (int index, byte value)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    	    
    backing_buffer [index] = value;
    return this;
  }
  
  final public char getChar ()
  {
    return ByteBufferHelper.getChar (this);
  }
  
  final public ByteBuffer putChar (char value)
  {
    return ByteBufferHelper.putChar (this, value);
  }
  
  final public char getChar (int index)
  {
    return ByteBufferHelper.getChar (this, index);
  }
  
  final public ByteBuffer putChar (int index, char value)
  {
    return ByteBufferHelper.putChar (this, index, value);
  }

  final public short getShort ()
  {
    return ByteBufferHelper.getShort (this);
  }
  
  final public ByteBuffer putShort (short value)
  {
    return ByteBufferHelper.putShort (this, value);
  }
  
  final public short getShort (int index)
  {
    return ByteBufferHelper.getShort (this, index);
  }
  
  final public ByteBuffer putShort (int index, short value)
  {
    return ByteBufferHelper.putShort (this, index, value);
  }

  final public int getInt ()
  {
    return ByteBufferHelper.getInt (this);
  }
  
  final public ByteBuffer putInt (int value)
  {
    return ByteBufferHelper.putInt (this, value);
  }
  
  final public int getInt (int index)
  {
    return ByteBufferHelper.getInt (this, index);
  }
  
  final public ByteBuffer putInt (int index, int value)
  {
    return ByteBufferHelper.putInt (this, index, value);
  }

  final public long getLong ()
  {
    return ByteBufferHelper.getLong (this);
  }
  
  final public ByteBuffer putLong (long value)
  {
    return ByteBufferHelper.putLong (this, value);
  }
  
  final public long getLong (int index)
  {
    return ByteBufferHelper.getLong (this, index);
  }
  
  final public ByteBuffer putLong (int index, long value)
  {
    return ByteBufferHelper.putLong (this, index, value);
  }

  final public float getFloat ()
  {
    return ByteBufferHelper.getFloat (this);
  }
  
  final public ByteBuffer putFloat (float value)
  {
    return ByteBufferHelper.putFloat (this, value);
  }
  
  final public float getFloat (int index)
  {
    return ByteBufferHelper.getFloat (this, index);
  }

  public final ByteBuffer putFloat (int index, float value)
  {
    return ByteBufferHelper.putFloat (this, index, value);
  }

  final public double getDouble ()
  {
    return ByteBufferHelper.getDouble (this);
  }

  final public ByteBuffer putDouble (double value)
  {
    return ByteBufferHelper.putDouble (this, value);
  }
  
  final public double getDouble (int index)
  {
    return ByteBufferHelper.getDouble (this, index);
  }
  
  final public ByteBuffer putDouble (int index, double value)
  {
    return ByteBufferHelper.putDouble (this, index, value);
  }
}
