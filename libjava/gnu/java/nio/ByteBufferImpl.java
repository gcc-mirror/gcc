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

  ByteBufferImpl (int capacity)
  {
    this (new byte [capacity], 0, capacity, capacity, 0, -1, false);
  }
  
  ByteBufferImpl (byte[] buffer, int offset, int capacity, int limit, int position, int mark, boolean readOnly)
  {
    super (buffer, offset, capacity, limit, position, mark);
    this.readOnly = readOnly;
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
    // FIXME: this handles big endian only
    return (char) (((get () & 0xff) << 8) + (get () & 0xff));
  }
  
  final public ByteBuffer putChar (char value)
  {
    // FIXME: this handles big endian only
    put ((byte) ((((int) value) & 0xff00) >> 8));
    put ((byte) (((int) value) & 0x00ff));
    return this;
  }
  
  final public char getChar (int index)
  {
    // FIXME: this handles big endian only
    return (char) (((get (index) & 0xff) << 8) + (get (index + 1) & 0xff));
  }
  
  final public ByteBuffer putChar (int index, char value)
  {
    // FIXME: this handles big endian only
    put (index, (byte) ((((int) value) & 0xff00) >> 8));
    put (index + 1, (byte) (((int) value) & 0x00ff));
    return this;
  }

  final public short getShort ()
  {
    // FIXME: this handles big endian only
    return (short) (((get () & 0xff) << 8) + (get () & 0xff));
  }
  
  final public ByteBuffer putShort (short value)
  {
    // FIXME: this handles big endian only
    put ((byte) ((((int) value) & 0xff00) >> 8));
    put ((byte) (((int) value) & 0x00ff));
    return this;
  }
  
  final public short getShort (int index)
  {
    // FIXME: this handles big endian only
    return (short) (((get (index) & 0xff) << 8) + (get (index + 1) & 0xff));
  }
  
  final public ByteBuffer putShort (int index, short value)
  {
    // FIXME: this handles big endian only
    put (index, (byte) ((((int) value) & 0xff00) >> 8));
    put (index + 1, (byte) (((int) value) & 0x00ff));
    return this;
  }

  final public int getInt ()
  {
    // FIXME: this handles big endian only
    return (int) (((get () & 0xff) << 24)
                  + (get () & 0xff) << 16
                  + (get () & 0xff) << 8
                  + (get () & 0xff));
  }
  
  final public ByteBuffer putInt (int value)
  {
    // FIXME: this handles big endian only
    put ((byte) ((((int) value) & 0xff000000) >> 24));
    put ((byte) ((((int) value) & 0x00ff0000) >> 16));
    put ((byte) ((((int) value) & 0x0000ff00) >> 8));
    put ((byte) (((int) value) & 0x000000ff));
    return this;
  }
  
  final public int getInt (int index)
  {
    // FIXME: this handles big endian only
    return (int) (((get (index) & 0xff) << 24)
                  + (get (index + 1) & 0xff) << 16
                  + (get (index + 2) & 0xff) << 8
                  + (get (index + 3) & 0xff));
  }
  
  final public ByteBuffer putInt (int index, int value)
  {
    // FIXME: this handles big endian only
    put (index, (byte) ((((int) value) & 0xff000000) >> 24));
    put (index + 1, (byte) ((((int) value) & 0x00ff0000) >> 16));
    put (index + 2, (byte) ((((int) value) & 0x0000ff00) >> 8));
    put (index + 3, (byte) (((int) value) & 0x000000ff));
    return this;
  }

  final public long getLong ()
  {
    // FIXME: this handles big endian only
    return (long) (((get () & 0xff) << 56)
                   + (get () & 0xff) << 48
                   + (get () & 0xff) << 40
                   + (get () & 0xff) << 32
                   + (get () & 0xff) << 24
                   + (get () & 0xff) << 16
                   + (get () & 0xff) << 8
                   + (get () & 0xff));
  }
  
  final public ByteBuffer putLong (long value)
  {
    // FIXME: this handles big endian only
    put ((byte) ((((int) value) & 0xff00000000000000) >> 56));
    put ((byte) ((((int) value) & 0x00ff000000000000) >> 48));
    put ((byte) ((((int) value) & 0x0000ff0000000000) >> 40));
    put ((byte) ((((int) value) & 0x000000ff00000000) >> 32));
    put ((byte) ((((int) value) & 0x00000000ff000000) >> 24));
    put ((byte) ((((int) value) & 0x0000000000ff0000) >> 16));
    put ((byte) ((((int) value) & 0x000000000000ff00) >> 8));
    put ((byte) (((int) value) & 0x00000000000000ff));
    return this;
  }
  
  final public long getLong (int index)
  {
    // FIXME: this handles big endian only
    return (long) (((get (index) & 0xff) << 56)
                   + (get (index + 1) & 0xff) << 48
                   + (get (index + 2) & 0xff) << 40
                   + (get (index + 3) & 0xff) << 32
                   + (get (index + 4) & 0xff) << 24
                   + (get (index + 5) & 0xff) << 16
                   + (get (index + 6) & 0xff) << 8
                   + (get (index + 7) & 0xff));
  }
  
  final public ByteBuffer putLong (int index, long value)
  {
    // FIXME: this handles big endian only
    put (index, (byte) ((((int) value) & 0xff00000000000000) >> 56));
    put (index + 1, (byte) ((((int) value) & 0x00ff000000000000) >> 48));
    put (index + 2, (byte) ((((int) value) & 0x0000ff0000000000) >> 40));
    put (index + 3, (byte) ((((int) value) & 0x000000ff00000000) >> 32));
    put (index + 4, (byte) ((((int) value) & 0x00000000ff000000) >> 24));
    put (index + 5, (byte) ((((int) value) & 0x0000000000ff0000) >> 16));
    put (index + 6, (byte) ((((int) value) & 0x000000000000ff00) >> 8));
    put (index + 7, (byte) (((int) value) & 0x00000000000000ff));
    return this;
  }

  final public float getFloat ()
  {
    // FIXME: this handles big endian only
    return (float) (((get () & 0xff) << 24)
                    + (get () & 0xff) << 16
                    + (get () & 0xff) << 8
                    + (get () & 0xff));
  }
  
  final public ByteBuffer putFloat (float value)
  {
    // FIXME: this handles big endian only
    put ((byte) ((((int) value) & 0xff000000) >> 24));
    put ((byte) ((((int) value) & 0x00ff0000) >> 16));
    put ((byte) ((((int) value) & 0x0000ff00) >> 8));
    put ((byte) (((int) value) & 0x000000ff));
    return this;
  }
  
  final public float getFloat (int index)
  {
    // FIXME: this handles big endian only
    return (float) (((get (index) & 0xff) << 24)
                    + (get (index + 1) & 0xff) << 16
                    + (get (index + 2) & 0xff) << 8
                    + (get (index + 3) & 0xff));
  }

  final public ByteBuffer putFloat (int index, float value)
  {
    // FIXME: this handles big endian only
    put (index, (byte) ((((int) value) & 0xff000000) >> 24));
    put (index + 1, (byte) ((((int) value) & 0x00ff0000) >> 16));
    put (index + 2, (byte) ((((int) value) & 0x0000ff00) >> 8));
    put (index + 3, (byte) (((int) value) & 0x000000ff));
    return this;
  }

  final public double getDouble ()
  {
    // FIXME: this handles big endian only
    return (double) (((get () & 0xff) << 56)
                     + (get () & 0xff) << 48
                     + (get () & 0xff) << 40
                     + (get () & 0xff) << 32
                     + (get () & 0xff) << 24
                     + (get () & 0xff) << 16
                     + (get () & 0xff) << 8
                     + (get () & 0xff));
  }

  final public ByteBuffer putDouble (double value)
  {
    // FIXME: this handles big endian only
    put ((byte) ((((int) value) & 0xff00000000000000) >> 56));
    put ((byte) ((((int) value) & 0x00ff000000000000) >> 48));
    put ((byte) ((((int) value) & 0x0000ff0000000000) >> 40));
    put ((byte) ((((int) value) & 0x000000ff00000000) >> 32));
    put ((byte) ((((int) value) & 0x00000000ff000000) >> 24));
    put ((byte) ((((int) value) & 0x0000000000ff0000) >> 16));
    put ((byte) ((((int) value) & 0x000000000000ff00) >> 8));
    put ((byte) (((int) value) & 0x00000000000000ff));
    return this;
  }
  
  final public double getDouble (int index)
  {
    // FIXME: this handles big endian only
    return (double) (((get (index) & 0xff) << 56)
                     + (get (index + 1) & 0xff) << 48
                     + (get (index + 2) & 0xff) << 40
                     + (get (index + 3) & 0xff) << 32
                     + (get (index + 4) & 0xff) << 24
                     + (get (index + 5) & 0xff) << 16
                     + (get (index + 6) & 0xff) << 8
                     + (get (index + 7) & 0xff));
  }
  
  final public ByteBuffer putDouble (int index, double value)
  {
    // FIXME: this handles big endian only
    put (index, (byte) ((((int) value) & 0xff00000000000000) >> 56));
    put (index + 1, (byte) ((((int) value) & 0x00ff000000000000) >> 48));
    put (index + 2, (byte) ((((int) value) & 0x0000ff0000000000) >> 40));
    put (index + 3, (byte) ((((int) value) & 0x000000ff00000000) >> 32));
    put (index + 4, (byte) ((((int) value) & 0x00000000ff000000) >> 24));
    put (index + 5, (byte) ((((int) value) & 0x0000000000ff0000) >> 16));
    put (index + 6, (byte) ((((int) value) & 0x000000000000ff00) >> 8));
    put (index + 7, (byte) (((int) value) & 0x00000000000000ff));
    return this;
  }
}
