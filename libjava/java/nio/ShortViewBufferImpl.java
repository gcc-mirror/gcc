/* ShortViewBufferImpl.java -- 
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

final class ShortViewBufferImpl extends ShortBuffer
{
  /** Position in bb (i.e. a byte offset) where this buffer starts. */
  private int offset;
  private ByteBuffer bb;
  private boolean readOnly;
  private ByteOrder endian;

  ShortViewBufferImpl (ByteBuffer bb, int capacity)
  {
    super (capacity, capacity, 0, -1);
    this.bb = bb;
    this.offset = bb.position();
    this.readOnly = bb.isReadOnly();
    this.endian = bb.order();
    if (bb.isDirect())
      this.address = VMDirectByteBuffer.adjustAddress(bb.address, offset);
  }
  
  public ShortViewBufferImpl (ByteBuffer bb, int offset, int capacity,
			      int limit, int position, int mark,
			      boolean readOnly, ByteOrder endian)
  {
    super (capacity, limit, position, mark);
    this.bb = bb;
    this.offset = offset;
    this.readOnly = readOnly;
    this.endian = endian;
    if (bb.isDirect())
      this.address = VMDirectByteBuffer.adjustAddress(bb.address, offset);
  }

  /**
   * Reads the <code>short</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferUnderflowException If there are no remaining
   * <code>short</code>s in this buffer.
   */
  public short get ()
  {
    int p = position();
    short result = ByteBufferHelper.getShort(bb, (p << 1) + offset, endian);
    position(p + 1);
    return result;
  }

  /**
   * Absolute get method. Reads the <code>short</code> at position
   * <code>index</code>.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  public short get (int index)
  {
    return ByteBufferHelper.getShort(bb, (index << 1) + offset, endian);
  }

  public ShortBuffer put (short value)
  {
    int p = position();
    ByteBufferHelper.putShort(bb, (p << 1) + offset, value, endian);
    position(p + 1);
    return this;
  }
  
  public ShortBuffer put (int index, short value)
  {
    ByteBufferHelper.putShort(bb, (index << 1) + offset, value, endian);
    return this;
  }

  public ShortBuffer compact ()
  {
    if (position () > 0)
      {
        int count = limit () - position ();
	bb.shiftDown(offset, offset + 2 * position(), 2 * count);
        position (count);
        limit (capacity ());
      }
    else
      {
	position(limit());
	limit(capacity());
      }
    return this;
  }
  
  public ShortBuffer slice ()
  {
    // Create a sliced copy of this object that shares its content.
    return new ShortViewBufferImpl (bb, (position () >> 1) + offset,
				    remaining(), remaining(), 0, -1,
				    readOnly, endian);
  }
  
  ShortBuffer duplicate (boolean readOnly)
  {
    int pos = position();
    reset();
    int mark = position();
    position(pos);
    return new ShortViewBufferImpl (bb, offset, capacity(), limit(),
				    pos, mark, readOnly, endian);
  }
  
  public ShortBuffer duplicate ()
  {
    return duplicate(readOnly);
  }

  public ShortBuffer asReadOnlyBuffer ()
  {
    return duplicate(true);
  }

  public boolean isReadOnly ()
  {
    return readOnly;
  }
  
  public boolean isDirect ()
  {
    return bb.isDirect ();
  }
  
  public ByteOrder order ()
  {
    return endian;
  }
}
