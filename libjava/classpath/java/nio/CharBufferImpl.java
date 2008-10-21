/* CharBufferImpl.java -- 
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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
final class CharBufferImpl extends CharBuffer
{
  private final boolean readOnly;

  CharBufferImpl (int capacity)
  {
    this (new char [capacity], 0, capacity, capacity, 0, -1, false);
  }
  
  CharBufferImpl (char[] buffer, int offset, int capacity, int limit, int position, int mark, boolean readOnly)
  {
    super (capacity, limit, position, mark, null, buffer, offset);
    this.readOnly = readOnly;
  }
  
  public CharBufferImpl (CharBufferImpl copy)
  {
    super (copy.capacity (), copy.limit (), copy.position (), 0, null, copy.backing_buffer, copy.array_offset);
    this.readOnly = copy.isReadOnly ();
  }
  
  public boolean isReadOnly ()
  {
    return readOnly;
  }
  
  public CharBuffer slice ()
  {
    return new CharBufferImpl (backing_buffer, array_offset + position (), remaining (), remaining (), 0, -1, isReadOnly ());
  }
  
  public CharBuffer duplicate ()
  {
    return new CharBufferImpl (backing_buffer, array_offset, capacity (), limit (), position (), mark, isReadOnly ());
  }
  
  public CharBuffer asReadOnlyBuffer ()
  {
    return new CharBufferImpl (backing_buffer, array_offset, capacity (), limit (), position (), mark, true);
  }
  
  public CharBuffer compact ()
  {
    checkIfReadOnly();
    mark = -1;
    int p = position();
    int n = limit() - p;
    if (n > 0)
      {
        System.arraycopy(backing_buffer, array_offset + p,
                         backing_buffer, array_offset, n);
      }
    position(n);
    limit(capacity());
    return this;
  }
  
  public boolean isDirect ()
  {
    return false;
  }

  public CharSequence subSequence (int start, int end)
  {
    if (start < 0
        || start > length ()
        || end < start
        || end > length ())
      throw new IndexOutOfBoundsException ();

    return new CharBufferImpl (backing_buffer, array_offset, capacity (), position () + end, position () + start, -1, isReadOnly ());
  }
  
  /**
   * Reads the <code>char</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferUnderflowException If there are no remaining
   * <code>char</code>s in this buffer.
   */
  public char get ()
  {
    if (pos >= limit)
        throw new BufferUnderflowException();

    return backing_buffer [(pos++) + array_offset];
  }
  
  /**
   * Relative put method. Writes <code>value</code> to the next position
   * in the buffer.
   * 
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public CharBuffer put (char value)
  {
    if (readOnly)
        throw new ReadOnlyBufferException();
    if (pos >= limit)
        throw new BufferOverflowException();

    backing_buffer [(pos++) + array_offset] = value;
    return this;
  }
  
  /**
   * Absolute get method. Reads the <code>char</code> at position
   * <code>index</code>.
   *
   * @param index Position to read the <code>char</code> from.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  public char get (int index)
  {
    checkIndex(index);
    
    return backing_buffer [index + array_offset];
  }
  
  /**
   * Bulk get, overloaded for speed.
   */
  public CharBuffer get (char[] dst, int offset, int length)
  {
    checkArraySize(dst.length, offset, length);
    checkForUnderflow(length);

    System.arraycopy(backing_buffer, pos + array_offset, 
		     dst, offset, length);
    pos += length;
    return this;
  }

  /**
   * Bulk put, overloaded for speed.
   */
  public CharBuffer put (char[] src, int offset, int length)
  {
    checkArraySize(src.length, offset, length);
    checkForOverflow(length);
		    
    System.arraycopy(src, offset,
		     backing_buffer, pos + array_offset, length);
    pos += length;
    return this;
  }

  /**
   * Absolute put method. Writes <code>value</code> to position
   * <code>index</code> in the buffer.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public CharBuffer put (int index, char value)
  {
    checkIndex(index);
    checkIfReadOnly();
    	    
    backing_buffer [index + array_offset] = value;
    return this;
  }
  
  public ByteOrder order ()
  {
    return ByteOrder.nativeOrder ();
  }
}
