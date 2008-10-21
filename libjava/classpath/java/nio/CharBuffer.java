/* CharBuffer.java -- 
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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

// GCJ LOCAL: Use RawData instead of gnu.classpath.Pointer
import gnu.gcj.RawData;

import java.io.IOException;

/**
 * @since 1.4
 */
public abstract class CharBuffer extends Buffer
  implements Comparable<CharBuffer>, CharSequence, Readable, Appendable
{
  final int array_offset;
  final char[] backing_buffer;

  CharBuffer (int capacity, int limit, int position, int mark,
	      RawData address, char[] backing_buffer, int array_offset)
  {
    super (capacity, limit, position, mark, address);
    this.backing_buffer = backing_buffer;
    this.array_offset = array_offset;
  }

  /**
   * Allocates a new <code>CharBuffer</code> object with a given capacity.
   */
  public static CharBuffer allocate (int capacity)
  {
    return new CharBufferImpl (capacity);
  }

  /**
   * Wraps a <code>char</code> array into a <code>CharBuffer</code>
   * object.
   *
   * @param array the array to wrap
   * @param offset the offset of the region in the array to wrap
   * @param length the length of the region in the array to wrap
   *
   * @return a new <code>CharBuffer</code> object
   * 
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  public static final CharBuffer wrap(char[] array, int offset, int length)
  {
    return new CharBufferImpl(array, 0, array.length, offset + length, offset,
			      -1, false);
  }
  
  /**
   * Wraps a character sequence into a <code>CharBuffer</code> object.
   *
   * @param seq the sequence to wrap
   *
   * @return a new <code>CharBuffer</code> object
   */
  public static final CharBuffer wrap(CharSequence seq)
  {
    return wrap(seq, 0, seq.length());
  }
  
  /**
   * Wraps a character sequence into a <code>CharBuffer</code> object.
   * 
   * @param seq the sequence to wrap
   * @param start the index of the first character to wrap
   * @param end the index of the first character not to wrap
   *
   * @return a new <code>CharBuffer</code> object
   * 
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  public static final CharBuffer wrap(CharSequence seq, int start, int end)
  {
    return new CharSequenceBuffer(seq, start, end);
  }

  /**
   * Wraps a <code>char</code> array into a <code>CharBuffer</code>
   * object.
   *
   * @param array the array to wrap
   *
   * @return a new <code>CharBuffer</code> object
   */
  public static final CharBuffer wrap(char[] array)
  {
    return wrap(array, 0, array.length);
  }
  
  /**
   * This method transfers <code>char</code>s from this buffer into the given
   * destination array. Before the transfer, it checks if there are fewer than
   * length <code>char</code>s remaining in this buffer. 
   *
   * @param dst The destination array
   * @param offset The offset within the array of the first <code>char</code>
   * to be written; must be non-negative and no larger than dst.length.
   * @param length The maximum number of bytes to be written to the given array;
   * must be non-negative and no larger than dst.length - offset.
   *
   * @exception BufferUnderflowException If there are fewer than length
   * <code>char</code>s remaining in this buffer.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold.
   */
  public CharBuffer get (char[] dst, int offset, int length)
  {
    checkArraySize(dst.length, offset, length);
    checkForUnderflow(length);

    for (int i = offset; i < offset + length; i++)
      {
        dst [i] = get ();
      }

    return this;
  }

  /** @since 1.5 */
  public int read(CharBuffer buffer) throws IOException
  {
    // We want to call put(), so we don't manipulate the CharBuffer
    // directly.
    int rem = Math.min(buffer.remaining(), remaining());
    char[] buf = new char[rem];
    get(buf);
    buffer.put(buf);
    return rem;
  }

  /**
   * This method transfers <code>char</code>s from this buffer into the given
   * destination array.
   *
   * @param dst The byte array to write into.
   *
   * @exception BufferUnderflowException If there are fewer than dst.length
   * <code>char</code>s remaining in this buffer.
   */
  public CharBuffer get (char[] dst)
  {
    return get (dst, 0, dst.length);
  }

  /**
   * Writes the content of the the <code>CharBUFFER</code> src
   * into the buffer. Before the transfer, it checks if there is fewer than
   * <code>src.remaining()</code> space remaining in this buffer.
   *
   * @param src The source data.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>char</code>s in the source buffer.
   * @exception IllegalArgumentException If the source buffer is this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public CharBuffer put (CharBuffer src)
  {
    if (src == this)
      throw new IllegalArgumentException ();

    checkForOverflow(src.remaining());

    if (src.remaining () > 0)
      {
        char[] toPut = new char [src.remaining ()];
        src.get (toPut);
	put (toPut);
      }

    return this;
  }

  /**
   * Writes the content of the the <code>char array</code> src
   * into the buffer. Before the transfer, it checks if there is fewer than
   * length space remaining in this buffer.
   *
   * @param src The array to copy into the buffer.
   * @param offset The offset within the array of the first byte to be read;
   * must be non-negative and no larger than src.length.
   * @param length The number of bytes to be read from the given array;
   * must be non-negative and no larger than src.length - offset.
   * 
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>char</code>s in the source array.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public CharBuffer put (char[] src, int offset, int length)
  {
    checkArraySize(src.length, offset, length);
    checkForOverflow(length);
		    
    for (int i = offset; i < offset + length; i++)
      put (src [i]);

    return this;
  }

  /**
   * Writes the content of the the <code>char array</code> src
   * into the buffer.
   *
   * @param src The array to copy into the buffer.
   * 
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>char</code>s in the source array.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public final CharBuffer put (char[] src)
  {
    return put (src, 0, src.length);
  }

  /**
   * Tells whether ot not this buffer is backed by an accessible
   * <code>char</code> array.
   */
  public final boolean hasArray ()
  {
    return (backing_buffer != null
            && !isReadOnly ());
  }

  /**
   * Returns the <code>char</code> array that backs this buffer.
   *
   * @exception ReadOnlyBufferException If this buffer is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final char[] array ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    checkIfReadOnly();

    return backing_buffer;
  }

  /**
   * Returns the offset within this buffer's backing array of the first element.
   *
   * @exception ReadOnlyBufferException If this buffer is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final int arrayOffset ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    checkIfReadOnly();
    
    return array_offset;
  }

  /**
   * Calculates a hash code for this buffer.
   *
   * This is done with int arithmetic,
   * where ** represents exponentiation, by this formula:<br>
   * <code>s[position()] + 31 + (s[position()+1] + 30)*31**1 + ... +
   * (s[limit()-1]+30)*31**(limit()-1)</code>.
   * Where s is the buffer data. Note that the hashcode is dependent
   * on buffer content, and therefore is not useful if the buffer
   * content may change.
   */
  public int hashCode ()
  {
    int hashCode = get(position()) + 31;
    int multiplier = 1;
    for (int i = position() + 1; i < limit(); ++i)
      {
	  multiplier *= 31;
	  hashCode += (get(i) + 30)*multiplier;
      }
    return hashCode;
  }

  /**
   * Checks if this buffer is equal to obj.
   */
  public boolean equals (Object obj)
  {
    if (obj instanceof CharBuffer)
      {
        return compareTo ((CharBuffer) obj) == 0;
      }

    return false;
  }

  /**
   * Compares two <code>CharBuffer</code> objects.
   *
   * @exception ClassCastException If obj is not an object derived from
   * <code>CharBuffer</code>.
   */
  public int compareTo (CharBuffer other)
  {
    int num = Math.min(remaining(), other.remaining());
    int pos_this = position();
    int pos_other = other.position();
    
    for (int count = 0; count < num; count++)
      {
	 char a = get(pos_this++);
	 char b = other.get(pos_other++);
      	 
	 if (a == b)
	   continue;
      	   
	 if (a < b)
	   return -1;
      	   
	 return 1;
      }
      
     return remaining() - other.remaining();
  }

  /**
   * Returns the byte order of this buffer.
   */
  public abstract ByteOrder order ();

  /**
   * Reads the <code>char</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferUnderflowException If there are no remaining
   * <code>char</code>s in this buffer.
   */
  public abstract char get ();

  /**
   * Writes the <code>char</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferOverflowException If there no remaining 
   * <code>char</code>s in this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract CharBuffer put (char b);

  /**
   * Absolute get method.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  public abstract char get (int index);
  
  /**
   * Absolute put method.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract CharBuffer put (int index, char b);

  /**
   * Compacts this buffer.
   * 
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract CharBuffer compact ();

  /**
   * Tells wether or not this buffer is direct.
   */
  public abstract boolean isDirect ();

  /**
   * Creates a new <code>CharBuffer</code> whose content is a shared
   * subsequence of this buffer's content.
   */
  public abstract CharBuffer slice ();

  /**
   * Creates a new <code>CharBuffer</code> that shares this buffer's
   * content.
   */
  public abstract CharBuffer duplicate ();

  /**
   * Creates a new read-only <code>CharBuffer</code> that shares this
   * buffer's content.
   */
  public abstract CharBuffer asReadOnlyBuffer ();
  
  /**
   * Returns the remaining content of the buffer as a string.
   */
  public String toString ()
  {
    if (hasArray ())
      return new String (array (), position (), length ());

    char[] buf = new char [length ()];
    int pos = position ();
    get (buf, 0, buf.length);
    position (pos);
    return new String (buf);
  }

  /**
   * Returns the length of the remaining chars in this buffer.
   */
  public final int length ()
  { 
    return remaining ();
  }

  /**
   * Creates a new character buffer that represents the specified subsequence
   * of this buffer, relative to the current position.
   *
   * @exception IndexOutOfBoundsException If the preconditions on start and
   * end do not hold.
   */
  public abstract CharSequence subSequence (int start, int length);

  /**
   * Relative put method.
   * 
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer.
   * @exception IndexOutOfBoundsException If the preconditions on the start
   * and end parameters do not hold.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public CharBuffer put (String str, int start, int length)
  {
    return put (str.toCharArray (), start, length);
  }
  
  /**
   * Relative put method.
   * 
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public final CharBuffer put (String str)
  {
    return put (str.toCharArray (), 0, str.length ());
  }
  
  /**
   * Returns the character at <code>position() + index</code>.
   * 
   * @exception IndexOutOfBoundsException If index is negative not smaller than
   * <code>remaining()</code>.
   */
  public final char charAt (int index)
  {
    if (index < 0
        || index >= remaining ())
      throw new IndexOutOfBoundsException ();
    
    return get (position () + index);
  }

  /** @since 1.5 */
  public CharBuffer append(char c)
  {
    put(c);
    return this;
  }

  /** @since 1.5 */
  public CharBuffer append(CharSequence cs)
  {
    put(cs == null ? "null" : cs.toString());
    return this;
  }

  /** @since 1.5 */
  public CharBuffer append(CharSequence cs, int start, int end)
  {
    put(cs == null ? "null" : cs.subSequence(start, end).toString());
    return this;
  }
}
