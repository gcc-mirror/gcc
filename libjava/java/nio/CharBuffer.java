/* CharBuffer.java -- 
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

import gnu.java.nio.CharBufferImpl;

/**
 * @since 1.4
 */
public abstract class CharBuffer extends Buffer
  implements Comparable, CharSequence
{
  protected char [] backing_buffer;
  
  /**
   * Allocates a new <code>CharBuffer</code> object with a given capacity.
   */
  public static CharBuffer allocate (int capacity)
  {
    return new CharBufferImpl (capacity, 0, capacity);
  }
  
  /**
   * Wraps a character array into a <code>CharBuffer</code> object.
   * 
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  final public static CharBuffer wrap (char[] array, int offset, int length)
  {
    return new CharBufferImpl (array, offset, length);
  }
  
  /**
   * Wraps a character sequence into a <code>CharBuffer</code> object.
   */
  final public static CharBuffer wrap (CharSequence a)
  {
    return wrap (a, 0, a.length ());
  }
  
  /**
   * Wraps a character sequence into a <code>CharBuffer</code> object.
   * 
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  final public static CharBuffer wrap (CharSequence a, int offset, int length)
  {
    if ((offset < 0)
        || (offset > a.length ())
        || (length < 0)
        || (length > (a.length () - offset)))
      throw new IndexOutOfBoundsException ();
    
    char [] buffer = new char [a.length ()];
    
    for (int i = offset; i < length; i++)
      {
        buffer [i] = a.charAt (i);
      }
    
    return wrap (buffer, offset, length);
  }
  
  /**
   * Wraps a character array into a <code>CharBuffer</code> object.
   */
  final public static CharBuffer wrap (char[] array)
  {
    return wrap (array, 0, array.length);
  }
 
  CharBuffer (int cap, int lim, int pos, int mark)
  {
    super (cap, lim, pos, mark);
  }
  
  /**
   * Relative get method.
   * 
   * @exception BufferUnderflowException If the buffer's current position is
   * not smaller than its limit.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  public CharBuffer get (char[] dst, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      {
        dst [i] = get ();
      }
    
    return this;
  }
  
  /**
   * Relative get method.
   * 
   * @exception BufferUnderflowException If there are fewer than length
   * characters remaining in this buffer.
   */
  public CharBuffer get (char[] dst)
  {
    return get (dst, 0, dst.length);
  }
  
  /**
   * @exception BufferOverflowException If there are fewer than length of
   * source buffer characters remaining in this buffer.
   * @exception IllegalArgumentException If the source buffer is this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public CharBuffer put (CharBuffer src)
  {
    if (src == this)
      throw new IllegalArgumentException ();

    if (src.length () > 0)
      {
        char [] toPut = new char [src.length ()];
        src.get (toPut);
        src.put (toPut);
      }

    return this;
  }
 
  /**
   * @exception BufferOverflowException If there are fewer then length
   * characters remaining in this buffer.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public CharBuffer put (char[] src, int offset, int length)
  {
    if (offset < 0
        || offset >= src.length
        || length < 0
        || length >= (src.length - offset))
      throw new IndexOutOfBoundsException ();
     
    // Put nothing into this buffer when not enough space left.
    if (length > remaining ())
      throw new BufferOverflowException ();
		    
    for (int i = offset; i < offset + length; i++)
      {
        put (src [i]);
      }

    return this;
  }

  /**
   * Relative put method.
   * 
   * @exception BufferOverflowException If there are fewer then length of the
   * array characters remaining in this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public final CharBuffer put (char[] src)
  {
    return put (src, 0, src.length);
  }

  /**
   * Tells wether this is buffer is backed by an array or not.
   */
  public final boolean hasArray ()
  {
    return (backing_buffer != null
            && ! isReadOnly ());
  }

  /**
   * Returns the array that backs this buffer.
   * 
   * @exception ReadOnlyBufferException If this buffer is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final char[] array ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    if (isReadOnly ())
      throw new ReadOnlyBufferException ();
    
    return backing_buffer;
  }
  
  /**
   * Returns the offset to the position of a character in this buffer.
   * 
   * @exception ReadOnlyBufferException If this buffer is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final int arrayOffset ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    if (isReadOnly ())
      throw new ReadOnlyBufferException ();
    
    return 0;
  }
  
  /**
   * Calculates a hash code for this buffer-
   */
  public int hashCode ()
  {
    // FIXME: Check what SUN calculates here.
    return super.hashCode ();
  }
  
  /**
   * Checks if this buffer is equal to obj.
   */
  public boolean equals (Object obj)
  {
    if (obj instanceof CharBuffer)
      return compareTo (obj) == 0;
    
    return false;
  }
 
  /**
   * Compares two character buffer objects.
   * 
   * @exception ClassCastException If obj is not an object derived from
   * <code>CharBuffer</code>.
   */
  public int compareTo(Object obj)
  {
    CharBuffer a = (CharBuffer) obj;
    
    if (a.remaining () != remaining ())
      return 1;
    
    if (! hasArray () || ! a.hasArray ())
      return 1;
    
    int r = remaining ();
    int i1 = position ();
    int i2 = a.position ();
    
    for (int i = 0; i < r; i++)
      {
        int t = (int) (get (i1)- a.get (i2));
	
        if (t != 0)
          return (int) t;
      }
    
    return 0;
  }
 
  /**
   * Relative get method.
   * 
   * @exception BufferUnderflowException If there are no remaining characters
   * in this buffer.
   */
  public abstract char get ();
  
  /**
   * Relative put method.
   * 
   * @exception BufferOverflowException If there no remaining characters in
   * this buffer.
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
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract CharBuffer compact ();
  
  /**
   * Tells wether this buffer is direct or not.
   */
  public abstract boolean isDirect ();
  
  public abstract CharBuffer slice ();
  
  /**
   * Duplicates this buffer.
   */
  public abstract CharBuffer duplicate ();
  
  /**
   * Returns this buffer made read-only.
   */
  public abstract CharBuffer asReadOnlyBuffer ();
  
  /**
   * Returns the remaining content of the buffer as a string.
   */
  public String toString ()
  {
    return new String (array (), position (), length ());
  }

  /**
   * Returns the length of the remaining chars in this buffer.
   */
  public final int length ()
  { 
    return remaining ();
  }

  /**
   * Returns the byte order of this buffer.
   */
  public abstract ByteOrder order ();

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
    return put (str, 0, str.length ());
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
}
