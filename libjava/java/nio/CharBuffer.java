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

public abstract class CharBuffer extends Buffer
{
  private ByteOrder endian = ByteOrder.BIG_ENDIAN;

  protected char [] backing_buffer;
  
  public static CharBuffer allocate (int capacity)
  {
    return null;
  }
  
  /**
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  final public static CharBuffer wrap (char[] array, int offset, int length)
  {
    if ((offset < 0) ||
        (offset > array.length) ||
        (length < 0) ||
        (length > (array.length - offset)))
      throw new IndexOutOfBoundsException ();
 
    return null;
  }
  
  final public static CharBuffer wrap (CharSequence a)
  {
    return wrap (a, 0, a.length ());
  }
  
  /**
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  final public static CharBuffer wrap (CharSequence a, int offset, int length)
  {
    char [] buffer = new char [length];
    
    for (int i = offset; i < length; i++)
      {
        buffer [i] = a.charAt (i);
      }
    
    return wrap (buffer, 0, length);
  }
  
  final public static CharBuffer wrap (char[] array)
  {
    return wrap  (array, 0, array.length);
  }
  
  /**
   * @exception BufferUnderflowException FIXME
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  final public CharBuffer get (char[] dst, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      {
        dst [i] = get ();
      }
    return this;
  }
  
  /**
   * @exception BufferUnderflowException FIXME
   */
  final public CharBuffer get (char[] dst)
  {
    return get (dst, 0, dst.length);
  }
  
  /**
   * @exception BufferOverflowException FIXME
   * @exception IllegalArgumentException FIXME
   * @exception ReadOnlyBufferException FIXME
   */
  final public CharBuffer put (CharBuffer src)
  {
    while (src.hasRemaining ())
      put (src.get ());

    return this;
  }
 
  /**
   * @exception BufferOverflowException FIXME
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   * @exception ReadOnlyBufferException FIXME
   */
  final public CharBuffer put (char[] src, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      put (src [i]);

    return this;
  }

  /**
   * @exception BufferOverflowException FIXME
   * @exception ReadOnlyBufferException FIXME
   */
  public final CharBuffer put (char[] src)
  {
    return put (src, 0, src.length);
  }

  public final boolean hasArray ()
  {
    return backing_buffer != null;
  }

  /**
   * @exception ReadOnlyBufferException FIXME
   * @exception UnsupportedOperationException FIXME
   */
  public final char[] array ()
  {
    return backing_buffer;
  }
  
  /**
   * @exception ReadOnlyBufferException FIXME
   * @exception UnsupportedOperationException FIXME
   */
  public final int arrayOffset ()
  {
    return 0;
  }
  
  public int hashCode ()
  {
    return super.hashCode ();
  }
  
  public boolean equals (Object obj)
  {
    if (obj instanceof CharBuffer)
      return compareTo (obj) == 0;
    
    return false;
  }
 
  /**
   * @exception ClassCastException FIXME
   */
  public int compareTo(Object obj)
  {
    CharBuffer a = (CharBuffer) obj;
    
    if (a.remaining () != remaining ())
      return 1;
    
    if (! hasArray () || ! a.hasArray ())
      return 1;
    
    int r = remaining ();
    int i1 = pos;
    int i2 = a.pos;
    
    for (int i = 0; i < r; i++)
      {
        int t = (int) (get (i1)- a.get (i2));
	
        if (t != 0)
          return (int) t;
      }
    return 0;
  }
 
  /**
   * @exception BufferUnderflowException FIXME
   */
  public abstract char get ();
  
  /**
   * @exception BufferOverflowException FIXME
   * @exception ReadOnlyBufferException FIXME
   */
  public abstract CharBuffer put (char b);
  
  /**
   * @exception IndexOutOfBoundsException FIXME
   */
  public abstract char get (int index);
  
  /**
   * @exception IndexOutOfBoundsException FIXME
   * @exception ReadOnlyBufferException FIXME
   */
  public abstract CharBuffer put (int index, char b);
 
  /**
   * @exception ReadOnlyBufferException FIXME
   */
  public abstract CharBuffer compact ();
  
  public abstract boolean isDirect ();
  
  public abstract CharBuffer slice ();
  
  public abstract CharBuffer duplicate ();
  
  public abstract CharBuffer asReadOnlyBuffer ();
  
  public String toString ()
  {
    return "";
  }

  public final int length ()
  { 
    return 0;
  }

  public abstract ByteOrder order ();

  /**
   * @exception IndexOutOfBoundsException FIXME
   */
  public abstract CharSequence subSequence (int start, int length);

  /**
   * @exception BufferOverflowException FIXME
   * @exception IndexOutOfBoundsException FIXME
   * @exception ReadOnlyBufferException FIXME
   */
  public CharBuffer put (String str, int start, int length)
  {
    return null;
  }
  
  /**
   * @exception BufferOverflowException FIXME
   * @exception ReadOnlyBufferException FIXME
   */
  public final CharBuffer put (String str)
  {
    return null;
  }
  
  /**
   * @exception IndexOutOfBoundsException FIXME
   */
  public final char charAt (int index)
  {
    return ' ';
  }
}
