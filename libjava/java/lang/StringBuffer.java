/* StringBuffer.java -- Growable strings
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

package java.lang;
import java.io.Serializable;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * Updated using online JDK 1.2 docs.
 * Believed complete and correct to JDK 1.2.
 * Merged with Classpath.
 */

/**
 * <code>StringBuffer</code> represents a changeable <code>String</code>.
 * It provides the operations required to modify the
 * <code>StringBuffer</code> including insert, replace, delete, append,
 * and reverse.
 * <P>
 *
 * <code>StringBuffer</code>s are variable-length in nature, so even if
 * you initialize them to a certain size, they can still grow larger than
 * that.  <EM>Capacity</EM> indicates the number of characters the
 * <code>StringBuffer</code> can have in it before it has to grow (growing
 * the char array is an expensive operation involving <code>new</code>).
 * <P>
 *
 * Incidentally, the String operator "+" actually is turned into a
 * <code>StringBuffer</code> operation:
 * <BR>
 * <code>a + b</code>
 * <BR>
 * is the same as
 * <BR>
 * <code>new StringBuffer(a).append(b).toString()</code>.
 *
 * @implnote Classpath's StringBuffer is capable of sharing memory with
 *           Strings for efficiency.  This will help in two instances:
 *           first, when a StringBuffer is created from a String but is
 *           never changed, and second, when a StringBuffer is converted
 *           to a String and the StringBuffer is not changed after that.
 *
 * @since JDK1.0
 * @author Paul Fisher
 * @author John Keiser
 * @author Tom Tromey
 * @see java.lang.String
 */
public final class StringBuffer implements Serializable, CharSequence
{
  /** Append the <code>String</code> value of the argument to this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param bool the <code>boolean</code> to convert and append.
   *  @return this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(boolean)
   */
  public StringBuffer append (boolean bool)
  {
    return append (String.valueOf(bool));
  }

  /** Append the <code>char</code> to this <code>StringBuffer</code>.
   *  @param c the <code>char</code> to append.
   *  @return this <code>StringBuffer</code>.
   */
  public synchronized StringBuffer append (char ch)
  {
    ensureCapacity_unsynchronized (count + 1);
    value[count++] = ch;
    return this;
  }

  /** Append the <code>String</code> value of the argument to this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param inum the <code>int</code> to convert and append.
   *  @return this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(int)
   */
  public native StringBuffer append (int inum);

  /** Append the <code>String</code> value of the argument to this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param lnum the <code>long</code> to convert and append.
   *  @return this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(long)
   */
  public StringBuffer append (long lnum)
  {
    return append (String.valueOf(lnum));
  }

  /** Append the <code>String</code> value of the argument to this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param fnum the <code>float</code> to convert and append.
   *  @return this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(float)
   */
  public StringBuffer append (float fnum)
  {
    return append (String.valueOf(fnum));
  }

  /** Append the <code>String</code> value of the argument to this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param dnum the <code>double</code> to convert and append.
   *  @return this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(double)
   */
  public StringBuffer append (double dnum)
  {
    return append (String.valueOf(dnum));
  }

  /** Append the <code>String</code> value of the argument to this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param obj the <code>Object</code> to convert and append.
   *  @return this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(java.lang.Object)
   */
  public StringBuffer append (Object obj)
  {
    return append (String.valueOf(obj));
  }

  /** Append the <code>String</code> to this <code>StringBuffer</code>.
   *  @param str the <code>String</code> to append.
   *  @return this <code>StringBuffer</code>.
   */
  public synchronized StringBuffer append (String str)
  {
    if (str == null)
      str = "null";
    int len = str.length();
    ensureCapacity_unsynchronized (count + len);
    str.getChars(0, len, value, count);
    count += len;
    return this;
  }

  /** Append the <code>char</code> array to this <code>StringBuffer</code>.
   *  @param data the <code>char[]</code> to append.
   *  @return this <code>StringBuffer</code>.
   *  @exception NullPointerException if <code>str</code> is <code>null</code>.
   */
  public StringBuffer append (char[] data)
  {
    return append (data, 0, data.length);
  }

  /** Append the <code>char</code> array to this <code>StringBuffer</code>.
   *  @param data the <code>char[]</code> to append.
   *  @param offset the place to start grabbing characters from
   *         <code>str</code>.
   *  @param count the number of characters to get from <code>str</code>.
   *  @return this <code>StringBuffer</code>.
   *  @exception NullPointerException if <code>str</code> is <code>null</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> or
   *             <code>offset+len</code> is out of range.
   */
  public synchronized StringBuffer append (char[] data, int offset, int count)
  {
    ensureCapacity_unsynchronized (this.count + count);
    System.arraycopy(data, offset, value, this.count, count);
    this.count += count;
    return this;
  } 

  /** Get the total number of characters this <code>StringBuffer</code>
   *  can support before it must be grown.  Not to be confused with
   *  <em>length</em>.
   *  @return the capacity of this <code>StringBuffer</code>
   *  @see #length()
   *  @see #ensureCapacity(int)
   */
  public int capacity ()
  {
    return value.length;
  }

  /** Get the character at the specified index.
   *  @param index the index of the character to get, starting at 0.
   *  @return the character at the specified index.
   *  @exception IndexOutOfBoundsException if the desired character index
   *             is negative or greater then length() - 1.
   */
  public synchronized char charAt (int index)
  {
    if (index >= count)
      throw new StringIndexOutOfBoundsException (index);
    return value[index];
  }

  /** Delete characters from this <code>StringBuffer</code>.
   *  <code>delete(10, 12)</code> will delete 10 and 11, but not 12.
   *  @param start the first character to delete.
   *  @param end the index after the last character to delete.
   *  @return this <code>StringBuffer</code>.
   *  @exception StringIndexOutOfBoundsException if <code>start</code>
   *             or <code>end-1</code> are out of bounds, or if
   *             <code>start > end</code>.
   */
  public synchronized StringBuffer delete (int start, int end)
  {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException (start);
    if (end > count)
      end = count;
    // This will unshare if required.
    ensureCapacity_unsynchronized (count);
    if (count - end != 0)
      System.arraycopy (value, end, value, start, count - end);
    count -= (end - start);
    return this;
  }

  /** Delete a character from this <code>StringBuffer</code>.
   *  @param index the index of the character to delete.
   *  @return this <code>StringBuffer</code>.
   *  @exception StringIndexOutOfBoundsException if <code>index</code>
   *             is out of bounds.
   */
  public StringBuffer deleteCharAt(int index)
  {
    return delete (index, index + 1);
  }

  /** Increase the capacity of this <code>StringBuffer</code>.
   *  This will ensure that an expensive growing operation will not occur
   *  until <code>minimumCapacity</code> is reached.
   *  If the capacity is actually already greater than <code>minimumCapacity</code>
   *  @param minimumCapacity the new capacity.
   *  @see #capacity()
   */
  public synchronized void ensureCapacity (int minimumCapacity)
  {
    if (shared || minimumCapacity > value.length)
      {
	// We don't want to make a larger vector when `shared' is
	// set.  If we do, then setLength becomes very inefficient
	// when repeatedly reusing a StringBuffer in a loop.
	int max = (minimumCapacity > value.length
		   ? value.length*2+2
		   : value.length);
	minimumCapacity = (minimumCapacity < max ? max : minimumCapacity);
	char[] nb = new char[minimumCapacity];
	System.arraycopy(value, 0, nb, 0, count);
	value = nb;
	shared = false;
      }
  }

  // ensureCapacity is used by several synchronized methods in StringBuffer.
  // There's no need to synchronize again.
  private void ensureCapacity_unsynchronized (int minimumCapacity)
  {
    if (shared || minimumCapacity > value.length)
      {
	// We don't want to make a larger vector when `shared' is
	// set.  If we do, then setLength becomes very inefficient
	// when repeatedly reusing a StringBuffer in a loop.
	int max = (minimumCapacity > value.length
		   ? value.length*2+2
		   : value.length);
	minimumCapacity = (minimumCapacity < max ? max : minimumCapacity);
	char[] nb = new char[minimumCapacity];
	System.arraycopy(value, 0, nb, 0, count);
	value = nb;
	shared = false;
      }
  }

  /** Get the specified array of characters.
   *  The characters will be copied into the array you pass in.
   *  @param srcOffset the index to start copying from in the
   *         <code>StringBuffer</code>.
   *  @param srcEnd the number of characters to copy.
   *  @param dst the array to copy into.
   *  @param dstOffset the index to start copying into <code>dst</code>.
   *  @exception NullPointerException if dst is null.
   *  @exception IndexOutOfBoundsException if any source or target
   *             indices are out of range.
   *  @see java.lang.System#arraycopy(java.lang.Object,int,java.lang.Object,int,int)
   */
  public synchronized void getChars (int srcOffset, int srcEnd,
				     char[] dst, int dstOffset)
  {
    if (srcOffset < 0 || srcOffset > srcEnd)
      throw new StringIndexOutOfBoundsException (srcOffset);
    int todo = srcEnd - srcOffset;
    if (srcEnd > count || dstOffset + todo > count)
      throw new StringIndexOutOfBoundsException (srcEnd);
    System.arraycopy(value, srcOffset, dst, dstOffset, todo);
  }

  /** Insert the <code>String</code> value of the argument into this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param offset the place to insert.
   *  @param bool the <code>boolean</code> to convert and insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(boolean)
   */
  public StringBuffer insert (int offset, boolean bool)
  {
    return insert (offset, bool ? "true" : "false");
  }

  /** Insert the <code>char</code> argument into this <code>StringBuffer</code>.
   *  @param offset the place to insert.
   *  @param ch the <code>char</code> to insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   */
  public synchronized StringBuffer insert (int offset, char ch)
  {
    if (offset < 0 || offset > count)
      throw new StringIndexOutOfBoundsException (offset);
    ensureCapacity_unsynchronized (count+1);
    System.arraycopy(value, offset, value, offset+1, count-offset);
    value[offset] = ch;
    count++;
    return this;
  }

  /** Insert the <code>String</code> value of the argument into this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param offset the place to insert.
   *  @param inum the <code>int</code> to convert and insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(int)
   */
  public StringBuffer insert (int offset, int inum)
  {
    return insert (offset, String.valueOf(inum));
  }

  /** Insert the <code>String</code> value of the argument into this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param offset the place to insert.
   *  @param lnum the <code>long</code> to convert and insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(long)
   */
  public StringBuffer insert (int offset, long lnum)
  {
    return insert (offset, String.valueOf(lnum));
  }

  /** Insert the <code>String</code> value of the argument into this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param offset the place to insert.
   *  @param fnum the <code>float</code> to convert and insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(float)
   */
  public StringBuffer insert (int offset, float fnum)
  {
    return insert (offset, String.valueOf(fnum));
  }

  /** Insert the <code>String</code> value of the argument into this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param offset the place to insert.
   *  @param dnum the <code>double</code> to convert and insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(double)
   */
  public StringBuffer insert (int offset, double dnum)
  {
    return insert (offset, String.valueOf(dnum));
  }

  /** Insert the <code>String</code> value of the argument into this <code>StringBuffer</code>.
   *  Uses <code>String.valueOf()</code> to convert to
   *  <code>String</code>.
   *  @param offset the place to insert.
   *  @param obj the <code>Object</code> to convert and insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   *  @see java.lang.String#valueOf(java.lang.Object)
   */
  public StringBuffer insert (int offset, Object obj)
  {
    return insert (offset, String.valueOf(obj));
  }

  /** Insert the <code>String</code> argument into this <code>StringBuffer</code>.
   *  @param offset the place to insert.
   *  @param str the <code>String</code> to insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   */
  public synchronized StringBuffer insert (int offset, String str)
  {
    if (offset < 0 || offset > count)
      throw new StringIndexOutOfBoundsException (offset);
    // Note that using `null' is from JDK 1.2.
    if (str == null)
      str = "null";
    int len = str.length();
    ensureCapacity_unsynchronized (count+len);
    System.arraycopy(value, offset, value, offset+len, count-offset);
    str.getChars(0, len, value, offset);
    count += len;
    return this;
  }

  /** Insert the <code>char[]</code> argument into this
   *  <code>StringBuffer</code>. 
   *  @param offset the place to insert.
   *  @param data the <code>char[]</code> to insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception NullPointerException if <code>data</code> is
   *             <code>null</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range for this <code>StringBuffer</code>.
   */
  public StringBuffer insert (int offset, char[] data)
  {
    // One could check if offset is invalid here instead of making sure that
    // data isn't null before dereferencing, but this works just as well.
    return insert (offset, data, 0, data == null ? 0 : data.length);
  }

  /** Insert the <code>char[]</code> argument into this
   *  <code>StringBuffer</code>.
   *  @param offset the place to insert.
   *  @param str the <code>char[]</code> to insert.
   *  @param str_offset the index in <code>str</code> to start inserting
   *         from.
   *  @param len the number of characters to insert.
   *  @return this <code>StringBuffer</code>.
   *  @exception NullPointerException if <code>str</code> is <code>null</code>.
   *  @exception IndexOutOfBoundsException if <code>offset</code> is out
   *             of range, for this <code>StringBuffer</code>, or if
   *             <code>str_offset</code> or <code>str_offset+len</code>
   *             are out of range for <code>str</code>.
   */
  public synchronized StringBuffer insert(int offset, char[] str,
					  int str_offset, int len)
  {
    if (offset < 0 || offset > count)
      throw new StringIndexOutOfBoundsException (offset);
    if (len < 0)
      throw new StringIndexOutOfBoundsException (len);
    if (str_offset < 0 || str_offset + len > str.length)
      throw new StringIndexOutOfBoundsException (str_offset);
    ensureCapacity_unsynchronized (count + len);
    System.arraycopy(value, offset, value, offset + len, count - offset);
    System.arraycopy(str, str_offset, value, offset, len);
    count += len;
    return this;
  }

  /** Get the length of the <code>String</code> this
   *  <code>StringBuffer</code> would create.  Not to be confused with the
   *  <em>capacity</em> of the <code>StringBuffer</code>.
   *  @return the length of this <code>StringBuffer</code>.
   *  @see #capacity()
   *  @see #setLength(int)
   */
  public int length ()
  {
    return count;
  }

  /** Replace characters between index <code>start</code> (inclusive) and 
   *  <code>end</code> (exclusive) with <code>str</code>. If <code>end</code> 
   *  is larger than the size of this StringBuffer, all characters after
   *  <code>start</code> are replaced.
   *  @param start the beginning index of characters to delete (inclusive).
   *  @param end the ending index of characters to delete (exclusive).
   *  @param str the new <code>String</code> to insert.
   *  @return this <code>StringBuffer</code>.
   */
  public synchronized StringBuffer replace (int start, int end, String str)
  {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException (start);
  
    int len = str.length();
    // Calculate the difference in 'count' after the replace.
    int delta = len - ((end > count ? count : end) - start);
    ensureCapacity_unsynchronized (count + delta);
        
    if (delta != 0 && end < count)
      System.arraycopy(value, end, value, end + delta, count - end);
    
    str.getChars (0, len, value, start);    
    count += delta;    
    return this;    
  }

  /** Reverse the characters in this StringBuffer.
   *  @return this <code>StringBuffer</code>.
   */
  public synchronized StringBuffer reverse ()
  {
    // Call ensureCapacity to enforce copy-on-write.
    ensureCapacity_unsynchronized (count);
    for (int i = 0; i < count / 2; ++i)
      {
	char c = value[i];
	value[i] = value[count - i - 1];
	value[count - i - 1] = c;
      }
    return this;
  }

  /** Set the character at the specified index.
   *  @param index the index of the character to set starting at 0.
   *  @param ch the value to set that character to.
   *  @exception IndexOutOfBoundsException if the specified character
   *             index is not between 0 and length() - 1 (inclusive).
   */
  public synchronized void setCharAt (int index, char ch)
  {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException (index);
    // Call ensureCapacity to enforce copy-on-write.
    ensureCapacity_unsynchronized (count);
    value[index] = ch;
  }

  /** Set the length of this StringBuffer.
   *  <P>
   *  If the new length is greater than the current length, all the new
   *  characters are set to '\0'.
   *  <P>
   *  If the new length is less than the current length, the first
   *  <code>newLength</code> characters of the old array will be
   * @param newLength the new length
   * @exception IndexOutOfBoundsException if the new length is
   *            negative.
   * @see #length()
   */
  public synchronized void setLength (int newLength)
  {
    if (newLength < 0)
      throw new StringIndexOutOfBoundsException (newLength);

    ensureCapacity_unsynchronized (newLength);
    for (int i = count; i < newLength; ++i)
      value[i] = '\0';
    count = newLength;
  }

  /** Create a new StringBuffer with default capacity 16.
   *  @see JLS 20.13.1
   */
  public StringBuffer ()
  {
    this (DEFAULT_CAPACITY);
  }

  /** Create an empty <code>StringBuffer</code> with the specified initial capacity.
   *  @param capacity the initial capacity.
   */
  public StringBuffer (int capacity)
  {
    count = 0;
    value = new char[capacity];
    shared = false;
  }

  /** Create a new <code>StringBuffer</code> with the characters in the specified <code>String</code>.
   *  Initial capacity will be the size of the String plus 16.
   *  @param str the <code>String</code> to make a <code>StringBuffer</code> out of.
   *  @XXX optimize for sharing.
   */
  public StringBuffer (String str)
  {
    // The documentation is not clear, but experimentation with
    // other implementations indicates that StringBuffer(null)
    // should throw a NullPointerException.
    count = str.length();
    // JLS: The initial capacity of the string buffer is 16 plus the
    // length of the argument string.
    value = new char[count + DEFAULT_CAPACITY];
    str.getChars(0, count, value, 0);
    shared = false;
  }

  /**
   * Creates a substring of this StringBuffer, starting at a specified index
   * and ending at the end of this StringBuffer.
   *
   * @param beginIndex index to start substring (base 0)
   * 
   * @return new String which is a substring of this StringBuffer
   *
   * @exception StringIndexOutOfBoundsException 
   *   if (beginIndex < 0 || beginIndex > this.length())
   */
  public String substring (int beginIndex)
  {
    return substring (beginIndex, count);
  }

  /**
   * Creates a substring of this StringBuffer, starting at a specified index
   * and ending at one character before a specified index.
   *
   * @param beginIndex index to start substring (base 0)
   * @param endIndex index after the last character to be 
   *   copied into the substring
   * 
   * @return new String which is a substring of this StringBuffer
   *
   * @exception StringIndexOutOfBoundsException 
   *   if (beginIndex < 0 || endIndex > this.length() || beginIndex > endIndex)
   */
  public synchronized String substring (int beginIndex, int endIndex) 
  {
    if (beginIndex < 0 || endIndex > count || beginIndex > endIndex)
      throw new StringIndexOutOfBoundsException ();
    // FIXME: for libgcj it would be possible, and more efficient, to
    // enable sharing here.
    return new String (value, beginIndex, endIndex - beginIndex);
  }

  /**
   * Creates a substring of this StringBuffer, starting at a specified index
   * and ending at one character before a specified index.
   * <p>
   * To implement <code>CharSequence</code>.
   * Calls <code>substring(beginIndex, endIndex)</code>.
   *
   * @param beginIndex index to start substring (base 0)
   * @param endIndex index after the last character to be 
   *   copied into the substring
   * 
   * @return new String which is a substring of this StringBuffer
   *
   * @exception StringIndexOutOfBoundsException 
   *   if (beginIndex < 0 || endIndex > this.length() || beginIndex > endIndex)
   */
  public CharSequence subSequence (int beginIndex, int endIndex) 
  {
    return substring(beginIndex, endIndex);
  }


  /** Convert this <code>StringBuffer</code> to a <code>String</code>.
   *  @return the characters in this StringBuffer
   */
  public String toString ()
  {
    // Note: in libgcj this causes the StringBuffer to be shared.  In
    // Classpath it does not.
    return new String (this);
  }

  // Index of next available character.  Note that this has
  // permissions set this way so that String can get the value.
  int count;

  // The buffer.  Note that this has permissions set this way so that
  // String can get the value.
  char[] value;

  // True if we need to copy the buffer before writing to it again.
  // FIXME: JDK 1.2 doesn't specify this.  The new buffer-growing
  // semantics make this less useful in that case, too.  Note that
  // this has permissions set this way so that String can get the
  // value.
  boolean shared;

  static final long serialVersionUID = 3388685877147921107L;
  private final static int DEFAULT_CAPACITY = 16; // JLS 20.13.1
}
