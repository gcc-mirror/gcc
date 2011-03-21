/* StringBuffer.java -- Growable strings
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2008
   Free Software Foundation, Inc.

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

package java.lang;

import java.io.Serializable;

/**
 * <code>StringBuffer</code> represents a changeable <code>String</code>.
 * It provides the operations required to modify the
 * <code>StringBuffer</code>, including insert, replace, delete, append,
 * and reverse. It is thread-safe; meaning that all modifications to a buffer
 * are in synchronized methods.
 *
 * <p><code>StringBuffer</code>s are variable-length in nature, so even if
 * you initialize them to a certain size, they can still grow larger than
 * that. <em>Capacity</em> indicates the number of characters the
 * <code>StringBuffer</code> can have in it before it has to grow (growing
 * the char array is an expensive operation involving <code>new</code>).
 *
 * <p>Incidentally, compilers often implement the String operator "+"
 * by using a <code>StringBuffer</code> operation:<br>
 * <code>a + b</code><br>
 * is the same as<br>
 * <code>new StringBuffer().append(a).append(b).toString()</code>.
 *
 * <p>Classpath's StringBuffer is capable of sharing memory with Strings for
 * efficiency.  This will help when a StringBuffer is converted to a String
 * and the StringBuffer is not changed after that (quite common when performing
 * string concatenation).
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Tom Tromey
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see String
 * @since 1.0
 * @status updated to 1.4
 */
public final class StringBuffer
  extends AbstractStringBuffer
  implements Serializable, CharSequence, Appendable
{
  // Implementation note: if you change this class, you usually will
  // want to change StringBuilder as well.

  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 3388685877147921107L;

  /**
   * True if the buffer is shared with another object (StringBuffer or
   * String); this means the buffer must be copied before writing to it again.
   * Note that this has permissions set this way so that String can get the
   * value.
   *
   * @serial whether the buffer is shared
   */
  boolean shared;

  /**
   * Create a new StringBuffer with default capacity 16.
   */
  public StringBuffer()
  {
    super();
  }

  /**
   * Create an empty <code>StringBuffer</code> with the specified initial
   * capacity.
   *
   * @param capacity the initial capacity
   * @throws NegativeArraySizeException if capacity is negative
   */
  public StringBuffer(int capacity)
  {
    super(capacity);
  }

  /**
   * Create a new <code>StringBuffer</code> with the characters in the
   * specified <code>String</code>. Initial capacity will be the size of the
   * String plus 16.
   *
   * @param str the <code>String</code> to convert
   * @throws NullPointerException if str is null
   */
  public StringBuffer(String str)
  {
    // Unfortunately, because the size is 16 larger, we cannot share.
    super(str);
  }

  /**
   * Create a new <code>StringBuffer</code> with the characters in the
   * specified <code>CharSequence</code>. Initial capacity will be the
   * length of the sequence plus 16; if the sequence reports a length
   * less than or equal to 0, then the initial capacity will be 16.
   *
   * @param seq the initializing <code>CharSequence</code>
   * @throws NullPointerException if str is null
   * @since 1.5
   */
  public StringBuffer(CharSequence seq)
  {
    super(seq);
  }

  /**
   * Get the length of the <code>String</code> this <code>StringBuffer</code>
   * would create. Not to be confused with the <em>capacity</em> of the
   * <code>StringBuffer</code>.
   *
   * @return the length of this <code>StringBuffer</code>
   * @see #capacity()
   * @see #setLength(int)
   */
  public synchronized int length()
  {
    return count;
  }

  /**
   * Get the total number of characters this <code>StringBuffer</code> can
   * support before it must be grown.  Not to be confused with <em>length</em>.
   *
   * @return the capacity of this <code>StringBuffer</code>
   * @see #length()
   * @see #ensureCapacity(int)
   */
  public synchronized int capacity()
  {
    return value.length;
  }

  /**
   * Increase the capacity of this <code>StringBuffer</code>. This will
   * ensure that an expensive growing operation will not occur until
   * <code>minimumCapacity</code> is reached. The buffer is grown to the
   * larger of <code>minimumCapacity</code> and
   * <code>capacity() * 2 + 2</code>, if it is not already large enough.
   *
   * @param minimumCapacity the new capacity
   * @see #capacity()
   */
  public synchronized void ensureCapacity(int minimumCapacity)
  {
    ensureCapacity_unsynchronized(minimumCapacity);
  }

  /**
   * Set the length of this StringBuffer. If the new length is greater than
   * the current length, all the new characters are set to '\0'. If the new
   * length is less than the current length, the first <code>newLength</code>
   * characters of the old array will be preserved, and the remaining
   * characters are truncated.
   *
   * @param newLength the new length
   * @throws IndexOutOfBoundsException if the new length is negative
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   * @see #length()
   */
  public synchronized void setLength(int newLength)
  {
    super.setLength(newLength);
  }

  /**
   * Get the character at the specified index.
   *
   * @param index the index of the character to get, starting at 0
   * @return the character at the specified index
   * @throws IndexOutOfBoundsException if index is negative or &gt;= length()
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public synchronized char charAt(int index)
  {
    return super.charAt(index);
  }

  /**
   * Get the code point at the specified index.  This is like #charAt(int),
   * but if the character is the start of a surrogate pair, and the
   * following character completes the pair, then the corresponding
   * supplementary code point is returned.
   * @param index the index of the codepoint to get, starting at 0
   * @return the codepoint at the specified index
   * @throws IndexOutOfBoundsException if index is negative or &gt;= length()
   * @since 1.5
   */
  public synchronized int codePointAt(int index)
  {
    return super.codePointAt(index);
  }

  /**
   * Get the code point before the specified index.  This is like
   * #codePointAt(int), but checks the characters at <code>index-1</code> and
   * <code>index-2</code> to see if they form a supplementary code point.
   * @param index the index just past the codepoint to get, starting at 0
   * @return the codepoint at the specified index
   * @throws IndexOutOfBoundsException if index is negative or &gt;= length()
   * @since 1.5
   */
  public synchronized int codePointBefore(int index)
  {
    return super.codePointBefore(index);
  }

  /**
   * Get the specified array of characters. <code>srcOffset - srcEnd</code>
   * characters will be copied into the array you pass in.
   *
   * @param srcOffset the index to start copying from (inclusive)
   * @param srcEnd the index to stop copying from (exclusive)
   * @param dst the array to copy into
   * @param dstOffset the index to start copying into
   * @throws NullPointerException if dst is null
   * @throws IndexOutOfBoundsException if any source or target indices are
   *         out of range (while unspecified, source problems cause a
   *         StringIndexOutOfBoundsException, and dest problems cause an
   *         ArrayIndexOutOfBoundsException)
   * @see System#arraycopy(Object, int, Object, int, int)
   */
  public synchronized void getChars(int srcOffset, int srcEnd,
                                    char[] dst, int dstOffset)
  {
    super.getChars(srcOffset, srcEnd, dst, dstOffset);
  }

  /**
   * Set the character at the specified index.
   *
   * @param index the index of the character to set starting at 0
   * @param ch the value to set that character to
   * @throws IndexOutOfBoundsException if index is negative or &gt;= length()
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public synchronized void setCharAt(int index, char ch)
  {
    super.setCharAt(index, ch);
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param obj the <code>Object</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(Object)
   * @see #append(String)
   */
  public synchronized StringBuffer append(Object obj)
  {
    super.append(obj);
    return this;
  }

  /**
   * Append the <code>String</code> to this <code>StringBuffer</code>. If
   * str is null, the String "null" is appended.
   *
   * @param str the <code>String</code> to append
   * @return this <code>StringBuffer</code>
   */
  public synchronized StringBuffer append(String str)
  {
    super.append(str);
    return this;
  }

  /**
   * Append the <code>StringBuffer</code> value of the argument to this
   * <code>StringBuffer</code>. This behaves the same as
   * <code>append((Object) stringBuffer)</code>, except it is more efficient.
   *
   * @param stringBuffer the <code>StringBuffer</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see #append(Object)
   * @since 1.4
   */
  public synchronized StringBuffer append(StringBuffer stringBuffer)
  {
    super.append(stringBuffer);
    return this;
  }

  /**
   * Append the <code>char</code> array to this <code>StringBuffer</code>.
   * This is similar (but more efficient) than
   * <code>append(new String(data))</code>, except in the case of null.
   *
   * @param data the <code>char[]</code> to append
   * @return this <code>StringBuffer</code>
   * @throws NullPointerException if <code>str</code> is <code>null</code>
   * @see #append(char[], int, int)
   */
  public synchronized StringBuffer append(char[] data)
  {
    super.append(data, 0, data.length);
    return this;
  }

  /**
   * Append part of the <code>char</code> array to this
   * <code>StringBuffer</code>. This is similar (but more efficient) than
   * <code>append(new String(data, offset, count))</code>, except in the case
   * of null.
   *
   * @param data the <code>char[]</code> to append
   * @param offset the start location in <code>str</code>
   * @param count the number of characters to get from <code>str</code>
   * @return this <code>StringBuffer</code>
   * @throws NullPointerException if <code>str</code> is <code>null</code>
   * @throws IndexOutOfBoundsException if offset or count is out of range
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public synchronized StringBuffer append(char[] data, int offset, int count)
  {
    super.append(data, offset, count);
    return this;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param bool the <code>boolean</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(boolean)
   */
  public synchronized StringBuffer append(boolean bool)
  {
    super.append(bool);
    return this;
  }

  /**
   * Append the <code>char</code> to this <code>StringBuffer</code>.
   *
   * @param ch the <code>char</code> to append
   * @return this <code>StringBuffer</code>
   */
  public synchronized StringBuffer append(char ch)
  {
    super.append(ch);
    return this;
  }

  /**
   * Append the characters in the <code>CharSequence</code> to this
   * buffer.
   *
   * @param seq the <code>CharSequence</code> providing the characters
   * @return this <code>StringBuffer</code>
   * @since 1.5
   */
  public synchronized StringBuffer append(CharSequence seq)
  {
    super.append(seq, 0, seq.length());
    return this;
  }

  /**
   * Append some characters from the <code>CharSequence</code> to this
   * buffer.  If the argument is null, the four characters "null" are
   * appended.
   *
   * @param seq the <code>CharSequence</code> providing the characters
   * @param start the starting index
   * @param end one past the final index
   * @return this <code>StringBuffer</code>
   * @since 1.5
   */
  public synchronized StringBuffer append(CharSequence seq, int start, int end)
  {
    super.append(seq, start, end);
    return this;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param inum the <code>int</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(int)
   */
  // This is native in libgcj, for efficiency.
  public synchronized StringBuffer append(int inum)
  {
    super.append(inum);
    return this;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param lnum the <code>long</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(long)
   */
  public synchronized StringBuffer append(long lnum)
  {
    super.append(lnum);
    return this;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param fnum the <code>float</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(float)
   */
  public synchronized StringBuffer append(float fnum)
  {
    super.append(fnum);
    return this;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param dnum the <code>double</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(double)
   */
  public synchronized StringBuffer append(double dnum)
  {
    super.append(dnum);
    return this;
  }

  /**
   * Append the code point to this <code>StringBuffer</code>.
   * This is like #append(char), but will append two characters
   * if a supplementary code point is given.
   *
   * @param code the code point to append
   * @return this <code>StringBuffer</code>
   * @see Character#toChars(int, char[], int)
   * @since 1.5
   */
  public synchronized StringBuffer appendCodePoint(int code)
  {
    super.appendCodePoint(code);
    return this;
  }

  /**
   * Delete characters from this <code>StringBuffer</code>.
   * <code>delete(10, 12)</code> will delete 10 and 11, but not 12. It is
   * harmless for end to be larger than length().
   *
   * @param start the first character to delete
   * @param end the index after the last character to delete
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if start or end are out of bounds
   * @since 1.2
   */
  public synchronized StringBuffer delete(int start, int end)
  {
    // This will unshare if required.
    super.delete(start, end);
    return this;
  }

  /**
   * Delete a character from this <code>StringBuffer</code>.
   *
   * @param index the index of the character to delete
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if index is out of bounds
   * @since 1.2
   */
  public synchronized StringBuffer deleteCharAt(int index)
  {
    super.deleteCharAt(index);
    return this;
  }

  /**
   * Replace characters between index <code>start</code> (inclusive) and
   * <code>end</code> (exclusive) with <code>str</code>. If <code>end</code>
   * is larger than the size of this StringBuffer, all characters after
   * <code>start</code> are replaced.
   *
   * @param start the beginning index of characters to delete (inclusive)
   * @param end the ending index of characters to delete (exclusive)
   * @param str the new <code>String</code> to insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if start or end are out of bounds
   * @throws NullPointerException if str is null
   * @since 1.2
   */
  public synchronized StringBuffer replace(int start, int end, String str)
  {
    super.replace(start, end, str);
    return this;
  }

  /**
   * Creates a substring of this StringBuffer, starting at a specified index
   * and ending at the end of this StringBuffer.
   *
   * @param beginIndex index to start substring (base 0)
   * @return new String which is a substring of this StringBuffer
   * @throws StringIndexOutOfBoundsException if beginIndex is out of bounds
   * @see #substring(int, int)
   * @since 1.2
   */
  public String substring(int beginIndex)
  {
    return substring(beginIndex, count);
  }

  /**
   * Creates a substring of this StringBuffer, starting at a specified index
   * and ending at one character before a specified index. This is implemented
   * the same as <code>substring(beginIndex, endIndex)</code>, to satisfy
   * the CharSequence interface.
   *
   * @param beginIndex index to start at (inclusive, base 0)
   * @param endIndex index to end at (exclusive)
   * @return new String which is a substring of this StringBuffer
   * @throws IndexOutOfBoundsException if beginIndex or endIndex is out of
   *         bounds
   * @see #substring(int, int)
   * @since 1.4
   */
  public CharSequence subSequence(int beginIndex, int endIndex)
  {
    return substring(beginIndex, endIndex);
  }

  /**
   * Creates a substring of this StringBuffer, starting at a specified index
   * and ending at one character before a specified index.
   *
   * @param beginIndex index to start at (inclusive, base 0)
   * @param endIndex index to end at (exclusive)
   * @return new String which is a substring of this StringBuffer
   * @throws StringIndexOutOfBoundsException if beginIndex or endIndex is out
   *         of bounds
   * @since 1.2
   */
  public synchronized String substring(int beginIndex, int endIndex)
  {
    int len = endIndex - beginIndex;
    if (beginIndex < 0 || endIndex > count || endIndex < beginIndex)
      throw new StringIndexOutOfBoundsException();
    if (len == 0)
      return "";
    // Don't copy unless substring is smaller than 1/4 of the buffer.
    boolean share_buffer = ((len << 2) >= value.length);
    if (share_buffer)
      this.shared = true;
    // Package constructor avoids an array copy.
    return new String(value, beginIndex, len, share_buffer);
  }

  /**
   * Insert a subarray of the <code>char[]</code> argument into this
   * <code>StringBuffer</code>.
   *
   * @param offset the place to insert in this buffer
   * @param str the <code>char[]</code> to insert
   * @param str_offset the index in <code>str</code> to start inserting from
   * @param len the number of characters to insert
   * @return this <code>StringBuffer</code>
   * @throws NullPointerException if <code>str</code> is <code>null</code>
   * @throws StringIndexOutOfBoundsException if any index is out of bounds
   * @since 1.2
   */
  public synchronized StringBuffer insert(int offset,
                                          char[] str, int str_offset, int len)
  {
    super.insert(offset, str, str_offset, len);
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param obj the <code>Object</code> to convert and insert
   * @return this <code>StringBuffer</code>
   * @exception StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(Object)
   */
  public synchronized StringBuffer insert(int offset, Object obj)
  {
    super.insert(offset, obj);
    return this;
  }

  /**
   * Insert the <code>String</code> argument into this
   * <code>StringBuffer</code>. If str is null, the String "null" is used
   * instead.
   *
   * @param offset the place to insert in this buffer
   * @param str the <code>String</code> to insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   */
  public synchronized StringBuffer insert(int offset, String str)
  {
    super.insert(offset, str);
    return this;
  }

  /**
   * Insert the <code>CharSequence</code> argument into this
   * <code>StringBuffer</code>.  If the sequence is null, the String
   * "null" is used instead.
   *
   * @param offset the place to insert in this buffer
   * @param sequence the <code>CharSequence</code> to insert
   * @return this <code>StringBuffer</code>
   * @throws IndexOutOfBoundsException if offset is out of bounds
   * @since 1.5
   */
  public synchronized StringBuffer insert(int offset, CharSequence sequence)
  {
    super.insert(offset, sequence);
    return this;
  }

  /**
   * Insert a subsequence of the <code>CharSequence</code> argument into this
   * <code>StringBuffer</code>.  If the sequence is null, the String
   * "null" is used instead.
   *
   * @param offset the place to insert in this buffer
   * @param sequence the <code>CharSequence</code> to insert
   * @param start the starting index of the subsequence
   * @param end one past the ending index of the subsequence
   * @return this <code>StringBuffer</code>
   * @throws IndexOutOfBoundsException if offset, start,
   * or end are out of bounds
   * @since 1.5
   */
  public synchronized StringBuffer insert(int offset, CharSequence sequence,
                                          int start, int end)
  {
    super.insert(offset, sequence, start, end);
    return this;
  }

  /**
   * Insert the <code>char[]</code> argument into this
   * <code>StringBuffer</code>.
   *
   * @param offset the place to insert in this buffer
   * @param data the <code>char[]</code> to insert
   * @return this <code>StringBuffer</code>
   * @throws NullPointerException if <code>data</code> is <code>null</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see #insert(int, char[], int, int)
   */
  public synchronized StringBuffer insert(int offset, char[] data)
  {
    super.insert(offset, data, 0, data.length);
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param bool the <code>boolean</code> to convert and insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(boolean)
   */
  public synchronized StringBuffer insert(int offset, boolean bool)
  {
    super.insert(offset, bool);
    return this;
  }

  /**
   * Insert the <code>char</code> argument into this <code>StringBuffer</code>.
   *
   * @param offset the place to insert in this buffer
   * @param ch the <code>char</code> to insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   */
  public synchronized StringBuffer insert(int offset, char ch)
  {
    super.insert(offset, ch);
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param inum the <code>int</code> to convert and insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(int)
   */
  public synchronized StringBuffer insert(int offset, int inum)
  {
    super.insert(offset, inum);
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param lnum the <code>long</code> to convert and insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(long)
   */
  public synchronized StringBuffer insert(int offset, long lnum)
  {
    super.insert(offset, lnum);
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param fnum the <code>float</code> to convert and insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(float)
   */
  public synchronized StringBuffer insert(int offset, float fnum)
  {
    super.insert(offset, fnum);
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param dnum the <code>double</code> to convert and insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(double)
   */
  public synchronized StringBuffer insert(int offset, double dnum)
  {
    super.insert(offset, dnum);
    return this;
  }

  /**
   * Finds the first instance of a substring in this StringBuffer.
   *
   * @param str String to find
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   * @see #indexOf(String, int)
   * @since 1.4
   */
  public synchronized int indexOf(String str)
  {
    return super.indexOf(str, 0);
  }

  /**
   * Finds the first instance of a String in this StringBuffer, starting at
   * a given index.  If starting index is less than 0, the search starts at
   * the beginning of this String.  If the starting index is greater than the
   * length of this String, or the substring is not found, -1 is returned.
   *
   * @param str String to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   * @since 1.4
   */
  public synchronized int indexOf(String str, int fromIndex)
  {
    return super.indexOf(str, fromIndex);
  }

  /**
   * Finds the last instance of a substring in this StringBuffer.
   *
   * @param str String to find
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   * @see #lastIndexOf(String, int)
   * @since 1.4
   */
  public synchronized int lastIndexOf(String str)
  {
    return super.lastIndexOf(str, count - str.count);
  }

  /**
   * Finds the last instance of a String in this StringBuffer, starting at a
   * given index.  If starting index is greater than the maximum valid index,
   * then the search begins at the end of this String.  If the starting index
   * is less than zero, or the substring is not found, -1 is returned.
   *
   * @param str String to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   * @since 1.4
   */
  public synchronized int lastIndexOf(String str, int fromIndex)
  {
    return super.lastIndexOf(str, fromIndex);
  }

  /**
   * Reverse the characters in this StringBuffer. The same sequence of
   * characters exists, but in the reverse index ordering.
   *
   * @return this <code>StringBuffer</code>
   */
  public synchronized StringBuffer reverse()
  {
    super.reverse();
    return this;
  }

  /**
   * Convert this <code>StringBuffer</code> to a <code>String</code>. The
   * String is composed of the characters currently in this StringBuffer. Note
   * that the result is a copy, and that future modifications to this buffer
   * do not affect the String.
   *
   * @return the characters in this StringBuffer
   */
  public String toString()
  {
    // The string will set this.shared = true.
    return new String(this);
  }

  /**
   * This may reduce the amount of memory used by the StringBuffer,
   * by resizing the internal array to remove unused space.  However,
   * this method is not required to resize, so this behavior cannot
   * be relied upon.
   * @since 1.5
   */
  public synchronized void trimToSize()
  {
    super.trimToSize();
  }

  /**
   * Return the number of code points between two indices in the
   * <code>StringBuffer</code>.  An unpaired surrogate counts as a
   * code point for this purpose.  Characters outside the indicated
   * range are not examined, even if the range ends in the middle of a
   * surrogate pair.
   *
   * @param start the starting index
   * @param end one past the ending index
   * @return the number of code points
   * @since 1.5
   */
  public synchronized int codePointCount(int start, int end)
  {
    return super.codePointCount(start, end);
  }

  /**
   * Starting at the given index, this counts forward by the indicated
   * number of code points, and then returns the resulting index.  An
   * unpaired surrogate counts as a single code point for this
   * purpose.
   *
   * @param start the starting index
   * @param codePoints the number of code points
   * @return the resulting index
   * @since 1.5
   */
  public synchronized int offsetByCodePoints(int start, int codePoints)
  {
    return super.offsetByCodePoints(start, codePoints);
  }

  /**
   * An unsynchronized version of ensureCapacity, used internally to avoid
   * the cost of a second lock on the same object. This also has the side
   * effect of duplicating the array, if it was shared (to form copy-on-write
   * semantics).
   *
   * @param minimumCapacity the minimum capacity
   * @see #ensureCapacity(int)
   */
  void ensureCapacity_unsynchronized(int minimumCapacity)
  {
    if (shared || minimumCapacity > value.length)
      {
        // We don't want to make a larger vector when `shared' is
        // set.  If we do, then setLength becomes very inefficient
        // when repeatedly reusing a StringBuffer in a loop.
        int max = (minimumCapacity > value.length
                   ? value.length * 2 + 2
                   : value.length);
        minimumCapacity = (minimumCapacity < max ? max : minimumCapacity);
        char[] nb = new char[minimumCapacity];
        VMSystem.arraycopy(value, 0, nb, 0, count);
        value = nb;
        shared = false;
      }
  }

}
