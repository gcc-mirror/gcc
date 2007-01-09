/* StringBuilder.java -- Unsynchronized growable strings
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
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
 * <code>StringBuilder</code> represents a changeable <code>String</code>.
 * It provides the operations required to modify the
 * <code>StringBuilder</code>, including insert, replace, delete, append,
 * and reverse. It like <code>StringBuffer</code>, but is not
 * synchronized.  It is ideal for use when it is known that the
 * object will only be used from a single thread.
 *
 * <p><code>StringBuilder</code>s are variable-length in nature, so even if
 * you initialize them to a certain size, they can still grow larger than
 * that. <em>Capacity</em> indicates the number of characters the
 * <code>StringBuilder</code> can have in it before it has to grow (growing
 * the char array is an expensive operation involving <code>new</code>).
 *
 * <p>Incidentally, compilers often implement the String operator "+"
 * by using a <code>StringBuilder</code> operation:<br>
 * <code>a + b</code><br>
 * is the same as<br>
 * <code>new StringBuilder().append(a).append(b).toString()</code>.
 *
 * <p>Classpath's StringBuilder is capable of sharing memory with Strings for
 * efficiency.  This will help when a StringBuilder is converted to a String
 * and the StringBuilder is not changed after that (quite common when
 * performing string concatenation).
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Tom Tromey
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see String
 * @see StringBuffer
 *
 * @since 1.5
 */
public final class StringBuilder
  implements Serializable, CharSequence, Appendable
{
  // Implementation note: if you change this class, you usually will
  // want to change StringBuffer as well.

  /**
   * For compatability with Sun's JDK
   */
  private static final long serialVersionUID = 4383685877147921099L;

  /**
   * Index of next available character (and thus the size of the current
   * string contents).  Note that this has permissions set this way so that
   * String can get the value.
   *
   * @serial the number of characters in the buffer
   */
  int count;

  /**
   * The buffer.  Note that this has permissions set this way so that String
   * can get the value.
   *
   * @serial the buffer
   */
  char[] value;

  /**
   * The default capacity of a buffer.
   */
  private static final int DEFAULT_CAPACITY = 16;

  /**
   * Create a new StringBuilder with default capacity 16.
   */
  public StringBuilder()
  {
    this(DEFAULT_CAPACITY);
  }

  /**
   * Create an empty <code>StringBuilder</code> with the specified initial
   * capacity.
   *
   * @param capacity the initial capacity
   * @throws NegativeArraySizeException if capacity is negative
   */
  public StringBuilder(int capacity)
  {
    value = new char[capacity];
  }

  /**
   * Create a new <code>StringBuilder</code> with the characters in the
   * specified <code>String</code>. Initial capacity will be the size of the
   * String plus 16.
   *
   * @param str the <code>String</code> to convert
   * @throws NullPointerException if str is null
   */
  public StringBuilder(String str)
  {
    // Unfortunately, because the size is 16 larger, we cannot share.
    count = str.count;
    value = new char[count + DEFAULT_CAPACITY];
    str.getChars(0, count, value, 0);
  }

  /**
   * Create a new <code>StringBuilder</code> with the characters in the
   * specified <code>CharSequence</code>. Initial capacity will be the
   * length of the sequence plus 16; if the sequence reports a length
   * less than or equal to 0, then the initial capacity will be 16.
   *
   * @param seq the initializing <code>CharSequence</code>
   * @throws NullPointerException if str is null
   */
  public StringBuilder(CharSequence seq)
  {
    int len = seq.length();
    count = len <= 0 ? 0 : len;
    value = new char[count + DEFAULT_CAPACITY];
    for (int i = 0; i < len; ++i)
      value[i] = seq.charAt(i);
  }

  /**
   * Get the length of the <code>String</code> this <code>StringBuilder</code>
   * would create. Not to be confused with the <em>capacity</em> of the
   * <code>StringBuilder</code>.
   *
   * @return the length of this <code>StringBuilder</code>
   * @see #capacity()
   * @see #setLength(int)
   */
  public int length()
  {
    return count;
  }

  /**
   * Get the total number of characters this <code>StringBuilder</code> can
   * support before it must be grown.  Not to be confused with <em>length</em>.
   *
   * @return the capacity of this <code>StringBuilder</code>
   * @see #length()
   * @see #ensureCapacity(int)
   */
  public int capacity()
  {
    return value.length;
  }

  /**
   * Increase the capacity of this <code>StringBuilder</code>. This will
   * ensure that an expensive growing operation will not occur until
   * <code>minimumCapacity</code> is reached. The buffer is grown to the
   * larger of <code>minimumCapacity</code> and
   * <code>capacity() * 2 + 2</code>, if it is not already large enough.
   *
   * @param minimumCapacity the new capacity
   * @see #capacity()
   */
  public void ensureCapacity(int minimumCapacity)
  {
    if (minimumCapacity > value.length)
      {
        int max = value.length * 2 + 2;
        minimumCapacity = (minimumCapacity < max ? max : minimumCapacity);
        char[] nb = new char[minimumCapacity];
        VMSystem.arraycopy(value, 0, nb, 0, count);
        value = nb;
      }
  }

  /**
   * Set the length of this StringBuilder. If the new length is greater than
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
  public void setLength(int newLength)
  {
    if (newLength < 0)
      throw new StringIndexOutOfBoundsException(newLength);

    int valueLength = value.length;

    /* Always call ensureCapacity in order to preserve copy-on-write
       semantics.  */
    ensureCapacity(newLength);

    if (newLength < valueLength)
      {
        /* If the StringBuilder's value just grew, then we know that
           value is newly allocated and the region between count and
           newLength is filled with '\0'.  */
	count = newLength;
      }
    else
      {
	/* The StringBuilder's value doesn't need to grow.  However,
	   we should clear out any cruft that may exist.  */
	while (count < newLength)
          value[count++] = '\0';
      }
  }

  /**
   * Get the character at the specified index.
   *
   * @param index the index of the character to get, starting at 0
   * @return the character at the specified index
   * @throws IndexOutOfBoundsException if index is negative or &gt;= length()
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public char charAt(int index)
  {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index);
    return value[index];
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
  public void getChars(int srcOffset, int srcEnd,
		       char[] dst, int dstOffset)
  {
    if (srcOffset < 0 || srcEnd > count || srcEnd < srcOffset)
      throw new StringIndexOutOfBoundsException();
    VMSystem.arraycopy(value, srcOffset, dst, dstOffset, srcEnd - srcOffset);
  }

  /**
   * Set the character at the specified index.
   *
   * @param index the index of the character to set starting at 0
   * @param ch the value to set that character to
   * @throws IndexOutOfBoundsException if index is negative or &gt;= length()
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public void setCharAt(int index, char ch)
  {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index);
    // Call ensureCapacity to enforce copy-on-write.
    ensureCapacity(count);
    value[index] = ch;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param obj the <code>Object</code> to convert and append
   * @return this <code>StringBuilder</code>
   * @see String#valueOf(Object)
   * @see #append(String)
   */
  public StringBuilder append(Object obj)
  {
    return append(obj == null ? "null" : obj.toString());
  }

  /**
   * Append the <code>String</code> to this <code>StringBuilder</code>. If
   * str is null, the String "null" is appended.
   *
   * @param str the <code>String</code> to append
   * @return this <code>StringBuilder</code>
   */
  public StringBuilder append(String str)
  {
    if (str == null)
      str = "null";
    int len = str.count;
    ensureCapacity(count + len);
    str.getChars(0, len, value, count);
    count += len;
    return this;
  }

  /**
   * Append the <code>StringBuilder</code> value of the argument to this
   * <code>StringBuilder</code>. This behaves the same as
   * <code>append((Object) stringBuffer)</code>, except it is more efficient.
   *
   * @param stringBuffer the <code>StringBuilder</code> to convert and append
   * @return this <code>StringBuilder</code>
   * @see #append(Object)
   */
  public StringBuilder append(StringBuffer stringBuffer)
  {
    if (stringBuffer == null)
      return append("null");
    synchronized (stringBuffer)
      {
	int len = stringBuffer.count;
	ensureCapacity(count + len);
	VMSystem.arraycopy(stringBuffer.value, 0, value, count, len);
	count += len;
      }
    return this;
  }

  /**
   * Append the <code>char</code> array to this <code>StringBuilder</code>.
   * This is similar (but more efficient) than
   * <code>append(new String(data))</code>, except in the case of null.
   *
   * @param data the <code>char[]</code> to append
   * @return this <code>StringBuilder</code>
   * @throws NullPointerException if <code>str</code> is <code>null</code>
   * @see #append(char[], int, int)
   */
  public StringBuilder append(char[] data)
  {
    return append(data, 0, data.length);
  }

  /**
   * Append part of the <code>char</code> array to this
   * <code>StringBuilder</code>. This is similar (but more efficient) than
   * <code>append(new String(data, offset, count))</code>, except in the case
   * of null.
   *
   * @param data the <code>char[]</code> to append
   * @param offset the start location in <code>str</code>
   * @param count the number of characters to get from <code>str</code>
   * @return this <code>StringBuilder</code>
   * @throws NullPointerException if <code>str</code> is <code>null</code>
   * @throws IndexOutOfBoundsException if offset or count is out of range
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public StringBuilder append(char[] data, int offset, int count)
  {
    if (offset < 0 || count < 0 || offset > data.length - count)
      throw new StringIndexOutOfBoundsException();
    ensureCapacity(this.count + count);
    VMSystem.arraycopy(data, offset, value, this.count, count);
    this.count += count;
    return this;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param bool the <code>boolean</code> to convert and append
   * @return this <code>StringBuilder</code>
   * @see String#valueOf(boolean)
   */
  public StringBuilder append(boolean bool)
  {
    return append(bool ? "true" : "false");
  }

  /**
   * Append the <code>char</code> to this <code>StringBuilder</code>.
   *
   * @param ch the <code>char</code> to append
   * @return this <code>StringBuilder</code>
   */
  public StringBuilder append(char ch)
  {
    ensureCapacity(count + 1);
    value[count++] = ch;
    return this;
  }

  /**
   * Append the characters in the <code>CharSequence</code> to this
   * buffer.
   *
   * @param seq the <code>CharSequence</code> providing the characters
   * @return this <code>StringBuilder</code>
   */
  public StringBuilder append(CharSequence seq)
  {
    return append(seq, 0, seq.length());
  }

  /**
   * Append some characters from the <code>CharSequence</code> to this
   * buffer.  If the argument is null, the four characters "null" are
   * appended.
   *
   * @param seq the <code>CharSequence</code> providing the characters
   * @param start the starting index
   * @param end one past the final index
   * @return this <code>StringBuilder</code>
   */
  public StringBuilder append(CharSequence seq, int start,
			      int end)
  {
    if (seq == null)
      return append("null");
    if (end - start > 0)
      {
	ensureCapacity(count + end - start);
	for (; start < end; ++start)
	  value[count++] = seq.charAt(start);
      }
    return this;
  }

  /**
   * Append the code point to this <code>StringBuilder</code>.
   * This is like #append(char), but will append two characters
   * if a supplementary code point is given.
   *
   * @param code the code point to append
   * @return this <code>StringBuilder</code>
   * @see Character#toChars(int, char[], int)
   * @since 1.5
   */
  public synchronized StringBuilder appendCodePoint(int code)
  {
    int len = Character.charCount(code);
    ensureCapacity(count + len);
    Character.toChars(code, value, count);
    count += len;
    return this;
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param inum the <code>int</code> to convert and append
   * @return this <code>StringBuilder</code>
   * @see String#valueOf(int)
   */
  // This is native in libgcj, for efficiency.
  public StringBuilder append(int inum)
  {
    return append(String.valueOf(inum));
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param lnum the <code>long</code> to convert and append
   * @return this <code>StringBuilder</code>
   * @see String#valueOf(long)
   */
  public StringBuilder append(long lnum)
  {
    return append(Long.toString(lnum, 10));
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param fnum the <code>float</code> to convert and append
   * @return this <code>StringBuilder</code>
   * @see String#valueOf(float)
   */
  public StringBuilder append(float fnum)
  {
    return append(Float.toString(fnum));
  }

  /**
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param dnum the <code>double</code> to convert and append
   * @return this <code>StringBuilder</code>
   * @see String#valueOf(double)
   */
  public StringBuilder append(double dnum)
  {
    return append(Double.toString(dnum));
  }

  /**
   * Delete characters from this <code>StringBuilder</code>.
   * <code>delete(10, 12)</code> will delete 10 and 11, but not 12. It is
   * harmless for end to be larger than length().
   *
   * @param start the first character to delete
   * @param end the index after the last character to delete
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if start or end are out of bounds
   */
  public StringBuilder delete(int start, int end)
  {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException(start);
    if (end > count)
      end = count;
    // This will unshare if required.
    ensureCapacity(count);
    if (count - end != 0)
      VMSystem.arraycopy(value, end, value, start, count - end);
    count -= end - start;
    return this;
  }

  /**
   * Delete a character from this <code>StringBuilder</code>.
   *
   * @param index the index of the character to delete
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if index is out of bounds
   */
  public StringBuilder deleteCharAt(int index)
  {
    return delete(index, index + 1);
  }

  /**
   * Replace characters between index <code>start</code> (inclusive) and
   * <code>end</code> (exclusive) with <code>str</code>. If <code>end</code>
   * is larger than the size of this StringBuilder, all characters after
   * <code>start</code> are replaced.
   *
   * @param start the beginning index of characters to delete (inclusive)
   * @param end the ending index of characters to delete (exclusive)
   * @param str the new <code>String</code> to insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if start or end are out of bounds
   * @throws NullPointerException if str is null
   */
  public StringBuilder replace(int start, int end, String str)
  {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException(start);

    int len = str.count;
    // Calculate the difference in 'count' after the replace.
    int delta = len - (end > count ? count : end) + start;
    ensureCapacity(count + delta);

    if (delta != 0 && end < count)
      VMSystem.arraycopy(value, end, value, end + delta, count - end);

    str.getChars(0, len, value, start);
    count += delta;
    return this;
  }

  /**
   * Creates a substring of this StringBuilder, starting at a specified index
   * and ending at the end of this StringBuilder.
   *
   * @param beginIndex index to start substring (base 0)
   * @return new String which is a substring of this StringBuilder
   * @throws StringIndexOutOfBoundsException if beginIndex is out of bounds
   * @see #substring(int, int)
   */
  public String substring(int beginIndex)
  {
    return substring(beginIndex, count);
  }

  /**
   * Creates a substring of this StringBuilder, starting at a specified index
   * and ending at one character before a specified index. This is implemented
   * the same as <code>substring(beginIndex, endIndex)</code>, to satisfy
   * the CharSequence interface.
   *
   * @param beginIndex index to start at (inclusive, base 0)
   * @param endIndex index to end at (exclusive)
   * @return new String which is a substring of this StringBuilder
   * @throws IndexOutOfBoundsException if beginIndex or endIndex is out of
   *         bounds
   * @see #substring(int, int)
   */
  public CharSequence subSequence(int beginIndex, int endIndex)
  {
    return substring(beginIndex, endIndex);
  }

  /**
   * Creates a substring of this StringBuilder, starting at a specified index
   * and ending at one character before a specified index.
   *
   * @param beginIndex index to start at (inclusive, base 0)
   * @param endIndex index to end at (exclusive)
   * @return new String which is a substring of this StringBuilder
   * @throws StringIndexOutOfBoundsException if beginIndex or endIndex is out
   *         of bounds
   */
  public String substring(int beginIndex, int endIndex)
  {
    int len = endIndex - beginIndex;
    if (beginIndex < 0 || endIndex > count || endIndex < beginIndex)
      throw new StringIndexOutOfBoundsException();
    if (len == 0)
      return "";
    return new String(value, beginIndex, len);
  }

  /**
   * Insert a subarray of the <code>char[]</code> argument into this
   * <code>StringBuilder</code>.
   *
   * @param offset the place to insert in this buffer
   * @param str the <code>char[]</code> to insert
   * @param str_offset the index in <code>str</code> to start inserting from
   * @param len the number of characters to insert
   * @return this <code>StringBuilder</code>
   * @throws NullPointerException if <code>str</code> is <code>null</code>
   * @throws StringIndexOutOfBoundsException if any index is out of bounds
   */
  public StringBuilder insert(int offset,
			      char[] str, int str_offset, int len)
  {
    if (offset < 0 || offset > count || len < 0
        || str_offset < 0 || str_offset > str.length - len)
      throw new StringIndexOutOfBoundsException();
    ensureCapacity(count + len);
    VMSystem.arraycopy(value, offset, value, offset + len, count - offset);
    VMSystem.arraycopy(str, str_offset, value, offset, len);
    count += len;
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param obj the <code>Object</code> to convert and insert
   * @return this <code>StringBuilder</code>
   * @exception StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(Object)
   */
  public StringBuilder insert(int offset, Object obj)
  {
    return insert(offset, obj == null ? "null" : obj.toString());
  }

  /**
   * Insert the <code>String</code> argument into this
   * <code>StringBuilder</code>. If str is null, the String "null" is used
   * instead.
   *
   * @param offset the place to insert in this buffer
   * @param str the <code>String</code> to insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   */
  public StringBuilder insert(int offset, String str)
  {
    if (offset < 0 || offset > count)
      throw new StringIndexOutOfBoundsException(offset);
    if (str == null)
      str = "null";
    int len = str.count;
    ensureCapacity(count + len);
    VMSystem.arraycopy(value, offset, value, offset + len, count - offset);
    str.getChars(0, len, value, offset);
    count += len;
    return this;
  }

  /**
   * Insert the <code>CharSequence</code> argument into this
   * <code>StringBuilder</code>.  If the sequence is null, the String
   * "null" is used instead.
   *
   * @param offset the place to insert in this buffer
   * @param sequence the <code>CharSequence</code> to insert
   * @return this <code>StringBuilder</code>
   * @throws IndexOutOfBoundsException if offset is out of bounds
   */
  public synchronized StringBuilder insert(int offset, CharSequence sequence)
  {
    if (sequence == null)
      sequence = "null";
    return insert(offset, sequence, 0, sequence.length());
  }

  /**
   * Insert a subsequence of the <code>CharSequence</code> argument into this
   * <code>StringBuilder</code>.  If the sequence is null, the String
   * "null" is used instead.
   *
   * @param offset the place to insert in this buffer
   * @param sequence the <code>CharSequence</code> to insert
   * @param start the starting index of the subsequence
   * @param end one past the ending index of the subsequence
   * @return this <code>StringBuilder</code>
   * @throws IndexOutOfBoundsException if offset, start,
   * or end are out of bounds
   */
  public synchronized StringBuilder insert(int offset, CharSequence sequence,
                      int start, int end)
  {
    if (sequence == null)
      sequence = "null";
    if (start < 0 || end < 0 || start > end || end > sequence.length())
      throw new IndexOutOfBoundsException();
    int len = end - start;
    ensureCapacity(count + len);
    VMSystem.arraycopy(value, offset, value, offset + len, count - offset);
    for (int i = start; i < end; ++i)
      value[offset++] = sequence.charAt(i);
    count += len;
    return this;
  }

  /**
   * Insert the <code>char[]</code> argument into this
   * <code>StringBuilder</code>.
   *
   * @param offset the place to insert in this buffer
   * @param data the <code>char[]</code> to insert
   * @return this <code>StringBuilder</code>
   * @throws NullPointerException if <code>data</code> is <code>null</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see #insert(int, char[], int, int)
   */
  public StringBuilder insert(int offset, char[] data)
  {
    return insert(offset, data, 0, data.length);
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param bool the <code>boolean</code> to convert and insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(boolean)
   */
  public StringBuilder insert(int offset, boolean bool)
  {
    return insert(offset, bool ? "true" : "false");
  }

  /**
   * Insert the <code>char</code> argument into this <code>StringBuilder</code>.
   *
   * @param offset the place to insert in this buffer
   * @param ch the <code>char</code> to insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   */
  public StringBuilder insert(int offset, char ch)
  {
    if (offset < 0 || offset > count)
      throw new StringIndexOutOfBoundsException(offset);
    ensureCapacity(count + 1);
    VMSystem.arraycopy(value, offset, value, offset + 1, count - offset);
    value[offset] = ch;
    count++;
    return this;
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param inum the <code>int</code> to convert and insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(int)
   */
  public StringBuilder insert(int offset, int inum)
  {
    return insert(offset, String.valueOf(inum));
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param lnum the <code>long</code> to convert and insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(long)
   */
  public StringBuilder insert(int offset, long lnum)
  {
    return insert(offset, Long.toString(lnum, 10));
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param fnum the <code>float</code> to convert and insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(float)
   */
  public StringBuilder insert(int offset, float fnum)
  {
    return insert(offset, Float.toString(fnum));
  }

  /**
   * Insert the <code>String</code> value of the argument into this
   * <code>StringBuilder</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param offset the place to insert in this buffer
   * @param dnum the <code>double</code> to convert and insert
   * @return this <code>StringBuilder</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   * @see String#valueOf(double)
   */
  public StringBuilder insert(int offset, double dnum)
  {
    return insert(offset, Double.toString(dnum));
  }

  /**
   * Finds the first instance of a substring in this StringBuilder.
   *
   * @param str String to find
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   * @see #indexOf(String, int)
   */
  public int indexOf(String str)
  {
    return indexOf(str, 0);
  }

  /**
   * Finds the first instance of a String in this StringBuilder, starting at
   * a given index.  If starting index is less than 0, the search starts at
   * the beginning of this String.  If the starting index is greater than the
   * length of this String, or the substring is not found, -1 is returned.
   *
   * @param str String to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   */
  public int indexOf(String str, int fromIndex)
  {
    if (fromIndex < 0)
      fromIndex = 0;
    int limit = count - str.count;
    for ( ; fromIndex <= limit; fromIndex++)
      if (regionMatches(fromIndex, str))
        return fromIndex;
    return -1;
  }

  /**
   * Finds the last instance of a substring in this StringBuilder.
   *
   * @param str String to find
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   * @see #lastIndexOf(String, int)
   */
  public int lastIndexOf(String str)
  {
    return lastIndexOf(str, count - str.count);
  }

  /**
   * Finds the last instance of a String in this StringBuilder, starting at a
   * given index.  If starting index is greater than the maximum valid index,
   * then the search begins at the end of this String.  If the starting index
   * is less than zero, or the substring is not found, -1 is returned.
   *
   * @param str String to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   */
  public int lastIndexOf(String str, int fromIndex)
  {
    fromIndex = Math.min(fromIndex, count - str.count);
    for ( ; fromIndex >= 0; fromIndex--)
      if (regionMatches(fromIndex, str))
        return fromIndex;
    return -1;
  }

  /**
   * Reverse the characters in this StringBuilder. The same sequence of
   * characters exists, but in the reverse index ordering.
   *
   * @return this <code>StringBuilder</code>
   */
  public StringBuilder reverse()
  {
    // Call ensureCapacity to enforce copy-on-write.
    ensureCapacity(count);
    for (int i = count >> 1, j = count - i; --i >= 0; ++j)
      {
        char c = value[i];
        value[i] = value[j];
        value[j] = c;
      }
    return this;
  }

  /**
   * Convert this <code>StringBuilder</code> to a <code>String</code>. The
   * String is composed of the characters currently in this StringBuilder. Note
   * that the result is a copy, and that future modifications to this buffer
   * do not affect the String.
   *
   * @return the characters in this StringBuilder
   */
  public String toString()
  {
    return new String(this);
  }

  /**
   * Predicate which determines if a substring of this matches another String
   * starting at a specified offset for each String and continuing for a
   * specified length. This is more efficient than creating a String to call
   * indexOf on.
   *
   * @param toffset index to start comparison at for this String
   * @param other non-null String to compare to region of this
   * @return true if regions match, false otherwise
   * @see #indexOf(String, int)
   * @see #lastIndexOf(String, int)
   * @see String#regionMatches(boolean, int, String, int, int)
   */
  private boolean regionMatches(int toffset, String other)
  {
    int len = other.count;
    int index = other.offset;
    while (--len >= 0)
      if (value[toffset++] != other.value[index++])
        return false;
    return true;
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
  public int codePointAt(int index)
  {
    return Character.codePointAt(value, index, count);
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
  public int codePointBefore(int index)
  {
    // Character.codePointBefore() doesn't perform this check.  We
    // could use the CharSequence overload, but this is just as easy.
    if (index >= count)
      throw new IndexOutOfBoundsException();
    return Character.codePointBefore(value, index, 1);
  }

  /**
   * Returns the number of Unicode code points in the specified sub sequence.
   * Surrogate pairs count as one code point.
   * @param beginIndex the start of the subarray
   * @param endIndex the index after the last char in the subarray
   * @return the number of code points
   * @throws IndexOutOfBoundsException if beginIndex is less than zero or 
   * greater than endIndex or if endIndex is greater than the length of this 
   * StringBuilder
   */
  public int codePointCount(int beginIndex,int endIndex)
  {
    if (beginIndex < 0 || beginIndex > endIndex || endIndex > count)
      throw new IndexOutOfBoundsException("invalid indices: " + beginIndex
                                          + ", " + endIndex);
    return Character.codePointCount(value, beginIndex, endIndex - beginIndex);
  }

  public void trimToSize()
  {
    if (count < value.length)
      {
        char[] newValue = new char[count];
        VMSystem.arraycopy(value, 0, newValue, 0, count);
        value = newValue;
      }
  }
}
