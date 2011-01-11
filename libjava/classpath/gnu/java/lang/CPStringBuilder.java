/* ClasspathStringBuffer.java -- Growable strings without locking or copying
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

package gnu.java.lang;

import gnu.classpath.SystemProperties;

import java.io.Serializable;

/**
 * This class is based on java.lang.AbstractStringBuffer but
 * without the copying of the string by toString.
 * If you modify this, please consider also modifying that code.
 * This code is not thread-safe; limit its use to internal use within
 * methods.
 */
public final class CPStringBuilder
  implements Serializable, CharSequence, Appendable
{

  /**
   * Index of next available character (and thus the size of the current
   * string contents).  Note that this has permissions set this way so that
   * String can get the value.
   *
   * @serial the number of characters in the buffer
   */
  private int count;

  /**
   * The buffer.  Note that this has permissions set this way so that String
   * can get the value.
   *
   * @serial the buffer
   */
  private char[] value;

  /**
   * A flag to denote whether the string being created has been
   * allocated to a {@link String} object.  On construction,
   * the character array, {@link #value} is referenced only
   * by this class.  Once {@link #toString()},
   * {@link #substring(int)} or {@link #substring(int,int)}
   * are called, the array is also referenced by a {@link String}
   * object and this flag is set.  Subsequent modifications to
   * this buffer cause a new array to be allocated and the flag
   * to be reset.
   */
  private boolean allocated = false;

  /**
   * The default capacity of a buffer.
   * This can be configured using gnu.classpath.cpstringbuilder.capacity
   */
  private static final int DEFAULT_CAPACITY;

  static
  {
    String cap =
      SystemProperties.getProperty("gnu.classpath.cpstringbuilder.capacity");
    if (cap == null)
      DEFAULT_CAPACITY = 32;
    else
      DEFAULT_CAPACITY = Integer.parseInt(cap);
  }

  /**
   * Create a new CPStringBuilder with the default capacity.
   */
  public CPStringBuilder()
  {
    this(DEFAULT_CAPACITY);
  }

  /**
   * Create an empty <code>CPStringBuilder</code> with the specified initial
   * capacity.
   *
   * @param capacity the initial capacity
   * @throws NegativeArraySizeException if capacity is negative
   */
  public CPStringBuilder(int capacity)
  {
    value = new char[capacity];
  }

  /**
   * Create a new <code>CPStringBuilder</code> with the characters in the
   * specified <code>String</code>. Initial capacity will be the size of the
   * String plus the default capacity.
   *
   * @param str the <code>String</code> to convert
   * @throws NullPointerException if str is null
   */
  public CPStringBuilder(String str)
  {
    count = str.length();
    value = new char[count + DEFAULT_CAPACITY];
    str.getChars(0, count, value, 0);
  }

  /**
   * Create a new <code>CPStringBuilder</code> with the characters in the
   * specified <code>StringBuffer</code>. Initial capacity will be the size of the
   * String plus the default capacity.
   *
   * @param str the <code>String</code> to convert
   * @throws NullPointerException if str is null
   */
  public CPStringBuilder(StringBuffer str)
  {
    count = str.length();
    value = new char[count + DEFAULT_CAPACITY];
    str.getChars(0, count, value, 0);
  }

  /**
   * Create a new <code>CPStringBuilder</code> with the characters in the
   * specified <code>StringBuilder</code>. Initial capacity will be the size of the
   * String plus the default capacity.
   *
   * @param str the <code>String</code> to convert
   * @throws NullPointerException if str is null
   */
  public CPStringBuilder(StringBuilder str)
  {
    count = str.length();
    value = new char[count + DEFAULT_CAPACITY];
    str.getChars(0, count, value, 0);
  }

  /**
   * Create a new <code>CPStringBuilder</code> with the characters in the
   * specified <code>CharSequence</code>. Initial capacity will be the
   * length of the sequence plus the default capacity; if the sequence
   * reports a length less than or equal to 0, then the initial capacity
   * will be the default.
   *
   * @param seq the initializing <code>CharSequence</code>
   * @throws NullPointerException if str is null
   * @since 1.5
   */
  public CPStringBuilder(CharSequence seq)
  {
    int len = seq.length();
    count = len <= 0 ? 0 : len;
    value = new char[count + DEFAULT_CAPACITY];
    for (int i = 0; i < len; ++i)
      value[i] = seq.charAt(i);
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
  public void setLength(int newLength)
  {
    if (newLength < 0)
      throw new StringIndexOutOfBoundsException(newLength);

    int valueLength = value.length;

    /* Always call ensureCapacity in order to preserve
       copy-on-write semantics, except when the position
       is simply being reset
    */
    if (newLength > 0)
      ensureCapacity(newLength);

    if (newLength < valueLength)
      {
        /* If the StringBuffer's value just grew, then we know that
           value is newly allocated and the region between count and
           newLength is filled with '\0'.  */
        count = newLength;
      }
    else
      {
        /* The StringBuffer's value doesn't need to grow.  However,
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
    System.arraycopy(value, srcOffset, dst, dstOffset, srcEnd - srcOffset);
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
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param obj the <code>Object</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(Object)
   * @see #append(String)
   */
  public CPStringBuilder append(Object obj)
  {
    return append(String.valueOf(obj));
  }

  /**
   * Append the <code>String</code> to this <code>StringBuffer</code>. If
   * str is null, the String "null" is appended.
   *
   * @param str the <code>String</code> to append
   * @return this <code>StringBuffer</code>
   */
  public CPStringBuilder append(String str)
  {
    if (str == null)
      str = "null";
    int len = str.length();
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
  public CPStringBuilder append(StringBuffer stringBuffer)
  {
    if (stringBuffer == null)
      return append("null");
    synchronized (stringBuffer)
      {
        int len = stringBuffer.length();
        ensureCapacity(count + len);
        stringBuffer.getChars(0, len, value, count);
        count += len;
      }
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
  public CPStringBuilder append(char[] data)
  {
    return append(data, 0, data.length);
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
  public CPStringBuilder append(char[] data, int offset, int count)
  {
    if (offset < 0 || count < 0 || offset > data.length - count)
      throw new StringIndexOutOfBoundsException();
    ensureCapacity(this.count + count);
    System.arraycopy(data, offset, value, this.count, count);
    this.count += count;
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
  public CPStringBuilder append(boolean bool)
  {
    return append(bool ? "true" : "false");
  }

  /**
   * Append the <code>char</code> to this <code>StringBuffer</code>.
   *
   * @param ch the <code>char</code> to append
   * @return this <code>StringBuffer</code>
   */
  public CPStringBuilder append(char ch)
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
   * @return this <code>StringBuffer</code>
   * @since 1.5
   */
  public CPStringBuilder append(CharSequence seq)
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
   * @return this <code>StringBuffer</code>
   * @since 1.5
   */
  public CPStringBuilder append(CharSequence seq, int start, int end)
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
   * Append the <code>String</code> value of the argument to this
   * <code>StringBuffer</code>. Uses <code>String.valueOf()</code> to convert
   * to <code>String</code>.
   *
   * @param inum the <code>int</code> to convert and append
   * @return this <code>StringBuffer</code>
   * @see String#valueOf(int)
   */
  // This is native in libgcj, for efficiency.
  public CPStringBuilder append(int inum)
  {
    return append(String.valueOf(inum));
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
  public CPStringBuilder append(long lnum)
  {
    return append(Long.toString(lnum, 10));
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
  public CPStringBuilder append(float fnum)
  {
    return append(Float.toString(fnum));
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
  public CPStringBuilder append(double dnum)
  {
    return append(Double.toString(dnum));
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
  public CPStringBuilder appendCodePoint(int code)
  {
    int len = Character.charCount(code);
    ensureCapacity(count + len);
    Character.toChars(code, value, count);
    count += len;
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
  public CPStringBuilder delete(int start, int end)
  {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException(start);
    if (end > count)
      end = count;
    ensureCapacity(count);
    if (count - end != 0)
      System.arraycopy(value, end, value, start, count - end);
    count -= end - start;
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
  public CPStringBuilder deleteCharAt(int index)
  {
    return delete(index, index + 1);
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
  public CPStringBuilder replace(int start, int end, String str)
  {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException(start);

    int len = str.length();
    // Calculate the difference in 'count' after the replace.
    int delta = len - (end > count ? count : end) + start;
    ensureCapacity(count + delta);

    if (delta != 0 && end < count)
      System.arraycopy(value, end, value, end + delta, count - end);

    str.getChars(0, len, value, start);
    count += delta;
    return this;
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
  public CPStringBuilder insert(int offset, char[] str, int str_offset, int len)
  {
    if (offset < 0 || offset > count || len < 0
        || str_offset < 0 || str_offset > str.length - len)
      throw new StringIndexOutOfBoundsException();
    ensureCapacity(count + len);
    System.arraycopy(value, offset, value, offset + len, count - offset);
    System.arraycopy(str, str_offset, value, offset, len);
    count += len;
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
  public CPStringBuilder insert(int offset, Object obj)
  {
    return insert(offset, obj == null ? "null" : obj.toString());
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
  public CPStringBuilder insert(int offset, String str)
  {
    if (offset < 0 || offset > count)
      throw new StringIndexOutOfBoundsException(offset);
    if (str == null)
      str = "null";
    int len = str.length();
    ensureCapacity(count + len);
    System.arraycopy(value, offset, value, offset + len, count - offset);
    str.getChars(0, len, value, offset);
    count += len;
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
  public CPStringBuilder insert(int offset, CharSequence sequence)
  {
    if (sequence == null)
      sequence = "null";
    return insert(offset, sequence, 0, sequence.length());
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
  public CPStringBuilder insert(int offset, CharSequence sequence, int start, int end)
  {
    if (sequence == null)
      sequence = "null";
    if (start < 0 || end < 0 || start > end || end > sequence.length())
      throw new IndexOutOfBoundsException();
    int len = end - start;
    ensureCapacity(count + len);
    System.arraycopy(value, offset, value, offset + len, count - offset);
    for (int i = start; i < end; ++i)
      value[offset++] = sequence.charAt(i);
    count += len;
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
  public CPStringBuilder insert(int offset, char[] data)
  {
    return insert(offset, data, 0, data.length);
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
  public CPStringBuilder insert(int offset, boolean bool)
  {
    return insert(offset, bool ? "true" : "false");
  }

  /**
   * Insert the <code>char</code> argument into this <code>StringBuffer</code>.
   *
   * @param offset the place to insert in this buffer
   * @param ch the <code>char</code> to insert
   * @return this <code>StringBuffer</code>
   * @throws StringIndexOutOfBoundsException if offset is out of bounds
   */
  public CPStringBuilder insert(int offset, char ch)
  {
    if (offset < 0 || offset > count)
      throw new StringIndexOutOfBoundsException(offset);
    ensureCapacity(count + 1);
    System.arraycopy(value, offset, value, offset + 1, count - offset);
    value[offset] = ch;
    count++;
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
  public CPStringBuilder insert(int offset, int inum)
  {
    return insert(offset, String.valueOf(inum));
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
  public CPStringBuilder insert(int offset, long lnum)
  {
    return insert(offset, Long.toString(lnum, 10));
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
  public CPStringBuilder insert(int offset, float fnum)
  {
    return insert(offset, Float.toString(fnum));
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
  public CPStringBuilder insert(int offset, double dnum)
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
  public int indexOf(String str, int fromIndex)
  {
    if (fromIndex < 0)
      fromIndex = 0;
    int olength = str.length();
    int limit = count - olength;
    String s = VMCPStringBuilder.toString(value, 0, count);
    for (; fromIndex <= limit; ++fromIndex)
      if (s.regionMatches(fromIndex, str, 0, olength))
        return fromIndex;
    return -1;
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
  public int lastIndexOf(String str)
  {
    return lastIndexOf(str, count - str.length());
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
  public int lastIndexOf(String str, int fromIndex)
  {
    fromIndex = Math.min(fromIndex, count - str.length());
    String s = VMCPStringBuilder.toString(value, 0, count);
    int olength = str.length();
    for ( ; fromIndex >= 0; fromIndex--)
      if (s.regionMatches(fromIndex, str, 0, olength))
        return fromIndex;
    return -1;
  }

  /**
   * Reverse the characters in this StringBuffer. The same sequence of
   * characters exists, but in the reverse index ordering.
   *
   * @return this <code>StringBuffer</code>
   */
  public CPStringBuilder reverse()
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
   * This may reduce the amount of memory used by the StringBuffer,
   * by resizing the internal array to remove unused space.  However,
   * this method is not required to resize, so this behavior cannot
   * be relied upon.
   * @since 1.5
   */
  public void trimToSize()
  {
    int wouldSave = value.length - count;
    // Some random heuristics: if we save less than 20 characters, who
    // cares.
    if (wouldSave < 20)
      return;
    // If we save more than 200 characters, shrink.
    // If we save more than 1/4 of the buffer, shrink.
    if (wouldSave > 200 || wouldSave * 4 > value.length)
      allocateArray(count);
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
  public int codePointCount(int start, int end)
  {
    if (start < 0 || end >= count || start > end)
      throw new StringIndexOutOfBoundsException();

    int count = 0;
    while (start < end)
      {
        char base = value[start];
        if (base < Character.MIN_HIGH_SURROGATE
            || base > Character.MAX_HIGH_SURROGATE
            || start == end
            || start == count
            || value[start + 1] < Character.MIN_LOW_SURROGATE
            || value[start + 1] > Character.MAX_LOW_SURROGATE)
          {
            // Nothing.
          }
        else
          {
            // Surrogate pair.
            ++start;
          }
        ++start;
        ++count;
      }
    return count;
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
  public int offsetByCodePoints(int start, int codePoints)
  {
    while (codePoints > 0)
      {
        char base = value[start];
        if (base < Character.MIN_HIGH_SURROGATE
            || base > Character.MAX_HIGH_SURROGATE
            || start == count
            || value[start + 1] < Character.MIN_LOW_SURROGATE
            || value[start + 1] > Character.MAX_LOW_SURROGATE)
          {
            // Nothing.
          }
        else
          {
            // Surrogate pair.
            ++start;
          }
        ++start;
        --codePoints;
      }
    return start;
  }

  /**
   * Increase the capacity of this <code>StringBuilder</code>. This will
   * ensure that an expensive growing operation will not occur until either
   * <code>minimumCapacity</code> is reached or the array has been allocated.
   * The buffer is grown to either <code>minimumCapacity * 2</code>, if
   * the array has been allocated or the larger of <code>minimumCapacity</code> and
   * <code>capacity() * 2 + 2</code>, if it is not already large enough.
   *
   * @param minimumCapacity the new capacity
   * @see #length()
   */
  public void ensureCapacity(int minimumCapacity)
  {
    if (allocated || minimumCapacity > value.length)
      {
        if (minimumCapacity > value.length)
          {
            int max = value.length * 2 + 2;
            minimumCapacity = (minimumCapacity < max ? max : minimumCapacity);
          }
        else
          minimumCapacity *= 2;
        allocateArray(minimumCapacity);
      }
  }

  /**
   * Allocates a new character array.  This method is triggered when
   * a write is attempted after the array has been passed to a
   * {@link String} object, so that the builder does not modify
   * the immutable {@link String}.
   *
   * @param capacity the size of the new array.
   */
  private void allocateArray(int capacity)
  {
    char[] nb = new char[capacity];
    System.arraycopy(value, 0, nb, 0, count);
    value = nb;
    allocated = false;
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
   * Creates a substring of this CPStringBuilder, starting at a specified index
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
   * Creates a substring of this CPStringBuilder, starting at a specified index
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
    if (beginIndex < 0 || endIndex > count || endIndex < beginIndex)
      throw new StringIndexOutOfBoundsException();
    int len = endIndex - beginIndex;
    if (len == 0)
      return "";
    allocated = true;
    return VMCPStringBuilder.toString(value, beginIndex, len);
  }

  /**
   * Convert this <code>CPStringBuilder</code> to a <code>String</code>. The
   * String is composed of the characters currently in this StringBuilder. Note
   * that the result is not a copy, so we flag this here and make sure to
   * allocate a new array on the next write attempt (see {@link #ensureCapacity(int)}).
   *
   * @return the characters in this StringBuilder
   */
  public String toString()
  {
    allocated = true;
    return VMCPStringBuilder.toString(value, 0, count);
  }

}
