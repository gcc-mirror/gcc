// This is a simplified copy of java.lang.StringBuffer with
// `synchronized' removed.

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

package gnu.gcj.runtime;

public final class StringBuffer
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
    return append (bool ? "true" : "false");
  }

  /** Append the <code>char</code> to this <code>StringBuffer</code>.
   *  @param c the <code>char</code> to append.
   *  @return this <code>StringBuffer</code>.
   */
  public StringBuffer append (char ch)
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
    return append (Long.toString (lnum));
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
    return append (Float.toString (fnum));
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
    return append (Double.toString (dnum));
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
  public StringBuffer append (String str)
  {
    if (str == null)
      str = "null";
    int len = str.length();
    ensureCapacity_unsynchronized (count + len);
    str.getChars(0, len, value, count);
    count += len;
    return this;
  }

  private void ensureCapacity_unsynchronized (int minimumCapacity)
  {
    if (minimumCapacity > value.length)
      {
	minimumCapacity = Math.max (minimumCapacity, value.length * 2 + 2);
	char[] nb = new char[minimumCapacity];
	System.arraycopy(value, 0, nb, 0, count);
	value = nb;
      }
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
  }

  /** Create a new <code>StringBuffer</code> with the characters in the specified <code>String</code>.
   *  Initial capacity will be the size of the String plus 16.
   *  @param str the <code>String</code> to make a <code>StringBuffer</code> out of.
   */
  public StringBuffer (String str)
  {
    if (str == null)
      str = "null";
    count = str.length();
    // JLS: The initial capacity of the string buffer is 16 plus the
    // length of the argument string.
    value = new char[count + DEFAULT_CAPACITY];
    str.getChars(0, count, value, 0);
  }

  /** Convert this <code>StringBuffer</code> to a <code>String</code>.
   *  @return the characters in this StringBuffer
   */
  // This is native because efficient implementation requires avoiding
  // the Java protection mechanism.
  public native String toString ();

  // Index of next available character.  Note that this has
  // permissions set this way so that String can get the value.
  int count;

  // The buffer.  Note that this has permissions set this way so that
  // String can get the value.
  char[] value;

  private final static int DEFAULT_CAPACITY = 16; // JLS 20.13.1
}
