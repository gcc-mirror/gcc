// StringBuffer.java - Growable strings.

/* Copyright (C) 1998, 1999, 2000  Red Hat

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.Serializable;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 23, 1998.  
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 */

public final class StringBuffer implements Serializable
{
  public StringBuffer append (boolean bool)
    {
      return append (String.valueOf(bool));
    }

  public synchronized StringBuffer append (char ch)
    {
      ensureCapacity (count + 1);
      value[count++] = ch;
      return this;
    }

  public StringBuffer append (int inum)
    {
      return append (String.valueOf(inum));
    }

  public StringBuffer append (long lnum)
    {
      return append (String.valueOf(lnum));
    }

  public StringBuffer append (float fnum)
    {
      return append (String.valueOf(fnum));
    }

  public StringBuffer append (double dnum)
    {
      return append (String.valueOf(dnum));
    }

  public StringBuffer append (Object obj)
    {
      return append (String.valueOf(obj));
    }

  public synchronized StringBuffer append (String str)
    {
      if (str == null)
	str = "null";
      int len = str.length();
      ensureCapacity (count + len);
      str.getChars(0, len, value, count);
      count += len;
      return this;
    }

  public StringBuffer append (char[] data)
    {
      return append (data, 0, data.length);
    }

  public synchronized StringBuffer append (char[] data, int offset, int count)
    {
      ensureCapacity (this.count + count);
      System.arraycopy(data, offset, value, this.count, count);
      this.count += count;
      return this;
    } 

  public int capacity ()
    {
      return value.length;
    }

  public synchronized char charAt (int index)
    {
      if (index >= count)
	throw new StringIndexOutOfBoundsException (index);
      return value[index];
    }

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
	  minimumCapacity = Math.max(minimumCapacity, max);
	  char[] nb = new char[minimumCapacity];
	  System.arraycopy(value, 0, nb, 0, count);
	  value = nb;
	  shared = false;
	}
    }

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

  public StringBuffer insert (int offset, boolean bool)
    {
      return insert (offset, bool ? "true" : "false");
    }

  public synchronized StringBuffer insert (int offset, char ch)
    {
      if (offset < 0 || offset > count)
	throw new StringIndexOutOfBoundsException (offset);
      ensureCapacity (count+1);
      System.arraycopy(value, offset, value, offset+1, count-offset);
      value[offset] = ch;
      count++;
      return this;
    }

  public StringBuffer insert (int offset, int inum)
    {
      return insert (offset, String.valueOf(inum));
    }

  public StringBuffer insert (int offset, long lnum)
    {
      return insert (offset, String.valueOf(lnum));
    }

  public StringBuffer insert (int offset, float fnum)
    {
      return insert (offset, String.valueOf(fnum));
    }

  public StringBuffer insert (int offset, double dnum)
    {
      return insert (offset, String.valueOf(dnum));
    }

  public StringBuffer insert (int offset, Object obj)
    {
      return insert (offset, String.valueOf(obj));
    }

  public synchronized StringBuffer insert (int offset, String str)
    {
      if (offset < 0 || offset > count)
	throw new StringIndexOutOfBoundsException (offset);
      // Note that using `null' is from JDK 1.2.
      if (str == null)
	str = "null";
      int len = str.length();
      ensureCapacity(count+len);
      System.arraycopy(value, offset, value, offset+len, count-offset);
      str.getChars(0, len, value, offset);
      count += len;
      return this;
    }

  public synchronized StringBuffer insert (int offset, char[] data)
    {
      if (offset < 0 || offset > count)
	throw new StringIndexOutOfBoundsException (offset);
      int len = data.length;
      ensureCapacity (count+len);
      System.arraycopy(value, offset, value, offset+len, count-offset);
      System.arraycopy(data, 0, value, offset, len);
      count += len;
      return this;
    }

  public int length ()
    {
      return count;
    }

  public synchronized StringBuffer reverse ()
    {
      for (int i = 0; i < count / 2; ++i)
	{
	  char c = value[i];
	  value[i] = value[count - i - 1];
	  value[count - i - 1] = c;
	}
      return this;
    }

  public synchronized void setCharAt (int index, char ch)
    {
      if (index < 0 || index >= count)
	throw new StringIndexOutOfBoundsException (index);
      // Call ensureCapacity to enforce copy-on-write.
      ensureCapacity (count);
      value[index] = ch;
    }

  public synchronized void setLength (int newLength)
    {
      if (newLength < 0)
	throw new StringIndexOutOfBoundsException (newLength);

      ensureCapacity (newLength);
      for (int i = count; i < newLength; ++i)
	value[i] = '\0';
      count = newLength;
    }

  public StringBuffer ()
    {
      this (16);
    }

  public StringBuffer (int capacity)
    {
      count = 0;
      value = new char[capacity];
      shared = false;
    }

  public StringBuffer (String str)
    {
      // The documentation is not clear, but experimentation with
      // other implementations indicates that StringBuffer(null)
      // should throw a NullPointerException.
      count = str.length();
      // JLS: The initial capacity of the string buffer is 16 plus the
      // length of the argument string.
      value = new char[count + 16];
      str.getChars(0, count, value, 0);
      shared = false;
    }

  public String toString ()
    {
      shared = true;
      return new String (this);
    }

  // The buffer.  Note that this has permissions set this way so that
  // String can get the value.
  char[] value;

  // Index of next available character.  Note that this has
  // permissions set this way so that String can get the value.
  int count;

  // True if we need to copy the buffer before writing to it again.
  // FIXME: JDK 1.2 doesn't specify this.  The new buffer-growing
  // semantics make this less useful in that case, too.
  private boolean shared;
}
