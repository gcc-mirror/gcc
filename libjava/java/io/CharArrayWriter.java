// CharArrayWriter.java - Character array output stream.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 25, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to 1.1.
 */

public class CharArrayWriter extends Writer
{
  public CharArrayWriter ()
  {
    this (32);
  }

  public CharArrayWriter (int size)
  {
    super ();
    buf = new char[size];
  }

  public void close ()
  {
    // JCL says this does nothing.  This seems to violate the Writer
    // contract, in that other methods should still throw and
    // IOException after a close.  Still, we just follow JCL.
  }

  public void flush ()
  {
  }

  public synchronized void reset ()
  {
    count = 0;
  }

  public int size ()
  {
    return count;
  }

  public char[] toCharArray ()
  {
    char[] nc = new char[count];
    System.arraycopy(buf, 0, nc, 0, count);
    return nc;
  }

  public String toString ()
  {
    return new String (buf, 0, count);
  }

  public void write (int oneChar)
  {
    synchronized (lock)
      {
	resize (1);
	buf[count++] = (char) oneChar;
      }
  }

  public void write (char[] buffer, int offset, int len)
  {
    synchronized (lock)
      {
	if (len >= 0)
	  resize (len);
	System.arraycopy(buffer, offset, buf, count, len);
	count += len;
      }
  }

  public void write (String str, int offset, int len)
  {
    synchronized (lock)
      {
	if (len >= 0)
	  resize (len);
	str.getChars(offset, offset + len, buf, count);
	count += len;
      }
  }

  public void writeTo (Writer out) throws IOException
  {
    out.write(buf, 0, count);
  }

  private final void resize (int len)
  {
    if (count + len >= buf.length)
      {
	int newlen = buf.length * 2;
	if (count + len > newlen)
	  newlen = count + len;
	char[] newbuf = new char[newlen];
	System.arraycopy(buf, 0, newbuf, 0, count);
	buf = newbuf;
      }
  }

  // The character buffer.
  protected char[] buf;
  // Number of valid characters in buffer.
  protected int count;
}
