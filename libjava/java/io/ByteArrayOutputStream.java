// ByteArrayOutputStream.java - Write bytes to an array.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 24, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

public class ByteArrayOutputStream extends OutputStream
{
  public ByteArrayOutputStream ()
  {
    this (32);
  }

  public ByteArrayOutputStream (int size)
  {
    buf = new byte[size];
    count = 0;
  }

  public synchronized void reset ()
  {
    count = 0;
  }

  public int size ()
  {
    return count;
  }

  public synchronized byte[] toByteArray ()
  {
    byte[] ret = new byte[count];
    System.arraycopy(buf, 0, ret, 0, count);
    return ret;
  }

  public String toString ()
  {
    return new String (buf, 0, count);
  }

  public String toString (String enc) throws UnsupportedEncodingException
  {
    return new String (buf, 0, count, enc);
  }

  // This is deprecated in the JCL book.
  public String toString (int hibyte)
  {
    return new String (buf, 0, count, hibyte);
  }

  // Resize buffer to accomodate new bytes.
  private void resize (int add)
  {
    if (count + add >= buf.length)
      {
	int newlen = buf.length * 2;
	if (count + add > newlen)
	  newlen = count + add;
	byte[] newbuf = new byte[newlen];
	System.arraycopy(buf, 0, newbuf, 0, count);
	buf = newbuf;
      }
  }

  public synchronized void write (int oneByte)
  {
    resize (1);
    buf[count++] = (byte) oneByte;
  }

  public synchronized void write (byte[] buffer, int offset, int add)
  {
    // If ADD < 0 then arraycopy will throw the appropriate error for
    // us.
    if (add >= 0)
      resize (add);
    System.arraycopy(buffer, offset, buf, count, add);
    count += add;
  }

  public synchronized void writeTo (OutputStream out) throws IOException
  {
    out.write(buf, 0, count);
  }

  // The byte buffer.
  protected byte[] buf;
  // Number of valid bytes in buffer.
  protected int count;
}
