// BufferedOutputStream.java - A buffered stream

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

public class BufferedOutputStream extends FilterOutputStream
{
  public BufferedOutputStream (OutputStream ox)
  {
    this (ox, 512);
  }

  public BufferedOutputStream (OutputStream ox, int size)
  {
    super (ox);
    buf = new byte[size];
  }

  public synchronized void flush () throws IOException
  {
    out.write(buf, 0, count);
    count = 0;
    out.flush();
  }

  public synchronized void write (int b) throws IOException
  {
    // Flush output on overflow though JDK (1.2) doc may infer to flush on fill.
    if (count < buf.length)
      buf[count++] = (byte) b;
    else
      {
	out.write(buf, 0, count);
	count = 0;
	out.write(b);
      }
  }

  public synchronized void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException
  {
    // Flush output on overflow though JDK (1.2) doc may infer to flush on fill.

    // If LEN < 0 then the downstream write will fail for us.
    if (len >= 0 && count + len <= buf.length)
      {
	System.arraycopy(b, off, buf, count, len);
	count += len;
      }
    else
      {
	out.write(buf, 0, count);
	count = 0;
	out.write(b, off, len);
      }
  }

  // The buffer.
  protected byte[] buf;
  // Number of valid bytes in BUF.
  protected int count;
}
