/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 8, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class BufferedInputStream extends FilterInputStream
{
  /* Internal buffer array for data. */
  protected byte[] buf;

  /* Index one greater than the last valid byte in the buffer. */
  protected int count = 0;

  /* The current position in the buffer. */
  protected int pos = 0;

  /* The value of pos the last time mark() was called. */
  protected int markpos = -1;

  /* The maximum read-ahead allowed before calls to reset() fail. */
  protected int marklimit = 0;

  public BufferedInputStream(InputStream in)
  {
    this(in, 2048);
  }

  public BufferedInputStream(InputStream in, int size)
  {
    super(in);
    if (size <= 0)
      throw new IllegalArgumentException();
    buf = new byte[size];
  }

  public synchronized int available() throws IOException
  {
    return count - pos + super.available();
  }

  public void close() throws IOException
  {
    // Free up the array memory.
    buf = null;
    super.close();
  }

  public synchronized void mark(int readlimit)
  {
    marklimit = readlimit;
    markpos = pos;
  }

  public boolean markSupported()
  {
    return true;
  }

  public synchronized int read() throws IOException
  {
    if (pos >= count && !refill())
      return -1;	// EOF

    if (markpos >= 0 && pos - markpos > marklimit)
      markpos = -1;

    return ((int) buf[pos++]) & 0xFF;
  }

  public synchronized int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    if (pos >= count && !refill())
      return -1;		// No bytes were read before EOF.

    int remain = Math.min(count - pos, len);
    System.arraycopy(buf, pos, b, off, remain);
    pos += remain;

    if (markpos >= 0 && pos - markpos > marklimit)
      markpos = -1;

    return remain;
  }

  public synchronized void reset() throws IOException
  {
    if (markpos < 0)
      throw new IOException();

    pos = markpos;
  }

  public synchronized long skip(long n) throws IOException
  {
    final long origN = n;

    while (n > 0L)
      {
	if (pos >= count && !refill())
	  if (n < origN)
	    break;
	  else
	    return -1;	// No bytes were read before EOF.

	int numread = (int) Math.min((long) (count - pos), n);
	pos += numread;
	n -= numread;

        if (markpos >= 0 && pos - markpos > marklimit)
          markpos = -1;
      }

    return origN - n;
  }

  private boolean refill() throws IOException
  {
    if (markpos < 0)
      count = pos = 0;
    else if (markpos > 0)
      {
        // Shift the marked bytes (if any) to the beginning of the array
	// but don't grow it.  This saves space in case a reset is done
	// before we reach the max capacity of this array.
        System.arraycopy(buf, markpos, buf, 0, count - markpos);
	count -= markpos;
	pos -= markpos;
	markpos = 0;
      }
    else if (marklimit >= buf.length)	// BTW, markpos == 0
      {
	// Need to grow the buffer now to have room for marklimit bytes.
	// Note that the new buffer is one greater than marklimit.
	// This is so that there will be one byte past marklimit to be read
	// before having to call refill again, thus allowing marklimit to be
	// invalidated.  That way refill doesn't have to check marklimit.
	byte[] newbuf = new byte[marklimit + 1];
	System.arraycopy(buf, 0, newbuf, 0, count);
	buf = newbuf;
      }

    int numread = super.read(buf, count, buf.length - count);

    if (numread < 0)	// EOF
      return false;

    count += numread;
    return true;
  }
}
