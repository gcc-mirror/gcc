/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 15, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class PushbackInputStream extends FilterInputStream
{
  /* Internal buffer array for data. */
  protected byte[] buf;

  /* The current position in the buffer. */
  protected int pos;

  public PushbackInputStream(InputStream in)
  {
    this(in, 1);
  }

  public PushbackInputStream(InputStream in, int size)
  {
    super(in);
    if (size < 0)
      throw new IllegalArgumentException();
    buf = new byte[size];
    pos = buf.length;
  }

  public int available() throws IOException
  {
    return pos + super.available();
  }

  public void close() throws IOException
  {
    buf = null;
    super.close();
  }

  public boolean markSupported()
  {
    return false;
  }

  public int read() throws IOException
  {
    if (pos < buf.length)
      return ((int) buf[pos++]) & 0xFF;

    return super.read();
  }

  public int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    int numBytes = Math.min(buf.length - pos, len);
    for (int i = 0; i < numBytes; i++)
      b[off++] = buf[pos++];

    // `off' was just incremented to include `numBytes', so we can
    // just pass ithere.
    return numBytes + super.read(b, off, len - numBytes);
  }

  public void unread(int b) throws IOException
  {
    if (pos <= 0)
      throw new IOException();

    buf[--pos] = (byte) b;
  }

  public void unread(byte[] b) throws IOException
  {
    unread(b, 0, b.length);
  }

  public void unread(byte[] b, int off, int len) throws IOException
  {
    if (pos < len)
      throw new IOException();

    // Note the order that these bytes are being added is the opposite
    // of what would be done if they were added to the buffer one at a time.
    // See the Java Class Libraries book p. 1390.
    System.arraycopy(b, off, buf, pos - len, len);

    // Don't put this into the arraycopy above, an exception might be thrown
    // and in that case we don't want to modify pos.
    pos -= len;
  }

  // JDK1.2
  public long skip(long n) throws IOException
  {
    final long origN = n;

    if (n > 0L)
      {
	int numread = (int) Math.min((long) (buf.length - pos), n);
	pos += numread;
	n -= numread;
	n -= super.skip(n);
      }

    return origN - n;
  }
}
