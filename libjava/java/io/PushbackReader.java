/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 16, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class PushbackReader extends FilterReader
{
  /* Internal buffer array for data. */
  private char[] buf;

  /* The current position in the buffer. */
  private int pos;

  public PushbackReader(Reader in)
  {
    this(in, 1);
  }

  public PushbackReader(Reader in, int size)
  {
    super(in);
    if (size < 0)
      throw new IllegalArgumentException();
    buf = new char[size];
    pos = buf.length;
  }

  public void close() throws IOException
  {
    synchronized (lock)
    {
      buf = null;
      super.close();
    }
  }

  public boolean markSupported()
  {
    return false;
  }

  public int read() throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      if (pos < buf.length)
        return ((int) buf[pos++]) & 0xFFFF;

      return super.read();
    }
  }

  public int read(char[] b, int off, int len) throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      if (off < 0 || len < 0 || off + len > b.length)
        throw new ArrayIndexOutOfBoundsException();

      int numBytes = Math.min(buf.length - pos, len);
      for (int i = 0; i < numBytes; i++)
        b[off++] = buf[pos++];

      return numBytes + super.read(b, off, len - numBytes);
    }
  }

  public boolean ready() throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      if (buf.length - pos > 0)
        return true;

      return super.ready();
    }
  }

  public void unread(int b) throws IOException
  {
    synchronized (lock)
    {
      if (buf == null || pos <= 0)
        throw new IOException();

      buf[--pos] = (char) b;
    }
  }

  public void unread(char[] b) throws IOException
  {
    unread(b, 0, b.length);
  }

  public void unread(char[] b, int off, int len) throws IOException
  {
    synchronized (lock)
    {
      if (buf == null || pos < len)
        throw new IOException();

      // Note the order that these chars are being added is the opposite
      // of what would be done if they were added to the buffer one at a time.
      // See the Java Class Libraries book p. 1397.
      System.arraycopy(b, off, buf, pos - len, len);

      // Don't put this into the arraycopy above, an exception might be thrown
      // and in that case we don't want to modify pos.
      pos -= len;
    }
  }
}
