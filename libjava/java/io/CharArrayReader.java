/* Copyright (C) 1998, 1999  Red Hat, Inc.

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
 * Status:  Believed complete and correct
 */
 
public class CharArrayReader extends Reader
{
  /* An array of chars provided by the creator of the stream. */
  protected char[] buf;

  /* Position of the next char in buf to be read. */
  protected int pos;

  /* The currently marked position in the stream. */
  protected int markedPos;

  /* The index in buf one greater than the last valid character. */
  protected int count;

  public CharArrayReader(char[] buffer)
  {
    this(buffer, 0, buffer.length);
  }

  public CharArrayReader(char[] buffer, int offset, int length)
  {
    super();
    buf = buffer;

    count = offset + length;
    if (count > buf.length)
      count = buf.length;

    pos = offset;
    // TBD: What should we do if pos is neg. or > count?  E.g. throw exc. or:
    // if (pos < 0 || pos > count)
    //   pos = 0;

    markedPos = pos;
  }

  public void close()
  {
    synchronized (lock)
    {
      buf = null;
    }
  }

  public void mark(int readAheadLimit)
  {
    synchronized (lock)
    {
      // readAheadLimit is ignored per Java Class Lib. book, p. 318.
      markedPos = pos;
    }
  }

  public boolean markSupported()
  {
    return true;
  }

  public int read() throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      if (pos < 0)
        throw new ArrayIndexOutOfBoundsException(pos);

      if (pos < count)
        return ((int) buf[pos++]) & 0xFFFF;
      return -1;
    }
  }

  public int read(char[] b, int off, int len) throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      /* Don't need to check pos value, arraycopy will check it. */
      if (off < 0 || len < 0 || off + len > b.length)
        throw new ArrayIndexOutOfBoundsException();

      if (pos >= count)
        return -1;

      int numChars = Math.min(count - pos, len);
      System.arraycopy(buf, pos, b, off, numChars);
      pos += numChars;
      return numChars;
    }
  }

  public boolean ready() throws IOException
  {
    if (buf == null)
      throw new IOException();

    return true;
  }

  public void reset() throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      pos = markedPos;
    }
  }

  public long skip(long n) throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      // Even though the var numChars is a long, in reality it can never
      // be larger than an int since the result of subtracting 2 positive
      // ints will always fit in an int.  Since we have to return a long
      // anyway, numChars might as well just be a long.
      long numChars = Math.min((long) (count - pos), n < 0 ? 0L : n);
      pos += numChars;
      return numChars;
    }
  }
}
