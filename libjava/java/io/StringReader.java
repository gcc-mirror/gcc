/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 19, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */
 
public class StringReader extends Reader
{
  /* A String provided by the creator of the stream. */
  private String buf;

  /* Position of the next char in buf to be read. */
  private int pos;

  /* The currently marked position in the stream. */
  private int markedPos;

  /* The index in buf one greater than the last valid character. */
  private int count;

  public StringReader(String buffer)
  {
    super();
    buf = buffer;

    count = buffer.length();
    markedPos = pos = 0;
  }

  public void close()
  {
    synchronized (lock)
    {
      buf = null;
    }
  }

  public void mark(int readAheadLimit) throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException();

      // readAheadLimit is ignored per Java Class Lib. book, p. 1692.
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

      if (pos < count)
        return ((int) buf.charAt(pos++)) & 0xFFFF;
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

      int lastChar = Math.min(count, pos + len);
      buf.getChars(pos, lastChar, b, off);
      int numChars = lastChar - pos;
      pos = lastChar;
      return numChars;
    }
  }

  public boolean ready() // TODO12: throws IOException
  {
    // TODO12: The JCL specifically says this returns true even if the
    // reader has been closed, whereas the online 1.2 doc specifically
    // says to throw an IOException if closed.
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
