/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 7, 1998.  
 */ 

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */
 
public class ByteArrayInputStream extends InputStream
{
  /* An array of bytes provided by the creator of the stream. */
  protected byte[] buf;

  /* Position of the next byte in buf to be read. */
  protected int pos;

  /* The currently marked position in the stream. */
  protected int mark;

  /* The index in buf one greater than the last valid character. */
  protected int count;

  public ByteArrayInputStream(byte[] buffer)
  {
    this(buffer, 0, buffer.length);
  }

  public ByteArrayInputStream(byte[] buffer, int offset, int length)
  {
    buf = buffer;

    count = offset + length;
    if (count > buf.length)
      count = buf.length;

    pos = offset;
    // TBD: What should we do if pos is neg. or > count?  E.g. throw exc. or:
    // if (pos < 0 || pos > count)
    //   pos = 0;

    mark = pos;
  }

  public synchronized int available()
  {
    return count - pos;
  }

  public synchronized void mark(int readAheadLimit)
  {
    // readAheadLimit is ignored per Java Class Lib. book, p.220.
    mark = pos;
  }

  public boolean markSupported()
  {
    return true;
  }

  public synchronized int read()
  {
    if (pos < 0)
      throw new ArrayIndexOutOfBoundsException(pos);

    if (pos < count)
      return ((int) buf[pos++]) & 0xFF;
    return -1;
  }

  public synchronized int read(byte[] b, int off, int len)
  {
    /* Don't need to check pos value, arraycopy will check it. */
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    if (pos >= count)
      return -1;

    int numBytes = Math.min(count - pos, len);
    System.arraycopy(buf, pos, b, off, numBytes);
    pos += numBytes;
    return numBytes;
  }

  public synchronized void reset()
  {
    pos = mark;
  }

  public synchronized long skip(long n)
  {
    // Even though the var numBytes is a long, in reality it can never
    // be larger than an int since the result of subtracting 2 positive
    // ints will always fit in an int.  Since we have to return a long
    // anyway, numBytes might as well just be a long.
    long numBytes = Math.min((long) (count - pos), n < 0 ? 0L : n);
    pos += numBytes;
    return numBytes;
  }
}
