/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date November 11, 1998.
 * @deprecated 
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.  Deprecated in JDK 1.1.
 */
 
public class StringBufferInputStream extends InputStream
{
  /* The String which is the input to this stream. */
  protected String buffer;

  /* Position of the next byte in buffer to be read. */
  protected int pos = 0;

  /* The length of the String buffer. */
  protected int count;

  public StringBufferInputStream(String s)
  {
    buffer = s;
    count = s.length();
  }

  public int available()
  {
    return count - pos;
  }

  public int read()
  {
    if (pos >= count)
      return -1;	// EOF

    return ((int) buffer.charAt(pos++)) & 0xFF;
  }

  public int read(byte[] b, int off, int len)
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    if (pos >= count)
      return -1;	// EOF

    int numRead = Math.min(len, count - pos);
    if (numRead < 0)
      return 0;

    buffer.getBytes(pos, pos + numRead, b, off);
    pos += numRead;
    return numRead;
  }

  public void reset()
  {
    pos = 0;
  }

  public long skip(long n)
  {
    if (n < 0)
      return 0L;

    long actualSkip = Math.min(n, count - pos);
    pos += actualSkip;
    return actualSkip;
  }
}
