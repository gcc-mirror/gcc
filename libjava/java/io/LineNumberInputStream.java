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
 
public class LineNumberInputStream extends FilterInputStream
{
  /* The current line number. */
  private int lineNumber = 0;

  /* The line number when the stream was marked. */
  private int markLineNumber = 0;

  /* Flag to indicate a '\r' was just read so that an immediately subsequent
   * '\n' can be ignored. */
  private boolean justReadReturnChar = false;

  public LineNumberInputStream(InputStream in)
  {
    super(in);
  }

  public int available() throws IOException
  {
    // We can only guarantee half the characters that might be available
    // without blocking because "\r\n" is treated as a single character.
    return in.available() / 2;
  }

  public int getLineNumber()
  {
    return lineNumber;
  }

  public void mark(int readlimit)
  {
    in.mark(readlimit);
    markLineNumber = lineNumber;
  }

  public int read() throws IOException
  {
    // Treat "\r\n" as a single character.  A '\r' may have been read by
    // a previous call to read so we keep an internal flag to avoid having
    // to read ahead.

    int ch = in.read();

    if (ch == '\n')
      if (justReadReturnChar)
	{
	  ch = in.read();
          justReadReturnChar = false;
	}
      else
	lineNumber++;
    else if (ch == '\r')
      {
	ch = '\n';
	justReadReturnChar = true;
	lineNumber++;
      }
    else
      justReadReturnChar = false;

    return ch;
  }

  public int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    // This case always succeeds.
    if (len == 0)
      return 0;

    // The simplest, though not necessarily the most time efficient thing
    // to do is simply call read(void) len times.  Since this is a deprecated
    // class, that should be ok.
    final int origOff = off;
    while (len-- > 0)
      {
	int ch = read();
	if (ch < 0)
	  break;

	b[off++] = (byte) ch;
      }

    // This is safe since we already know that some bytes were
    // actually requested.
    return off == origOff ? -1 : off - origOff;
  }

  public void reset() throws IOException
  {
    in.reset();
    lineNumber = markLineNumber;
    justReadReturnChar = false;
  }

  public void setLineNumber(int lineNumber)
  {
    this.lineNumber = lineNumber;
  }

  public long skip(long n) throws IOException
  {
    if (n <= 0)
      return 0L;

    final long origN = n;

    do
      {
	int ch = read();
	if (ch < 0)
	  break;
	if (ch == '\n' || ch == '\r')
	  lineNumber++;
      }
    while (--n > 0);

    return origN - n;
  }
}
