/* Copyright (C) 1998, 1999, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 22, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 *
 * This implementation has the feature that if '\r' is read, it
 * does not look for a '\n', but immediately returns '\n'.
 * On the next read(), if a '\n' is read, it is skipped.
 * This has the advantage that we do not read (and hang) unnecessarily.
 *
 * This implementation is also minimal in the number of fields it uses.
 */

public class LineNumberReader extends BufferedReader
{
  /** The current line number. */
  int lineNumber;

  public LineNumberReader(Reader in)
  {
    super(in, 8192);
  }

  public LineNumberReader(Reader in, int size)
  {
    super(in, size);
  }

  public int getLineNumber()
  {
    return lineNumber;
  }

  public void setLineNumber(int lineNumber)
  {
    this.lineNumber = lineNumber;
  }

  private static int countLines (char[] buffer, int off, int len)
  {
    int count = 0;
    char prev = '\0';
    for (int i = 0;  i < len;  i++)
      {
        char ch = buffer[i+off];
        if ((ch == '\n' && prev != '\r') || ch == '\r')
          count++;
        prev = ch;
      }
    return count;
  }

  public void mark(int readLimit) throws IOException
  {
    synchronized (lock)
      {
	// This is basically the same as BufferedReader.mark.
	// However, if the previous character was a '\r', we need to
	// save that 'r', in case the next character is a '\n'.
	if (pos + readLimit > limit)
	  {
	    int saveCR = (pos > 0 && buffer[pos-1] == '\r') ? 1 : 0;
	    char[] old_buffer = buffer;
	    if (readLimit > limit)
	      buffer = new char[saveCR + readLimit];
	    int copy_start = pos - saveCR;
	    limit -= copy_start;
	    System.arraycopy(old_buffer, copy_start, buffer, 0, limit);
	    pos = saveCR;
	  }
	markPos = pos;
      }
  }

  public void reset() throws IOException
  {
    synchronized (lock)
      {
	if (markPos < 0)
	  throw new IOException("mark never set or invalidated");
	if (markPos > 0 && pos > markPos && buffer[markPos-1] == '\r'
	    && buffer[markPos] == '\n')
	  lineNumber--;
	lineNumber -= countLines(buffer, markPos, pos - markPos);
	pos = markPos;
      }
  }

  public int read() throws IOException
  {
    synchronized (lock)
      {
	skipRedundantLF();
	if (pos >= limit)
	  {
	    if (markPos >= 0 && limit == buffer.length)
	      markPos = -1;
	    if (markPos <= 0)
	      pos = limit = 0;
	    int count = in.read(buffer, limit, buffer.length - limit);
	    if (count <= 0)
	      return -1;
	    limit += count;
	  }
	char ch = buffer[pos++];
	if (ch == '\r' || ch == '\n')
	  {
	    lineNumber++;
	    return '\n';
	  }
	return (int) ch;
      }
  }

  public int read(char[] buf, int offset, int count) throws IOException
  {
    if (count <= 0)
      {
	if (count < 0)
	  throw new IndexOutOfBoundsException();
	return 0;
      }
    synchronized (lock)
      {
	int first = read();
	if (first < 0)
	  return -1;
	int start_offset = offset;
	buf[offset++] = (char) first;
	if (buffer[pos-1] == '\r' && pos < limit && buffer[pos] == '\n')
	  pos++;
	count--;
	while (count-- > 0 && pos < limit)
	  {
	    char ch = buffer[pos++];
	    if (ch == '\r')
	      {
		lineNumber++;
		ch = '\n';
		if (pos < limit && buffer[pos] == '\n')
		  pos++;
	      }
	    else if (ch == '\n')
	      lineNumber++;
	    buf[offset++] = ch;
	  }
	return offset - start_offset;
      }
  }

  private void skipRedundantLF() throws IOException
  {
    if (pos > 0 && buffer[pos-1] == '\r')
      {
	if (pos < limit)
	  { // fast case
	    if (buffer[pos] == '\n')
	      pos++;
	  }
	else
	  { // use read() to deal with the general case.
	    // Set pos and limit to zero to avoid infinite recursion in read.
	    // May need to invalidate markPos if we've exceeded the buffer.  
	    if (pos >= buffer.length)
	      markPos = -1;
	    pos = limit = 0;
	    int ch = read();
	    if (ch >= 0 && ch != '\n')
	      pos--;
	  }
      }
  }

  public String readLine() throws IOException
  {
    // BufferedReader.readLine already does this.  Shouldn't need to keep
    // track of newlines (since the read method deals with this for us).
    // But if the buffer is large, we may not call the read method at all
    // and super.readLine can't increment lineNumber itself.
    // Though it may seem kludgy, the safest thing to do is to save off
    // lineNumber and increment it explicitly when we're done (iff we
    // ended with a '\n' or '\r' as opposed to EOF).
    //
    // Also, we need to undo the special casing done by BufferedReader.readLine
    // when a '\r' is the last char in the buffer.  That situation is marked
    // by 'pos > limit'.
    int tmpLineNumber = lineNumber;
    skipRedundantLF();
    String str = super.readLine();
    if (pos > limit)
      --pos;

    int ch;
    if (pos > 0 && ((ch = buffer[pos - 1]) == '\n' || ch == '\r'))
      lineNumber = tmpLineNumber + 1;

    return str;
  }

  public long skip(long count) throws IOException
  {
    if (count <= 0)
      return 0;
    long to_do = count;
    do
      {
	int ch = read();
	if (ch < 0)
	  break;
	to_do--;
	if (ch == '\n' || ch == '\r')
	  lineNumber++;
	else
	  {
	    long fence = pos + to_do;
	    if (limit < fence)
	      fence = limit;
	    int end = pos;
	    for (; end < fence; end++)
	      {
		char endch = buffer[end];
		if (endch == '\n' || endch == '\r')
		  break;
	      }
	    to_do -= end - pos;
	    pos = end;
	  }
      }
    while (to_do > 0);
    return count - to_do;
  }
}
