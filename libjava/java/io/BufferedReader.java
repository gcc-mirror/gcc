/* Copyright (C) 1998, 1999  Free Software Foundation

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
 */

public class BufferedReader extends Reader
{
  Reader in;
  char[] buffer;
  /* Index of current read position.  Must be >= 0 and <= limit. */
  /* There is a special case where pos may be equal to limit+1; this
   * is used as an indicator that a readLine was done with a '\r' was
   * the very last char in the buffer.  Since we don't want to read-ahead
   * and potentially block, we set pos this way to indicate the situation
   * and deal with it later.  Doing it this way rather than having a
   * separate boolean field to indicate the condition has the advantage
   * that it is self-clearing on things like mark/reset.
   */
  int pos;
  /* Limit of valid data in buffer.  Must be >= pos and <= buffer.length. */
  /* This can be < pos in the one special case described above. */
  int limit;

  /* The value -1 means there is no mark, or the mark has been invalidated.
     Otherwise, markPos is the index in the buffer of the marked position.
     Must be >= 0 and <= pos.
     Note we do not explicitly store the read-limit.
     The implicit read-limit is (buffer.length - markPos), which is
     guaranteed to be >= the read-limit requested in the call to mark. */
  int markPos = -1;

  public BufferedReader(Reader in)
  {
    this(in, 8192);
  }

  public BufferedReader(Reader in, int size)
  {
    super(in.lock);
    this.in = in;
    buffer = new char[size];
  }

  public void close() throws IOException
  {
    synchronized (lock)
      {
	if (in != null)
	  in.close();
	in = null;
	buffer = null;
      }
  }

  public boolean markSupported()
  {
    return true;
  }

  public void mark(int readLimit) throws IOException
  {
    synchronized (lock)
      {
	// In this method we need to be aware of the special case where
	// pos + 1 == limit.  This indicates that a '\r' was the last char
	// in the buffer during a readLine.  We'll want to maintain that
	// condition after we shift things around and if a larger buffer is
	// needed to track readLimit, we'll have to make it one element
	// larger to ensure we don't invalidate the mark too early, if the
	// char following the '\r' is NOT a '\n'.  This is ok because, per
	// the spec, we are not required to invalidate when passing readLimit.
	//
	// Note that if 'pos > limit', then doing 'limit -= pos' will cause
	// limit to be negative.  This is the only way limit will be < 0.

	if (pos + readLimit > limit)
	  {
	    char[] old_buffer = buffer;
	    int extraBuffSpace = 0;
	    if (pos > limit)
	      extraBuffSpace = 1;
	    if (readLimit + extraBuffSpace > limit)
	      buffer = new char[readLimit + extraBuffSpace];
	    limit -= pos;
	    if (limit >= 0)
	      {
	        System.arraycopy(old_buffer, pos, buffer, 0, limit);
	        pos = 0;
	      }
	  }

	if (limit < 0)
	  {
	    // Maintain the relationship of 'pos > limit'.
	    pos = 1;
	    limit = markPos = 0;
	  }
	else
	  markPos = pos;
	// Now pos + readLimit <= buffer.length. thus if we need to read
	// beyond buffer.length, then we are allowed to invalidate markPos.
      }
  }

  public void reset() throws IOException
  {
    synchronized (lock)
      {
	if (markPos < 0)
	  throw new IOException("mark never set or invalidated");

	// Need to handle the extremely unlikely case where a readLine was
	// done with a '\r' as the last char in the buffer; which was then
	// immediately followed by a mark and a reset with NO intervening
	// read of any sort.  In that case, setting pos to markPos would
	// lose that info and a subsequent read would thus not skip a '\n'
	// (if one exists).  The value of limit in this rare case is zero.
	// We can assume that if limit is zero for other reasons, then
	// pos is already set to zero and doesn't need to be readjusted.
	if (limit > 0)
	  pos = markPos;
      }
  }

  public boolean ready() throws IOException
  {
    synchronized (lock)
      {
	return pos < limit || in.ready();
      }
  }

  public int read(char[] buf, int offset, int count) throws IOException
  {
    synchronized (lock)
      {
	// Once again, we need to handle the special case of a readLine
	// that has a '\r' at the end of the buffer.  In this case, we'll
	// need to skip a '\n' if it is the next char to be read.
	// This special case is indicated by 'pos > limit'.
	boolean retAtEndOfBuffer = false;

	int avail = limit - pos;
	if (count > avail)
	  {
	    if (avail > 0)
	      count = avail;
	    else // pos >= limit
	      {
		if (limit == buffer.length)
		  markPos = -1; // read too far - invalidate the mark.
		if (pos > limit)
		  {
		    // Set a boolean and make pos == limit to simplify things.
		    retAtEndOfBuffer = true;
		    --pos;
		  }
		if (markPos < 0)
		  {
		    // Optimization:  can read directly into buf.
		    if (count >= buffer.length && !retAtEndOfBuffer)
		      return in.read(buf, offset, count);
		    pos = limit = 0;
		  }
		avail = in.read(buffer, limit, buffer.length - limit);
		if (retAtEndOfBuffer && avail > 0 && buffer[limit] == '\n')
		  {
		    --avail;
		    limit++;
		  }
		if (avail < count)
		  {
		    if (avail <= 0)
		      return avail;
		    count = avail;
		  }
		limit += avail;
	      }
	  }
	System.arraycopy(buffer, pos, buf, offset, count);
	pos += count;
	return count;
      }
  }

  /* Read more data into the buffer.  Update pos and limit appropriately.
     Assumes pos==limit initially.  May invalidate the mark if read too much.
     Return number of chars read (never 0), or -1 on eof. */
  private int fill() throws IOException
  {
    // Handle the special case of a readLine that has a '\r' at the end of
    // the buffer.  In this case, we'll need to skip a '\n' if it is the
    // next char to be read.  This special case is indicated by 'pos > limit'.
    boolean retAtEndOfBuffer = false;
    if (pos > limit)
      {
        retAtEndOfBuffer = true;
	--pos;
      }

    if (markPos >= 0 && limit == buffer.length)
      markPos = -1;
    if (markPos < 0)
      pos = limit = 0;
    int count = in.read(buffer, limit, buffer.length - limit);
    if (count > 0)
      limit += count;

    if (retAtEndOfBuffer && buffer[pos] == '\n')
      {
	--count;
	pos++;
      }

    return count;
  }

  public int read() throws IOException
  {
    synchronized (lock)
      {
	if (pos >= limit && fill () <= 0)
	  return -1;
	return buffer[pos++];
      }
  }

  /* Return the end of the line starting at this.pos and ending at limit.
   * The index returns is *before* any line terminators, or limit
   * if no line terminators were found.
   */
  private int lineEnd(int limit)
  {
    int i = pos;
    for (; i < limit; i++)
      {
	char ch = buffer[i];
	if (ch == '\n' || ch == '\r')
	  break;
      }
    return i;
  }

  public String readLine() throws IOException
  {
    // Handle the special case where a previous readLine (with no intervening
    // reads/skips) had a '\r' at the end of the buffer.
    // In this case, we'll need to skip a '\n' if it's the next char to be read.
    // This special case is indicated by 'pos > limit'.
    if (pos > limit)
      {
	int ch = read();
	if (ch < 0)
	  return null;
	if (ch != '\n')
	  --pos;
      }
    int i = lineEnd(limit);
    if (i < limit)
      {
	String str = new String(buffer, pos, i - pos);
	pos = i + 1;
	// If the last char in the buffer is a '\r', we must remember
	// to check if the next char to be read after the buffer is refilled
	// is a '\n'.  If so, skip it.  To indicate this condition, we set pos
	// to be limit + 1, which normally is never possible.
	if (buffer[i] == '\r')
	  if (pos == limit || buffer[pos] == '\n')
	    pos++;
	return str;
      }
    StringBuffer sbuf = new StringBuffer(200);
    sbuf.append(buffer, pos, i - pos);
    pos = i;
    // We only want to return null when no characters were read before
    // EOF.  So we must keep track of this separately.  Otherwise we
    // would treat an empty `sbuf' as an EOF condition, which is wrong
    // when there is just a newline.
    boolean eof = false;
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  {
	    eof = true;
	    break;
	  }
	if (ch == '\n' || ch == '\r')
	  {
	    // Check here if a '\r' was the last char in the buffer; if so,
	    // mark it as in the comment above to indicate future reads
	    // should skip a newline that is the next char read after
	    // refilling the buffer.
	    if (ch == '\r')
	      if (pos == limit || buffer[pos] == '\n')
	        pos++;
	    break;
	  }
	i = lineEnd(limit);
	sbuf.append(buffer, pos - 1, i - (pos - 1));
	pos = i;
      }
    return (sbuf.length() == 0 && eof) ? null : sbuf.toString();
  }

  public long skip(long count) throws IOException
  {
    if (count <= 0)
      return 0;
    synchronized (lock)
      {
	// Yet again, we need to handle the special case of a readLine
	// that has a '\r' at the end of the buffer.  In this case, we need
	// to ignore a '\n' if it is the next char to be read.
	// This special case is indicated by 'pos > limit' (i.e. avail < 0).
	// To simplify things, if we're dealing with the special case for
	// readLine, just read the next char (since the fill method will
	// skip the '\n' for us).  By doing this, we'll have to back up pos.
	// That's easier than trying to keep track of whether we've skipped
	// one element or not.
	int ch;
	if (pos > limit)
	  if ((ch = read()) < 0)
	    return 0;
	  else
	    --pos; 

	int avail = limit - pos;

	if (count < avail)
	  {
	    pos += count;
	    return count;
	  }

	pos = limit;
	long todo = count - avail;
	if (todo > buffer.length)
	  {
	    markPos = -1;
	    todo -= in.skip(todo);
	  }
	else
	  {
	    while (todo > 0)
	      {
		avail = fill();
		if (avail <= 0)
		  break;
		if (avail > todo)
		  avail = (int) todo;
		pos += avail;
		todo -= avail;
	      }
	  }
	return count - todo;
      }
  }
}
