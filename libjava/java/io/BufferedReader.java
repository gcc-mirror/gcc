/* BufferedReader.java
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.io;

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

/**
  * This subclass of <code>FilterReader</code> buffers input from an 
  * underlying implementation to provide a possibly more efficient read
  * mechanism.  It maintains the buffer and buffer state in instance 
  * variables that are available to subclasses.  The default buffer size
  * of 512 chars can be overridden by the creator of the stream.
  * <p>
  * This class also implements mark/reset functionality.  It is capable
  * of remembering any number of input chars, to the limits of
  * system memory or the size of <code>Integer.MAX_VALUE</code>
  *
  * @author Per Bothner <bothner@cygnus.com>
  * @author Aaron M. Renn <arenn@urbanophile.com>
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

  // The JCL book specifies the default buffer size as 8K characters.
  // This is package-private because it is used by LineNumberReader.
  static final int DEFAULT_BUFFER_SIZE = 8192;

  /**
    * Create a new <code>BufferedReader</code> that will read from the 
    * specified subordinate stream with a default buffer size of 4096 chars.
    *
    * @param in The subordinate stream to read from
    */
  public BufferedReader(Reader in)
  {
    this(in, DEFAULT_BUFFER_SIZE);
  }

  /**
    * Create a new <code>BufferedReader</code> that will read from the 
    * specified subordinate stream with a buffer size that is specified by the 
    * caller.
    *
    * @param in The subordinate stream to read from
    * @param bufsize The buffer size to use
    */
  public BufferedReader(Reader in, int size)
  {
    super(in.lock);
    this.in = in;
    buffer = new char[size];
  }

  /**
    * This method closes the stream 
    *
    * @exception IOException If an error occurs
    */
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

  /**
    * Returns <code>true</code> to indicate that this class supports mark/reset 
    * functionality.
    *
    * @return <code>true</code>
    */
  public boolean markSupported()
  {
    return true;
  }

  /**
    * Mark a position in the input to which the stream can be
    * "reset" by calling the <code>reset()</code> method.  The parameter
    * <code>readlimit</code> is the number of chars that can be read from the 
    * stream after setting the mark before the mark becomes invalid.  For
    * example, if <code>mark()</code> is called with a read limit of 10, then 
    * when 11 chars of data are read from the stream before the 
    * <code>reset()</code> method is called, then the mark is invalid and the 
    * stream object instance is not required to remember the mark.
    * <p>
    * Note that the number of chars that can be remembered by this method
    * can be greater than the size of the internal read buffer.  It is also
    * not dependent on the subordinate stream supporting mark/reset
    * functionality.
    *
    * @param readlimit The number of chars that can be read before the mark 
    *        becomes invalid
    *
    * @exception IOException If an error occurs
    */
  public void mark(int readLimit) throws IOException
  {
    synchronized (lock)
      {
	checkStatus();
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

  /**
    * Reset the stream to the point where the <code>mark()</code> method
    * was called.  Any chars that were read after the mark point was set will
    * be re-read during subsequent reads.
    * <p>
    * This method will throw an IOException if the number of chars read from
    * the stream since the call to <code>mark()</code> exceeds the mark limit
    * passed when establishing the mark.
    *
    * @exception IOException If an error occurs;
    */
  public void reset() throws IOException
  {
    synchronized (lock)
      {
	checkStatus();
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

  /**
    * This method determines whether or not a stream is ready to be read.  If
    * This method returns <code>false</code> then this stream could (but is
    * not guaranteed to) block on the next read attempt.
    *
    * @return <code>true</code> if this stream is ready to be read, <code>false</code> otherwise
    *
    * @exception IOException If an error occurs
    */
  public boolean ready() throws IOException
  {
    synchronized (lock)
      {
	checkStatus();
	return pos < limit || in.ready();
      }
  }

  /**
    * This method read chars from a stream and stores them into a caller
    * supplied buffer.  It starts storing the data at index <code>offset</code> into
    * the buffer and attempts to read <code>len</code> chars.  This method can
    * return before reading the number of chars requested.  The actual number
    * of chars read is returned as an int.  A -1 is returned to indicate the
    * end of the stream.
    * <p>
    * This method will block until some data can be read.
    *
    * @param buf The array into which the chars read should be stored
    * @param offset The offset into the array to start storing chars
    * @param count The requested number of chars to read
    *
    * @return The actual number of chars read, or -1 if end of stream.
    *
    * @exception IOException If an error occurs.
    */
  public int read(char[] buf, int offset, int count) throws IOException
  {
    synchronized (lock)
      {
	checkStatus();
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
    checkStatus();
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
	// If the mark was set to the location of the \n, then we
	// must change it to fully pretend that the \n does not
	// exist.
	if (markPos == pos)
	  ++markPos;
	++pos;
      }

    return count;
  }
  
  public int read() throws IOException
  {
    synchronized (lock)
      {
	checkStatus();
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

  /**
    * This method reads a single line of text from the input stream, returning
    * it as a <code>String</code>.  A line is terminated by "\n", a "\r", or
    * an "\r\n" sequence.  The system dependent line separator is not used.
    * The line termination characters are not returned in the resulting
    * <code>String</code>.
    * 
    * @return The line of text read, or <code>null</code> if end of stream.
    * 
    * @exception IOException If an error occurs
    */
  public String readLine() throws IOException
  {
    checkStatus();
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

  /**
    * This method skips the specified number of chars in the stream.  It
    * returns the actual number of chars skipped, which may be less than the
    * requested amount.
    * <p>
    * This method first discards chars in the buffer, then calls the
    * <code>skip</code> method on the underlying stream to skip the remaining chars.
    *
    * @param num_chars The requested number of chars to skip
    *
    * @return The actual number of chars skipped.
    *
    * @exception IOException If an error occurs
    */
  public long skip(long count) throws IOException
  {
    synchronized (lock)
      {
	checkStatus();
	if (count <= 0)
	  return 0;
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
  
  private void checkStatus() throws IOException
  {
    if (in == null)
      throw new IOException("Stream closed");
  }  
}
