/* LineNumberInputStream.java -- An input stream which counts line numbers
   Copyright (C) 1998, 1999, 2002, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

/**
 * This class functions like a standard <code>InputStream</code>
 * except that it counts line numbers, and canonicalizes newline
 * characters.  As data is read, whenever the byte sequences "\r",
 * "\n", or "\r\n" are encountered, the running line count is
 * incremeted by one.  Additionally, the whatever line termination
 * sequence was encountered will be converted to a "\n" byte.  Note
 * that this class numbers lines from 0.  When the first line
 * terminator is encountered, the line number is incremented to 1, and
 * so on.
 * <p>
 * This class counts only line termination characters.  If the last line
 * read from the stream does not end in a line termination sequence, it
 * will not be counted as a line.
 * <p>
 * Note that since this class operates as a filter on an underlying
 * stream, it has the same mark/reset functionality as the underlying
 * stream.  The <code>mark()</code> and <code>reset()</code> methods
 * in this class handle line numbers correctly.  Calling
 * <code>reset()</code> resets the line number to the point at which
 * <code>mark()</code> was called if the subordinate stream supports
 * that functionality.
 * <p>
 * @deprecated This class is deprecated in favor if
 * <code>LineNumberReader</code> because it operates on ASCII bytes
 * instead of an encoded character stream.  This class is for backward
 * compatibility only and should not be used in new applications.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 */
public class LineNumberInputStream extends FilterInputStream
{
  /** The current line number. */
  private int lineNumber = 0;

  /** The line number when the stream was marked. */
  private int markLineNumber = 0;

  /** Flag to indicate a '\r' was just read so that an immediately
   * subsequent '\n' can be ignored. */
  private boolean justReadReturnChar = false;

  /**
   * Create a new <code>LineNumberInputStream</code> that reads from the 
   * specified subordinate <code>InputStream</code>
   *
   * @param in The subordinate <code>InputStream</code> to read from
   */
  public LineNumberInputStream(InputStream in)
  {
    super(in);
  }

  /**
   * This method returns the number of bytes that can be read from the
   * stream before the stream can block.  This method is tricky
   * because the subordinate <code>InputStream</code> might return
   * only "\r\n" characters, which are replaced by a single "\n"
   * character by the <code>read()</code> method of this class.  So
   * this method can only guarantee that <code>in.available() /
   * 2</code> bytes can actually be read before blocking.  In
   * practice, considerably more bytes might be read before blocking
   * <p>
   * Note that the stream may not block if additional bytes beyond the count
   * returned by this method are read.
   *
   * @return The number of bytes that can be read before blocking could occur
   *
   * @exception IOException If an error occurs
   */
  public int available() throws IOException
  {
    // We can only guarantee half the characters that might be available
    // without blocking because "\r\n" is treated as a single character.
    return in.available() / 2;
  }

  /**
   * This method returns the current line number
   *
   * @return The current line number
   */
  public int getLineNumber()
  {
    return lineNumber;
  }

  /**
   * This method marks a position in the input to which the stream can
   * be "reset" byte calling the <code>reset()</code> method.  The
   * parameter <code>readlimit</code> is the number of bytes that can
   * be read from the stream after setting the mark before the mark
   * becomes invalid.  For example, if <code>mark()</code> is called
   * with a read limit of 10, then when 11 bytes of data are read from
   * the stream before the <code>reset()</code> method is called, then
   * the mark is invalid and the stream object instance is not
   * required to remember the mark.
   * <p>
   * In this class, this method will remember the current line number
   * as well as the current position in the stream.  When the
   * <code>reset()</code> method is called, the line number will be
   * restored to the saved line number in addition to the stream
   * position.
   * <p>
   * This method only works if the subordinate stream supports mark/reset
   * functionality.
   *
   * @param readlimit The number of bytes that can be read before the
   * mark becomes invalid 
   */
  public void mark(int readlimit)
  {
    in.mark(readlimit);
    markLineNumber = lineNumber;
  }

  /**
   * This method reads an unsigned byte from the input stream and returns it
   * as an int in the range of 0-255.  This method will return -1 if the
   * end of the stream has been reached.
   * <p>
   * Note that if a line termination sequence is encountered (ie, "\r",
   * "\n", or "\r\n") then that line termination sequence is converted to
   * a single "\n" value which is returned from this method.  This means
   * that it is possible this method reads two bytes from the subordinate
   * stream instead of just one.
   * <p>
   * Note that this method will block until a byte of data is available
   * to be read.
   *
   * @return The byte read or -1 if end of stream
   * 
   * @exception IOException If an error occurs
   */
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

  /**
   * This method reads bytes from a stream and stores them into a caller
   * supplied buffer.  It starts storing data at index <code>offset</code> into
   * the buffer and attemps to read <code>len</code> bytes.  This method can
   * return before reading the number of bytes requested.  The actual number
   * of bytes read is returned as an int.  A -1 is returned to indicated the
   * end of the stream.
   * <p>
   * This method will block until some data can be read.
   * <p>
   * Note that if a line termination sequence is encountered (ie, "\r",
   * "\n", or "\r\n") then that line termination sequence is converted to
   * a single "\n" value which is stored in the buffer.  Only a single
   * byte is counted towards the number of bytes read in this case.
   *
   * @param b The array into which the bytes read should be stored
   * @param off The offset into the array to start storing bytes
   * @param len The requested number of bytes to read
   *
   * @return The actual number of bytes read, or -1 if end of stream
   *
   * @exception IOException If an error occurs.
   */
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

  /**
   * This method resets a stream to the point where the
   * <code>mark()</code> method was called.  Any bytes that were read
   * after the mark point was set will be re-read during subsequent
   * reads.
   * <p>
   * In this class, this method will also restore the line number that was
   * current when the <code>mark()</code> method was called.
   *  <p>
   * This method only works if the subordinate stream supports mark/reset
   * functionality.
   *
   * @exception IOException If an error occurs
   */
  public void reset() throws IOException
  {
    in.reset();
    lineNumber = markLineNumber;
    justReadReturnChar = false;
  }

  /**
   * This method sets the current line number to the specified value.
   * 
   * @param lineNumber The new line number
   */
  public void setLineNumber(int lineNumber)
  {
    this.lineNumber = lineNumber;
  }

  /**
   * This method skips up to the requested number of bytes in the 
   * input stream.  The actual number of bytes skipped is returned.  If the
   * desired number of bytes to skip is negative, no bytes are skipped.
   *
   * @param n requested number of bytes to skip.
   *
   * @return The actual number of bytes skipped.
   *
   * @exception IOException If an error occurs.
   */
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
