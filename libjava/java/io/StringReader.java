/* StringReader.java -- permits a String to be read as a character input stream
   Copyright (C) 1998, 1999, 2000, 2003  Free Software Foundation

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

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */

/**
 * This class permits a <code>String</code> to be read as a character 
 * input stream.
 * <p>
 * The mark/reset functionality in this class behaves differently than
 * normal.  If no mark has been set, then calling the <code>reset()</code>
 * method rewinds the read pointer to the beginning of the <code>String</code>.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 19, 1998.  
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

  /**
   * Create a new <code>StringReader</code> that will read chars from the 
   * passed in <code>String</code>.  This stream will read from the beginning 
   * to the end of the <code>String</code>.
   *
   * @param s The <code>String</code> this stream will read from.
   */
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
        throw new IOException("Stream closed");

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
        throw new IOException("Stream closed");

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
        throw new IOException("Stream closed");

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

  /**
   * This method determines if the stream is ready to be read.  This class
   * is always ready to read and so always returns <code>true</code>, unless
   * close() has previously been called in which case an IOException is
   * thrown.
   *
   * @return <code>true</code> to indicate that this object is ready to be read.
   * @exception IOException If the stream is closed.
   */
  public boolean ready() throws IOException
  {
    if (buf == null)
      throw new IOException("Stream closed");

    return true;
  }

  /**
   * Sets the read position in the stream to the previously
   * marked position or to 0 (i.e., the beginning of the stream) if the mark
   * has not already been set.
   */
  public void reset() throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException("Stream closed");

      pos = markedPos;
    }
  }

  /**
    * This method attempts to skip the requested number of chars in the
    * input stream.  It does this by advancing the <code>pos</code> value by 
    * the specified number of chars.  It this would exceed the length of the
    * buffer, then only enough chars are skipped to position the stream at
    * the end of the buffer.  The actual number of chars skipped is returned.
    *
    * @param num_chars The requested number of chars to skip
    *
    * @return The actual number of chars skipped.
    */
  public long skip(long n) throws IOException
  {
    synchronized (lock)
    {
      if (buf == null)
        throw new IOException("Stream closed");

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

