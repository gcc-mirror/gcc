/* Reader.java -- base class of classes that read input as a stream of chars
   Copyright (C) 1998, 1999, 2000  Free Software Foundation

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
 * This abstract class forms the base of the hierarchy of classes that read
 * input as a stream of characters.  It provides a common set of methods for
 * reading characters from streams.  Subclasses implement and extend these
 * methods to read characters from a particular input source such as a file
 * or network connection.
 *
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 21, 1998.  
 * @author Aaron M. Renn (arenn@urbanophile.com) 
 */
public abstract class Reader
{
  /**
   * This is the <code>Object</code> used for synchronizing critical code
   * sections.  Subclasses should use this variable instead of a 
   * synchronized method or an explicit synchronization on <code>this</code>
   */
  protected Object lock;
  
  /**
    * Unitializes a <code>Reader</code> that will use the object
    * itself for synchronization of critical code sections.
    */
  protected Reader()
  {
    this.lock = this;
  }

  /**
    * Initializes a <code>Reader</code> that will use the specified
    * <code>Object</code> for synchronization of critical code sections.
    *
    * @param lock The <code>Object</code> to use for synchronization
    */
  protected Reader(Object lock)
  {
    this.lock = lock;
  }

  /**
   * Read chars from a stream and stores them into a caller
   * supplied buffer.  It starts storing the data at index <code>offset</code> 
   * into the buffer and attempts to read <code>len</code> chars.  This method 
   * can return before reading the number of chars requested.  The actual 
   * number of chars read is returned as an int.  A -1 is returned to indicate 
   * the end of the stream.
   * <p>
   * This method will block until some data can be read.
   * <p>
   * This method operates by calling the single char <code>read()</code> method
   * in a loop until the desired number of chars are read.  The read loop
   * stops short if the end of the stream is encountered or if an IOException
   * is encountered on any read operation except the first.  If the first
   * attempt to read a chars fails, the IOException is allowed to propagate
   * upward.  And subsequent IOException is caught and treated identically
   * to an end of stream condition.  Subclasses can (and should if possible)
   * override this method to provide a more efficient implementation.
   *
   * @param buf The array into which the chars read should be stored
   * @param offset The offset into the array to start storing chars
   * @param len The requested number of chars to read
   *
   * @return The actual number of chars read, or -1 if end of stream.
   *
   * @exception IOException If an error occurs.
   */
  public abstract int read(char buf[], int offset, int count)
    throws IOException;
    
  /**
   * Reads chars from a stream and stores them into a caller
   * supplied buffer.  This method attempts to completely fill the buffer,
   * but can return before doing so.  The actual number of chars read is
   * returned as an int.  A -1 is returned to indicate the end of the stream.
   * <p>
   * This method will block until some data can be read.
   * <p>
   * This method operates by calling an overloaded read method like so:
   * <code>read(buf, 0, buf.length)</code>
   *
   * @param buf The buffer into which the chars read will be stored.
   *
   * @return The number of chars read or -1 if end of stream.
   *
   * @exception IOException If an error occurs.
   */
  public int read(char buf[]) throws IOException
  {
    return read(buf, 0, buf.length);
  }

  /**
   * Reads an char from the input stream and returns it
   * as an int in the range of 0-65535.  This method also will return -1 if
   * the end of the stream has been reached.
   * <p>
   * This method will block until the char can be read.
   *
   * @return The char read or -1 if end of stream
   *
   * @exception IOException If an error occurs
   */
  public int read() throws IOException
  {
    char[] buf = new char[1];
    int count = read(buf, 0, 1);
    return count > 0 ? buf[0] : -1;
  }

  /**
   * Closes the stream.  Any futher attempts to read from the
   * stream may generate an <code>IOException</code>.
   *
   * @exception IOException If an error occurs
   */
  public abstract void close() throws IOException;

  /**
   * Returns a boolean that indicates whether the mark/reset
   * methods are supported in this class.  Those methods can be used to
   * remember a specific point in the stream and reset the stream to that
   * point.
   * <p>
   * This method always returns <code>false</code> in this class, but
   * subclasses can override this method to return <code>true</code> if they 
   * support mark/reset functionality.
   *
   * @return <code>true</code> if mark/reset functionality is supported, 
   *         <code>false</code> otherwise
   *
   */
  public boolean markSupported()
  {
    return false;
  }

  /**
    * Marks a position in the input to which the stream can be
    * "reset" by calling the <code>reset()</code> method.  The parameter
    * <code>readlimit</code> is the number of chars that can be read from the 
    * stream after setting the mark before the mark becomes invalid.  For
    * example, if <code>mark()</code> is called with a read limit of 10, then 
    * when 11 chars of data are read from the stream before the 
    * <code>reset()</code> method is called, then the mark is invalid and the 
    * stream object instance is not required to remember the mark.
    *
    * @param readlimit The number of chars that can be read before the mark 
    *        becomes invalid
    *
    * @exception IOException If an error occurs such as mark not being 
    *            supported for this class
    */
  public void mark(int readLimit) throws IOException
  {
    throw new IOException("mark not supported");
  }

  /**
    * Resets a stream to the point where the <code>mark()</code> 
    * method was called.  Any chars that were read after the mark point was 
    * set will be re-read during subsequent reads.
    * <p>
    * This method always throws an IOException in this class, but subclasses
    * can override this method if they provide mark/reset functionality.
    *
    * @exception IOException Always thrown for this class
    */
  public void reset() throws IOException
  {
    throw new IOException("reset not supported");
  }

  /**
    * Determines whether or not this stream is ready to be
    * read.  If it returns <code>false</code> the stream may block if a
    * read is attempted, but it is not guaranteed to do so.
    * <p>
    * This method always returns <code>false</code> in this class
    *
    * @return <code>true</code> if the stream is ready to be read, <code>false</code> otherwise.
    *
    * @exception IOException If an error occurs
    */
  public boolean ready() throws IOException
  {
    return false;
  }

  /**
    * Skips the specified number of chars in the stream.  It
    * returns the actual number of chars skipped, which may be less than the
    * requested amount.
    * <p>
    * This method reads and discards chars into a 256 char array until the
    * specified number of chars were skipped or until either the end of stream
    * is reached or a read attempt returns a short count.  Subclasses can
    * override this method to provide a more efficient implementation where
    * one exists.
    *
    * @param num_chars The requested number of chars to skip
    *
    * @return The actual number of chars skipped.
    *
    * @exception IOException If an error occurs
    */
  public long skip(long count) throws IOException
  {
    if (count <= 0)
      return 0;
    int bsize = count > 1024 ? 1024 : (int) count;
    char[] buffer = new char[bsize];
    long todo = count;
    synchronized (lock)
    {
      while (todo > 0)
	{
	  int skipped = read(buffer, 0, bsize > todo ? (int) todo : bsize);
	  if (skipped <= 0)
	    break;
	  todo -= skipped;
	}
    }
    return count - todo;
  }
}
