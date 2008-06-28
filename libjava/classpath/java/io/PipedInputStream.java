/* PipedInputStream.java -- Read portion of piped streams.
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005  Free Software Foundation, Inc.

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

// NOTE: This implementation is very similar to that of PipedReader.  If you 
// fix a bug in here, chances are you should make a similar change to the 
// PipedReader code.

/**
  * An input stream that reads its bytes from an output stream
  * to which it is connected. 
  * <p>
  * Data is read and written to an internal buffer.  It is highly recommended
  * that the <code>PipedInputStream</code> and connected 
  * <code>PipedOutputStream</code>
  * be part of different threads.  If they are not, the read and write 
  * operations could deadlock their thread.
  *
  * @specnote The JDK implementation appears to have some undocumented 
  *           functionality where it keeps track of what thread is writing
  *           to pipe and throws an IOException if that thread susequently
  *           dies. This behaviour seems dubious and unreliable - we don't
  *           implement it.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PipedInputStream extends InputStream
{
  /** PipedOutputStream to which this is connected. Null only if this 
    * InputStream hasn't been connected yet. */
  PipedOutputStream source;

  /** Set to true if close() has been called on this InputStream. */
  boolean closed;


  /**
   * The size of the internal buffer used for input/output.
   */
  /* The "Constant Field Values" Javadoc of the Sun J2SE 1.4
   * specifies 1024.
   */
  protected static final int PIPE_SIZE = 1024;


  /**
    * This is the internal circular buffer used for storing bytes written
    * to the pipe and from which bytes are read by this stream
    */
  protected byte[] buffer = null;

  /**
    * The index into buffer where the next byte from the connected
    * <code>PipedOutputStream</code> will be written. If this variable is 
    * equal to <code>out</code>, then the buffer is full. If set to < 0,
    * the buffer is empty.
    */
  protected int in = -1;

  /**
    * This index into the buffer where bytes will be read from.
    */
  protected int out = 0;

  /** Buffer used to implement single-argument read/receive */
  private byte[] read_buf = new byte[1];

  /**
    * Creates a new <code>PipedInputStream</code> that is not connected to a 
    * <code>PipedOutputStream</code>.  It must be connected before bytes can 
    * be read from this stream.
    */
  public PipedInputStream()
  {
    this(PIPE_SIZE);
  }

  /**
   * Creates a new <code>PipedInputStream</code> of the given size that is not
   * connected to a <code>PipedOutputStream</code>.
   * It must be connected before bytes can be read from this stream.
   * 
   * @since 1.6
   * @since IllegalArgumentException If pipeSize <= 0.
   */
  public PipedInputStream(int pipeSize) throws IllegalArgumentException 
  {
    if (pipeSize <= 0)
      throw new IllegalArgumentException("pipeSize must be > 0");
    
    this.buffer = new byte[pipeSize];
  }
  
  /**
    * This constructor creates a new <code>PipedInputStream</code> and connects
    * it to the passed in <code>PipedOutputStream</code>. The stream is then 
    * ready for reading.
    *
    * @param source The <code>PipedOutputStream</code> to connect this 
    * stream to
    *
    * @exception IOException If <code>source</code> is already connected.
    */
  public PipedInputStream(PipedOutputStream source) throws IOException
  {
    this();
    connect(source);
  }

  /**
   * This constructor creates a new <code>PipedInputStream</code> of the given
   * size and connects it to the passed in <code>PipedOutputStream</code>.
   * The stream is then ready for reading.
   *
   * @param source The <code>PipedOutputStream</code> to connect this 
   * stream to
   *
   * @since 1.6
   * @exception IOException If <code>source</code> is already connected.
   */
 public PipedInputStream(PipedOutputStream source, int pipeSize)
   throws IOException
 {
   this(pipeSize);
   connect(source);
 }
  
  /**
    * This method connects this stream to the passed in 
    * <code>PipedOutputStream</code>.
    * This stream is then ready for reading.  If this stream is already
    * connected or has been previously closed, then an exception is thrown
    *
    * @param source The <code>PipedOutputStream</code> to connect this stream to
    *
    * @exception IOException If this PipedInputStream or <code>source</code> 
    *                        has been connected already.
    */
  public void connect(PipedOutputStream source) throws IOException
  {
    // The JDK (1.3) does not appear to check for a previously closed 
    // connection here.
    
    if (this.source != null || source.sink != null)
      throw new IOException ("Already connected");
    
    source.sink = this;
    this.source = source;
  }
  
  /**
  * This method receives a byte of input from the source PipedOutputStream.
  * If the internal circular buffer is full, this method blocks.
  *
  * @param val The byte to write to this stream
  *
  * @exception IOException if error occurs
  * @specnote Weird. This method must be some sort of accident.
  */
  protected synchronized void receive(int val) throws IOException
  {
    read_buf[0] = (byte) (val & 0xff);
    receive (read_buf, 0, 1);
  }

  /**
    * This method is used by the connected <code>PipedOutputStream</code> to
    * write bytes into the buffer.
    *
    * @param buf The array containing bytes to write to this stream
    * @param offset The offset into the array to start writing from
    * @param len The number of bytes to write.
    *
    * @exception IOException If an error occurs
    * @specnote This code should be in PipedOutputStream.write, but we
    *           put it here in order to support that bizarre recieve(int)
    *           method.
    */  
  synchronized void receive(byte[] buf, int offset, int len)
    throws IOException
  {
    if (closed)
      throw new IOException ("Pipe closed");

    int bufpos = offset;
    int copylen;
    
    while (len > 0)
      {
        try
	  {
	    while (in == out)
	      {
		// The pipe is full. Wake up any readers and wait for them.
		notifyAll();
		wait();
		// The pipe could have been closed while we were waiting.
	        if (closed)
		  throw new IOException ("Pipe closed");
	      }
	  }
	catch (InterruptedException ix)
	  {
            throw new InterruptedIOException ();
	  }

	if (in < 0) // The pipe is empty.
	  in = 0;
	
	// Figure out how many bytes from buf can be copied without 
	// overrunning out or going past the length of the buffer.
	if (in < out)
	  copylen = Math.min (len, out - in);
	else
	  copylen = Math.min (len, buffer.length - in);

	// Copy bytes until the pipe is filled, wrapping if necessary.
	System.arraycopy(buf, bufpos, buffer, in, copylen);
	len -= copylen;
	bufpos += copylen;
	in += copylen;
	if (in == buffer.length)
	  in = 0;
      }
    // Notify readers that new data is in the pipe.
    notifyAll();
  }
  
  /**
    * This method reads one byte from the stream.
    * -1 is returned to indicated that no bytes can be read
    * because the end of the stream was reached.  If the stream is already
    * closed, a -1 will again be returned to indicate the end of the stream.
    * 
    * <p>This method will block if no byte is available to be read.</p>
    *
    * @return the value of the read byte value, or -1 of the end of the stream
    * was reached
    * 
    * @throws IOException if an error occured
    */
  public int read() throws IOException
  {
    // Method operates by calling the multibyte overloaded read method
    // Note that read_buf is an internal instance variable.  I allocate it
    // there to avoid constant reallocation overhead for applications that
    // call this method in a loop at the cost of some unneeded overhead
    // if this method is never called.

    int r = read(read_buf, 0, 1);
    return r != -1 ? (read_buf[0] & 0xff) : -1;
  }
  
  /**
    * This method reads bytes from the stream into a caller supplied buffer.
    * It starts storing bytes at position <code>offset</code> into the 
    * buffer and
    * reads a maximum of <code>len</code> bytes.  Note that this method 
    * can actually
    * read fewer than <code>len</code> bytes.  The actual number of bytes 
    * read is
    * returned.  A -1 is returned to indicated that no bytes can be read
    * because the end of the stream was reached - ie close() was called on the
    * connected PipedOutputStream.
    * <p>
    * This method will block if no bytes are available to be read.
    *
    * @param buf The buffer into which bytes will be stored
    * @param offset The index into the buffer at which to start writing.
    * @param len The maximum number of bytes to read.
    *
    * @exception IOException If <code>close()</code> was called on this Piped
    *                        InputStream.
    */  
  public synchronized int read(byte[] buf, int offset, int len)
    throws IOException
  {
    if (source == null)
      throw new IOException ("Not connected");
    if (closed)
      throw new IOException ("Pipe closed");

    // Don't block if nothing was requested.
    if (len == 0)
      return 0;

    // If the buffer is empty, wait until there is something in the pipe 
    // to read.
    try
      {
	while (in < 0)
	  {
	    if (source.closed)
	      return -1;
	    wait();
	  }
      }
    catch (InterruptedException ix)
      {
        throw new InterruptedIOException();
      }
    
    int total = 0;
    int copylen;
    
    while (true)
      {
	// Figure out how many bytes from the pipe can be copied without 
	// overrunning in or going past the length of buf.
	if (out < in)
	  copylen = Math.min (len, in - out);
	else
	  copylen = Math.min (len, buffer.length - out);

        System.arraycopy (buffer, out, buf, offset, copylen);
	offset += copylen;
	len -= copylen;
	out += copylen;
	total += copylen;
	
	if (out == buffer.length)
	  out = 0;
	
	if (out == in)
	  {
	    // Pipe is now empty.
	    in = -1;
	    out = 0;
	  }

        // If output buffer is filled or the pipe is empty, we're done.
	if (len == 0 || in == -1)
	  {
	    // Notify any waiting outputstream that there is now space
	    // to write.
	    notifyAll();
	    return total;
	  }
      }
  }
  
  /**
    * This method returns the number of bytes that can be read from this stream
    * before blocking could occur.  This is the number of bytes that are
    * currently unread in the internal circular buffer.  Note that once this
    * many additional bytes are read, the stream may block on a subsequent
    * read, but it not guaranteed to block.
    *
    * @return The number of bytes that can be read before blocking might occur
    *
    * @exception IOException If an error occurs
    */  
  public synchronized int available() throws IOException
  {
    // The JDK 1.3 implementation does not appear to check for the closed or 
    // unconnected stream conditions here.
    
    if (in < 0)
      return 0;
    else if (out < in)
      return in - out;
    else
      return (buffer.length - out) + in;
  }
  
  /**
  * This methods closes the stream so that no more data can be read
  * from it.
  *
  * @exception IOException If an error occurs
  */
  public synchronized void close() throws IOException
  {
    closed = true;
    // Wake any thread which may be in receive() waiting to write data.
    notifyAll();
  }
}

