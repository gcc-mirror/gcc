/* PipedOutputStream.java -- Write portion of piped streams.
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.io;

/**
  * This class writes its bytes to a <code>PipedInputStream</code> to 
  * which it is connected.
  * <p>
  * It is highly recommended that a <code>PipedOutputStream</code> and its
  * connected <code>PipedInputStream</code> be in different threads.  If 
  * they are in the same thread, read and write operations could deadlock
  * the thread.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PipedOutputStream extends OutputStream
{

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This is the <code>PipedInputStream</code> to which this object
  * is connected.
  */
private PipedInputStream snk;

/**
  * This flag indicates whether or not this stream has ever been
  * connected to an input stream
  */
private boolean ever_connected;

/**
  * This flag indicates whether the <code>close</code> method has ever
  * been called for this stream.
  */
private boolean closed;

/*************************************************************************/

/**
  * This method initializes a new <code>PipedOutputStream</code> instance.
  * This constructor creates an unconnected object.  It must be connected
  * to a <code>PipedInputStream</code> object using the <code>connect</code>
  * method prior to writing any data or an exception will be thrown.
  */
public
PipedOutputStream()
{
  ; // Do Nothing
}

/*************************************************************************/

/**
  * This method initializes a new <code>PipedOutputStream</code> instance
  * to write to the specified <code>PipedInputStream</code>.  This stream
  * is then ready for writing.
  *
  * @param snk The <code>PipedInputStream</code> to connect this stream to.
  *
  * @exception IOException If an error occurs
  */
public
PipedOutputStream(PipedInputStream snk) throws IOException
{
  connect(snk);
} 

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method connects this object to the specified 
  * <code>PipedInputStream</code> object.  This stream will then be ready 
  * for writing.  If this stream is already connected or has been 
  * previously closed, then an exception is thrown.
  *
  * @param snk The <code>PipedInputStream</code> to connect this stream to
  *
  * @exception IOException If an error occurs
  */
public synchronized void
connect(PipedInputStream snk) throws IOException
{
  if (snk == this.snk)
    return;

  if (ever_connected)
    throw new IOException("Already connected");

  if (closed)
    throw new IOException("Stream is closed and cannot be reopened");

  this.snk = snk;
  ever_connected = true;
  snk.src = this;

  snk.connect(this);
}

/*************************************************************************/

/**
  * This method closes this stream so that no more data can be written
  * to it. Any further attempts to write to this stream may throw an
  * exception
  *
  * @exception IOException If an error occurs
  */
public synchronized void
close() throws IOException
{
  closed = true;
  snk.close();
  notifyAll();
}

/*************************************************************************/

/**
  * This method writes a single byte of date to the stream.  Note that
  * this method will block if the <code>PipedInputStream</code> to which
  * this object is connected has a full buffer.
  *
  * @param b The byte of data to be written, passed as an <code>int</code>.
  *
  * @exception IOException If an error occurs
  */
public synchronized void
write(int b) throws IOException
{
  snk.receive (b);
}

/*************************************************************************/

/**
  * This method writes <code>len</code> bytes of data from the byte array
  * <code>buf</code> starting at index <code>offset</code> in the array
  * to the stream.  Note that this method will block if the  
  * <code>PipedInputStream</code> to which this object is connected has
  * a buffer that cannot hold all of the bytes to be written.
  *
  * @param buf The array containing bytes to write to thes stream.
  * @param offset The index into the array to start writing bytes from.
  * @param len The number of bytes to write.
  *
  * @exception IOException If an error occurs
  */
public void
write(byte[] buf, int offset, int len) throws IOException
{
  snk.receive (buf, 0, len);
}

/*************************************************************************/

/**
  * This method flushes any unwritten bytes to the output and notifies
  * any waiting readers that the pipe is ready to be read.
  *
  * @exception IOException If an error occurs.
  */
public void
flush() throws IOException
{
  return;
}

} // class PipedOutputStream

