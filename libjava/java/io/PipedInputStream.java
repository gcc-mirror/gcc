/* PipedInputStream.java -- Input stream that reads from an output stream
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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
  * This class is an input stream that reads its bytes from an output stream
  * to which it is connected. 
  * <p>
  * Data is read and written to an internal buffer.  It is highly recommended
  * that the <code>PipedInputStream</code> and connected <code>PipedOutputStream</code>
  * be part of different threads.  If they are not, there is a possibility
  * that the read and write operations could deadlock their thread.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PipedInputStream extends InputStream
{

/*************************************************************************/

/*
 * Class Variables
 */

/**
  * The size of the internal buffer used for input/output.  Note that this
  * can be overriden by setting the system property 
  * <code>gnu.java.io.PipedInputStream.pipe_size</code> to the desired size shown 
  * in bytes.  This is not a standard part of the class library.  Note that 
  * since this variable is <code>final</code>, it cannot be changed to refect 
  * the size specified in the property.
  * <p>
  * The value for this variable is 2048.
  */
protected static final int PIPE_SIZE = 2048;

/**
  * This is the real pipe size.  It defaults to PIPE_SIZE, unless overridden
  * by use of the system property <code>gnu.java.io.PipedInputStream.pipe_size</code>.
  */
private static int pipe_size;

static
{
  pipe_size = Integer.getInteger("gnu.java.io.PipedInputStream.pipe_size",
                                 PIPE_SIZE).intValue();
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This is the internal circular buffer used for storing bytes written
  * to the pipe and from which bytes are read by this stream
  */
protected byte[] buffer = new byte[pipe_size];

/**
  * The index into buffer where the bytes written byte the connected
  * <code>PipedOutputStream</code> will be written.  If this variables is less
  * than 0, then the buffer is empty.  If this variable is equal to 
  * <code>out</code>, then the buffer is full
  */
protected int in = -1;

/**
  * This index into the buffer where bytes will be read from.
  */
protected int out = 0;

/**
  * This variable is <code>true</code> if this object has ever been connected
  * to a <code>PipedOutputStream</code>, and <code>false</code> otherwise.  It is used
  * to detect an attempt to connect an already connected stream or to
  * otherwise use the stream before it is connected.
  */
private boolean ever_connected = false;

/**
  * This variable is set to <code>true</code> if the <code>close()</code> method is
  * called.  This value is checked prevents a caller from re-opening the
  * stream.
  */
private boolean closed = false;

/**
  * This variable is the PipedOutputStream to which this stream is connected.
  */
PipedOutputStream src;

/**
  * Used by <code>read()</code> to call an overloaded method
  */
private byte[] read_buf = new byte[1];

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This constructor creates a new <code>PipedInputStream</code> that is not 
  * connected to a <code>PipedOutputStream</code>.  It must be connected before
  * bytes can be read from this stream.
  */
public
PipedInputStream()
{
  return;
}

/*************************************************************************/

/**
  * This constructor creates a new <code>PipedInputStream</code> and connects
  * it to the passed in <code>PipedOutputStream</code>. The stream is then read
  * for reading.
  *
  * @param src The <code>PipedOutputStream</code> to connect this stream to
  *
  * @exception IOException If an error occurs
  */
public
PipedInputStream(PipedOutputStream src) throws IOException
{
  connect(src);
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This method connects this stream to the passed in <code>PipedOutputStream</code>.
  * This stream is then ready for reading.  If this stream is already
  * connected or has been previously closed, then an exception is thrown
  *
  * @param src The <code>PipedOutputStream</code> to connect this stream to
  *
  * @exception IOException If an error occurs
  */
public synchronized void
connect(PipedOutputStream src) throws IOException
{
  if (src == this.src)
    return;

  if (ever_connected)
    throw new IOException("Already connected");

  if (closed)
    throw new IOException("Stream is closed and cannot be reopened");

  src.connect(this);

  ever_connected = true;
}

/*************************************************************************/

/**
  * This methods closes the stream so that no more data can be read
  * from it.
  *
  * @exception IOException If an error occurs
  */
public synchronized void
close() throws IOException
{
  closed = true;
  notifyAll();
}

/*************************************************************************/

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
public synchronized int
available() throws IOException
{
  if (in == -1)
    return(0);
  else if (in > out)
    return(in - out);
  else
    return(in + (pipe_size - out));
}

/*************************************************************************/

/**
  * Reads the next byte from the stream.  The byte read is returned as
  * and int in the range of 0-255.  If a byte cannot be read because of an
  * end of stream condition, -1 is returned.  If the stream is already
  * closed, an IOException will be thrown.
  *  <code>
  * This method will block if no bytes are available to be read.
  *
  * @return The byte read or -1 if end of stream.
  *
  * @exception IOException If an error occurs
  */
public synchronized int
read() throws IOException
{
  // Method operates by calling the multibyte overloaded read method
  // Note that read_buf is an internal instance variable.  I allocate it
  // there to avoid constant reallocation overhead for applications that
  // call this method in a loop at the cost of some unneeded overhead
  // if this method is never called.
 
  int bytes_read = read(read_buf, 0, read_buf.length);

  if (bytes_read == -1)
    return(-1);
  else
    return((read_buf[0] & 0xFF));
}

/*************************************************************************/

/**
  * This method reads bytes from the stream into a caller supplied buffer.
  * It starts storing bytes at position <code>offset</code> into the buffer and
  * reads a maximum of <cod>>len</code> bytes.  Note that this method can actually
  * read fewer than <code>len</code> bytes.  The actual number of bytes read is
  * returned.  A -1 is returned to indicated that no bytes can be read
  * because the end of the stream was reached.  If the stream is already
  * closed, a -1 will again be returned to indicate the end of the stream.
  * <p>
  * This method will block if no bytes are available to be read.
  *
  * @param buf The buffer into which bytes will be stored
  * @param offset The index into the buffer at which to start writing.
  * @param len The maximum number of bytes to read.
  */
public synchronized int
read(byte[] buf, int offset, int len) throws IOException
{
  if (!ever_connected)
    throw new IOException("Not connected"); 

  int bytes_read = 0;
  for (;;)
    {
      // If there are bytes, take them
      if (in != -1)
        {
          int desired_bytes = len - bytes_read;

          // We are in a "wrap" condition
          if (out > in)
            {
              if (desired_bytes > (pipe_size - out))
                {
                  if (in == 0)
                    desired_bytes = (pipe_size - out) - 1;
                  else
                    desired_bytes = pipe_size - out;

                  System.arraycopy(buffer, out, buf, offset + bytes_read,
                                   desired_bytes);

                  bytes_read += desired_bytes;
                  out += desired_bytes;
                  desired_bytes = len - bytes_read;

                  if (out == pipe_size)
                    out = 0;

                  notifyAll();
                }
              else
                {
                  if ((out + desired_bytes) == in)
                    --desired_bytes;

                  if (((out + desired_bytes) == pipe_size) && (in == 0)) 
                    desired_bytes = (pipe_size - out) - 1;

                  System.arraycopy(buffer, out, buf, offset + bytes_read,
                                   desired_bytes); 

                  bytes_read += desired_bytes;
                  out += desired_bytes;
                  desired_bytes = len - bytes_read;

                  if (out == pipe_size)
                    out = 0;

                  notifyAll();
                }
            }
 
          // We are in a "no wrap" or condition (can also be fall through
          // from above
          if (in > out)
            {
              if (desired_bytes >= ((in - out) - 1))
                desired_bytes = (in - out) - 1;

              System.arraycopy(buffer, out, buf, offset + bytes_read, 
                               desired_bytes);

              bytes_read += desired_bytes;
              out += desired_bytes;
              desired_bytes = len - bytes_read;

              if (out == pipe_size)
                out = 0;

              notifyAll();
            }
        }

      // If we are done, return
      if (bytes_read == len)
        return(bytes_read);

      // Return a short count if necessary
      if (bytes_read > 0 && bytes_read < len)
	return(bytes_read);

      // Handle the case where the end of stream was encountered.
      if (closed)
        {
          // We never let in == out so there might be one last byte
          // available that we have not copied yet.
          if (in != -1)
            {
              buf[offset + bytes_read] = buffer[out];
              in = -1;
              ++out;
              ++bytes_read;
            }

          if (bytes_read != 0)
            return(bytes_read);
          else
            return(-1);
        }

      // Wait for a byte to be read
      try
        {
          wait();
        }
      catch(InterruptedException e) { ; }
    } 
}

/*************************************************************************/

/**
  * This method receives a byte of input from the source PipedOutputStream.
  * If there is no data ready to be written, or if the internal circular
  * buffer is full, this method blocks.
  *
  * *****What is this method really supposed to do *********
  */
protected synchronized void
receive(int byte_received) throws IOException
{
  int orig_in = in;

  for (;;)
    {
      // Wait for something to happen
      try
        {
          wait();
        }
      catch(InterruptedException e) { ; }

      // See if we woke up because the stream was closed on us
      if (closed)
        throw new IOException("Stream closed before receiving byte");

      // See if a byte of data was received
      if (in != orig_in)
        return;
    }
}

/*************************************************************************/

/**
  * This method is used by the connected <code>PipedOutputStream</code> to
  * write bytes into the buffer.  It uses this method instead of directly
  * writing the bytes in order to obtain ownership of the object's monitor
  * for the purposes of calling <code>notify</code>.
  *
  * @param buf The array containing bytes to write to this stream
  * @param offset The offset into the array to start writing from
  * @param len The number of bytes to write.
  *
  * @exception IOException If an error occurs
  */
synchronized void
write(byte[] buf, int offset, int len) throws IOException
{
  if (len <= 0)
    return;

  int total_written = 0;
  while (total_written < len)
    {
      // If we are not at the end of the buffer with out = 0
      if (!((in == (buffer.length - 1)) && (out == 0)))
        {
          // This is the "no wrap" situation
          if ((in - 1) >= out)
            {
              int bytes_written = 0;
              if ((buffer.length - in) > (len - total_written))
                bytes_written = (len - total_written);
              else if (out == 0)
                bytes_written = (buffer.length - in) - 1;
              else 
                bytes_written = (buffer.length - in);

              if (bytes_written > 0) 
                System.arraycopy(buf, offset + total_written, buffer, in, 
                                 bytes_written);
              total_written += bytes_written;
              in += bytes_written;

              if (in == buffer.length)
                in = 0;

              notifyAll();
            }
          // This is the "wrap" situtation
          if ((out > in) && (total_written != len))
            {
              int bytes_written = 0;

              // Do special processing if we are at the beginning
              if (in == -1)
                {
                  in = 0;

                  if (buffer.length > len)
                    bytes_written = len;
                  else
                    bytes_written = buffer.length - 1;
                }
              else if (((out - in) - 1) < (len - total_written))
                {
                  bytes_written = (out - in) - 1;
                }
              else
                {
                  bytes_written = len - total_written;
                }

              // If the buffer is full, wait for it to empty out
              if ((out - 1) == in)
                {
                  try
                    {         
                      wait(); 
                    }
                  catch (InterruptedException e) 
                    { 
                      continue; 
                    }
                }

              System.arraycopy(buf, offset + total_written, buffer, in,
                               bytes_written);
              total_written += bytes_written;
              in += bytes_written;

              if (in == buffer.length)
                in = 0;

              notifyAll();
            }
        }
      // Wait for some reads to occur before we write anything.
      else
        {
          try
            {
              wait();
            }
          catch (InterruptedException e) { ; }
        }
    }
}

} // class PipedInputStream

