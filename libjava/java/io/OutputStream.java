/* OutputStream.java -- Base class for byte output streams
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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
  * This abstract class forms the base of the hierarchy of classes that 
  * write output as a stream of bytes.  It provides a common set of methods
  * for writing bytes to stream.  Subclasses implement and/or extend these
  * methods to write bytes in a particular manner or to a particular 
  * destination such as a file on disk or network connection.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@cygnus.com)
  */
public abstract class OutputStream
{
  /**
   * This is the default no-argument constructor for this class.  This method
   * does nothing in this class.
   */
  public OutputStream ()
  {
  }

  /**
   * This method writes a single byte to the output stream.  The byte written
   * is the low eight bits of the <code>int</code> passed and a argument.
   * <p>
   * Subclasses must provide an implementation of this abstract method
   *
   * @param b The byte to be written to the output stream, passed as
   *          the low eight bits of an <code>int</code> 
   *
   * @exception IOException If an error occurs
   */
  public abstract void write (int b) throws IOException;

  /**
   * This method all the writes bytes from the passed array to the
   * output stream.  This method is equivalent to <code>write(b, 0,
   * buf.length)</code> which is exactly how it is implemented in this
   * class.
   *
   * @param b The array of bytes to write
   *
   * @exception IOException If an error occurs
   */
  public void write (byte[] b) throws IOException, NullPointerException
  {
    write (b, 0, b.length);
  }

  /**
   * This method writes <code>len</code> bytes from the specified array
   * <code>b</code> starting at index <code>off</code> into the array.
   * <p>
   * This method in this class calls the single byte <code>write()</code>
   * method in a loop until all bytes have been written.  Subclasses should
   * override this method if possible in order to provide a more efficent
   * implementation.
   *
   * @param b The array of bytes to write from
   * @param off The index into the array to start writing from
   * @param len The number of bytes to write
   * 
   * @exception IOException If an error occurs
   */
  public void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException ();
    for (int i = 0; i < len; ++i)
      write (b[off + i]);
  }

  /**
   * This method forces any data that may have been buffered to be written
   * to the underlying output device.  Please note that the host environment
   * might perform its own buffering unbeknowst to Java.  In that case, a
   * write made (for example, to a disk drive) might be cached in OS
   * buffers instead of actually being written to disk.
   * <p>
   * This method in this class does nothing.
   *
   * @exception IOException If an error occurs
   */
  public void flush () throws IOException
  {
  }

  /**
   * This method closes the stream.  Any internal or native resources
   * associated with this stream are freed.  Any subsequent attempt to
   * access the stream might throw an exception.
   * <p>
   * This method in this class does nothing.
   *
   * @exception IOException If an error occurs
   */
  public void close () throws IOException
  {
  }
}
