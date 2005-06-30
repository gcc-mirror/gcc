/* ObjectInput.java -- Read object data from a stream
   Copyright (C) 1998,2003 Free Software Foundation, Inc.

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
  * This interface extends the <code>DataInput</code> interface to provide a
  * facility to read objects as well as primitive types from a stream.  It
  * also has methods that allow input to be done in a manner similar to
  * <code>InputStream</code>
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  *
  * @see DataInput
  */
public interface ObjectInput extends DataInput
{
  /**
    * This method returns the number of bytes that can be read without
    * blocking.
    *
    * @return The number of bytes available before blocking
    *
    * @exception IOException If an error occurs
    */
  int available() throws IOException;

  /**
    * This method reading a byte of data from a stream.  It returns that byte
    * as an <code>int</code>.  This method blocks if no data is available 
    * to be read.
    * 
    * @return The byte of data read
    *
    * @exception IOException If an error occurs
    */
  int read() throws IOException;

  /**
    * This method reads raw bytes and stores them them a byte array buffer.
    * Note that this method will block if no data is available.  However, 
    * it will not necessarily block until it fills the entire buffer.  That is,
    * a "short count" is possible.
    *
    * @param buf The byte array to receive the data read
    *
    * @return The actual number of bytes read or -1 if end of stream
    *
    * @exception IOException If an error occurs
    */
  int read(byte[] buf) throws IOException;

  /**
    * This method reads raw bytes and stores them in a byte array buffer
    * <code>buf</code> starting at position <code>offset</code> into the 
    * buffer.  A 
    * maximum of <code>len</code> bytes will be read.  Note that this method
    * blocks if no data is available, but will not necessarily block until
    * it can read <code>len</code> bytes of data.  That is, a "short count" is
    * possible.
    *
    * @param buf The byte array to receive the data read
    * @param offset The offset into <code>buf</code> to start storing data
    * @param len The maximum number of bytes to read
    *
    * @return The actual number of bytes read or -1 if end of stream
    *
    * @exception IOException If an error occurs
    */
  int read(byte[] buf, int offset, int len) throws IOException;

  /**
    * Reads an object instance and returns it.  If the class for the object
    * being read cannot be found, then a <code>ClassNotFoundException</code>
    * will be thrown.
    *
    * @return The object instance that was read
    *
    * @exception ClassNotFoundException If a class for the object cannot be 
    * found
    * @exception IOException If any other error occurs
    */
  Object readObject() 
    throws ClassNotFoundException, IOException;

  /**
    * This method causes the specified number of bytes to be read and
    * discarded.  It is possible that fewer than the requested number of bytes
    * will actually be skipped.
    *
    * @param numBytes The number of bytes to skip
    *
    * @return The actual number of bytes skipped
    *
    * @exception IOException If an error occurs
    */
  long skip(long numBytes) throws IOException;

  /**
    * This method closes the input source
    *
    * @exception IOException If an error occurs
    */
  void close() throws IOException;
}
