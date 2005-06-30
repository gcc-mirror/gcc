/* ObjectOutput.java -- Interface for writing objects to a stream
   Copyright (C) 1998, 2003 Free Software Foundation, Inc.

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
  * This interface extends <code>DataOutput</code> to provide the additional
  * facility of writing object instances to a stream.  It also adds some
  * additional methods to make the interface more 
  * <code>OutputStream</code> like.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  *
  * @see DataOutput
  */
public interface ObjectOutput extends DataOutput
{
  /**
    * This method writes the specified byte to the output stream.
    *
    * @param b The byte to write.
    *
    * @exception IOException If an error occurs.
    */
  void write(int b) throws IOException;

  /**
    * This method writes all the bytes in the specified byte array to the
    * output stream.
    *
    * @param buf The array of bytes to write.
    * 
    * @exception IOException If an error occurs.
    */
  void write(byte[] buf) throws IOException;

  /**
    * This method writes <code>len</code> bytes from the specified array
    * starting at index <code>offset</code> into that array.
    *
    * @param buf The byte array to write from.
    * @param offset The index into the byte array to start writing from.
    * @param len The number of bytes to write.
    *
    * @exception IOException If an error occurs.
    */
  void write(byte[] buf, int offset, int len) 
    throws IOException;

  /**
    * This method writes a object instance to a stream.  The format of the
    * data written is determined by the actual implementation of this method
    *
    * @param obj The object to write
    *
    * @exception IOException If an error occurs
    */
  void writeObject(Object obj) throws IOException;

  /**
    * This method causes any buffered data to be flushed out to the underlying
    * stream
    *
    * @exception IOException If an error occurs
    */
  void flush() throws IOException;

  /**
    * This method closes the underlying stream.
    *
    * @exception IOException If an error occurs
    */
  void close() throws IOException;

} // interface ObjectOutput

