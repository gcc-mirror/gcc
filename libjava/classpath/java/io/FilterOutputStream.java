/* FilterOutputStream.java -- Parent class for output streams that filter
   Copyright (C) 1998, 1999, 2001, 2003, 2005  Free Software Foundation, Inc.

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

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

/**
  * This class is the common superclass of output stream classes that 
  * filter the output they write.  These classes typically transform the
  * data in some way prior to writing it out to another underlying
  * <code>OutputStream</code>.  This class simply overrides all the 
  * methods in <code>OutputStream</code> to redirect them to the
  * underlying stream.  Subclasses provide actual filtering.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@cygnus.com)
  */
public class FilterOutputStream extends OutputStream
{
  /**
    * This is the subordinate <code>OutputStream</code> that this class
    * redirects its method calls to.
    */
  protected OutputStream out;

  /**
    * This method initializes an instance of <code>FilterOutputStream</code>
    * to write to the specified subordinate <code>OutputStream</code>.
    *
    * @param out The <code>OutputStream</code> to write to
    */
  public FilterOutputStream(OutputStream out)
  {
    this.out = out;
  }

  /**
    * This method closes the underlying <code>OutputStream</code>.  Any
    * further attempts to write to this stream may throw an exception.
    *
    * @exception IOException If an error occurs
    */
  public void close() throws IOException
  {
    flush();
    out.close();
  }

  /**
    * This method attempt to flush all buffered output to be written to the
    * underlying output sink.
    *
    * @exception IOException If an error occurs
    */
  public void flush() throws IOException
  {
    out.flush();
  }

  /**
    * This method writes a single byte of output to the underlying
    * <code>OutputStream</code>.
    *
    * @param b The byte to write, passed as an int.
    *
    * @exception IOException If an error occurs
    */
  public void write(int b) throws IOException
  {
    out.write(b);
  }

  /**
    * This method writes all the bytes in the specified array to the underlying
    * <code>OutputStream</code>.  It does this by calling the three parameter
    * version of this method - <code>write(byte[], int, int)</code> in this
    * class instead of writing to the underlying <code>OutputStream</code>
    * directly.  This allows most subclasses to avoid overriding this method.
    *
    * @param buf The byte array to write bytes from
    *
    * @exception IOException If an error occurs
    */
  public void write(byte[] buf) throws IOException
  {
    // Don't do checking here, per Java Lang Spec.
    write(buf, 0, buf.length);
  }

  /**
    * This method calls the <code>write(int)</code> method <code>len</code>
    * times for all bytes from the array <code>buf</code> starting at index
    * <code>offset</code>. Subclasses should overwrite this method to get a
    * more efficient implementation.
    *
    * @param buf The byte array to write bytes from
    * @param offset The index into the array to start writing bytes from
    * @param len The number of bytes to write
    *
    * @exception IOException If an error occurs
    */
  public void write(byte[] buf, int offset, int len) throws IOException
  {
    // Don't do checking here, per Java Lang Spec.
    for (int i=0; i < len; i++) 
      write(buf[offset + i]);

  }

} // class FilterOutputStream

