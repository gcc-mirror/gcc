/* FilterWriter.java -- Parent class for output streams that filter
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
  * This class is the common superclass of output character stream classes
  * that filter the output they write.  These classes typically transform the
  * data in some way prior to writing it out to another underlying
  * <code>Writer</code>.  This class simply overrides all the
  * methods in <code>Writer</code> to redirect them to the
  * underlying stream.  Subclasses provide actual filtering.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@cygnus.com)
  */
public abstract class FilterWriter extends Writer
{
  /**
    * This is the subordinate <code>Writer</code> that this class
    * redirects its method calls to.
    */
  protected Writer out;

  /**
    * This method initializes an instance of <code>FilterWriter</code>
    * to write to the specified subordinate <code>Writer</code>.
    * The given <code>Writer</code> will be used as <code>lock</code> for
    * the newly created <code>FilterWriter</code>.
    *
    * @param out The <code>Writer</code> to write to
    */
  protected FilterWriter(Writer out)
  {
    super(out.lock);
    this.out = out;
  }

  /**
    * This method closes the underlying <code>Writer</code>.  Any
    * further attempts to write to this stream may throw an exception.
    *
    * @exception IOException If an error occurs
    */
  public void close() throws IOException
  {
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
    * This method writes a single char of output to the underlying
    * <code>Writer</code>.
    *
    * @param b The char to write, passed as an int.
    *
    * @exception IOException If an error occurs
    */
  public void write(int b) throws IOException
  {
    out.write(b);
  }

  /**
    * This method writes <code>len</code> chars from the array <code>buf</code>
    * starting at index <code>offset</code> to the underlying
    * <code>Writer</code>.
    *
    * @param buf The char array to write chars from
    * @param offset The index into the array to start writing chars from
    * @param len The number of chars to write
    *
    * @exception IOException If an error occurs
    */
  public void write(char[] buf, int offset, int len) throws IOException
  {
    out.write(buf, offset, len);
  }

  /**
    * This method writes <code>len</code> chars from the <code>String</code>
    * starting at position <code>offset</code>.
    *
    * @param str The <code>String</code> that is to be written
    * @param offset The character offset into the <code>String</code>
    * to start writing from
    * @param len The number of chars to write
    *
    * @exception IOException If an error occurs
    */
  public void write(String str, int offset, int len) throws IOException
  {
    out.write(str, offset, len);
  }

} // class FilterWriter
