/* FilterReader.java -- Base class for char stream classes that filter input
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
 * Status:  Believed complete and correct.
 */

/**
  * This is the common superclass of all standard classes that filter 
  * input.  It acts as a layer on top of an underlying <code>Reader</code>
  * and simply redirects calls made to it to the subordinate Reader
  * instead.  Subclasses of this class perform additional filtering
  * functions in addition to simply redirecting the call.
  * <p>
  * When creating a subclass of <code>FilterReader</code>, override the
  * appropriate methods to implement the desired filtering.  However, note
  * that the <code>read(char[])</code> method does not need to be overridden
  * as this class redirects calls to that method to 
  * <code>read(yte[], int, int)</code> instead of to the subordinate
  * <code>Reader} read(yte[])</code> method.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy (warrenl@cygnus.com)
  */
public abstract class FilterReader extends Reader
{
  /**
    * This is the subordinate <code>Reader</code> to which method calls
    * are redirected
    */
  protected Reader in;

  /**
    * Create a <code>FilterReader</code> with the specified subordinate
    * <code>Reader</code>.
    * The <code>lock</code> of the new <code>FilterReader</code> will be set
    * to <code>in.lock</code>.
    *
    * @param in The subordinate <code>Reader</code>
    */
  protected FilterReader(Reader in)
  {
    super(in.lock);
    this.in = in;
  }

  /**
    * Calls the <code>in.mark(int)</code> method.
    *
    * @param readlimit The parameter passed to <code>in.mark(int)</code>
    *
    * @exception IOException If an error occurs
    */
  public void mark(int readlimit) throws IOException
  {
    in.mark(readlimit);
  }

  /**
    * Calls the <code>in.markSupported()</code> method.
    *
    * @return <code>true</code> if mark/reset is supported, 
    * <code>false</code> otherwise
    */
  public boolean markSupported()
  {
    return(in.markSupported());
  }

  /**
    * Calls the <code>in.reset()</code> method.
    *
    * @exception IOException If an error occurs
    */
  public void reset() throws IOException
  {
    in.reset();
  }

  /**
    * Calls the <code>in.read()</code> method.
    *
    * @return The value returned from <code>in.available()</code>
    *
    * @exception IOException If an error occurs
    */
  public boolean ready() throws IOException
  {
    return(in.ready());
  }

  /**
    * Calls the <code>in.skip(long)</code> method
    *
    * @param numBytes The requested number of chars to skip. 
    *
    * @return The value returned from <code>in.skip(long)</code>
    *
    * @exception IOException If an error occurs
    */
  public long skip(long num_chars) throws IOException
  {
    return(in.skip(num_chars));
  }

  /**
    * Calls the <code>in.read()</code> method
    *
    * @return The value returned from <code>in.read()</code>
    *
    * @exception IOException If an error occurs
    */
  public int read() throws IOException
  {
    return(in.read());
  }

  /**
    * Calls the <code>in.read(char[], int, int)</code> method.
    *
    * @param buf The buffer to read chars into
    * @param offset The index into the buffer to start storing chars
    * @param len The maximum number of chars to read.
    *
    * @return The value retured from <code>in.read(char[], int, int)</code>
    *
    * @exception IOException If an error occurs
    */
  public int read(char[] buf, int offset, int len) throws IOException
  {
    return(in.read(buf, offset, len));
  }

  /**
    * This method closes the stream by calling the <code>close()</code> method
    * of the underlying stream.
    *
    * @exception IOException If an error occurs
    */
  public void close() throws IOException
  {
    in.close();
  }

} // class FilterReader

