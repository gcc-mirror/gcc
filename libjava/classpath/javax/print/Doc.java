/* Doc.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package javax.print;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import javax.print.attribute.DocAttributeSet;

/**
 * <code>Doc</code> specifies the interface for print services how to obtain 
 * the print data and document specific attributes for printing. 
 * <p>
 * The print data is always passed to a {@link javax.print.DocPrintJob} object 
 * as a <code>Doc</code> object which allows the print services to:
 * <ul>
 * <li>Determine the actual document format of the supplied print data. This
 *  is supplied as a {@link javax.print.DocFlavor} object with the MIME type
 *  and the representation class of the print data.</li>
 * <li>Obtain the print data either in its representation class or depending
 *  on the document format through convenience methods as a 
 *  {@link java.io.Reader} or an {@link java.io.InputStream}.</li>
 * <li>Obtain the document's attribute set specifying the attributes which
 *  apply to this document instance.</li>
 * </ul> 
 * </p><p>
 * Every method of a <code>Doc</code> implementation has to return always the 
 * same object on every method call. Therefore if the print job consumes the 
 * print data via a stream or a reader object it can read only once the 
 * supplied print data. Implementations of this interface have to be thread 
 * safe. 
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface Doc
{
  /**
   * Returns the unmodifiable view of the attributes of this doc object.
   * <p>
   * The attributes of this doc's attributes set overrides attributes of 
   * the same category in the print job's attribute set. If an attribute 
   * is not available in this doc's attributes set or <code>null</code>
   * is returned the attributes of the same category of the print job are
   * used. 
   * </p>
   * 
   * @return The unmodifiable attributes set, or <code>null</code>.
   */
  DocAttributeSet getAttributes();

  /**
   * Returns the flavor of this doc objects print data.
   * 
   * @return The document flavor.
   */
  DocFlavor getDocFlavor();

  /**
   * Returns the print data of this doc object.
   * <p>
   * The returned object is an instance as described by the associated
   * document flavor ({@link DocFlavor#getRepresentationClassName()})
   * and can be cast to this representation class.
   * </p>
   * 
   * @return The print data in the representation class.
   * @throws IOException if representation class is a stream and I/O
   * exception occures.
   */
  Object getPrintData() throws IOException;

  /**
   * Returns a <code>Reader</code> object for extracting character print data
   * from this document.
   * <p>
   * This method is supported if the document flavor is of type:
   * <ul>
   * <li><code>char[]</code></li>
   * <li><code>java.lang.String</code></li>
   * <li><code>java.io.Reader</code></li>
   * </ul>
   * otherwise this method returns <code>null</code>.
   * </p> 
   * 
   * @return The <code>Reader</code> object, or <code>null</code>.
   * 
   * @throws IOException if an error occurs.
   */
  Reader getReaderForText() throws IOException;

  /**
   * Returns an <code>InputStream</code> object for extracting byte print data
   * from this document.
   * <p>
   * This method is supported if the document flavor is of type:
   * <ul>
   * <li><code>byte[]</code></li>
   * <li><code>java.io.InputStream</code></li>
   * </ul>
   * otherwise this method returns <code>null</code>.
   * </p> 
   * 
   * @return The <code>InputStream</code> object, or <code>null</code>.
   * 
   * @throws IOException if an error occurs.
   */
  InputStream getStreamForBytes() throws IOException;
}