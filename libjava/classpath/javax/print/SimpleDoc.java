/* SimpleDoc.java -- 
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import java.io.ByteArrayInputStream;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import javax.print.attribute.AttributeSetUtilities;
import javax.print.attribute.DocAttributeSet;

/**
 * Simple implementation of the <code>Doc</code> interface capable of handling 
 * the predefined document flavors of <code>DocFlavor</code>.
 * <p>
 * This implementation can construct a reader or stream for the service from 
 * the print data and ensures that always the same object is returned on each
 * method call. It does simple checks that the supplied data matches the 
 * specified flavor of the doc object and supports thread safe access.
 * </p> 
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class SimpleDoc implements Doc
{  
  private final Object printData;
  private final DocFlavor flavor;
  private final DocAttributeSet attributes;
  
  private InputStream stream;
  private Reader reader;

  /**
   * Constructs a SimpleDoc with the specified print data, doc flavor and doc attribute set.
   * @param printData the object with the data to print.
   * @param flavor the document flavor of the print data.
   * @param attributes the attributes of the doc (may be <code>null</code>).
   * 
   * @throws IllegalArgumentException if either <code>printData</code> or
   *   <code>flavor</code> are <code>null</code>, or the print data is not
   *   supplied in the document format specified by the given flavor object.
   */
  public SimpleDoc(Object printData, DocFlavor flavor, 
      DocAttributeSet attributes)
  {
    if (printData == null || flavor == null)
      throw new IllegalArgumentException("printData/flavor may not be null");
    
    if (! (printData.getClass().getName().equals(
           flavor.getRepresentationClassName())
        || flavor.getRepresentationClassName().equals("java.io.Reader")
           && printData instanceof Reader
        || flavor.getRepresentationClassName().equals("java.io.InputStream")
           && printData instanceof InputStream))
      {
        throw new IllegalArgumentException("data is not of declared flavor type");
      }          
    
    this.printData = printData;
    this.flavor = flavor;
    
    if (attributes != null)
      this.attributes = AttributeSetUtilities.unmodifiableView(attributes);
    else
      this.attributes = null;
    
    stream = null;
    reader = null;
  }

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
  public DocAttributeSet getAttributes()
  {
    return attributes;
  }

  /**
   * Returns the flavor of this doc objects print data.
   * 
   * @return The document flavor.
   */
  public DocFlavor getDocFlavor()
  {
    return flavor;
  }

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
  public Object getPrintData() throws IOException
  {
    return printData;
  }

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
  public Reader getReaderForText() throws IOException
  {
    synchronized (this)
      {
        // construct the reader if applicable on request
        if (reader == null)
          {
            if (flavor instanceof DocFlavor.CHAR_ARRAY)
              reader = new CharArrayReader((char[]) printData);
            else if (flavor instanceof DocFlavor.STRING)
              reader = new StringReader((String) printData);
            else if (flavor instanceof DocFlavor.READER)
              reader = (Reader) printData;
          }
        
        return reader;
      }   
  }

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
  public InputStream getStreamForBytes() throws IOException
  {
    synchronized (this)
      {
        // construct the stream if applicable on request
        if (stream == null)
          {
            if (flavor instanceof DocFlavor.BYTE_ARRAY)
              stream = new ByteArrayInputStream((byte[]) printData);
            else if (flavor instanceof DocFlavor.INPUT_STREAM)
              stream = (InputStream) printData;
          }
        
        return stream;
      }    
  }

}
