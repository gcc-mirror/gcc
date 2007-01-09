/* Destination.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import java.net.URI;

import javax.print.attribute.Attribute;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;
import javax.print.attribute.URISyntax;

/**
 * The <code>Destination</code> attribute provides a URI for an alternate
 * destination of the printing output.
 * <p>
 * As not an IPP attribute many print services will not support this 
 * attribute and only provide the printer device as a destination.
 * An alternate output destination would be a file on the local harddisk
 * given as a file scheme URI.
 * </p>
 * <p> 
 * If a print service does not support the destination attributes URI it 
 * will throw a PrintException. This exception may further implement the
 * interface {@link javax.print.URIException}. 
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> Destination is not an IPP 1.1 attribute.
 * </p>
 * @see javax.print.PrintException
 * @see javax.print.URIException
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class Destination extends URISyntax
  implements PrintJobAttribute, PrintRequestAttribute
{
  private static final long serialVersionUID = 6776739171700415321L;

  /**
   * Constructs a <code>Destination</code> object.
   * 
   * @param uri the URI of the output destination.
   * @throws NullPointerException if the given uri is null.
   */
  public Destination(URI uri)
  {
    super(uri);
  }
  
  /**
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal, 
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if(! (obj instanceof Destination))
      return false;

    return super.equals(obj);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>Destination</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return Destination.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "spool-data-destination"
   */
  public String getName()
  {
    return "spool-data-destination";
  }
}
