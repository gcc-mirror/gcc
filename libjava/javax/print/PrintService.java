/* PrintService.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.print;

import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.PrintServiceAttribute;
import javax.print.attribute.PrintServiceAttributeSet;
import javax.print.event.PrintServiceAttributeListener;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface PrintService
{
  /**
   * Returns a new print job capable to handle all supported document flavors.
   * 
   * @return the new print job
   */
  DocPrintJob createPrintJob();
  
  /**
   * Determines if two services refer to the same underlying service.
   * 
   * @param obj the service to check against
   * 
   * @return <code>true</code> if both services refer to the sam underlying
   * service, <code>false</code> otherwise
   */
  boolean equals(Object obj);
  
  /**
   * Returns the value of a single specified attribute.
   * 
   * @param category the category of a <code>PrintServiceAttribute</code>
   * 
   * @return the value of the attribute
   * 
   * @throws NullPointerException if category is null
   * @throws IllegalArgumentException if category is not a class that
   * implements <code>PrintServiceAttribute</code>
   */
  PrintServiceAttribute getAttribute(Class category);
  
  /**
   * Returns all attributes of this printer service
   * 
   * @return all attributes of this print service
   */
  PrintServiceAttributeSet getAttributes();

  /**
   * Returns the service's default value for a given attribute.
   * 
   * @param category the category of the attribute
   * 
   * @return the default value
   * 
   * @throws NullPointerException if <code>category</code> is null
   * @throws IllegalArgumentException if <code>category</code> is a class
   * not implementing <code>Attribute</code> 
   */
  Object getDefaultAttributeValue(Class category);
  
  /**
   * Returns the name of this print service.
   * 
   * @return the name
   */
  String getName();
  
  /**
   * Returns a factory for UI components.
   * 
   * @return the factory
   */
  ServiceUIFactory getServiceUIFactory();
  
  /**
   * Returns all supported attribute categories.
   * 
   * @return an array of all supported attribute categories
   */
  Class[] getSupportedAttributeCategories();
  
  /**
   * Returns all supported attribute values a client can use when setting up
   * a print job with this service.
   * 
   * @param category the attribute category to test
   * @param flavor the document flavor to use, or null
   * @param attributes set of printing attributes for a supposed job, or null
   * 
   * @return object indicating supported values for <code>category</code>,
   * or null if this print service doesnt support specifying doc-level or
   * job-level attribute in a print request.
   * 
   * @throws NullPointerException if <code>category</code> is null
   * @throws IllegalArgumentException if <code>category</code> is a class not
   * implementing <code>Attribute</code>, or if <code>flavor</code> is not
   * supported
   */
  Object getSupportedAttributeValues(Class category, DocFlavor flavor, AttributeSet attributes);
  
  /**
   * Returns an array of all supproted document flavors.
   * 
   * @return the supported document flavors
   */
  DocFlavor[] getSupportedDocFlavors();
  
  /**
   * Returns all attributes that are unsupported for a print request in the
   * context of a particular document flavor.
   * 
   * @param flavor document flavor to test, or null
   * @param attributes set of printing attributes for a supposed job
   * 
   * @return null if this <code>PrintService</code> supports the print request
   * specification, else the unsupported attributes
   * 
   * @throws IllegalArgumentException if <code>flavor</code> is unsupported
   */
  AttributeSet getUnsupportedAttributes(DocFlavor flavor, AttributeSet attributes);
  
  /**
   * Returns a hashcode for this printer service.
   * 
   * @return the hashcode
   */
  int hashCode();
  
  /**
   * Determines a given attribute category is supported or not.
   * 
   * @param category the category to check
   * 
   * @return <code>true</code> if <code>category</code> is supported,
   * <code>false</code> otherwise
   * 
   * @throws NullPointerException if <code>category</code> is null
   * @throws IllegalArgumentException if <code>category</code> is a class not
   * implementing <code>Attribute</code>.
   */
  boolean isAttributeCategorySupported(Class category);
  
  /**
   * Determines a given attribute value is supported when creating a print job
   * for this print service.
   * 
   * @param attrval the attribute value to check
   * @param flavor the document flavor to use, or null
   * @param attributes set of printing attributes to use, or null
   * 
   * @return <code>true</code> if the attribute value is supported,
   * <code>false</code> otherwise
   * 
   * @throws NullPointerException if <code>attrval</code> is null
   * @throws IllegalArgumentException if <code>flavor</code> is not supported
   * by this print service
   */
  boolean isAttributeValueSupported(Attribute attrval, DocFlavor flavor, AttributeSet attributes);
  
  /**
   * Determines a given document flavor is supported or not.
   * 
   * @param flavor the document flavor to check
   * 
   * @return <code>true</code> if <code>flavor</code> is supported,
   * <code>false</code> otherwise
   * 
   * @throws NullPointerException if <code>flavor</code> is null
   */
  boolean isDocFlavorSupported(DocFlavor flavor);
  
  /**
   * Registers a print service attribute listener to this print service.
   * 
   * @param listener the listener to add
   */
  void addPrintServiceAttributeListener(PrintServiceAttributeListener listener);
  
  /**
   * De-registers a print service attribute listener from this print service.
   * 
   * @param listener the listener to remove
   */
  void removePrintServiceAttributeListener(PrintServiceAttributeListener listener);
}
