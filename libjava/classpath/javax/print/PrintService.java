/* PrintService.java --
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

import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.PrintServiceAttribute;
import javax.print.attribute.PrintServiceAttributeSet;
import javax.print.event.PrintServiceAttributeListener;

/**
 * A <code>PrintService</code> represents a printer available for printing.
 * <p>
 * The print service hereby may be a real physical printer device, a printer
 * group with same capabilities or a logical print service (like for example
 * a PDF writer). The print service is used to query the capabilities of the
 * represented printer instance. If a suitable print service is found it is
 * used to create a print job for the actual printing process.
 * </p>
 * @see javax.print.DocPrintJob
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface PrintService
{
  /**
   * Creates and returns a new print job which is capable to handle all
   * the document flavors supported by this print service.
   *
   * @return The created print job object.
   */
  DocPrintJob createPrintJob();

  /**
   * Determines if two services refer to the same underlying service.
   *
   * @param obj the service to check against
   *
   * @return <code>true</code> if both services refer to the same underlying
   * service, <code>false</code> otherwise.
   */
  boolean equals(Object obj);

  /**
   * Returns the value of the single specified attribute.
   *
   * @param category the category of a <code>PrintServiceAttribute</code>
   *
   * @return The value of the attribute, or <code>null</code> if the attribute
   * category is not supported by this print service implementation.
   *
   * @throws NullPointerException if category is <code>null</code>.
   * @throws IllegalArgumentException if category is not a class that
   * implements <code>PrintServiceAttribute</code>.
   */
  <T extends PrintServiceAttribute> T getAttribute(Class<T> category);

  /**
   * Returns the attributes describing this print service. The returned
   * attributes set is unmodifiable and represents the current state of
   * the print service. As some print service attributes may change
   * (depends on the print service implementation) a subsequent call to
   * this method may return a different set. To monitor changes a
   * <code>PrintServiceAttributeListener</code> may be registered.
   *
   * @return All the description attributes of this print service.
   * @see #addPrintServiceAttributeListener(PrintServiceAttributeListener)
   */
  PrintServiceAttributeSet getAttributes();

  /**
   * Determines and returns the default value for a given attribute category
   * of this print service.
   * <p>
   * A return value of <code>null</code> means either that the print service
   * does not support the attribute category or there is no default value
   * available for this category. To distinguish these two case one can test
   * with {@link #isAttributeCategorySupported(Class)} if the category is
   * supported.
   * </p>
   *
   * @param category the category of the attribute
   *
   * @return The default value, or <code>null</code>.
   *
   * @throws NullPointerException if <code>category</code> is <code>null</code>
   * @throws IllegalArgumentException if <code>category</code> is a class
   * not implementing <code>Attribute</code>
   */
  Object getDefaultAttributeValue(Class<? extends Attribute> category);

  /**
   * Returns the name of this print service.
   * This may be the value of the <code>PrinterName</code> attribute.
   *
   * @return The print service name.
   */
  String getName();

  /**
   * Returns a factory for UI components if supported by the print service.
   *
   * @return A factory for UI components or <code>null</code>.
   */
  ServiceUIFactory getServiceUIFactory();

  /**
   * Returns all supported attribute categories.
   *
   * @return The class array of all supported attribute categories.
   */
  Class<?>[] getSupportedAttributeCategories();

  /**
   * Determines and returns all supported attribute values of a given
   * attribute category a client can use when setting up a print job
   * for this print service.
   * <p>
   * The returned object may be one of the following types:
   * <ul>
   * <li>A single instance of the attribute category to indicate that any
   * value will be supported.</li>
   * <li>An array of the same type as the attribute category to test,
   * containing all the supported values for this category.</li>
   * <li>A single object (of any other type than the attribute category)
   * which indicates bounds on the supported values.</li>
   * </ul>
   * </p>
   *
   * @param category the attribute category to test
   * @param flavor the document flavor to use, or <code>null</code>
   * @param attributes set of attributes for a supposed job,
   * or <code>null</code>
   *
   * @return A object (as defined above) indicating the supported values
   * for the given attribute category, or <code>null</code> if this print
   * service doesn't support the given attribute category at all.
   *
   * @throws NullPointerException if <code>category</code> is null
   * @throws IllegalArgumentException if <code>category</code> is a class not
   * implementing <code>Attribute</code>, or if <code>flavor</code> is not
   * supported
   */
  Object getSupportedAttributeValues(Class<? extends Attribute> category,
                                     DocFlavor flavor,
                                     AttributeSet attributes);

  /**
   * Determines and returns an array of all supported document flavors which
   * can be used to supply print data to this print service.
   * <p>
   * The supported attribute categories may differ between the supported
   * document flavors. To test for supported attributes one can use the
   * {@link #getUnsupportedAttributes(DocFlavor, AttributeSet)} method with
   * the specific doc flavor and attributes set.
   * </p>
   *
   * @return the supported document flavors
   */
  DocFlavor[] getSupportedDocFlavors();

  /**
   * Identifies all the unsupported attributes of the given set of attributes
   * in the context of the specified document flavor.
   * <p>
   * The given flavor has to be supported by the print service (use
   * {@link #isDocFlavorSupported(DocFlavor)} to verify). The method will
   * return <code>null</code> if all given attributes are supported. Otherwise
   * a set of unsupported attributes are returned. The attributes in the
   * returned set may be completely unsupported or only the specific requested
   * value. If flavor is <code>null</code> the default document flavor of the
   * print service is used in the identification process.
   * </p>
   *
   * @param flavor document flavor to test, or <code>null</code>.
   * @param attributes set of printing attributes for a supposed job
   *
   * @return <code>null</code> if this print service supports all the given
   * attributes for the specified doc flavor. Otherwise the set of unsupported
   * attributes are returned.
   *
   * @throws IllegalArgumentException if <code>flavor</code> is unsupported
   */
  AttributeSet getUnsupportedAttributes(DocFlavor flavor, AttributeSet attributes);


  /**
   * Returns a hashcode for this print service.
   *
   * @return The hashcode.
   */
  int hashCode();

  /**
   * Determines a given attribute category is supported by this
   * print service implementation. This only tests for the category
   * not for any specific values of this category nor in the context
   * of a specific document flavor.
   *
   * @param category the category to check
   *
   * @return <code>true</code> if <code>category</code> is supported,
   * <code>false</code> otherwise.
   *
   * @throws NullPointerException if <code>category</code> is <code>null</code>
   * @throws IllegalArgumentException if <code>category</code> is a class not
   * implementing <code>Attribute</code>.
   */
  boolean isAttributeCategorySupported(Class<? extends Attribute> category);

  /**
   * Determines if a given attribute value is supported when creating a print
   * job for this print service.
   * <p>
   * If either the document flavor or the provided attributes are
   * <code>null</code> it is determined if the given attribute value is
   * supported in some combination of the available document flavors and
   * attributes of the print service. Otherwise it is checked for the
   * specific context of the given document flavor/attributes set.
   * </p>
   *
   * @param attrval the attribute value to check
   * @param flavor the document flavor to use, or <code>null</code>.
   * @param attributes set of attributes to use, or <code>null</code>.
   *
   * @return <code>true</code> if the attribute value is supported in the
   * requested context, <code>false</code> otherwise.
   *
   * @throws NullPointerException if <code>attrval</code> is <code>null</code>.
   * @throws IllegalArgumentException if <code>flavor</code> is not supported
   * by this print service
   */
  boolean isAttributeValueSupported(Attribute attrval, DocFlavor flavor, AttributeSet attributes);

  /**
   * Determines if a given document flavor is supported or not.
   *
   * @param flavor the document flavor to check
   *
   * @return <code>true</code> if <code>flavor</code> is supported,
   * <code>false</code> otherwise.
   *
   * @throws NullPointerException if <code>flavor</code> is null.
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
