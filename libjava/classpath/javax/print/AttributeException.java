/* AttributeException.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

/**
 * <code>AttributeException</code> specifies two methods a specific
 * subclass of {@link javax.print.PrintException} may implement to
 * provide further information of printing errors if unsupported
 * attribute classes or values of attributes are involved.
 * <p>
 * There exists no <code>PrintException</code> class implementing this 
 * interface. Providing these extensions in <code>PrintException</code> 
 * subclasses is left to the concrete print service implementation. 
 * </p> 
 * 
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface AttributeException
{
  /**
   * Returns the unsupported printing attribute classes for a print service
   * that does not support the attribute category at all. The returned 
   * class instances are sublcasses of the base interface {@link Attribute}.
   * 
   * @return The unsupported attribute classes, or <code>null</code> if there
   * are no such attribute classes.
   */
  Class[] getUnsupportedAttributes();
  
  /**
   * Returns the unsupported attribute values of printing attributes a specific
   * print service does support but not the particular provided value.
   *   
   * @return The unsupported attribute values, or <code>null</code> if there
   * are no such attributes values.
   */
  Attribute[] getUnsupportedValues();
}
