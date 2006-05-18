/* PrintAttributeException.java -- 
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


package gnu.javax.print;

import javax.print.AttributeException;
import javax.print.PrintException;
import javax.print.attribute.Attribute;

/**
 * A <code>PrintException</code> further refining the exception
 * cause by providing an implementation of the print exception
 * interface <code>AttributeException</code>.
 * 
 * @see javax.print.PrintException
 * @see javax.print.AttributeException
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PrintAttributeException extends PrintException 
  implements AttributeException
{
  private Class[] categories;
  private Attribute[] values;
  
  /**
   * Constructs a <code>PrintAttributeException</code>
   * with the given unsupported attributes and/or values.
   * 
   * @param unsupportedAttributes the unsupported categories, 
   * may be <code>null</code>.
   * @param unsupportedValues the unsupported attribute values, 
   * may be <code>null</code>.
   */
  public PrintAttributeException(Class[] unsupportedAttributes,
                                 Attribute[] unsupportedValues)
  {
    super();
    categories = unsupportedAttributes;
    values = unsupportedValues;
  }

  /**
   * Constructs a <code>PrintAttributeException</code>
   * with the given unsupported attributes and/or values.
   * 
   * @param e chained exception
   * @param unsupportedAttributes the unsupported categories, 
   * may be <code>null</code>.
   * @param unsupportedValues the unsupported attribute values, 
   * may be <code>null</code>.
   */
  public PrintAttributeException(Exception e,
      Class[] unsupportedAttributes, Attribute[] unsupportedValues)
  {
    super(e);
    categories = unsupportedAttributes;
    values = unsupportedValues;
  }

  /**
   * Constructs a <code>PrintAttributeException</code>
   * with the given unsupported attributes and/or values.
   * 
   * @param s detailed message
   * @param unsupportedAttributes the unsupported categories, 
   * may be <code>null</code>.
   * @param unsupportedValues the unsupported attribute values, 
   * may be <code>null</code>.
   */
  public PrintAttributeException(String s,
      Class[] unsupportedAttributes, Attribute[] unsupportedValues)
  {
    super(s);
    categories = unsupportedAttributes;
    values = unsupportedValues;
  }

  /**
   * Constructs a <code>PrintAttributeException</code>
   * with the given unsupported attributes and/or values.
   * 
   * @param s detailed message
   * @param e chained exception
   * @param unsupportedAttributes the unsupported categories, 
   * may be <code>null</code>.
   * @param unsupportedValues the unsupported attribute values, 
   * may be <code>null</code>.
   */
  public PrintAttributeException(String s, Exception e,
      Class[] unsupportedAttributes, Attribute[] unsupportedValues)
  {
    super(s, e);
    categories = unsupportedAttributes;
    values = unsupportedValues;
  }

  /**
   * @see AttributeException#getUnsupportedAttributes()
   */
  public Class[] getUnsupportedAttributes()
  {
    return categories;
  }

  /**
   * @see AttributeException#getUnsupportedValues()
   */
  public Attribute[] getUnsupportedValues()
  {
    return values;
  }
}
