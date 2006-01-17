/* Media.java --
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

package javax.print.attribute.standard;

import javax.print.attribute.DocAttribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;


/**
 * The <code>Media</code> printing attribute specifies which
 * type of media should be used for printing.
 * <p>
 * The media to be used can be specified in three ways represented
 * by the media subclasses {@link javax.print.attribute.standard.MediaTray},
 * {@link javax.print.attribute.standard.MediaName} and 
 * {@link javax.print.attribute.standard.MediaSizeName}:
 * <ul>
 * <li>Selection by paper source - selection of printer tray to be used.</li>
 * <li>Selection by name - e.g. A4 paper.</li>
 * <li>Selection by standard sizes - e.g. ISO A5, JIS B4.</li>
 * </ul>
 * Each of the sublcasses represents the IPP attribute <code>media</code>
 * and provides predefined values to be used.
 * </p> 
 * <p>
 * <b>IPP Compatibility:</b> Media is an IPP 1.1 attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public abstract class Media extends EnumSyntax
  implements DocAttribute, PrintRequestAttribute, PrintJobAttribute
{
  private static final long serialVersionUID = -2823970704630722439L;

  /**
   * Constructs a <code>Media</code> object.
   * 
   * @param value the enum value.
   */
  protected Media(int value)
  {
    super(value);
  }
    
  /**
   * Tests if the given object is equal to this object.
   * The objects are considered equal if both are of the same
   * Media subclass, not null and the values are equal.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal, 
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;
    
    return (obj.getClass() == this.getClass()
            && ((Media) obj).getValue() == this.getValue());
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>Media</code> itself.
   */
  public Class getCategory()
  {
    return Media.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "media".
   */
  public String getName()
  {
    return "media";
  }
}
