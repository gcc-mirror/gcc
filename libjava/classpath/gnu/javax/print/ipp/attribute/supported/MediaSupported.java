/* MediaSupported.java -- 
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


package gnu.javax.print.ipp.attribute.supported;

import gnu.javax.print.ipp.IppUtilities;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

import javax.print.attribute.SupportedValuesAttribute;
import javax.print.attribute.TextSyntax;
import javax.print.attribute.standard.Media;

/**
 * MediaSupported attribute provides the keyword values
 * of the media types supported by the printer object.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class MediaSupported extends TextSyntax 
    implements SupportedValuesAttribute
{

  /**
   * Creates a <code>MediaSupported</code> object with the 
   * given value and locale.
   *
   * @param value the value for this syntax
   * @param locale the locale to use, if <code>null</code> the default
   * locale is used.
   *
   * @throws NullPointerException if value is null
   */
  public MediaSupported(String value, Locale locale)
  {
    super(value, locale);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>MediaSupported</code> itself.
   */
  public Class getCategory()
  {
    return MediaSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "media-supported".
   */
  public String getName()
  {
    return "media-supported";
  }
  
  /**
   * Constructs an array from a set of -supported attributes.
   * @param set set to process
   * @return The constructed array.
   */
  public static Media[] getAssociatedAttributeArray(Set set) 
  {
    MediaSupported tmp;
    Media tmp2;
    ArrayList result = new ArrayList();          
    Iterator it = set.iterator();
    while (it.hasNext())
      {
        tmp = (MediaSupported) it.next();
        tmp2 = (Media) IppUtilities.getEnumAttribute("media", tmp.toString());
        if (tmp2 != null)
          result.add(tmp2);
      }            
    return (Media[]) result.toArray(new Media[result.size()]);
  }

}
