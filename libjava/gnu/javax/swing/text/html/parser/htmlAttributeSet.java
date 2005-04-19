/* htmlAttributeSet.java -- A set to store HTML attributes
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.javax.swing.text.html.parser;

import java.util.Enumeration;

import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;

/**
 * A set, adapted to store HTML attributes.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class htmlAttributeSet
  extends SimpleAttributeSet
{
  public static final htmlAttributeSet EMPTY_HTML_ATTRIBUTE_SET =
    new htmlAttributeSet();
  AttributeSet parent;

  /**
   * Looks in this set and, if not found, later looks in the parent set.
   * Calls toString(), allowing to pass as HTML.Attribute, as String
   * to this method.
   * @param key A key to search for a value.
   * @return The value, if one is defined.
   */
  public Object getAttribute(Object _key)
  {
    Object key = _key.toString().toLowerCase();

    Object v = super.getAttribute(key);
    if (v != null)
      return v;
    else if (parent != null)
      return parent.getAttribute(key);
    else
      return null;
  }

  /**
   * The name set must return HTML.Attribute and not a string,
   * where applicable.
   */
  public Enumeration getAttributeNames()
  {
    // Replace the string keys by HTML.attribute, where applicable
    final Enumeration enumeration = super.getAttributeNames();

    return new Enumeration()
      {
        public boolean hasMoreElements()
        {
          return enumeration.hasMoreElements();
        }

        public Object nextElement()
        {
          Object key = enumeration.nextElement();
          HTML.Attribute hKey = HTML.getAttributeKey((String) key);
          if (hKey != null)
            return hKey;
          else
            return key;
        }
      };
  }

  /**
   * Set the parent set, containing the default values.
   * @param a_parent
   */
  public void setResolveParent(AttributeSet a_parent)
  {
    parent = a_parent;
  }

  /**
   * Get the parent set, containing the default values.
   * @return
   */
  public AttributeSet getResolveParent()
  {
    return parent;
  }

  /**
   * Add the attribute to this attribute set.
   * @param key Attribute key (will be case insensitive)
   * @param value Attribute value
   */
  public void addAttribute(Object key, Object value)
  {
    super.addAttribute(key.toString().toLowerCase(), value);
  }
}
