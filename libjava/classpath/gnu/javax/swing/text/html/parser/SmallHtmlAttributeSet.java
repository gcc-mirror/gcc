/* SmallHtmlAttributeSet.java -- Small fixed HTML attribute set
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


package gnu.javax.swing.text.html.parser;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.NoSuchElementException;

import javax.swing.text.AttributeSet;
import javax.swing.text.html.HTML.Attribute;
import javax.swing.text.html.HTML.Tag;

/**
 * Small fixed HTML attribute set. The most of the HTML elements contain only
 * several attributes. With four attributes, the number of operations in more
 * complex algorithms is not larger than using the naive algorithm.
 *
 * Same as HtmlAttributeSet, this set allows both strings and non-string as
 * keys. The strings are case insensitive, the non strings are compared with
 * .equals.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class SmallHtmlAttributeSet
    implements AttributeSet, Cloneable, Serializable
{
  private static final long serialVersionUID = 1;

  /**
   * The keys, stored in this attribute set.
   */
  final Object[] keys;

  /**
   * The values, stored in this attribute set.
   */
  final Object[] values;

  /**
   * The parent, used for resolving the values, not found in this set.
   */
  final AttributeSet parent;

  /**
   * Create a new small fixed attribute set that contains the unchangeable copy
   * of the passed attribute set and inherits its parent.
   *
   * @param copyFrom the attribute set, containing the attribute values to copy.
   */
  public SmallHtmlAttributeSet(AttributeSet copyFrom)
  {
    int n = copyFrom.getAttributeCount();

    keys = new Object[n];
    values = new Object[n];
    parent = copyFrom.getResolveParent();

    Enumeration en = copyFrom.getAttributeNames();
    Object key;
    Object value;

    for (int i = 0; i < n; i++)
      {
        key = en.nextElement();
        keys[i] = key;
        value = copyFrom.getAttribute(key);
        values[i] = value;
      }
  }

  public boolean containsAttribute(Object name, Object value)
  {
    Object contains = getAttribute(name);
    if (value == null)
      return value == contains;
    else
      return value.equals(contains);
  }

  public boolean containsAttributes(AttributeSet attributes)
  {
    if (attributes == this)
      return true;
    Object v;
    for (int i = 0; i < keys.length; i++)
      {
        v = attributes.getAttribute(keys[i]);
        if (v != values[i])
          {
            if (values[i] == null)
              return false;
            else if (! values[i].equals(v))
              return false;
          }
      }
    return true;
  }

  /**
   * THIS can be safely returned as the set is not mutable.
   */
  public AttributeSet copyAttributes()
  {
    return this;
  }

  /**
   * Get the attribute value, matching this key. If not found in this set, the
   * call is delegated to parent.
   *
   * @return the value, matching key (or null if none).
   */
  public Object getAttribute(Object key)
  {
    // Null and HTML attributes or tags can be searched by direct comparison.
    if (key == null || key instanceof Attribute || key instanceof Tag)
      {
        for (int i = 0; i < keys.length; i++)
          {
            if (keys[i] == key)
              return values[i];
          }
      }

    // Strings are case insensitive. Only string can be match the string.
    else if (key instanceof String)
      {
        String ks = (String) key;
        for (int i = 0; i < keys.length; i++)
          {
            if (keys[i] instanceof String)
              if (ks.equalsIgnoreCase((String) keys[i]))
                return values[i];
          }
      }

    // Otherwise, defaults to .equals
    else
      {
        for (int i = 0; i < keys.length; i++)
          {
            if (key.equals(keys[i]))
              return values[i];
          }
      }

    if (parent != null)
      return parent.getAttribute(key);
    else
      return null;
  }

  /**
   * Get the number of the stored attributes.
   */
  public int getAttributeCount()
  {
    return keys.length;
  }

  /**
   * Get enumeration, containing the attribute names. No guard agains the
   * concurent modification is required as the set is not mutable.
   */
  public Enumeration getAttributeNames()
  {
    return new Enumeration()
    {
      int p = 0;

      public boolean hasMoreElements()
      {
        return p < keys.length;
      }

      public Object nextElement()
      {
        if (p < keys.length)
          return keys[p++];
        else
          throw new NoSuchElementException();
      }
    };
  }

  /**
   * Get the parent that this set uses to resolve the not found attributes.
   */
  public AttributeSet getResolveParent()
  {
    return parent;
  }

  /**
   * Check if the given attribute is defined in this set (not in the parent).
   */
  public boolean isDefined(Object attrName)
  {
    if (attrName instanceof String)
      attrName = ((String) attrName).toLowerCase();

    for (int i = 0; i < keys.length; i++)
      {
        if (attrName.equals(keys[i]))
          return true;
      }
    return false;
  }

  /**
   * Check this set and another set for equality by content.
   */
  public boolean isEqual(AttributeSet attr)
  {
    return keys.length == attr.getAttributeCount() && containsAttributes(attr);
  }

  /**
   * It is safe to return THIS on cloning, if one happens.
   */
  protected Object clone()
  {
    return this;
  }
}
