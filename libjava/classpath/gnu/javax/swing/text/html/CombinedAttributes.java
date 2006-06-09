/* CombinedAttributes.java -- A two combined sets of attributes
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


package gnu.javax.swing.text.html;

import java.io.Serializable;
import java.util.Enumeration;

import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;

/**
 * Contains the two combined attribute sets what are searched subsequently.
 * This is used to combine style sheet attributes with the HTML view attributes.
 * The parent cannot be used as the view may have its own attribute hierarchy.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)  
 */
public class CombinedAttributes implements AttributeSet, Serializable
{
  /**
   * Returns the elements from both enumerations.
   */
  class CombinedEnumeration implements Enumeration
  {
    /**
     * Create a combined enumeration that enumerates over two enumerations.
     * 
     * @param first the first enumeration to enumerate
     * @param second the second enumeration to enumerate
     */
    CombinedEnumeration(Enumeration first, Enumeration second)
    {
      a = first;
      b = second;
    }
    
    /**
     * The first enumeration (elements returned first)
     */
    final Enumeration a;
    
    /**
     * The second enumeration (elements returned later)
     */
    final Enumeration b;
    
    /** @inheritDoc */
    public boolean hasMoreElements()
    {
      return a.hasMoreElements() || b.hasMoreElements();
    }
    
    /** @inheritDoc */
    public Object nextElement()
    {
      return a.hasMoreElements() ? a.nextElement():b.nextElement();
    }
  }
  
  
  /**
   * The first attribute set.
   */
  final AttributeSet a;
  
  /**
   * The second attribute set.
   */
  final AttributeSet b;
  
  /**
   * Create the CombinedAttributes what search in the two sets. If any of the
   * two passed sets is null, another set is returned. Otherwise, the combined
   * attribute set is returned.
   * 
   * @param primary the first set (searched first)
   * @param secondary the second set (searched later).
   */
  public static AttributeSet combine(AttributeSet primary,
                                     AttributeSet secondary)
  {
    if (primary == null)
      return secondary;
    else if (secondary == null)
      return primary;
    else
      return new CombinedAttributes(primary, secondary);
  }
  
  /**
   * Create the CombinedAttributes what search in the two sets.
   * 
   * @param primary the first set (searched first)
   * @param secondary the second set (searched later).
   */
  private CombinedAttributes(AttributeSet primary, AttributeSet secondary)
  {
    a = primary;
    b = secondary;
  }

  /** @inheritDoc */
  public boolean containsAttribute(Object name, Object value)
  {
    return a.containsAttribute(name, value) || b.containsAttribute(name, value);
  }

  /** @inheritDoc */
  public boolean containsAttributes(AttributeSet attributes)
  {
    Enumeration names = attributes.getAttributeNames();
    Object name;
    while (names.hasMoreElements())
      {
        name = names.nextElement();
        if (!containsAttribute(name, attributes.getAttribute(name)))
          return false;
      }
    return true;
  }

  /** @inheritDoc */
  public AttributeSet copyAttributes()
  {
    SimpleAttributeSet copy = new SimpleAttributeSet();
    copy.addAttributes(a);
    copy.addAttributes(b);
    return copy;
  }

  /** @inheritDoc */
  public Object getAttribute(Object key)
  {
    Object value = a.getAttribute(key);
    if (value == null)
      value = b.getAttribute(key);
    
    return value;
  }

  /** @inheritDoc */
  public int getAttributeCount()
  {
    return a.getAttributeCount()+b.getAttributeCount();
  }

  /** @inheritDoc */
  public Enumeration getAttributeNames()
  {
    return new CombinedEnumeration(a.getAttributeNames(), b.getAttributeNames());
  }

  /**
   * There is no one.
   * 
   * @return null, always.
   */
  public AttributeSet getResolveParent()
  {
    return null;
  }

  /** @inheritDoc */
  public boolean isDefined(Object attrName)
  {
    return a.isDefined(attrName) || b.isDefined(attrName);
  }

  /** @inheritDoc */
  public boolean isEqual(AttributeSet attr)
  {
    if (attr.getAttributeCount() == getAttributeCount())
      return containsAttributes(attr);
    else
      return false;
  }
}
