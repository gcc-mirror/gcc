/* EmptyAttributeSet.java -- An empty attribute set
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


package javax.swing.text;

import java.util.Enumeration;
import java.util.NoSuchElementException;

/**
 * An immutable, empty attribute set.
 *
 * @see SimpleAttributeSet#EMPTY
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
final class EmptyAttributeSet
  implements AttributeSet
{

  /**
   * Always return false as this AttributeSet doesn't contain any attributes.
   */
  public boolean containsAttribute(Object name, Object value)
  {
    return false;
  }

  /**
   * Return true only if the attributes argument also contains no attributes.
   */
  public boolean containsAttributes(AttributeSet attributes)
  {
    return attributes.getAttributeCount() == 0;
  }

  /**
   * Return this, as this is immutable.
   */
  public AttributeSet copyAttributes()
  {
    return this;
  }

  /**
   * Always return null as this AttributeSet doesn't contain any attributes.
   */
  public Object getAttribute(Object key)
  {
    return null;
  }

  /**
   * Always return 0.
   */
  public int getAttributeCount()
  {
    return 0;
  }

  /**
   * Returns an empty Enumeration.
   */
  public Enumeration getAttributeNames()
  {
    return new Enumeration()
    {
      public boolean hasMoreElements()
      {
        return false;
      }

      public Object nextElement()
      {
        throw new NoSuchElementException("No more elements");
      }
      
    };
  }

  /**
   * Always return null as this has no resolve parent.
   */
  public AttributeSet getResolveParent()
  {
    return null;
  }

  /**
   * Always return false as this AttributeSet doesn't contain any attributes.
   */
  public boolean isDefined(Object attrName)
  {
    return false;
  }

  /**
   * Other attribute sets are equal if they are empty too.
   */
  public boolean isEqual(AttributeSet attr)
  {
    return attr.getAttributeCount() == 0;
  }

  /**
   * Other objects are equal if it's the same instance as this, or if
   * it's another attribute set without attributes.
   */
  public boolean equals(Object o)
  {
    boolean eq = o == this;
    if (! eq)
      {
        eq = (o instanceof AttributeSet)
             && ((AttributeSet) o).getAttributeCount() == 0;
      }
    return eq;
  }
}
