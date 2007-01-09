/* MultiAttributeSet.java -- Multiplexes between a set of AttributeSets
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


package javax.swing.text.html;

import java.util.Enumeration;
import java.util.NoSuchElementException;

import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;

/**
 * An AttributeSet impl that multiplexes between a set of other AttributeSets.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
class MultiAttributeSet
  implements AttributeSet
{

  /**
   * The Enumeration for the multiplexed names.
   */
  private class MultiNameEnumeration
    implements Enumeration
  {
    /**
     * The index of the current AttributeSet.
     */
    private int index;

    /**
     * The names Enumeration of the current AttributeSet.
     */
    private Enumeration current;

    /**
     * Creates a new instance.
     */
    MultiNameEnumeration()
    {
      index = 0;
      current = multi[0].getAttributeNames();
    }

    public boolean hasMoreElements()
    {
      return current.hasMoreElements() || index < multi.length - 1;
    }

    public Object nextElement()
    {
      if (! current.hasMoreElements())
        {
          if (index < multi.length - 1)
            {
              index++;
              current = multi[index].getAttributeNames();
            }
          else
            throw new NoSuchElementException();
        }
      return current.nextElement();
    }
    
  }

  /**
   * The AttributeSets to multiplex.
   */
  AttributeSet[] multi;

  /**
   * Provided for subclasses that need to initialize via {@link #init}.
   */
  MultiAttributeSet()
  {
    // Nothing to do here.
  }

  /**
   * Creates a new instance.
   *
   * @param m the AttributeSets to multiplex
   */
  MultiAttributeSet(AttributeSet[] m)
  {
    init(m);
  }

  /**
   * Provided for subclasses to initialize the attribute set.
   *
   * @param m the attributes to multiplex
   */
  void init(AttributeSet[] m)
  {
    multi = m;
  }

  public boolean containsAttribute(Object name, Object value)
  {
    boolean ret = false;
    for (int i = 0; i < multi.length && ret == false; i++)
      {
        if (multi[i].containsAttribute(name, value))
          ret = true;
      }
    return ret;
  }

  public boolean containsAttributes(AttributeSet attributes)
  {
    boolean ret = true;
    Enumeration e = attributes.getAttributeNames();
    while (ret && e.hasMoreElements())
      {
        Object key = e.nextElement();
        ret = attributes.getAttribute(key).equals(getAttribute(key));
      }
    return ret;
  }

  public AttributeSet copyAttributes()
  {
    SimpleAttributeSet copy = new SimpleAttributeSet();
    for (int i = 0; i < multi.length; i++)
      {
        copy.addAttributes(multi[i]);
      }
    return copy;
  }

  public Object getAttribute(Object key)
  {
    Object ret = null;
    for (int i = 0; i < multi.length && ret == null; i++)
      {
        ret = multi[i].getAttribute(key);
      }
    return ret;
  }

  public int getAttributeCount()
  {
    int n = 0;
    for (int i = 0; i < multi.length; i++)
      {
        n += multi[i].getAttributeCount();
      }
    return n;
  }

  public Enumeration getAttributeNames()
  {
    return new MultiNameEnumeration();
  }

  public AttributeSet getResolveParent()
  {
    return null;
  }

  public boolean isDefined(Object attrName)
  {
    boolean ret = false;
    for (int i = 0; i < multi.length && ! ret; i++)
      ret = multi[i].isDefined(attrName);
    return ret;
  }

  public boolean isEqual(AttributeSet attr)
  {
    return getAttributeCount() == attr.getAttributeCount()
           && containsAttributes(attr);
  }

}
