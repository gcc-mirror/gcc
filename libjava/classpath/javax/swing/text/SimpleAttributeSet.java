/* SimpleAttributeSet.java --
   Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * A set of attributes.
 */
public class SimpleAttributeSet
  implements MutableAttributeSet, Serializable, Cloneable
{
  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = 8267656273837665219L;

  /**
   * An empty attribute set.
   */
  public static final AttributeSet EMPTY = new EmptyAttributeSet();

  /** Storage for the attributes. */
  Hashtable tab;

  /**
   * Creates a new attribute set that is initially empty.
   */
  public SimpleAttributeSet()
  {
    tab = new Hashtable();
  }
  
  /**
   * Creates a new <code>SimpleAttributeSet</code> with the same attributes
   * and resolve parent as the specified set.
   * 
   * @param a  the attributes (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   */
  public SimpleAttributeSet(AttributeSet a)
  {
    tab = new Hashtable();
    addAttributes(a);
  }

  /**
   * Adds an attribute with the given <code>name</code> and <code>value</code>
   * to the set.  If the set already contains an attribute with the given
   * <code>name</code>, the attribute value is updated.
   * 
   * @param name  the attribute name (<code>null</code> not permitted).
   * @param value  the value (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   */
  public void addAttribute(Object name, Object value)
  {
    tab.put(name, value);
  }

  /**
   * Adds all the attributes from <code>attributes</code> to this set.
   * 
   * @param attributes  the set of attributes to add (<code>null</code> not
   *                    permitted).
   *                    
   * @throws NullPointerException if <code>attributes</code> is 
   *         <code>null</code>.
   */
  public void addAttributes(AttributeSet attributes)
  {
    Enumeration e = attributes.getAttributeNames();
    while (e.hasMoreElements())
      {
        Object name = e.nextElement();
        Object val = attributes.getAttribute(name);
        tab.put(name, val);
      }
  }

  /**
   * Returns a clone of the attribute set.
   * 
   * @return A clone of the attribute set.
   */
  public Object clone()
  {
    SimpleAttributeSet attr = null;
    try
      {
        attr = (SimpleAttributeSet) super.clone();
        attr.tab = (Hashtable) tab.clone();
      }
    catch (CloneNotSupportedException ex)
      {
        assert false;
      }
    return attr;
  }

  /**
   * Returns true if the given name and value represent an attribute
   * found either in this AttributeSet or in its resolve parent hierarchy.
   * @param name the key for the attribute
   * @param value the value for the attribute
   * @return true if the attribute is found here or in this set's resolve
   * parent hierarchy
   */
  public boolean containsAttribute(Object name, Object value)
  {
    if (value == null)
      throw new NullPointerException("Null 'value' argument.");
    if (tab.containsKey(name))
      return tab.get(name).equals(value);
    else
      {
        AttributeSet p = getResolveParent();
        if (p != null)
          return p.containsAttribute(name, value);
        else
          return false;
      }
  }
  
  /**
   * Returns true if the given name and value are found in this AttributeSet.
   * Does not check the resolve parent.
   * @param name the key for the attribute
   * @param value the value for the attribute
   * @return true if the attribute is found in this AttributeSet
   */
  boolean containsAttributeLocally(Object name, Object value)
  {
    return tab.containsKey(name) 
      && tab.get(name).equals(value);
  }

  /**
   * Returns <code>true</code> of this <code>AttributeSet</code> contains all
   * of the specified <code>attributes</code>.
   *
   * @param attributes the requested attributes
   *
   * @return <code>true</code> of this <code>AttributeSet</code> contains all
   *         of the specified <code>attributes</code>
   */
  public boolean containsAttributes(AttributeSet attributes)
  {
    Enumeration e = attributes.getAttributeNames();
    while (e.hasMoreElements())
      {
        Object name = e.nextElement();
        Object val = attributes.getAttribute(name);
        if (! containsAttribute(name, val))
          return false;		
      }
    return true;
  }

  /**
   * Creates and returns a copy of this <code>AttributeSet</code>.
   *
   * @return a copy of this <code>AttributeSet</code>
   */
  public AttributeSet copyAttributes()
  {
    return (AttributeSet) clone();
  }

  /**
   * Checks this set for equality with an arbitrary object.
   * 
   * @param obj  the object (<code>null</code> permitted).
   * 
   * @return <code>true</code> if this set is equal to <code>obj</code>, and
   *         <code>false</code> otherwise. 
   */
  public boolean equals(Object obj)
  {
    return 
      (obj instanceof AttributeSet)
      && this.isEqual((AttributeSet) obj);
  }

  /**
   * Returns the value of the specified attribute, or <code>null</code> if 
   * there is no attribute with that name.  If the attribute is not defined
   * directly in this set, the parent hierarchy (if there is one) will be
   * used.
   * 
   * @param name  the attribute (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public Object getAttribute(Object name)
  {
    Object val = tab.get(name);
    if (val != null) 
      return val;

    AttributeSet p = getResolveParent();
    if (p != null)
      return p.getAttribute(name);

    return null;
  }

  /**
   * Returns the number of attributes stored in this set, plus 1 if a parent
   * has been specified (the reference to the parent is stored as a special 
   * attribute).  The attributes stored in the parent do NOT contribute
   * to the count.
   * 
   * @return The attribute count.
   */
  public int getAttributeCount()
  {
    return tab.size();
  }

  /**
   * Returns an enumeration of the attribute names.
   * 
   * @return An enumeration of the attribute names.
   */
  public Enumeration<?> getAttributeNames()
  {
    return tab.keys();
  }

  /**
   * Returns the resolving parent.
   * 
   * @return The resolving parent (possibly <code>null</code>).
   * 
   * @see #setResolveParent(AttributeSet)
   */
  public AttributeSet getResolveParent()
  {
    return (AttributeSet) tab.get(ResolveAttribute);
  }

  /**
   * Returns a hash code for this instance.
   * 
   * @return A hash code.
   */
  public int hashCode()
  {
    return tab.hashCode();
  }

  /**
   * Returns <code>true</code> if the given attribute is defined in this set,
   * and <code>false</code> otherwise.  The parent attribute set is not
   * checked.
   * 
   * @param attrName  the attribute name (<code>null</code> not permitted).
   */
  public boolean isDefined(Object attrName)
  {
    return tab.containsKey(attrName);
  }

  /**
   * Returns <code>true</code> if the set contains no attributes, and 
   * <code>false</code> otherwise.  Note that the resolving parent is 
   * stored as an attribute, so this method will return <code>false</code> if 
   * a resolving parent is set.
   * 
   * @return <code>true</code> if the set contains no attributes, and 
   * <code>false</code> otherwise.
   */
  public boolean isEmpty()
  {
    return tab.isEmpty();	
  }

  /**
   * Returns true if the given set has the same number of attributes
   * as this set and <code>containsAttributes(attr)</code> returns
   * <code>true</code>.
   * 
   * @param attr  the attribute set (<code>null</code> not permitted).
   * 
   * @return A boolean.
   * 
   * @throws NullPointerException if <code>attr</code> is <code>null</code>.
   */
  public boolean isEqual(AttributeSet attr)
  {
    return getAttributeCount() == attr.getAttributeCount()
      && this.containsAttributes(attr);
  }
    
  /**
   * Removes the attribute with the specified <code>name</code>, if this 
   * attribute is defined.  This method will only remove an attribute from
   * this set, not from the resolving parent.
   * 
   * @param name  the attribute name (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public void removeAttribute(Object name)
  {
    tab.remove(name);
  }

  /**
   * Removes attributes from this set if they are found in the 
   * given set.  Only attributes whose key AND value are removed.
   * Removes attributes only from this set, not from the resolving parent.  
   * Since the resolving parent is stored as an attribute, if 
   * <code>attributes</code> has the same resolving parent as this set, the
   * parent will be removed from this set.
   * 
   * @param attributes  the attributes (<code>null</code> not permitted).
   */
  public void removeAttributes(AttributeSet attributes)
  {
    Enumeration e = attributes.getAttributeNames();
    while (e.hasMoreElements())
      {
        Object name = e.nextElement();
        Object val = attributes.getAttribute(name);
        if (containsAttributeLocally(name, val))
          removeAttribute(name);     
      }
  }

  /**
   * Removes the attributes listed in <code>names</code>.
   * 
   * @param names  the attribute names (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>names</code> is <code>null</code> 
   *         or contains any <code>null</code> values.
   */
  public void removeAttributes(Enumeration<?> names)
  {
    while (names.hasMoreElements())
      {
        removeAttribute(names.nextElement());
      }	
  }

  /**
   * Sets the reolving parent for this set.  When looking up an attribute, if
   * it is not found in this set, then the resolving parent is also used for
   * the lookup.
   * <p>
   * Note that the parent is stored as an attribute, and will contribute 1 to 
   * the count returned by {@link #getAttributeCount()}. 
   * 
   * @param parent  the parent attribute set (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>parent</code> is <code>null</code>.
   * 
   * @see #setResolveParent(AttributeSet)
   */
  public void setResolveParent(AttributeSet parent)
  {
    addAttribute(ResolveAttribute, parent);
  }
  
  /**
   * Returns a string representation of this instance, typically used for
   * debugging purposes.
   * 
   * @return A string representation of this instance.
   */
  public String toString()
  {
    return tab.toString();
  }    
}
