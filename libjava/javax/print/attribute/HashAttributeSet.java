/* HashAttributeSet.java -- 
   Copyright (C) 2003 Free Software Foundation, Inc.

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

package javax.print.attribute;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;

public class HashAttributeSet implements AttributeSet, Serializable
{
  private static final long serialVersionUID = 5311560590283707917L;
  
  private Class interfaceName;
  private HashMap attributeMap = new HashMap();

  /**
   * Creates an empty <code>HashAttributeSet</code> object.
   */
  public HashAttributeSet()
  {
    this(Attribute.class);
  }

  /**
   * Creates a <code>HashAttributeSet</code> object with the given
   * attribute in it.
   *
   * @param attribute the attribute to put into the set
   *
   * @exception NullPointerException if attribute is null
   */
  public HashAttributeSet(Attribute attribute)
  {
    this(attribute, Attribute.class);
  }

  /**
   * Creates a <code>HashAttributeSet</code> object with the given
   * attributes in it.
   *
   * @param attributes the attributes to put into the set
   *
   * @exception NullPointerException If attributes is null
   */
  public HashAttributeSet(Attribute[] attributes)
  {
    this(attributes, Attribute.class);
  }

  /**
   * Creates a <code>HashAttributeSet</code> object with the given
   * attributes in it.
   *
   * @param attributes the attributes to put into the set
   *
   * @exception NullPointerException If attributes is null
   */
  public HashAttributeSet(AttributeSet attributes)
  {
    this(attributes, Attribute.class);
  }

  /**
   * Creates an empty <code>HashAttributeSet</code> object.
   *
   * @param interfaceName the interface that all members must implement
   *
   * @exception NullPointerException if interfaceName is null
   */
  protected HashAttributeSet(Class interfaceName)
  {
    if (interfaceName == null)
      throw new NullPointerException("interfaceName may not be null");
    
    this.interfaceName = interfaceName;
  }
  
  /**
   * Creates an empty <code>HashAttributeSet</code> object.
   *
   * @exception ClassCastException if attribute is not an interface of
   * interfaceName
   * @exception NullPointerException if attribute or interfaceName is null
   */
  protected HashAttributeSet(Attribute attribute, Class interfaceName)
  {
    this(interfaceName);
    
    if (attribute == null)
      throw new NullPointerException();
    
    addInternal(attribute, interfaceName);
  }

  /**
   * Creates an empty <code>HashAttributeSet</code> object.
   *
   * @exception ClassCastException if any element of attributes is not an
   * interface of interfaceName
   * @exception NullPointerException if attributes or interfaceName is null
   */
  protected HashAttributeSet(Attribute[] attributes, Class interfaceName)
  {
    this(interfaceName);
    
    if (attributes == null)
      throw new NullPointerException();
    
    for (int index = 0; index < attributes.length; index++)
      addInternal(attributes[index], interfaceName);
  }

  /**
   * Creates an empty <code>HashAttributeSet</code> object.
   *
   * @exception ClassCastException if any element of attributes is not an
   * interface of interfaceName
   */
  public HashAttributeSet(AttributeSet attributes, Class interfaceName)
  {
    this(interfaceName);
    
    if (attributes != null)
      addAllInternal(attributes, interfaceName);
  }

  /**
   * Adds the given attribute to the set.
   *
   * @param attribute the attribute to add
   *
   * @return true if the attribute set has changed, false otherwise
   *
   * @exception NullPointerException if attribute is null
   * @exception UnmodifiableSetException if this attribute set does not
   * support this action.
   */
  public boolean add(Attribute attribute)
  {
    return addInternal(attribute, interfaceName);
  }

  private boolean addInternal(Attribute attribute, Class interfaceName)
  {
    if (attribute == null)
      throw new NullPointerException("attribute may not be null");

    AttributeSetUtilities.verifyAttributeCategory(interfaceName,
						  this.interfaceName);

    Object old = attributeMap.put
      (attribute.getCategory(), AttributeSetUtilities.verifyAttributeValue
                                  (attribute, interfaceName));
    return !attribute.equals(old);
  }

  /**
   * Adds the given attributes to the set.
   *
   * @param attributes the attributes to add
   *
   * @return true if the attribute set has changed, false otherwise
   *
   * @exception UnmodifiableSetException if this attribute set does not
   * support this action.
   */
  public boolean addAll(AttributeSet attributes)
  {
    return addAllInternal(attributes, interfaceName);
  }

  private boolean addAllInternal(AttributeSet attributes, Class interfaceName)
  {
    boolean modified = false;
    Attribute[] array = attributes.toArray();

    for (int index = 0; index < array.length; index++)
      if (addInternal(array[index], interfaceName))
        modified = true;

    return modified;
  }

  /**
   * Removes all attributes from this attribute set.
   *
   * @exception UnmodifiableSetException if this attribute set does not
   * support this action.
   */
  public void clear()
  {
    attributeMap.clear();
  }

  /**
   * Checks if this attribute set contains an entry with the given category.
   *
   * @param category the category to test for
   *
   * @result true if the category exists in this attribute set, false otherwise.
   */
  public boolean containsKey(Class category)
  {
    return attributeMap.containsKey(category);
  }

  /**
   * Checks if this attribute set contains an entry with the given attribute.
   *
   * @param attribute the attribute to test for
   *
   * @result true if the attribute exists in this attribute set,
   * false otherwise.
   */
  public boolean containsValue(Attribute attribute)
  {
    return attributeMap.containsValue(attribute);
  }

  /**
   * Tests of obj is equal to this object.
   *
   * @param obj the object to test
   *
   * @returns true if both objects are equal, false otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof HashAttributeSet))
      return false;

    return attributeMap.equals(((HashAttributeSet) obj).attributeMap);
  }

  /**
   * Returns the attribute value that is connected to the given attribute
   * category. If the attribute set does not contains the given category null
   * will be returned.
   *
   * @param category the attribute category to return the attribute value for
   *
   * @return the attribute associated to category, or null
   */
  public Attribute get(Class category)
  {
    return (Attribute) attributeMap.get(category);
  }
  
  /**
   * Returns the hashcode for this object.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return attributeMap.hashCode() + interfaceName.hashCode();
  }

  /**
   * Checks if the attribute set is empty.
   *
   * @return true if the attribute set is empty, false otherwise
   */
  public boolean isEmpty()
  {
    return attributeMap.isEmpty();
  }

  /**
   * Removes the entry with the given attribute in it.
   *
   * @param attribute the attribute value of the entry to be removed
   *
   * @return true if the attribute set has changed, false otherwise.
   *
   * @exception UnmodifiableSetException if this attribute set does not
   * support this action.
   */
  public boolean remove(Attribute attribute)
  {
    if (attribute == null)
      return false;

    return attributeMap.remove(attribute.getCategory()) != null;
  }

  /**
   * Removes the entry with the given category in it.
   *
   * @param category the category value of the entry to be removed
   *
   * @return true if the attribute set has changed, false otherwise.
   */
  public boolean remove(Class category)
  {
    if (category == null)
      return false;

    return attributeMap.remove(category) != null;
  }

  /**
   * Returns the number of elements in this attribute set.
   *
   * @return the number of elements.
   */
  public int size()
  {
    return attributeMap.size();
  }

  /**
   * Returns the content of the attribute set as an array
   *
   * @return an array of attributes
   */
  public Attribute[] toArray()
  {
    int index = 0;
    Iterator it = attributeMap.entrySet().iterator();
    Attribute[] array = new Attribute[size()];

    while (it.hasNext())
      {
        array[index] = (Attribute) it.next();
        index++;
      }
    
    return array;
  }
}
