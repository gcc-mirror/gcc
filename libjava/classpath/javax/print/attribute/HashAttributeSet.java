/* HashAttributeSet.java -- 
   Copyright (C) 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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

package javax.print.attribute;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;

/**
 * <code>HashAttributeSet</code> provides an implementation of
 * {@link javax.print.attribute.AttributeSet}.
 */
public class HashAttributeSet implements AttributeSet, Serializable
{
  private static final long serialVersionUID = 5311560590283707917L;
  
  private Class myInterface;
  private transient HashMap attributeMap = new HashMap();

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
   * @param attributes the array of attributes to put into the set. If
   * <code>null</code> an empty set is created.
   *
   * @exception NullPointerException if one of the attributes of the given
   * array is null.
   */
  public HashAttributeSet(Attribute[] attributes)
  {
    this(attributes, Attribute.class);
  }

  /**
   * Creates a <code>HashAttributeSet</code> object with attributes
   * of the given attributes set in it.
   *
   * @param attributes the attributes set to put into the set. If 
   * <code>null</code> an empty set is created.
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
  protected HashAttributeSet(Class<?> interfaceName)
  {
    if (interfaceName == null)
      throw new NullPointerException("interfaceName may not be null");
    
    myInterface = interfaceName;
  }
  
  /**
   * Creates a <code>HashAttributeSet</code> object with the given
   * attribute in it.
   * 
   * @param attribute the attribute to put into the set.
   * @param interfaceName the interface that all members must implement.
   *
   * @exception ClassCastException if attribute is not an interface of
   * interfaceName
   * @exception NullPointerException if attribute or interfaceName is null
   */
  protected HashAttributeSet(Attribute attribute, Class<?> interfaceName)
  {
    this(interfaceName);
    
    if (attribute == null)
      throw new NullPointerException();
    
    addInternal(attribute, interfaceName);
  }

  /**
   * Creates a <code>HashAttributeSet</code> object with the given
   * attributes in it.
   *
   * @param attributes the array of attributes to put into the set. If
   * <code>null</code> an empty set is created.
   * @param interfaceName the interface that all members must implement.
   *
   * @exception ClassCastException if any element of attributes is not an
   * interface of interfaceName
   * @exception NullPointerException if attributes or interfaceName is null
   */
  protected HashAttributeSet(Attribute[] attributes, Class<?> interfaceName)
  {
    this(interfaceName);
    
    if (attributes != null)
      {
        for (int index = 0; index < attributes.length; index++)
          addInternal(attributes[index], interfaceName);
      }
  }

  /**
   * Creates a <code>HashAttributeSet</code> object with attributes
   * of the given attributes set in it.
   *
   * @param attributes the attributes set to put into the set. If 
   * <code>null</code> an empty set is created.
   * @param interfaceName the interface that all members must implement.
   *
   * @exception ClassCastException if any element of attributes is not an
   * interface of interfaceName
   */
  protected HashAttributeSet(AttributeSet attributes, Class<?> interfaceName)
  {
    this(interfaceName);
    
    if (attributes != null)
      addAllInternal(attributes, interfaceName);
  }

  /**
   * Adds the specified attribute value to this attribute set 
   * if it is not already present.
   * 
   * This operation removes any existing attribute of the same category 
   * before adding the given attribute to the set. 
   * 
   * @param attribute the attribute to add.
   * @return <code>true</code> if the set is changed, false otherwise.
   * @throws NullPointerException if the attribute is <code>null</code>.
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  public boolean add(Attribute attribute)
  {
    return addInternal(attribute, myInterface);
  }

  private boolean addInternal(Attribute attribute, Class interfaceName)
  {
    if (attribute == null)
      throw new NullPointerException("attribute may not be null");

    AttributeSetUtilities.verifyAttributeCategory(interfaceName,
                                                  myInterface);

    Object old = attributeMap.put
      (attribute.getCategory(), AttributeSetUtilities.verifyAttributeValue
                                  (attribute, interfaceName));
    return !attribute.equals(old);
  }

  /**
   * Adds all of the elements in the specified set to this attribute set.
   * 
   * @param attributes the set of attributes to add.
   * @return <code>true</code> if the set is changed, false otherwise.
   * @throws UnmodifiableSetException if the set does not support modification.
   * 
   * @see #add(Attribute)
   */
  public boolean addAll(AttributeSet attributes)
  {
    return addAllInternal(attributes, myInterface);
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
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  public void clear()
  {
    attributeMap.clear();
  }

  /**
   * Checks if this attributes set contains an attribute with the given 
   * category.
   * 
   * @param category the category to test for.
   * @return <code>true</code> if an attribute of the category is contained
   * in the set, <code>false</code> otherwise.
   */
  public boolean containsKey(Class<?> category)
  {
    return attributeMap.containsKey(category);
  }

  /**
   * Checks if this attribute set contains the given attribute.
   * 
   * @param attribute the attribute to test for.
   * @return <code>true</code> if the attribute is contained in the set,
   * <code>false</code> otherwise.
   */
  public boolean containsValue(Attribute attribute)
  {
    return attributeMap.containsValue(attribute);
  }

  /**
   * Tests this set for equality with the given object. <code>true</code> is
   * returned, if the given object is also of type <code>AttributeSet</code>
   * and the contained attributes are the same as in this set.
   * 
   * @param obj the Object to test.
   * @return <code>true</code> if equal, false otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof HashAttributeSet))
      return false;

    return attributeMap.equals(((HashAttributeSet) obj).attributeMap);
  }

  /**
   * Returns the attribute object contained in this set for the given attribute
   * category. 
   * 
   * @param category the category of the attribute. A <code>Class</code> 
   * instance of a class implementing the <code>Attribute</code> interface. 
   * @return The attribute for this category or <code>null</code> if no 
   * attribute is contained for the given category. 
   * @throws NullPointerException if category is null.
   * @throws ClassCastException if category is not implementing 
   * <code>Attribute</code>.
   */
  public Attribute get(Class<?> category)
  {
    if (category == null)
      throw new NullPointerException("category may not be null");
    
    return (Attribute) attributeMap.get(category);
  }
  
  /**
   * Returns the hashcode value. The hashcode value is the sum of all hashcodes
   * of the attributes contained in this set.
   * 
   * @return The hashcode for this attribute set.
   */
  public int hashCode()
  {
    int hashcode = 0;
    Iterator it = attributeMap.values().iterator();
    while (it.hasNext())
      hashcode = hashcode + it.next().hashCode();
          
    return hashcode;
  }

  /**
   * Checks if the attribute set is empty.
   *
   * @return <code>true</code> if the attribute set is empty, false otherwise.
   */
  public boolean isEmpty()
  {
    return attributeMap.isEmpty();
  }

  /**
   * Removes the given attribute from the set. If the given attribute is <code>null</code>
   * nothing is done and <code>false</code> is returned.
   * 
   * @param attribute the attribute to remove.  
   * @return <code>true</code> if removed, false in all other cases. 
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  public boolean remove(Attribute attribute)
  {
    if (attribute == null)
      return false;

    return attributeMap.remove(attribute.getCategory()) != null;
  }

  /**
   * Removes the attribute entry of the given category from the set. If the given
   * category is <code>null</code> nothing is done and <code>false</code> is returned.
   * 
   * @param category the category of the entry to be removed.
   * @return <code>true</code> if an attribute is removed, false in all other cases. 
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  public boolean remove(Class<?> category)
  {
    if (category == null)
      return false;

    return attributeMap.remove(category) != null;
  }

  /**
   * Returns the number of elements in this attribute set.
   *
   * @return The number of elements.
   */
  public int size()
  {
    return attributeMap.size();
  }

  /**
   * Returns the content of the attribute set as an array
   *
   * @return An array of attributes.
   */
  public Attribute[] toArray()
  {
    int index = 0;
    Iterator it = attributeMap.values().iterator();
    Attribute[] array = new Attribute[size()];

    while (it.hasNext())
      {
        array[index] = (Attribute) it.next();
        index++;
      }
    
    return array;
  }
  
  // Implemented as specified in serialized form
  private void readObject(ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    myInterface = (Class) s.readObject();
    int size = s.readInt();
    attributeMap = new HashMap(size);
    for (int i=0; i < size; i++)
      add((Attribute) s.readObject());
  }
         
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.writeObject(myInterface);
    s.writeInt(size());
    Iterator it = attributeMap.values().iterator();
    while (it.hasNext())
      s.writeObject(it.next());    
  }
}
