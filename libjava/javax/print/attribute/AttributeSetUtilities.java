/* AttributeSetUtilities.java -- 
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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

public final class AttributeSetUtilities
{
  /**
   * This class isn't intended to be instantiated.
   */
  private AttributeSetUtilities() {}

  private static class UnmodifiableAttributeSet
    implements AttributeSet, Serializable
  {
    private AttributeSet set;

    public UnmodifiableAttributeSet(AttributeSet attributeSet)
    {
      if (attributeSet == null)
        throw new NullPointerException("attributeSet may not be null");

      this.set = attributeSet;
    }

    public boolean add(Attribute attribute)
    {
      throw new UnmodifiableSetException();
    }

    public boolean addAll(AttributeSet attributes)
    {
      throw new UnmodifiableSetException();
    }
    
    public void clear()
    {
      throw new UnmodifiableSetException();
    }

    public boolean containsKey(Class category)
    {
      return set.containsKey(category);
    }

    public boolean containsValue(Attribute attribute)
    {
      return set.containsValue(attribute);
    }

    public boolean equals(Object obj)
    {
      return set.equals(obj);
    }
    
    public Attribute get(Class interfaceName)
    {
      return set.get(interfaceName);
    }

    public int hashCode()
    {
      return set.hashCode();
    }
    
    public boolean isEmpty()
    {
      return set.isEmpty();
    }

    public boolean remove(Class category)
    {
      throw new UnmodifiableSetException();
    }

    public boolean remove(Attribute attribute)
    {
      throw new UnmodifiableSetException();
    }

    public int size()
    {
      return set.size();
    }

    public Attribute[] toArray()
    {
      return set.toArray();
    }
  }

  private static class UnmodifiableDocAttributeSet
    extends UnmodifiableAttributeSet
    implements DocAttributeSet, Serializable
  {
    public UnmodifiableDocAttributeSet(DocAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  private static class UnmodifiablePrintJobAttributeSet
    extends UnmodifiableAttributeSet
    implements PrintJobAttributeSet, Serializable
  {
    public UnmodifiablePrintJobAttributeSet(PrintJobAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  private static class UnmodifiablePrintRequestAttributeSet
    extends UnmodifiableAttributeSet
    implements PrintRequestAttributeSet, Serializable
  {
    public UnmodifiablePrintRequestAttributeSet(PrintRequestAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  private static class UnmodifiablePrintServiceAttributeSet
    extends UnmodifiableAttributeSet
    implements PrintServiceAttributeSet, Serializable
  {
    public UnmodifiablePrintServiceAttributeSet(PrintServiceAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  private static class SynchronizedAttributeSet
    implements AttributeSet, Serializable
  {
    private AttributeSet set;

    public SynchronizedAttributeSet(AttributeSet attributeSet)
    {
      if (attributeSet == null)
        throw new NullPointerException("attributeSet may not be null");

      this.set = attributeSet;
    }

    public synchronized boolean add(Attribute attribute)
    {
      return set.add(attribute);
    }

    public synchronized boolean addAll(AttributeSet attributes)
    {
      return set.addAll(attributes);
    }
    
    public synchronized void clear()
    {
      set.clear();
    }

    public synchronized boolean containsKey(Class category)
    {
      return set.containsKey(category);
    }

    public synchronized boolean containsValue(Attribute attribute)
    {
      return set.containsValue(attribute);
    }

    public synchronized boolean equals(Object obj)
    {
      return set.equals(obj);
    }
    
    public synchronized Attribute get(Class interfaceName)
    {
      return set.get(interfaceName);
    }

    public synchronized int hashCode()
    {
      return set.hashCode();
    }
    
    public synchronized boolean isEmpty()
    {
      return set.isEmpty();
    }

    public synchronized boolean remove(Class category)
    {
      return set.remove(category);
    }

    public synchronized boolean remove(Attribute attribute)
    {
      return set.remove(attribute);
    }

    public synchronized int size()
    {
      return set.size();
    }

    public synchronized Attribute[] toArray()
    {
      return set.toArray();
    }
  }

  private static class SynchronizedDocAttributeSet
    extends SynchronizedAttributeSet
    implements DocAttributeSet, Serializable
  {
    public SynchronizedDocAttributeSet(DocAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  private static class SynchronizedPrintJobAttributeSet
    extends SynchronizedAttributeSet
    implements PrintJobAttributeSet, Serializable
  {
    public SynchronizedPrintJobAttributeSet(PrintJobAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  private static class SynchronizedPrintRequestAttributeSet
    extends SynchronizedAttributeSet
    implements PrintRequestAttributeSet, Serializable
  {
    public SynchronizedPrintRequestAttributeSet(PrintRequestAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  private static class SynchronizedPrintServiceAttributeSet
    extends SynchronizedAttributeSet
    implements PrintServiceAttributeSet, Serializable
  {
    public SynchronizedPrintServiceAttributeSet(PrintServiceAttributeSet attributeSet)
    {
      super(attributeSet);
    }
  }

  /**
   * Returns a synchronized view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static AttributeSet synchronizedView(AttributeSet attributeSet)
  {
    return new SynchronizedAttributeSet(attributeSet);
  }

  /**
   * Returns a synchronized view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static DocAttributeSet synchronizedView(DocAttributeSet attributeSet)
  {
    return new SynchronizedDocAttributeSet(attributeSet);
  }
  
  /**
   * Returns a synchronized view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static PrintJobAttributeSet synchronizedView(PrintJobAttributeSet attributeSet)
  {
    return new SynchronizedPrintJobAttributeSet(attributeSet);
  }
  
  /**
   * Returns a synchronized view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static PrintRequestAttributeSet synchronizedView(PrintRequestAttributeSet attributeSet)
  {
    return new SynchronizedPrintRequestAttributeSet(attributeSet);
  }
  
  /**
   * Returns a synchronized view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static PrintServiceAttributeSet synchronizedView(PrintServiceAttributeSet attributeSet)
  {
    return new SynchronizedPrintServiceAttributeSet(attributeSet);
  }
  
  /**
   * Returns an unmodifiable view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static AttributeSet unmodifiableView(AttributeSet attributeSet)
  {
    return new UnmodifiableAttributeSet(attributeSet);
  }
  
  /**
   * Returns an unmodifiable view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static DocAttributeSet unmodifiableView(DocAttributeSet attributeSet)
  {
    return new UnmodifiableDocAttributeSet(attributeSet);
  }
  
  /**
   * Returns an unmodifiable view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static PrintJobAttributeSet unmodifiableView(PrintJobAttributeSet attributeSet)
  {
    return new UnmodifiablePrintJobAttributeSet(attributeSet);
  }
  
  /**
   * Returns an unmodifiable view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static PrintRequestAttributeSet unmodifiableView(PrintRequestAttributeSet attributeSet)
  {
    return new UnmodifiablePrintRequestAttributeSet(attributeSet);
  }
  
  /**
   * Returns an unmodifiable view of the given attribute set.
   *
   * @return the sychronized attribute set
   */
  public static PrintServiceAttributeSet unmodifiableView(PrintServiceAttributeSet attributeSet)
  {
    return new UnmodifiablePrintServiceAttributeSet(attributeSet);
  }

  /**
   * Verifies that the given object is a <code>Class</code> that
   * implements the given interface name.
   *
   * @return object casted to <code>Class</code>
   *
   * @exception ClassCastException if object is not a <code>Class</code>
   * that implements interfaceName
   * @exception NullPointerException if object is null
   */
  public static Class verifyAttributeCategory(Object object,
                                              Class interfaceName)
  {
    if (object == null)
      throw new NullPointerException("object may not be null");

    Class clazz = (Class) object;

    if (interfaceName.isAssignableFrom(clazz))
      return clazz;

    throw new ClassCastException();
  }
  
  /**
   * Verifies that the given object is an attribute of the given interface.
   *
   * @return the object casted to <code>Attribute</code>
   *
   * @exception ClassCastException if object is no instance of interfaceName.
   * @exception NullPointerException if object is null
   */
  public static Attribute verifyAttributeValue(Object object,
                                               Class interfaceName)
  {
    if (object == null)
      throw new NullPointerException("object may not be null");

    if (interfaceName.isInstance(object))
      return (Attribute) object;

    throw new ClassCastException();
  }

  /**
   * Verifies that the category of attribute is equals to category.
   *
   * @param category the category the atteribute should be
   * @param attribute the attribute to verify
   *
   * @exception IllegalArgumentException if the categories are not equal
   * @exception NullPointerException if category is null
   */
  public static void verifyCategoryForValue(Class category,
                                            Attribute attribute)
  {
    if (category == null)
      throw new NullPointerException("object may not be null");

    if (category.equals(attribute.getCategory()))
      throw new IllegalArgumentException
        ("category of attribute not equal to category");
  }
}
