/* java.beans.Introspector
   Copyright (C) 1998, 2002, 2003 Free Software Foundation, Inc.

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


package java.beans;

import gnu.java.beans.BeanInfoEmbryo;
import gnu.java.beans.ExplicitBeanInfo;
import gnu.java.beans.IntrospectionIncubator;
import gnu.java.lang.ClassHelper;

import java.util.Hashtable;
import java.util.Vector;

/**
 * Introspector is the class that does the bulk of the
 * design-time work in Java Beans.  Every class must have
 * a BeanInfo in order for an RAD tool to use it; but, as
 * promised, you don't have to write the BeanInfo class
 * yourself if you don't want to.  All you have to do is
 * call getBeanInfo() in the Introspector and it will use
 * standard JavaBeans-defined method signatures to
 * determine the information about your class.<P>
 *
 * Don't worry about it too much, though: you can provide
 * JavaBeans with as much customized information as you
 * want, or as little as you want, using the BeanInfo
 * interface (see BeanInfo for details).<P>
 *
 * <STRONG>Order of Operations</STRONG><P>
 *
 * When you call getBeanInfo(class c), the Introspector
 * first searches for BeanInfo class to see if you
 * provided any explicit information.  It searches for a
 * class named &lt;bean class name&gt;BeanInfo in different
 * packages, first searching the bean class's package
 * and then moving on to search the beanInfoSearchPath.<P>
 *
 * If it does not find a BeanInfo class, it acts as though
 * it had found a BeanInfo class returning null from all
 * methods (meaning it should discover everything through
 * Introspection).  If it does, then it takes the
 * information it finds in the BeanInfo class to be
 * canonical (that is, the information speaks for its
 * class as well as all superclasses).<P>
 *
 * When it has introspected the class, calls
 * getBeanInfo(c.getSuperclass) and adds that information
 * to the information it has, not adding to any information
 * it already has that is canonical.<P>
 *
 * <STRONG>Introspection Design Patterns</STRONG><P>
 *
 * When the Introspector goes in to read the class, it
 * follows a well-defined order in order to not leave any
 * methods unaccounted for.  Its job is to step over all
 * of the public methods in a class and determine whether
 * they are part of a property, an event, or a method (in
 * that order).
 *
 *
 * <STRONG>Properties:</STRONG><P>
 *
 * <OL>
 * <LI>If there is a <CODE>public boolean isXXX()</CODE>
 *     method, then XXX is a read-only boolean property.
 *     <CODE>boolean getXXX()</CODE> may be supplied in
 *     addition to this method, although isXXX() is the
 *     one that will be used in this case and getXXX()
 *     will be ignored.  If there is a
 *     <CODE>public void setXXX(boolean)</CODE> method,
 *     it is part of this group and makes it a read-write
 *     property.</LI>
 * <LI>If there is a
 *     <CODE>public &lt;type&gt; getXXX(int)</CODE>
 *     method, then XXX is a read-only indexed property of
 *     type &lt;type&gt;.  If there is a
 *     <CODE>public void setXXX(int,&lt;type&gt;)</CODE>
 *     method, then it is a read-write indexed property of
 *     type &lt;type&gt;.  There may also be a
 *     <CODE>public &lt;type&gt;[] getXXX()</CODE> and a
 *     <CODE>public void setXXX(&lt;type&gt;)</CODE>
 *     method as well.</LI>
 * <LI>If there is a
 *     <CODE>public void setXXX(int,&lt;type&gt;)</CODE>
 *     method, then it is a write-only indexed property of
 *     type &lt;type&gt;.  There may also be a
 *     <CODE>public &lt;type&gt;[] getXXX()</CODE> and a
 *     <CODE>public void setXXX(&lt;type&gt;)</CODE>
 *     method as well.</LI>
 * <LI>If there is a
 *     <CODE>public &lt;type&gt; getXXX()</CODE> method,
 *     then XXX is a read-only property of type
 *     &lt;type&gt;.  If there is a
 *     <CODE>public void setXXX(&lt;type&gt;)</CODE>
 *     method, then it will be used for the property and
 *     the property will be considered read-write.</LI>
 * <LI>If there is a
 *     <CODE>public void setXXX(&lt;type&gt;)</CODE>
 *     method, then as long as XXX is not already used as
 *     the name of a property, XXX is assumed to be a
 *     write-only property of type &lt;type&gt;.</LI>
 * <LI>In all of the above cases, if the setXXX() method
 *     throws <CODE>PropertyVetoException</CODE>, then the
 *     property in question is assumed to be constrained.
 *     No properties are ever assumed to be bound
 *     (<STRONG>Spec Note:</STRONG> this is not in the
 *     spec, it just makes sense).  See PropertyDescriptor
 *     for a description of bound and constrained
 *     properties.</LI>
 * </OL>
 *
 * <STRONG>Events:</STRONG><P>
 *
 * If there is a pair of methods,
 * <CODE>public void addXXX(&lt;type&gt;)</CODE> and
 * <CODE>public void removeXXX(&lt;type&gt;)</CODE>, where
 * &lt;type&gt; is a descendant of
 * <CODE>java.util.EventListener</CODE>, then the pair of
 * methods imply that this Bean will fire events to
 * listeners of type &lt;type&gt;.<P>
 *
 * If the addXXX() method throws
 * <CODE>java.util.TooManyListenersException</CODE>, then
 * the event set is assumed to be <EM>unicast</EM>.  See
 * EventSetDescriptor for a discussion of unicast event
 * sets.<P>
 *
 * <STRONG>Spec Note:</STRONG> the spec seems to say that
 * the listener type's classname must be equal to the XXX
 * part of addXXX() and removeXXX(), but that is not the
 * case in Sun's implementation, so I am assuming it is
 * not the case in general.<P>
 *
 * <STRONG>Methods:</STRONG><P>
 *
 * Any public methods (including those which were used
 * for Properties or Events) are used as Methods.
 *
 * @author John Keiser
 * @since JDK1.1
 * @see java.beans.BeanInfo
 */
public class Introspector {

  public static final int USE_ALL_BEANINFO = 1;
  public static final int IGNORE_IMMEDIATE_BEANINFO = 2;
  public static final int IGNORE_ALL_BEANINFO = 3;

  static String[] beanInfoSearchPath = {"gnu.java.beans.info"};
  static Hashtable<Class<?>,BeanInfo> beanInfoCache =
    new Hashtable<Class<?>,BeanInfo>();

  private Introspector() {}

  /**
   * Get the BeanInfo for class <CODE>beanClass</CODE>,
   * first by looking for explicit information, next by
   * using standard design patterns to determine
   * information about the class.
   *
   * @param beanClass the class to get BeanInfo about.
   * @return the BeanInfo object representing the class.
   */
  public static BeanInfo getBeanInfo(Class<?> beanClass)
    throws IntrospectionException
  {
    BeanInfo cachedInfo;
    synchronized(beanClass)
      {
        cachedInfo = beanInfoCache.get(beanClass);
        if(cachedInfo != null)
          {
            return cachedInfo;
          }
        cachedInfo = getBeanInfo(beanClass,null);
        beanInfoCache.put(beanClass,cachedInfo);
        return cachedInfo;
      }
  }

  /**
   * Returns a {@BeanInfo} instance for the given Bean class where a flag
   * controls the usage of explicit BeanInfo class to retrieve that
   * information.
   *
   * <p>You have three options:</p>
   * <p>With {@link #USE_ALL_BEANINFO} the result is the same as
   * {@link #getBeanInfo(Class)}.</p>
   *
   * <p>Calling the method with <code>flag</code> set to
   * {@link #IGNORE_IMMEDIATE_BEANINFO} will let it use all
   * explicit BeanInfo classes for the beans superclasses
   * but not for the bean class itself. Furthermore eventset,
   * property and method information is retrieved by introspection
   * if the explicit <code>BeanInfos</code> did not provide such data
   * (ie. return <code>null</code> on {@link BeanInfo.getMethodDescriptors},
   * {@link BeanInfo.getEventSetDescriptors} and
   * {@link BeanInfo.getPropertyDescriptors}.)
   * </p>
   *
   * <p>When the method is called with <code>flag</code< set to
   * {@link #IGNORE_ALL_BEANINFO} all the bean data is retrieved
   * by inspecting the class.</p>
   *
   * <p>Note: Any unknown value for <code>flag</code> is interpreted
   * as {@link #IGNORE_ALL_BEANINFO}</p>.
   *
   * @param beanClass The class whose BeanInfo should be returned.
   * @param flag Controls the usage of explicit <code>BeanInfo</code> classes.
   * @return A BeanInfo object describing the class.
   * @throws IntrospectionException If something goes wrong while retrieving
   *    the bean data.
   */
  public static BeanInfo getBeanInfo(Class<?> beanClass, int flag)
    throws IntrospectionException
  {
    IntrospectionIncubator ii;
    BeanInfoEmbryo infoEmbryo;

    switch(flag)
    {
      case USE_ALL_BEANINFO:
        return getBeanInfo(beanClass);
      case IGNORE_IMMEDIATE_BEANINFO:
        Class superclass = beanClass.getSuperclass();
        ExplicitInfo explicit = new ExplicitInfo(superclass, null);

        ii = new IntrospectionIncubator();
        if (explicit.explicitEventSetDescriptors != null)
          ii.setEventStopClass(superclass);

        if (explicit.explicitMethodDescriptors != null)
          ii.setMethodStopClass(superclass);

        if (explicit.explicitPropertyDescriptors != null)
          ii.setPropertyStopClass(superclass);

        ii.addMethods(beanClass.getMethods());

        infoEmbryo = ii.getBeanInfoEmbryo();
        merge(infoEmbryo, explicit);

        infoEmbryo.setBeanDescriptor(new BeanDescriptor(beanClass, null));

        return infoEmbryo.getBeanInfo();
      case IGNORE_ALL_BEANINFO:
      default:
        ii = new IntrospectionIncubator();
        ii.addMethods(beanClass.getMethods());
        infoEmbryo = ii.getBeanInfoEmbryo();
        infoEmbryo.setBeanDescriptor(new BeanDescriptor(beanClass, null));

        return infoEmbryo.getBeanInfo();
    }
  }

  /**
   * Flush all of the Introspector's internal caches.
   *
   * @since 1.2
   */
  public static void flushCaches()
  {
    beanInfoCache.clear();

        // Clears all the intermediate ExplicitInfo instances which
        // have been created.
        // This makes sure we have to retrieve stuff like BeanDescriptors
        // again. (Remember that FeatureDescriptor can be modified by the user.)
        ExplicitInfo.flushCaches();
  }

  /**
   * Flush the Introspector's internal cached information for a given
   * class.
   *
   * @param clz the class to be flushed.
   * @throws NullPointerException if clz is null.
   * @since 1.2
   */
  public static void flushFromCaches(Class<?> clz)
  {
    synchronized (clz)
      {
        beanInfoCache.remove(clz);
      }
  }

  /** Adds all explicity given bean info data to the introspected
   * data.
   *
   * @param infoEmbryo Bean info data retrieved by introspection.
   * @param explicit Bean info data retrieved by BeanInfo classes.
   */
  private static void merge(BeanInfoEmbryo infoEmbryo, ExplicitInfo explicit)
  {
    PropertyDescriptor[] p = explicit.explicitPropertyDescriptors;
    if(p!=null)
      {
    for(int i=0;i<p.length;i++)
      {
        if(!infoEmbryo.hasProperty(p[i]))
          {
        infoEmbryo.addProperty(p[i]);
          }
      }

    // -1 should be used to denote a missing default property but
    // for robustness reasons any value below zero is discarded.
    // Not doing so would let Classpath fail where the JDK succeeds.
    if(explicit.defaultProperty > -1)
      {
        infoEmbryo.setDefaultPropertyName(p[explicit.defaultProperty].getName());
      }
      }
    EventSetDescriptor[] e = explicit.explicitEventSetDescriptors;
    if(e!=null)
      {
    for(int i=0;i<e.length;i++)
      {
        if(!infoEmbryo.hasEvent(e[i]))
          {
        infoEmbryo.addEvent(e[i]);
          }
      }

    // -1 should be used to denote a missing default event but
    // for robustness reasons any value below zero is discarded.
    // Not doing so would let Classpath fail where the JDK succeeds.
    if(explicit.defaultEvent > -1)
      {
        infoEmbryo.setDefaultEventName(e[explicit.defaultEvent].getName());
      }
      }
    MethodDescriptor[] m = explicit.explicitMethodDescriptors;
    if(m!=null)
      {
    for(int i=0;i<m.length;i++)
      {
        if(!infoEmbryo.hasMethod(m[i]))
          {
        infoEmbryo.addMethod(m[i]);
          }
      }
      }

    infoEmbryo.setAdditionalBeanInfo(explicit.explicitBeanInfo);
    infoEmbryo.setIcons(explicit.im);

  }

  /**
   * Get the BeanInfo for class <CODE>beanClass</CODE>,
   * first by looking for explicit information, next by
   * using standard design patterns to determine
   * information about the class.  It crawls up the
   * inheritance tree until it hits <CODE>topClass</CODE>.
   *
   * @param beanClass the Bean class.
   * @param stopClass the class to stop at.
   * @return the BeanInfo object representing the class.
   */
  public static BeanInfo getBeanInfo(Class<?> beanClass, Class<?> stopClass)
    throws IntrospectionException
  {
    ExplicitInfo explicit = new ExplicitInfo(beanClass, stopClass);

    IntrospectionIncubator ii = new IntrospectionIncubator();
    ii.setPropertyStopClass(explicit.propertyStopClass);
    ii.setEventStopClass(explicit.eventStopClass);
    ii.setMethodStopClass(explicit.methodStopClass);
    ii.addMethods(beanClass.getMethods());

    BeanInfoEmbryo currentInfo = ii.getBeanInfoEmbryo();

    merge(currentInfo, explicit);

    //  Sets the info's BeanDescriptor to the one we extracted from the
    // explicit BeanInfo instance(s) if they contained one. Otherwise we
    // create the BeanDescriptor from scratch.
    // Note: We do not create a copy the retrieved BeanDescriptor which will allow
    // the user to modify the instance while it is cached. However this is how
    // the RI does it.
    currentInfo.setBeanDescriptor(
        (explicit.explicitBeanDescriptor == null ?
            new BeanDescriptor(beanClass, null) :
            explicit.explicitBeanDescriptor));
    return currentInfo.getBeanInfo();
  }

  /**
   * Get the search path for BeanInfo classes.
   *
   * @return the BeanInfo search path.
   */
  public static String[] getBeanInfoSearchPath()
  {
    return beanInfoSearchPath;
  }

  /**
   * Set the search path for BeanInfo classes.
   * @param beanInfoSearchPath the new BeanInfo search
   *        path.
   */
  public static void setBeanInfoSearchPath(String[] beanInfoSearchPath)
  {
    Introspector.beanInfoSearchPath = beanInfoSearchPath;
  }

  /**
   * A helper method to convert a name to standard Java
   * naming conventions: anything with two capitals as the
   * first two letters remains the same, otherwise the
   * first letter is decapitalized.  URL = URL, I = i,
   * MyMethod = myMethod.
   *
   * @param name the name to decapitalize.
   * @return the decapitalized name.
   */
  public static String decapitalize(String name)
  {
    try
      {
      if(!Character.isUpperCase(name.charAt(0)))
        {
          return name;
        }
      else
        {
        try
          {
          if(Character.isUpperCase(name.charAt(1)))
            {
              return name;
            }
          else
            {
              char[] c = name.toCharArray();
              c[0] = Character.toLowerCase(c[0]);
              return new String(c);
            }
          }
        catch(StringIndexOutOfBoundsException E)
          {
            char[] c = new char[1];
            c[0] = Character.toLowerCase(name.charAt(0));
            return new String(c);
          }
        }
      }
    catch(StringIndexOutOfBoundsException E)
      {
        return name;
      }
    catch(NullPointerException E)
      {
        return null;
      }
  }

  static BeanInfo copyBeanInfo(BeanInfo b)
  {
    java.awt.Image[] icons = new java.awt.Image[4];
    for(int i=1;i<=4;i++)
      {
        icons[i-1] = b.getIcon(i);
      }

    return new ExplicitBeanInfo(b.getBeanDescriptor(),
                                b.getAdditionalBeanInfo(),
                                b.getPropertyDescriptors(),
                                b.getDefaultPropertyIndex(),
                                b.getEventSetDescriptors(),
                                b.getDefaultEventIndex(),
                                b.getMethodDescriptors(),
                                icons);
  }
}

class ExplicitInfo
{
  BeanDescriptor explicitBeanDescriptor;
  BeanInfo[] explicitBeanInfo;

  PropertyDescriptor[] explicitPropertyDescriptors;
  EventSetDescriptor[] explicitEventSetDescriptors;
  MethodDescriptor[] explicitMethodDescriptors;

  int defaultProperty;
  int defaultEvent;

  java.awt.Image[] im = new java.awt.Image[4];

  Class propertyStopClass;
  Class eventStopClass;
  Class methodStopClass;

  static Hashtable explicitBeanInfos = new Hashtable();
  static Vector emptyBeanInfos = new Vector();

  ExplicitInfo(Class beanClass, Class stopClass)
  {
    while(beanClass != null && !beanClass.equals(stopClass))
      {

        BeanInfo explicit = findExplicitBeanInfo(beanClass);


        if(explicit != null)
          {

            if(explicitBeanDescriptor == null)
              {
                explicitBeanDescriptor = explicit.getBeanDescriptor();
              }

            if(explicitBeanInfo == null)
              {
                explicitBeanInfo = explicit.getAdditionalBeanInfo();
              }

            if(explicitPropertyDescriptors == null)
              {
                if(explicit.getPropertyDescriptors() != null)
                  {
                    explicitPropertyDescriptors = explicit.getPropertyDescriptors();
                    defaultProperty = explicit.getDefaultPropertyIndex();
                    propertyStopClass = beanClass;
                  }
              }

            if(explicitEventSetDescriptors == null)
              {
                if(explicit.getEventSetDescriptors() != null)
                  {
                    explicitEventSetDescriptors = explicit.getEventSetDescriptors();
                    defaultEvent = explicit.getDefaultEventIndex();
                    eventStopClass = beanClass;
                  }
              }

            if(explicitMethodDescriptors == null)
              {
                if(explicit.getMethodDescriptors() != null)
                  {
                    explicitMethodDescriptors = explicit.getMethodDescriptors();
                    methodStopClass = beanClass;
                  }
              }

            if(im[0] == null && im[1] == null
               && im[2] == null && im[3] == null)
              {
                im[0] = explicit.getIcon(0);
                im[1] = explicit.getIcon(1);
                im[2] = explicit.getIcon(2);
                im[3] = explicit.getIcon(3);
              }
          }
        beanClass = beanClass.getSuperclass();
      }

    if(propertyStopClass == null)
      {
        propertyStopClass = stopClass;
      }

    if(eventStopClass == null)
      {
        eventStopClass = stopClass;
      }

    if(methodStopClass == null)
      {
        methodStopClass = stopClass;
      }
  }

  /** Throws away all cached data and makes sure we re-instantiate things
    * like BeanDescriptors again.
    */
  static void flushCaches() {
        explicitBeanInfos.clear();
        emptyBeanInfos.clear();
  }

  static BeanInfo findExplicitBeanInfo(Class beanClass)
  {
    BeanInfo retval = (BeanInfo)explicitBeanInfos.get(beanClass);
    if(retval != null)
      {
        return retval;
      }
    else if(emptyBeanInfos.indexOf(beanClass) != -1)
      {
        return null;
      }
    else
      {
        retval = reallyFindExplicitBeanInfo(beanClass);
        if(retval != null)
          {
            explicitBeanInfos.put(beanClass,retval);
          }
        else
          {
            emptyBeanInfos.addElement(beanClass);
          }
        return retval;
      }
  }

  static BeanInfo reallyFindExplicitBeanInfo(Class beanClass)
  {
    ClassLoader beanClassLoader = beanClass.getClassLoader();
    BeanInfo beanInfo;

    beanInfo = getBeanInfo(beanClassLoader, beanClass.getName() + "BeanInfo");
    if (beanInfo == null)
      {
        String newName;
        newName = ClassHelper.getTruncatedClassName(beanClass) + "BeanInfo";

        for(int i = 0; i < Introspector.beanInfoSearchPath.length; i++)
          {
            if (Introspector.beanInfoSearchPath[i].equals(""))
              beanInfo = getBeanInfo(beanClassLoader, newName);
            else
              beanInfo = getBeanInfo(beanClassLoader,
                                     Introspector.beanInfoSearchPath[i] + "."
                                     + newName);

                // Returns the beanInfo if it exists and the described class matches
                // the one we searched.
            if (beanInfo != null && beanInfo.getBeanDescriptor() != null &&
                        beanInfo.getBeanDescriptor().getBeanClass() == beanClass)

              return beanInfo;
          }
      }

    return beanInfo;
  }

  /**
   * Returns an instance of the given class name when it can be loaded
   * through the given class loader, or null otherwise.
   */
  private static BeanInfo getBeanInfo(ClassLoader cl, String infoName)
  {
    try
      {
        return (BeanInfo) Class.forName(infoName, true, cl).newInstance();
      }
    catch (ClassNotFoundException cnfe)
      {
        return null;
      }
    catch (IllegalAccessException iae)
      {
        return null;
      }
    catch (InstantiationException ie)
      {
        return null;
      }
  }

}
