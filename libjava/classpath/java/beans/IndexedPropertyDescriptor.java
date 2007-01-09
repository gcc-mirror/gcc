/* IndexedPropertyDescriptor.java --
   Copyright (C) 1998, 2003 Free Software Foundation, Inc.

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

import java.lang.reflect.Array;
import java.lang.reflect.Method;

/**
 * IndexedPropertyDescriptor describes information about a JavaBean
 * indexed property, by which we mean an array-like property that
 * has been exposed via a pair of get and set methods and another
 * pair that allows you to get to the property by an index.<P>
 *
 * An example property would have four methods like this:<P>
 * <CODE>FooBar[] getFoo()</CODE><BR>
 * <CODE>void setFoo(FooBar[])</CODE><BR>
 * <CODE>FooBar getFoo(int)</CODE><BR>
 * <CODE>void setFoo(int,FooBar)</CODE><P>
 *
 * The constraints put on get and set methods are:<P>
 * <OL>
 * <LI>There must be at least a get(int) or a set(int,...) method.
 * Nothing else is required.  <B>Spec note:</B>One nice restriction
 * would be that if there is a get() there must be a get(int), same
 * with set, but that is not in the spec and is fairly harmless.)</LI>
 * <LI>A get array method must have signature
 *     <CODE>&lt;propertyType&gt;[] &lt;getMethodName&gt;()</CODE></LI>
 * <LI>A set array method must have signature
 *     <CODE>void &lt;setMethodName&gt;(&lt;propertyType&gt;[])</CODE></LI>
 * <LI>A get index method must have signature
 *     <CODE>&lt;propertyType&gt; &lt;getMethodName&gt;(int)</CODE></LI>
 * <LI>A set index method must have signature
 *     <CODE>void &lt;setMethodName&gt;(int,&lt;propertyType&gt;)</CODE></LI>
 * <LI>All these methods may throw any exception.</LI>
 * <LI>All these methods must be public.</LI>
 * </OL>
 *
 * @author John Keiser
 * @since JDK1.1
 */
public class IndexedPropertyDescriptor extends PropertyDescriptor
{
  private Class<?> indexedPropertyType;
  private Method setIndex;
  private Method getIndex;

  /**
   * Create a new IndexedPropertyDescriptor by introspection.
   * This form of constructor creates the PropertyDescriptor by
   * looking for getter methods named <CODE>get&lt;name&gt;()</CODE>
   * and setter methods named
   * <CODE>set&lt;name&gt;()</CODE> in class
   * <CODE>&lt;beanClass&gt;</CODE>, where &lt;name&gt; has its
   * first letter capitalized by the constructor.<P>
   *
   * <B>Implementation note:</B> If there is a get(int) method,
   * then the return type of that method is used to find the
   * remaining methods.  If there is no get method, then the
   * set(int) method is searched for exhaustively and that type
   * is used to find the others.<P>
   *
   * <B>Spec note:</B>
   * If there is no get(int) method and multiple set(int) methods with
   * the same name and the correct parameters (different type of course),
   * then an IntrospectionException is thrown.  While Sun's spec
   * does not state this, it can make Bean behavior different on
   * different systems (since method order is not guaranteed) and as
   * such, can be treated as a bug in the spec.  I am not aware of
   * whether Sun's implementation catches this.
   *
   * @param name the programmatic name of the property, usually
   *             starting with a lowercase letter (e.g. fooManChu
   *             instead of FooManChu).
   * @param beanClass the class the get and set methods live in.
   *
   * @exception IntrospectionException if the methods are not found or
   *            invalid.
   */
  public IndexedPropertyDescriptor(String name, Class<?> beanClass)
    throws IntrospectionException
  {
    super(name);
    String capitalized;
    try
      {
        capitalized = Character.toUpperCase(name.charAt(0))
          + name.substring(1);
      }
    catch(StringIndexOutOfBoundsException e)
      {
        capitalized = "";
      }
    findMethods(beanClass, "get" + capitalized, "set" + capitalized,
                "get" + capitalized, "set" + capitalized);
  }

  /**
   * Create a new IndexedPropertyDescriptor by introspection.
   * This form of constructor allows you to specify the
   * names of the get and set methods to search for.<P>
   *
   * <B>Implementation note:</B> If there is a get(int) method,
   * then the return type of that method is used to find the
   * remaining methods.  If there is no get method, then the
   * set(int) method is searched for exhaustively and that type
   * is used to find the others.<P>
   *
   * <B>Spec note:</B>
   * If there is no get(int) method and multiple set(int) methods with
   * the same name and the correct parameters (different type of course),
   * then an IntrospectionException is thrown.  While Sun's spec
   * does not state this, it can make Bean behavior different on
   * different systems (since method order is not guaranteed) and as
   * such, can be treated as a bug in the spec.  I am not aware of
   * whether Sun's implementation catches this.
   *
   * @param name the programmatic name of the property, usually
   *             starting with a lowercase letter (e.g. fooManChu
   *             instead of FooManChu).
   * @param beanClass the class the get and set methods live in.
   * @param getMethodName the name of the get array method.
   * @param setMethodName the name of the set array method.
   * @param getIndexName the name of the get index method.
   * @param setIndexName the name of the set index method.
   *
   * @exception IntrospectionException if the methods are not found or invalid.
   */
  public IndexedPropertyDescriptor(String name, Class<?> beanClass,
                                   String getMethodName, String setMethodName,
                                   String getIndexName, String setIndexName)
    throws IntrospectionException
  {
    super(name);
    findMethods(beanClass, getMethodName, setMethodName, getIndexName,
                setIndexName);
  }

  /**
   * Create a new PropertyDescriptor using explicit Methods.
   * Note that the methods will be checked for conformance to standard
   * Property method rules, as described above at the top of this class.
   * 
   * @param name the programmatic name of the property, usually
   *             starting with a lowercase letter (e.g. fooManChu
   *             instead of FooManChu).
   * @param getMethod the get array method.
   * @param setMethod the set array method.
   * @param getIndex the get index method.
   * @param setIndex the set index method.
   *
   * @exception IntrospectionException if the methods are not found or invalid.
   */
  public IndexedPropertyDescriptor(String name, Method getMethod,
                                   Method setMethod, Method getIndex,
                                   Method setIndex)
    throws IntrospectionException
  {
    super(name);
    if(getMethod != null && getMethod.getParameterTypes().length > 0)
      throw new IntrospectionException("get method has parameters");
    if(getMethod != null && setMethod.getParameterTypes().length != 1)
      throw new IntrospectionException("set method does not have exactly one parameter");
    if(getMethod != null && setMethod != null)
      {
        if(!getMethod.getReturnType().equals(setMethod.getParameterTypes()[0]))
          {
            throw new IntrospectionException("set and get methods do not "
                                             + "share the same type");
          }
        if(!getMethod.getDeclaringClass().isAssignableFrom
           (setMethod.getDeclaringClass())
           && !setMethod.getDeclaringClass().isAssignableFrom
           (getMethod.getDeclaringClass()))
          {
            throw new IntrospectionException("set and get methods are not in "
                                             + "the same class.");
          }
      }

    if (getIndex != null
        && (getIndex.getParameterTypes().length != 1
         || !(getIndex.getParameterTypes()[0]).equals(java.lang.Integer.TYPE)))
      {
        throw new IntrospectionException("get index method has wrong "
                                         + "parameters");
      }
    if (setIndex != null
       && (setIndex.getParameterTypes().length != 2
         || !(setIndex.getParameterTypes()[0]).equals(java.lang.Integer.TYPE)))
      {
        throw new IntrospectionException("set index method has wrong "
                                         + "parameters");
      }
    if (getIndex != null && setIndex != null)
      {
        if(!getIndex.getReturnType().equals(setIndex.getParameterTypes()[1]))
          {
            throw new IntrospectionException("set index methods do not share "
                                             + "the same type");
          }
        if(!getIndex.getDeclaringClass().isAssignableFrom
           (setIndex.getDeclaringClass())
           && !setIndex.getDeclaringClass().isAssignableFrom
           (getIndex.getDeclaringClass()))
          {
            throw new IntrospectionException("get and set index methods are "
                                             + "not in the same class.");
          }
      }

    if (getIndex != null && getMethod != null
        && !getIndex.getDeclaringClass().isAssignableFrom
        (getMethod.getDeclaringClass())
        && !getMethod.getDeclaringClass().isAssignableFrom
        (getIndex.getDeclaringClass()))
      {
        throw new IntrospectionException("methods are not in the same class.");
      }

    if (getIndex != null && getMethod != null
        && !Array.newInstance(getIndex.getReturnType(),0)
        .getClass().equals(getMethod.getReturnType()))
      {
        throw new IntrospectionException("array methods do not match index "
                                         + "methods.");
      }

    this.getMethod = getMethod;
    this.setMethod = setMethod;
    this.getIndex = getIndex;
    this.setIndex = setIndex;
    this.indexedPropertyType = getIndex != null ? getIndex.getReturnType()
                                             : setIndex.getParameterTypes()[1];
    this.propertyType = getMethod != null ? getMethod.getReturnType()
      : (setMethod != null ? setMethod.getParameterTypes()[0]
         : Array.newInstance(this.indexedPropertyType,0).getClass());
  }

  public Class<?> getIndexedPropertyType()
  {
    return indexedPropertyType;
  }

  public Method getIndexedReadMethod()
  {
    return getIndex;
  }

  /**
   * Sets the method that is used to read an indexed property.
   *
   * @param m the method to set
   */
  public void setIndexedReadMethod(Method m) throws IntrospectionException
  {
    getIndex = m;
  }

  public Method getIndexedWriteMethod()
  {
    return setIndex;
  }

  /**
   * Sets the method that is used to write an indexed property.
   *
   * @param m the method to set
   */
  public void setIndexedWriteMethod(Method m) throws IntrospectionException
  {
    setIndex = m;
  }

  private void findMethods(Class beanClass, String getMethodName,
                           String setMethodName, String getIndexName,
                           String setIndexName)
    throws IntrospectionException
  {
    try
      {
        if(getIndexName != null)
          {
            try
              {
                Class[] getArgs = new Class[1];
                getArgs[0] = java.lang.Integer.TYPE;
                getIndex = beanClass.getMethod(getIndexName,getArgs);
                indexedPropertyType = getIndex.getReturnType();
              }
            catch(NoSuchMethodException E)
              {
              }
          }
        if(getIndex != null)
          {
            if(setIndexName != null)
              {
                try
                  {
                    Class[] setArgs = new Class[2];
                    setArgs[0] = java.lang.Integer.TYPE;
                    setArgs[1] = indexedPropertyType;
                    setIndex = beanClass.getMethod(setIndexName,setArgs);
                    if(!setIndex.getReturnType().equals(java.lang.Void.TYPE))
                      {
                        throw new IntrospectionException(setIndexName
                                                + " has non-void return type");
                      }
                  }
                catch(NoSuchMethodException E)
                  {
                  }
              }
          }
        else if(setIndexName != null)
          {
            Method[] m = beanClass.getMethods();
            for(int i=0;i<m.length;i++)
              {
                Method current = m[i];
                if(current.getName().equals(setIndexName)
                   && current.getParameterTypes().length == 2
                   && (current.getParameterTypes()[0])
                   .equals(java.lang.Integer.TYPE)
                   && current.getReturnType().equals(java.lang.Void.TYPE))
                  {
                    if(setIndex != null)
                      {
                        throw new IntrospectionException("Multiple, different "
                                     + "set methods found that fit the bill!");
                      }
                    else
                      {
                        setIndex = current;
                        indexedPropertyType = current.getParameterTypes()[1];
                      }
                  }
              }
            if(setIndex == null)
              {
                throw new IntrospectionException("Cannot find get or set "
                                                 + "methods.");
              }
          }
        else
          {
           throw new IntrospectionException("Cannot find get or set methods.");
          }

        Class arrayType = Array.newInstance(indexedPropertyType,0).getClass();

        Class[] setArgs = new Class[1];
        setArgs[0] = arrayType;
        try
          {
            setMethod = beanClass.getMethod(setMethodName,setArgs);
            if (!setMethod.getReturnType().equals(java.lang.Void.TYPE))
              {
                setMethod = null;
              }
          }
        catch(NoSuchMethodException E)
          {
          }

        Class[] getArgs = new Class[0];
        try
          {
            getMethod = beanClass.getMethod(getMethodName,getArgs);
            if (!getMethod.getReturnType().equals(arrayType))
              {
                getMethod = null;
              }
          }
        catch(NoSuchMethodException E)
          {
          }
      }
    catch(SecurityException E)
      {
        throw new IntrospectionException("SecurityException while trying to "
                                         + "find methods.");
      }
  }
}
