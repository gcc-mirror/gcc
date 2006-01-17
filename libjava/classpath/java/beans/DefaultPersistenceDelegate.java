/* DefaultPersistenceDelegate.java
 Copyright (C) 2005 Free Software Foundation, Inc.

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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/** <p><code>DefaultPersistenceDelegate</code> is a {@link PersistenceDelegate}
 * implementation that can be used to serialize objects which adhere to the
 * Java Beans naming convention.</p>
 * 
 * @author Robert Schuster (robertschuster@fsfe.org)
 * @since 1.4
 */
public class DefaultPersistenceDelegate extends PersistenceDelegate
{

  private String[] constructorPropertyNames;

  /** Using this constructor the object to be serialized will be instantiated
   * with the default non-argument constructor.
   */
  public DefaultPersistenceDelegate()
  {
  }

  /** This constructor allows to specify which Bean properties appear
   * in the constructor.
   * 
   * <p>The implementation reads the mentioned properties from the Bean
   * instance and applies it in the given order to a corresponding
   * constructor.</p>
   * 
   * @param constructorPropertyNames The properties the Bean's constructor
   * should be given to.
   */
  public DefaultPersistenceDelegate(String[] constructorPropertyNames)
  {
    this.constructorPropertyNames = constructorPropertyNames;
  }

  protected boolean mutatesTo(Object oldInstance, Object newInstance)
  {
    try
      {

        return (constructorPropertyNames != null
               && constructorPropertyNames.length > 0
               && oldInstance.getClass()
               .getDeclaredMethod("equals",
                                  new Class[] { Object.class }) != null)
                                  ? oldInstance.equals(newInstance)
                                  : super.mutatesTo(oldInstance, newInstance);
      }
    catch (NoSuchMethodException nsme)
      {
        return super.mutatesTo(oldInstance, newInstance);
      }
  }

  protected Expression instantiate(Object oldInstance, Encoder out)
  {
    Object[] args = null;

    try
      {
        // If there are property names in the array, then we create
        // a corresponding argument array and store every
        // argument in it. To retrieve an argument object we have
        // dig up the right property in the bean class' BeanInfo
        // object.
        // This is so costly in terms of execution time I better
        // not think twice about it ...
        if (constructorPropertyNames != null)
          {
            args = new Object[constructorPropertyNames.length];

            // Look up the properties of oldInstance's class to find matches for
            // the
            // names given in the constructor.
            PropertyDescriptor[] propertyDescs = Introspector.getBeanInfo(
                                                                          oldInstance.getClass()).getPropertyDescriptors();

            for (int i = 0; i < constructorPropertyNames.length; i++)
              {
                // Scan the property descriptions for a matching name.
                for (int j = 0; j < propertyDescs.length; j++)
                  {
                    if (propertyDescs[i].getName().equals(
                                                          constructorPropertyNames[i]))
                      {
                        Method readMethod = propertyDescs[i].getReadMethod();

                        args[i] = readMethod.invoke(oldInstance, null);
                      }
                  }
              }
          }

      }
    catch (IllegalAccessException iae)
      {
        out.getExceptionListener().exceptionThrown(iae);
      }
    catch (IllegalArgumentException iarge)
      {
        out.getExceptionListener().exceptionThrown(iarge);
      }
    catch (InvocationTargetException ite)
      {
        out.getExceptionListener().exceptionThrown(ite);
      }
    catch (IntrospectionException ie)
      {
        out.getExceptionListener().exceptionThrown(ie);
      }

    return new Expression(oldInstance, oldInstance.getClass(), "new", args);
  }

  protected void initialize(Class type, Object oldInstance, Object newInstance,
                            Encoder out)
  {
    try
      {
        PropertyDescriptor[] propertyDescs = Introspector.getBeanInfo(
                                                                      oldInstance.getClass()).getPropertyDescriptors();

        for (int i = 0; i < propertyDescs.length; i++)
          {
            Method readMethod = propertyDescs[i].getReadMethod();
            Method writeMethod = propertyDescs[i].getWriteMethod();

            if (readMethod != null && writeMethod != null)
              {
                Object oldValue = readMethod.invoke(oldInstance, null);

                if (oldValue != null)
                  out.writeStatement(new Statement(oldInstance,
                                                   writeMethod.getName(),
                                                   new Object[] { oldValue }));
              }
          }
      }
    catch (IntrospectionException ie)
      {
        out.getExceptionListener().exceptionThrown(ie);
      }
    catch (IllegalAccessException iae)
      {
        out.getExceptionListener().exceptionThrown(iae);
      }
    catch (InvocationTargetException ite)
      {
        out.getExceptionListener().exceptionThrown(ite);
      }
  }
}
