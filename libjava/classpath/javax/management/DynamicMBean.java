/* DynamicMBean.java -- A management bean with a dynamic interface.
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

package javax.management;

/**
 * Represents a management bean that provides a
 * dynamic interface.  Users of a {@link DynamicMBean}
 * may retrieve information about its attributes at
 * runtime and use this information to dynamically
 * obtain the corresponding values of these attributes.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface DynamicMBean
{
  
  /**
   * Obtains the value of the specified attribute of the
   * management bean.  The management bean should perform
   * a lookup for the named attribute, and return its value
   * by calling the appropriate getter method, if possible.
   *
   * @param name the name of the attribute to retrieve.
   * @return the value of the specified attribute.
   * @throws AttributeNotFoundException if the name does not
   *                                    correspond to an attribute
   *                                    of the bean.
   * @throws MBeanException if retrieving the attribute causes
   *                        the bean to throw an exception (which
   *                        becomes the cause of this exception).
   * @throws ReflectionException if an exception occurred in trying
   *                             to use the reflection interface
   *                             to lookup the attribute.  The
   *                             thrown exception is the cause of
   *                             this exception.
   * @see #setAttribute(String)
   */
  Object getAttribute(String name)
    throws AttributeNotFoundException, MBeanException,
	   ReflectionException;

  /**
   * Obtains the values of each of the specified attributes
   * of the management bean.  The returned list includes
   * those attributes that were retrieved and their
   * corresponding values.
   *
   * @param names the names of the attributes to retrieve.
   * @return a list of the retrieved attributes.
   * @see #setAttributes(AttributeList)
   */
  AttributeList getAttributes(String[] names);

  /**
   * Returns an information object which lists the attributes
   * and actions associated with the management bean.
   *
   * @return a description of the management bean, including
   *         all exposed attributes and actions.
   */
  MBeanInfo getMBeanInfo();

  /**
   * Invokes the specified action on the management bean using
   * the supplied parameters.  The signature of the action is
   * specified by a {@link String} array, which lists the classes
   * corresponding to each parameter.  The class loader used to
   * load these classes is the same as that used for loading the
   * management bean itself.
   * 
   * @param name the name of the action to invoke.
   * @param params the parameters used to call the action.
   * @param signature the signature of the action.
   * @return the return value of the action.
   * @throws MBeanException if the action throws an exception.  The
   *                        thrown exception is the cause of this
   *                        exception.
   * @throws ReflectionException if an exception occurred in trying
   *                             to use the reflection interface
   *                             to invoke the action.  The
   *                             thrown exception is the cause of
   *                             this exception.
   */
  Object invoke(String name, Object[] params, String[] signature)
    throws MBeanException, ReflectionException;

  /**
   * Sets the value of the specified attribute of the
   * management bean.  The management bean should perform
   * a lookup for the named attribute, and sets its value
   * using the associated setter method, if possible.
   *
   * @param attribute the attribute to set.
   * @throws AttributeNotFoundException if the attribute does not
   *                                    correspond to an attribute
   *                                    of the bean.
   * @throws InvalidAttributeValueException if the value is invalid
   *                                        for this particular
   *                                        attribute of the bean.
   * @throws MBeanException if setting the attribute causes
   *                        the bean to throw an exception (which
   *                        becomes the cause of this exception).
   * @throws ReflectionException if an exception occurred in trying
   *                             to use the reflection interface
   *                             to lookup the attribute.  The
   *                             thrown exception is the cause of
   *                             this exception.
   * @see #getAttribute(String)
   */
  void setAttribute(Attribute attribute)
    throws AttributeNotFoundException, InvalidAttributeValueException,
	   MBeanException, ReflectionException;

  /**
   * Sets the value of each of the specified attributes
   * to that supplied by the {@link Attribute} object.
   * The returned list contains the attributes that were
   * set and their new values.
   *
   * @param attributes the attributes to set.
   * @return a list of the changed attributes.
   * @see #getAttributes(AttributeList)
   */
  AttributeList setAttributes(AttributeList attributes);

}
