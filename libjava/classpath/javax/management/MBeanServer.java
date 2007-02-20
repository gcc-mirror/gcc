/* MBeanServer.java -- Represents a management server.
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

import java.io.ObjectInputStream;

import java.util.Set;

import javax.management.loading.ClassLoaderRepository;

/**
 * <p>
 * This interface represents a server for management beans,
 * providing facilities for the creation, registration and
 * removal of such beans.  This interface is central to the
 * Java management architecture.  Users do not usually implement
 * this class.  Instead, implementations of this class
 * may be obtained using an {@link MBeanServerFactory}.
 * </p>
 * <p>
 * Registering a bean with the server makes its attributes and
 * operations accessible via the server.  Only JMX compliant
 * beans may be registered with the server.  When a bean
 * is registered or unregistered, an {@link MBeanServerNotification}
 * is emitted by the server's {@link MBeanServerDelegate}.
 * Listeners may be registered with this bean in order to
 * obtain such notifications.  It has the {@link ObjectName}
 * <code>JMImplementation:type=MBeanServerDelegate</code>.
 * </p>
 * <p>
 * Security checks are applied on the methods of the server,
 * as detailed below, if it is obtained using the
 * {@link MBeanServerFactory#createMBeanServer()} or
 * {@link MBeanServerFactory#newMBeanServer()} methods and
 * {@link System.getSecurityManager()} returns a non-<code>null</code>
 * value.  If a check fails, a {@link SecurityException}
 * is thrown.  Note than the class name used in the exception
 * is that of the bean, and thus, as a result, an
 * {@link InstanceNotFoundException}
 * precludes these security checks, due to the class name 
 * that would be used in the exception being unavailable.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface MBeanServer
  extends MBeanServerConnection
{

  /**
   * Registers the supplied listener with the specified management
   * bean.  Notifications emitted by the management bean are forwarded
   * to the listener via the server, which will convert any MBean
   * references in the source to portable {@link ObjectName}
   * instances.  The notification is otherwise unchanged.
   *
   * @param name the name of the management bean with which the listener
   *             should be registered.
   * @param listener the listener which will handle notifications from
   *                 the bean.
   * @param filter the filter to apply to incoming notifications, or
   *               <code>null</code> if no filtering should be applied.
   * @param passback an object to be passed to the listener when a
   *                 notification is emitted.
   * @throws InstanceNotFoundException if the name of the management bean
   *                                   could not be resolved.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "addNotificationListener")</code>}.
   * @see #removeNotificationListener(ObjectName, NotificationListener)
   * @see #removeNotificationListener(ObjectName, NotificationListener,
   *                                  NotificationFilter, Object)
   * @see NotificationBroadcaster#addNotificationListener(NotificationListener,
   *                                                      NotificationFilter,
   *                                                      Object)
   */
  void addNotificationListener(ObjectName name, NotificationListener listener,
			       NotificationFilter filter, Object passback)
    throws InstanceNotFoundException;

  /**
   * <p>
   * Registers the supplied listener with the specified management
   * bean.  Notifications emitted by the management bean are forwarded
   * to the listener via the server, which will convert any MBean
   * references in the source to portable {@link ObjectName}
   * instances.  The notification is otherwise unchanged.  
   * </p>
   * <p>
   * The listener that receives notifications will be the one that is
   * registered with the given name at the time this method is called.
   * Even if it later unregisters and ceases to use that name, it will
   * still receive notifications.
   * </p>
   *
   * @param name the name of the management bean with which the listener
   *             should be registered.
   * @param listener the name of the listener which will handle
   *                 notifications from the bean.
   * @param filter the filter to apply to incoming notifications, or
   *               <code>null</code> if no filtering should be applied.
   * @param passback an object to be passed to the listener when a
   *                 notification is emitted.
   * @throws InstanceNotFoundException if the name of the management bean
   *                                   could not be resolved.
   * @throws RuntimeOperationsException if the bean associated with the given
   *                                    object name is not a
   *                                    {@link NotificationListener}.  This
   *                                    exception wraps an
   *                                    {@link IllegalArgumentException}.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "addNotificationListener")</code>}.
   * @see #removeNotificationListener(ObjectName, NotificationListener)
   * @see #removeNotificationListener(ObjectName, NotificationListener,
   *                                  NotificationFilter, Object)
   * @see NotificationBroadcaster#addNotificationListener(NotificationListener,
   *                                                      NotificationFilter,
   *                                                      Object)
   */
  void addNotificationListener(ObjectName name, ObjectName listener,
			       NotificationFilter filter, Object passback)
    throws InstanceNotFoundException;

  /**
   * <p>
   * Instantiates a new instance of the specified management bean
   * using the default constructor and registers it with the server
   * under the supplied name.  The class is loaded using the
   * {@link javax.management.loading.ClassLoaderRepository default
   * loader repository} of the server. 
   * </p>
   * <p>
   * If the name supplied is <code>null</code>, then the bean is
   * expected to implement the {@link MBeanRegistration} interface.
   * The {@link MBeanRegistration#preRegister preRegister} method
   * of this interface will be used to obtain the name in this case.
   * </p>
   * <p>
   * This method is equivalent to calling {@link 
   * #createMBean(String, ObjectName, Object[], String[])
   * <code>createMBean(className, name, (Object[]) null,
   * (String[]) null)</code>} with <code>null</code> parameters
   * and signature.
   * </p>
   *
   * @param className the class of the management bean, of which
   *                  an instance should be created.
   * @param name the name to register the new bean with.
   * @return an {@link ObjectInstance} containing the {@link ObjectName}
   *         and Java class name of the created instance.
   * @throws ReflectionException if an exception occurs in creating
   *                             an instance of the bean.
   * @throws InstanceAlreadyExistsException if a matching instance
   *                                        already exists.
   * @throws MBeanRegistrationException if an exception occurs in
   *                                    calling the preRegister
   *                                    method.
   * @throws MBeanException if the bean's constructor throws an exception.
   * @throws NotCompliantMBeanException if the created bean is not
   *                                    compliant with the JMX specification.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> class name or object
   *                                    name or if the object name is a pattern.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply the
   *                           use of the <code>instantiate</code>
   *                           and <code>registerMBean</code> methods.
   * @see #createMBean(String, ObjectName, Object[], String[])
   */
  ObjectInstance createMBean(String className, ObjectName name)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException;

  /**
   * <p>
   * Instantiates a new instance of the specified management bean
   * using the given constructor and registers it with the server
   * under the supplied name.  The class is loaded using the
   * {@link javax.management.loading.ClassLoaderRepository default
   * loader repository} of the server. 
   * </p>
   * <p>
   * If the name supplied is <code>null</code>, then the bean is
   * expected to implement the {@link MBeanRegistration} interface.
   * The {@link MBeanRegistration#preRegister preRegister} method
   * of this interface will be used to obtain the name in this case.
   * </p>
   * 
   * @param className the class of the management bean, of which
   *                  an instance should be created.
   * @param name the name to register the new bean with.
   * @param params the parameters for the bean's constructor.
   * @param sig the signature of the constructor to use.
   * @return an {@link ObjectInstance} containing the {@link ObjectName}
   *         and Java class name of the created instance.
   * @throws ReflectionException if an exception occurs in creating
   *                             an instance of the bean.
   * @throws InstanceAlreadyExistsException if a matching instance
   *                                        already exists.
   * @throws MBeanRegistrationException if an exception occurs in
   *                                    calling the preRegister
   *                                    method.
   * @throws MBeanException if the bean's constructor throws an exception.
   * @throws NotCompliantMBeanException if the created bean is not
   *                                    compliant with the JMX specification.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> class name or object
   *                                    name or if the object name is a pattern.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply the
   *                           use of the <code>instantiate</code>
   *                           and <code>registerMBean</code> methods.
   */
  ObjectInstance createMBean(String className, ObjectName name,
			     Object[] params, String[] sig)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException;

  /**
   * <p>
   * Instantiates a new instance of the specified management bean
   * using the default constructor and registers it with the server
   * under the supplied name.  The class is loaded using the
   * given class loader.  If this argument is <code>null</code>,
   * then the same class loader as was used to load the server
   * is used.
   * </p>
   * <p>
   * If the name supplied is <code>null</code>, then the bean is
   * expected to implement the {@link MBeanRegistration} interface.
   * The {@link MBeanRegistration#preRegister preRegister} method
   * of this interface will be used to obtain the name in this case.
   * </p>
   * <p>
   * This method is equivalent to calling {@link 
   * #createMBean(String, ObjectName, ObjectName, Object[], String)
   * <code>createMBean(className, name, loaderName, (Object[]) null,
   * (String) null)</code>} with <code>null</code> parameters
   * and signature.
   * </p>
   *
   * @param className the class of the management bean, of which
   *                  an instance should be created.
   * @param name the name to register the new bean with.
   * @param loaderName the name of the class loader.
   * @return an {@link ObjectInstance} containing the {@link ObjectName}
   *         and Java class name of the created instance.
   * @throws ReflectionException if an exception occurs in creating
   *                             an instance of the bean.
   * @throws InstanceAlreadyExistsException if a matching instance
   *                                        already exists.
   * @throws MBeanRegistrationException if an exception occurs in
   *                                    calling the preRegister
   *                                    method.
   * @throws MBeanException if the bean's constructor throws an exception.
   * @throws NotCompliantMBeanException if the created bean is not
   *                                    compliant with the JMX specification.
   * @throws InstanceNotFoundException if the specified class loader is not
   *                                   registered with the server.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> class name or object
   *                                    name or if the object name is a pattern.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply the
   *                           use of the <code>instantiate</code>
   *                           and <code>registerMBean</code> methods.
   * @see #createMBean(String, ObjectName, ObjectName, Object[], String[])
   */
  ObjectInstance createMBean(String className, ObjectName name, 
			     ObjectName loaderName)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException, InstanceNotFoundException;

  /**
   * <p>
   * Instantiates a new instance of the specified management bean
   * using the given constructor and registers it with the server
   * under the supplied name.  The class is loaded using the
   * given class loader.  If this argument is <code>null</code>,
   * then the same class loader as was used to load the server
   * is used. 
   * </p>
   * <p>
   * If the name supplied is <code>null</code>, then the bean is
   * expected to implement the {@link MBeanRegistration} interface.
   * The {@link MBeanRegistration#preRegister preRegister} method
   * of this interface will be used to obtain the name in this case.
   * </p>
   * 
   * @param className the class of the management bean, of which
   *                  an instance should be created.
   * @param name the name to register the new bean with.
   * @param loaderName the name of the class loader.
   * @param params the parameters for the bean's constructor.
   * @param sig the signature of the constructor to use.
   * @return an {@link ObjectInstance} containing the {@link ObjectName}
   *         and Java class name of the created instance.
   * @throws ReflectionException if an exception occurs in creating
   *                             an instance of the bean.
   * @throws InstanceAlreadyExistsException if a matching instance
   *                                        already exists.
   * @throws MBeanRegistrationException if an exception occurs in
   *                                    calling the preRegister
   *                                    method.
   * @throws MBeanException if the bean's constructor throws an exception.
   * @throws NotCompliantMBeanException if the created bean is not
   *                                    compliant with the JMX specification.
   * @throws InstanceNotFoundException if the specified class loader is not
   *                                   registered with the server.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> class name or object
   *                                    name or if the object name is a pattern.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply the
   *                           use of the <code>instantiate</code>
   *                           and <code>registerMBean</code> methods.
   */
  ObjectInstance createMBean(String className, ObjectName name,
			     ObjectName loaderName, Object[] params,
			     String[] sig)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException, InstanceNotFoundException;

  /**
   * Deserializes a byte array using the class loader of the specified
   * management bean as its context.
   *
   * @param name the name of the bean whose class loader should be used.
   * @param data the byte array to be deserialized.
   * @return the deserialized object stream.
   * @deprecated {@link #getClassLoaderFor(ObjectName)} should be used
   *             to obtain the class loader of the bean, which can then
   *             be used to perform deserialization in the user's code.
   * @throws InstanceNotFoundException if the specified bean is not
   *                                   registered with the server.
   * @throws OperationsException if any I/O error is thrown by the
   *                             deserialization process.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "getClassLoaderFor")</code>
   */
  ObjectInputStream deserialize(ObjectName name, byte[] data)
    throws InstanceNotFoundException, OperationsException;

  /**
   * Deserializes a byte array using the same class loader for its context
   * as was used to load the given class.  This class loader is obtained by
   * loading the specified class using the {@link
   * javax.management.loading.ClassLoaderRepository Class Loader Repository}
   * and then using the class loader of the resulting {@link Class} instance.
   *
   * @param name the name of the class which should be loaded to obtain the
   *             class loader.
   * @param data the byte array to be deserialized.
   * @return the deserialized object stream.
   * @deprecated {@link #getClassLoaderRepository} should be used
   *             to obtain the class loading repository, which can then
   *             be used to obtain the {@link Class} instance and deserialize
   *             the array using its class loader.
   * @throws OperationsException if any I/O error is thrown by the
   *                             deserialization process.
   * @throws ReflectionException if an error occurs in obtaining the
   *                             {@link Class} instance.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(null, null, null,
   *                           "getClassLoaderRepository")</code>
   */
  ObjectInputStream deserialize(String name, byte[] data)
    throws OperationsException, ReflectionException;

  /**
   * Deserializes a byte array using the same class loader for its context
   * as was used to load the given class.  The name of the class loader to
   * be used is supplied, and may be <code>null</code> if the server's
   * class loader should be used instead.
   *
   * @param name the name of the class which should be loaded to obtain the
   *             class loader.
   * @param loader the name of the class loader to use, or <code>null</code>
   *               if the class loader of the server should be used.
   * @param data the byte array to be deserialized.
   * @return the deserialized object stream.
   * @deprecated {@link #getClassLoader(ObjectName} can be used to obtain
   *              the named class loader and deserialize the array.
   * @throws InstanceNotFoundException if the specified class loader is not
   *                                   registered with the server.
   * @throws OperationsException if any I/O error is thrown by the
   *                             deserialization process.
   * @throws ReflectionException if an error occurs in obtaining the
   *                             {@link Class} instance.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, loader,
   *                           "getClassLoader")</code>
   */
  ObjectInputStream deserialize(String name, ObjectName loader, byte[] data)
    throws InstanceNotFoundException, ReflectionException,
	   OperationsException;

  /**
   * Returns the value of the supplied attribute from the specified
   * management bean.
   *
   * @param bean the bean to retrieve the value from.
   * @param name the name of the attribute to retrieve.
   * @return the value of the attribute.
   * @throws AttributeNotFoundException if the attribute could not be
   *                                    accessed from the bean.
   * @throws MBeanException if the management bean's accessor throws
   *                        an exception.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ReflectionException if an exception was thrown in trying
   *                             to invoke the bean's accessor.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean or attribute
   *                                    name.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, name, bean,
   *                           "getAttribute")</code>}.
   * @see DynamicMBean#getAttribute(String)
   */
  Object getAttribute(ObjectName bean, String name)
    throws MBeanException, AttributeNotFoundException,
	   InstanceNotFoundException, ReflectionException;

  /**
   * Returns the values of the named attributes from the specified
   * management bean.
   *
   * @param bean the bean to retrieve the value from.
   * @param names the names of the attributes to retrieve.
   * @return the values of the attributes.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ReflectionException if an exception was thrown in trying
   *                             to invoke the bean's accessor.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean or attribute
   *                                    name.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, bean,
   *                           "getAttribute")</code>}.  Additionally,
   *                           for an attribute name, <code>n</code>, the
   *                           caller's permission must imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, n, bean,
   *                           "getAttribute")</code>} or that attribute will
   *                           not be included.
   *                           
   * @see DynamicMBean#getAttributes(String[])
   */
  AttributeList getAttributes(ObjectName bean, String[] names)
    throws InstanceNotFoundException, ReflectionException;

  /**
   * Returns the specified class loader.  If the specified value is
   * <code>null</code>, then the class loader of the server will be
   * returned.  If <code>l</code> is the requested class loader,
   * and <code>r</code> is the actual class loader returned, then
   * either <code>l</code> and <code>r</code> will be identical,
   * or they will at least return the same class from
   * {@link ClassLoader#loadClass(String)} for any given string.
   * They may not be identical due to one or the other
   * being wrapped in another class loader (e.g. for security).
   *
   * @param name the name of the class loader to return.
   * @return the class loader.
   * @throws InstanceNotFoundException if the class loader can not
   *                                   be found.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "getClassLoader")</code>
   */
  ClassLoader getClassLoader(ObjectName name)
    throws InstanceNotFoundException;
 
  /**
   * Returns the class loader of the specified management bean.  If
   * <code>l</code> is the requested class loader, and <code>r</code>
   * is the actual class loader returned, then either <code>l</code>
   * and <code>r</code> will be identical, or they will at least
   * return the same class from {@link ClassLoader#loadClass(String)}
   * for any given string.  They may not be identical due to one or
   * the other being wrapped in another class loader (e.g. for
   * security).
   *
   * @param name the name of the bean whose class loader should be
   *             returned.
   * @return the class loader.
   * @throws InstanceNotFoundException if the bean is not registered
   *                                   with the server.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "getClassLoaderFor")</code>
   */
  ClassLoader getClassLoaderFor(ObjectName name)
    throws InstanceNotFoundException;

  /**
   * Returns the class loader repository used by this server.
   *
   * @return the class loader repository.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(null, null, null,
   *                           "getClassLoaderRepository")</code>
   */
  ClassLoaderRepository getClassLoaderRepository();

  /**
   * Returns the default domain this server applies to beans that have
   * no specified domain.
   *
   * @return the default domain.
   */
  String getDefaultDomain();

  /**
   * Returns an array containing all the domains used by beans registered
   * with this server.  The ordering of the array is undefined.
   *
   * @return the list of domains.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(null, null, name,
   *                           "getDomains")</code>}.  Additionally,
   *                           for an domain, <code>d</code>, the
   *                           caller's permission must imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(null, null,
   *                           new ObjectName("d:x=x"), "getDomains")</code>}
   *                           or that domain will not be included.  Note
   *                           that "x=x" is an arbitrary key-value pair
   *                           provided to satisfy the constructor.
   * @see ObjectName#getDomain()
   */
  String[] getDomains();

  /**
   * Returns the number of management beans registered with this server.
   * This may be less than the real number if the caller's access is
   * restricted.
   *
   * @return the number of registered beans.
   */
  Integer getMBeanCount();

  /**
   * Returns information on the given management bean.
   *
   * @param name the name of the management bean.
   * @return an instance of {@link MBeanInfo} for the bean.
   * @throws IntrospectionException if an exception occurs in examining
   *                                the bean.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ReflectionException if an exception occurs when trying
   *                             to invoke {@link DynamicMBean#getMBeanInfo()}
   *                             on the bean.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "getMBeanInfo")</code>}.
   * @see DynamicMBean#getMBeanInfo()
   */
  MBeanInfo getMBeanInfo(ObjectName name)
    throws InstanceNotFoundException, IntrospectionException,
	   ReflectionException;

  /**
   * Returns the {@link ObjectInstance} created for the specified
   * management bean on registration.
   *
   * @param name the name of the bean.
   * @return the corresponding {@link ObjectInstance} instance.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "getObjectInstance")</code>
   * @see #createMBean(String, ObjectName)
   */
  ObjectInstance getObjectInstance(ObjectName name)
    throws InstanceNotFoundException;

  /**
   * <p>
   * Creates an instance of the specified class using the list of
   * class loaders from the {@link
   * javax.management.loading.ClassLoaderRepository Class Loader
   * Repository}.  The class should have a public constructor
   * with no arguments.  A reference to the new instance is returned,
   * but the instance is not yet registered with the server.
   * </p>
   * <p>
   * This method is equivalent to calling {@link 
   * #instantiate(String, Object[], String[])
   * <code>instantiate(name, (Object[]) null, (String[]) null)</code>}
   * with <code>null</code> parameters and signature.
   * </p>
   *
   * @param name the name of the class of bean to be instantiated.
   * @return an instance of the given class.
   * @throws ReflectionException if an exception is thrown during
   *                             loading the class or calling the
   *                             constructor.
   * @throws MBeanException if the constructor throws an exception.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> name.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, null,
   *                           "instantiate")</code>}.
   * @see #instantiate(String, Object[], String[])
   */
  Object instantiate(String name)
    throws ReflectionException, MBeanException;

  /**
   * Creates an instance of the specified class using the list of
   * class loaders from the {@link
   * javax.management.loading.ClassLoaderRepository Class Loader
   * Repository}.  The class should have a public constructor
   * matching the supplied signature.  A reference to the new
   * instance is returned, but the instance is not yet
   * registered with the server.
   *
   * @param name the name of the class of bean to be instantiated.
   * @param params the parameters for the constructor.
   * @param sig the signature of the constructor.
   * @return an instance of the given class.
   * @throws ReflectionException if an exception is thrown during
   *                             loading the class or calling the
   *                             constructor.
   * @throws MBeanException if the constructor throws an exception.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> name.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, null,
   *                           "instantiate")</code>}.
   */
  Object instantiate(String name, Object[] params, String[] sig)
    throws ReflectionException, MBeanException;

  /**
   * <p>
   * Creates an instance of the specified class using the supplied
   * class loader.  If the class loader given is <code>null</code>,
   * then the class loader of the server will be used.  The class
   * should have a public constructor with no arguments.  A reference
   * to the new instance is returned, but the instance is not yet
   * registered with the server.
   * </p>
   * <p>
   * This method is equivalent to calling {@link 
   * #instantiate(String, ObjectName, Object[], String[])
   * <code>instantiate(name, loaderName, (Object[]) null,
   * (String[]) null)</code>} with <code>null</code> parameters
   * and signature.
   * </p>
   *
   * @param name the name of the class of bean to be instantiated.
   * @param loaderName the name of the class loader to use.
   * @return an instance of the given class.
   * @throws InstanceNotFoundException if the class loader is not
   *                                   registered with the server.
   * @throws ReflectionException if an exception is thrown during
   *                             loading the class or calling the
   *                             constructor.
   * @throws MBeanException if the constructor throws an exception.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> name.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, null,
   *                           "instantiate")</code>}.
   * @see #instantiate(String, Object[], String[])
   */
  Object instantiate(String name, ObjectName loaderName)
    throws InstanceNotFoundException, ReflectionException,
	   MBeanException;

  /**
   * Creates an instance of the specified class using the supplied
   * class loader.  If the class loader given is <code>null</code>,
   * then the class loader of the server will be used.  The class
   * should have a public constructor matching the supplied
   * signature.  A reference to the new instance is returned,
   * but the instance is not yet registered with the server.
   *
   * @param name the name of the class of bean to be instantiated.
   * @param loaderName the name of the class loader to use.
   * @param params the parameters for the constructor.
   * @param sig the signature of the constructor.
   * @return an instance of the given class.
   * @throws InstanceNotFoundException if the class loader is not
   *                                   registered with the server.
   * @throws ReflectionException if an exception is thrown during
   *                             loading the class or calling the
   *                             constructor.
   * @throws MBeanException if the constructor throws an exception.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> name.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, null,
   *                           "instantiate")</code>}.
   */
  Object instantiate(String name, ObjectName loaderName,
		     Object[] params, String[] sig)
    throws InstanceNotFoundException, ReflectionException,
	   MBeanException;

  /**
   * Invokes the supplied operation on the specified management
   * bean.  The class objects specified in the signature are loaded
   * using the same class loader as was used for the management bean.
   *
   * @param bean the management bean whose operation should be invoked.
   * @param name the name of the operation to invoke.
   * @param params the parameters of the operation.
   * @param sig the signature of the operation.
   * @return the return value of the method.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws MBeanException if the method invoked throws an exception.
   * @throws ReflectionException if an exception is thrown in invoking the
   *                             method.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, name, bean,
   *                           "invoke")</code>}.
   * @see DynamicMBean#invoke(String, Object[], String[])
   */
  Object invoke(ObjectName bean, String name, Object[] params, String[] sig)
    throws InstanceNotFoundException, MBeanException,
	   ReflectionException;

  /**
   * <p>
   * Returns true if the specified management bean is an instance
   * of the supplied class.
   * </p>
   * <p>
   * A bean, B, is an instance of a class, C, if either of the following
   * conditions holds:
   * </p>
   * <ul>
   * <li>The class name in B's {@link MBeanInfo} is equal to the supplied
   * name.</li>
   * <li>Both the class of B and C were loaded by the same class loader,
   * and B is assignable to C.</li>
   * </ul>
   * 
   * @param name the name of the management bean.
   * @param className the name of the class to test if <code>name</code> is
   *                  an instance of.
   * @return true if either B is directly an instance of the named class,
   *         or B is assignable to the class, given that both it and B's
   *         current class were loaded using the same class loader.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "isInstanceOf")</code>
   */
  boolean isInstanceOf(ObjectName name, String className)
    throws InstanceNotFoundException;

  /**
   * Returns true if the specified management bean is registered with
   * the server.
   *
   * @param name the name of the management bean.
   * @return true if the bean is registered.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean name.
   */
  boolean isRegistered(ObjectName name);

  /**
   * <p>
   * Returns a set of {@link ObjectInstance}s matching the specified
   * criteria.  The full set of beans registered with the server
   * are passed through two filters:
   * </p>
   * <ol>
   * <li>Pattern matching is performed using the supplied
   * {@link ObjectName}.</li>
   * <li>The supplied query expression is applied.</li>
   * </ol>
   * <p>
   * If both the object name and the query expression are <code>null</code>,
   * or the object name has no domain and no key properties,
   * no filtering will be performed and all beans are returned. 
   * </p>
   *
   * @param name an {@link ObjectName} to use as a filter.
   * @param query a query expression to apply to each of the beans that match
   *              the given object name.
   * @return a set of {@link ObjectInstance}s matching the filtered beans.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(null, null, name,
   *                           "queryMBeans")</code>}.  Additionally,
   *                           for an bean, <code>b</code>, the
   *                           caller's permission must imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, b, name,
   *                           "queryMBeans")</code>} or that bean will
   *                           not be included.  Such an exception may also
   *                           arise from the execution of the query, in which
   *                           case that particular bean will again be excluded.
   */
  Set<ObjectInstance> queryMBeans(ObjectName name, QueryExp query);
  
  /**
   * <p>
   * Returns a set of {@link ObjectName}s matching the specified
   * criteria.  The full set of beans registered with the server
   * are passed through two filters:
   * </p>
   * <ol>
   * <li>Pattern matching is performed using the supplied
   * {@link ObjectName}.</li>
   * <li>The supplied query expression is applied.</li>
   * </ol>
   * <p>
   * If both the object name and the query expression are <code>null</code>,
   * or the object name has no domain and no key properties,
   * no filtering will be performed and all beans are returned. 
   * </p>
   *
   * @param name an {@link ObjectName} to use as a filter.
   * @param query a query expression to apply to each of the beans that match
   *              the given object name.
   * @return a set of {@link ObjectName}s matching the filtered beans.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(null, null, name,
   *                           "queryNames")</code>}.  Additionally,
   *                           for an name, <code>n</code>, the
   *                           caller's permission must imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, n, name,
   *                           "queryNames")</code>} or that name will
   *                           not be included.  Such an exception may also
   *                           arise from the execution of the query, in which
   *                           case that particular bean will again be excluded.
   *                           Note that these permissions are implied if the
   *                           <code>queryMBeans</code> permissions are available.
   */
  Set<ObjectName> queryNames(ObjectName name, QueryExp query);

  /**
   * Registers the supplied instance with the server, using the specified
   * {@link ObjectName}.  If the name given is <code>null</code>, then
   * the bean supplied is expected to implement the {@link MBeanRegistration}
   * interface and provide the name via the 
   * {@link MBeanRegistration#preRegister preRegister} method
   * of this interface.
   *
   * @param obj the object to register with the server.
   * @param name the name under which to register the object,
   *             or <code>null</code> if the {@link MBeanRegistration}
   *             interface should be used.
   * @throws InstanceAlreadyExistsException if a matching instance
   *                                        already exists.
   * @throws MBeanRegistrationException if an exception occurs in
   *                                    calling the preRegister
   *                                    method.
   * @throws NotCompliantMBeanException if the created bean is not
   *                                    compliant with the JMX specification.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> object.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "registerMBean")</code>}.  <code>className</code>
   *                           here corresponds to the result of
   *                           {@link MBeanInfo#getClassName()} for objects of
   *                           this class.  If this check succeeds, a check
   *                           is also made on its
   *                           {@link java.security.ProtectionDomain} to ensure
   *                           it implies {@link MBeanTrustPermission(String)
   *                           <code>MBeanTrustPermission("register")</code>}.
   *                           The use of the {@link MBeanRegistration} interface
   *                           results in another {@link MBeanPermission} check
   *                           being made on the returned {@link ObjectName}.
   */
  ObjectInstance registerMBean(Object obj, ObjectName name)
    throws InstanceAlreadyExistsException, MBeanRegistrationException,
	   NotCompliantMBeanException;

  /**
   * Removes the specified listener from the list of recipients
   * of notifications from the supplied bean.  This includes all
   * combinations of filters and passback objects registered for
   * this listener.  For more specific removal of listeners, see
   * {@link #removeNotificationListener(ObjectName,
   * NotificationListener,NotificationFilter,Object)}
   *
   * @param name the name of the management bean from which the
   *             listener should be removed.
   * @param listener the listener to remove.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with the bean.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "removeNotificationListener")</code>}.
   * @see #addNotificationListener(NotificationListener, NotificationFilter,
   *                               java.lang.Object)
   * @see NotificationBroadcaster#removeNotificationListener(NotificationListener)
   */
  void removeNotificationListener(ObjectName name,
				  NotificationListener listener)
    throws InstanceNotFoundException, ListenerNotFoundException;

  /**
   * Removes the specified listener from the list of recipients
   * of notifications from the supplied bean.  Only the first instance with
   * the supplied filter and passback object is removed.
   * <code>null</code> is used as a valid value for these parameters,
   * rather than as a way to remove all registration instances for
   * the specified listener; for this behaviour instead, see
   * {@link #removeNotificationListener(ObjectName, NotificationListener)}.
   *
   * @param name the name of the management bean from which the
   *             listener should be removed.
   * @param listener the listener to remove.
   * @param filter the filter of the listener to remove.
   * @param passback the passback object of the listener to remove.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with the bean.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "removeNotificationListener")</code>}.
   * @see #addNotificationListener(ObjectName, NotificationListener,
   *                               NotificationFilter, Object)
   * @see NotificationEmitter#removeNotificationListener(NotificationListener,
   *                                                     NotificationFilter,
   *                                                     Object)
   */
  void removeNotificationListener(ObjectName name,
				  NotificationListener listener,
				  NotificationFilter filter,
				  Object passback)
    throws InstanceNotFoundException, ListenerNotFoundException;

  /**
   * Removes the specified listener from the list of recipients
   * of notifications from the supplied bean.  This includes all
   * combinations of filters and passback objects registered for
   * this listener.  For more specific removal of listeners, see
   * {@link #removeNotificationListener(ObjectName,
   * ObjectName,NotificationFilter,Object)}
   *
   * @param name the name of the management bean from which the
   *             listener should be removed.
   * @param listener the name of the listener to remove.
   * @throws InstanceNotFoundException if a name doesn't match a registered
   *                                   bean.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with the bean.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "removeNotificationListener")</code>}.
   * @see #addNotificationListener(NotificationListener, NotificationFilter,
   *                               java.lang.Object)
   * @see NotificationBroadcaster#removeNotificationListener(NotificationListener)
   */
  void removeNotificationListener(ObjectName name, ObjectName listener)
    throws InstanceNotFoundException, ListenerNotFoundException;

  /**
   * Removes the specified listener from the list of recipients
   * of notifications from the supplied bean.  Only the first instance with
   * the supplied filter and passback object is removed.
   * <code>null</code> is used as a valid value for these parameters,
   * rather than as a way to remove all registration instances for
   * the specified listener; for this behaviour instead, see
   * {@link #removeNotificationListener(ObjectName, ObjectName)}.
   *
   * @param name the name of the management bean from which the
   *             listener should be removed.
   * @param listener the name of the listener to remove.
   * @param filter the filter of the listener to remove.
   * @param passback the passback object of the listener to remove.
   * @throws InstanceNotFoundException if a name doesn't match a registered
   *                                   bean.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with the bean.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "removeNotificationListener")</code>}.
   * @see #addNotificationListener(ObjectName, NotificationListener,
   *                               NotificationFilter, Object)
   * @see NotificationEmitter#removeNotificationListener(NotificationListener,
   *                                                     NotificationFilter,
   *                                                     Object)
   */
  void removeNotificationListener(ObjectName name,
				  ObjectName listener,
				  NotificationFilter filter,
				  Object passback)
    throws InstanceNotFoundException, ListenerNotFoundException;

  /**
   * Sets the value of the specified attribute of the supplied
   * management bean.  
   *
   * @param name the name of the management bean.
   * @param attribute the attribute to set.
   * @throws InstanceNotFoundException if the bean can not be found.
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
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean or attribute
   *                                    name.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, name, bean,
   *                           "setAttribute")</code>}.
   * @see #getAttribute(ObjectName, String)
   * @see DynamicMBean#setAttribute(Attribute)
   */
  void setAttribute(ObjectName name, Attribute attribute)
    throws InstanceNotFoundException, AttributeNotFoundException,
	   InvalidAttributeValueException, MBeanException,
	   ReflectionException;

  /**
   * Sets the value of each of the specified attributes
   * of the supplied management bean to that specified by
   * the {@link Attribute} object.  The returned list contains
   * the attributes that were set and their new values.
   *
   * @param name the name of the management bean.
   * @param attributes the attributes to set.
   * @return a list of the changed attributes.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ReflectionException if an exception occurred in trying
   *                             to use the reflection interface
   *                             to lookup the attribute.  The
   *                             thrown exception is the cause of
   *                             this exception.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean or attribute
   *                                    list.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, bean,
   *                           "setAttribute")</code>}.  Additionally,
   *                           for an attribute name, <code>n</code>, the
   *                           caller's permission must imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, n, bean,
   *                           "setAttribute")</code>} or that attribute will
   *                           not be included.
   * @see #getAttributes(ObjectName, String[])
   * @see DynamicMBean#setAttributes(AttributeList)
   */
  AttributeList setAttributes(ObjectName name, AttributeList attributes)
    throws InstanceNotFoundException, ReflectionException;

  /**
   * Unregisters the specified management bean.  Following this operation,
   * the bean instance is no longer accessible from the server via this
   * name.  Prior to unregistering the bean, the
   * {@link MBeanRegistration#preDeregister()} method will be called if
   * the bean implements the {@link MBeanRegistration} interface.
   *
   * @param name the name of the management bean.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws MBeanRegistrationException if an exception occurs in
   *                                    calling the preDeregister
   *                                    method.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean name or a
   *                                    request being made to unregister the
   *                                    {@link MBeanServerDelegate} bean.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, null, name,
   *                           "unregisterMBean")</code>}.
   */ 
  void unregisterMBean(ObjectName name)
    throws InstanceNotFoundException, MBeanRegistrationException;

}
