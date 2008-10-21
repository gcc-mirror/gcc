/* RMIConnection.java -- RMI object representing a MBean server connection.
   Copyright (C) 2007, 2008 Free Software Foundation, Inc.

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

package javax.management.remote.rmi;

import java.io.Closeable;
import java.io.IOException;

import java.rmi.MarshalledObject;
import java.rmi.Remote;

import java.util.Set;

import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanInfo;
import javax.management.MBeanException;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.ReflectionException;

import javax.management.remote.NotificationResult;

import javax.security.auth.Subject;

/**
 * <p>
 * RMI interface for forwarding requests to a remote
 * {@link javax.management.MBeanServer}.  This interface
 * parallels the {@link javax.management.MBeanServerConnection}
 * interface, providing a way of invoking those methods using
 * the RMI protocol.  When a client wishes to call a method
 * of an MBean server using RMI, the method is called on the stub
 * on the client side, which serializes the object parameters
 * and sends them to the server where they are deserialized and
 * an implementation of this interface forwards them to the
 * appropriate MBean server.  Return values follow the same
 * process, only in reverse.  Each client obtains its own
 * implementation of this interface from an {@link RMIServer}
 * instance.
 * </p>
 * <p>
 * Implementations of this interface do more than simply
 * forward requests directly to the server.  The arguments
 * of the server methods are wrapped in {@link MarshalledObject}
 * instances, so that the correct classloader can be used to
 * deserialize the arguments.  When a method is called, the
 * implementation must first retrieve the appropriate classloader
 * and then use it to deserialize the marshalled object.  Unless
 * explicitly specified in the documentation for the method,
 * a parameter of the type {@link MarshalledObject} or an array
 * of that type should not be {@code null}.
 * </p>
 * <p>
 * Security is also handled by this interface, as the methods
 * use an additional {@link javax.security.auth.Subject} parameter
 * for role delegation.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface RMIConnection
  extends Closeable, Remote
{

  /**
   * Handles {@link
   * MBeanServerConnection#addNotificationListener(ObjectName,
   * ObjectName, NotificationFilter, Object)} by
   * registering the supplied listener with the specified management
   * bean.  Notifications emitted by the management bean are forwarded
   * to the listener via the server, which will convert any MBean
   * references in the source to portable {@link ObjectName}
   * instances.  The notification is otherwise unchanged.  The filter
   * and handback object are wrapped in a {@link MarshalledObject}
   * so that they are deserialised using the bean's classloader.
   *
   * @param name the name of the management bean with which the listener
   *             should be registered.
   * @param listener the listener which will handle notifications from
   *                 the bean.
   * @param filter a wrapper containing a filter to apply to incoming
   *               notifications, or <code>null</code> if no filtering
   *               should be applied.
   * @param passback a wrapper containing an object to be passed to the
   *                 listener when a notification is emitted.
   * @param delegationSubject a {@link javax.security.auth.Subject} instance
   *                          containing the delegation principles or
   *                          {@code null} if authentication is used.
   * @throws InstanceNotFoundException if the name of the management bean
   *                                   could not be resolved.
   * @throws RuntimeOperationsException if the bean associated with the given
   *                                    object name is not a
   *                                    {@link NotificationListener}.  This
   *                                    exception wraps an
   *                                    {@link IllegalArgumentException}.
   * @throws SecurityException if the client or delegated subject (if any)
   *                           does not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #removeNotificationListener(ObjectName, ObjectName,
   *                                  javax.security.auth.Subject)
   * @see #removeNotificationListener(ObjectName, ObjectName,
   *                                  java.rmi.MarshalledObject,
   *                                  java.rmi.MarshalledObject,
   *                                  javax.security.auth.Subject)
   * @see #removeNotificationListeners(ObjectName, Integer[],
   *                                  javax.security.auth.Subject)
   * @see NotificationBroadcaster#addNotificationListener(NotificationListener,
   *                                                      NotificationFilter,
   *                                                      Object)
   */
  @SuppressWarnings("unchecked")
  void addNotificationListener(ObjectName name, ObjectName listener,
			       MarshalledObject filter, MarshalledObject passback,
			       Subject delegationSubject)
    throws InstanceNotFoundException, IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#addNotificationListener(ObjectName,
   * NotificationListener, NotificationFilter, Object)} by
   * registering for notifications from the specified management
   * beans.  The array of filters is assumed to be aligned with
   * the array of bean names, so that the notifications from each
   * bean are matched against the appropriate filter (or left as
   * is if the filter is {@code null}.  Notifications emitted by
   * the management beans are forwarded to a local listener created
   * by this method, via the server, which converts any MBean
   * references in the source to portable {@link ObjectName}
   * instances.  The notification is otherwise unchanged.
   * </p>
   * <p>
   * This local listener buffers the notifications for retrieval by
   * {@link #fetchNotifications(long,int,long).  This method returns
   * an array of listener identifiers which aligns with the supplied
   * array of beans so that the appropriate listener can be identified
   * by the client, which retains its own listener and handback object.
   * The filters are wrapped in {@link MarshalledObject}s so that they are
   * deserialised using the bean's classloader.
   * </p>
   *
   * @param names the names of the management bean whose notifications
   *              should be recorded.
   * @param filters an array of wrappers containing filters to apply to
   *                incoming notifications.  An element may be <code>null</code>
   *                if no filtering should be applied to a bean's notifications.
   * @param delegationSubjects an array of {@link javax.security.auth.Subject}
   *                          instances containing the delegation principles for
   *                          each listener.  An element may be {@code null} if
   *                          authentication is used instead, or the entire
   *                          argument itself may be {@code null}.  In the latter
   *                          case, this is treated as an array of {@code null}
   *                          values.
   * @return an array of integers which act as listener identifiers, so that
   *         notifications retrieved from {@link #fetchNotifications(long,int,long)
   *         can be matched to the beans they were emitted from.  The array is
   *         aligned against the array of beans supplied to this methods, so that
   *         the identifier in position 0 represents the bean in position 0 of the
   *         input array.
   * @throws IllegalArgumentException if the {@code names} or {@code filters} array
   *                                  is {@code null}, the {@code names} array contains
   *                                  a {@code null} value or the three arrays are not
   *                                  of the same size.
   * @throws ClassCastException if an element of the {@code filters} array unmarshalls
   *                            as a non-null object that is not a {@link NotificationFilter}.
   * @throws InstanceNotFoundException if the name of one of the management beans
   *                                   could not be resolved.
   * @throws SecurityException if, for one of the beans, the client or delegated subject
   *                           (if any) does not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #removeNotificationListener(ObjectName, ObjectName,
   *                                  javax.security.auth.Subject)
   * @see #removeNotificationListener(ObjectName, ObjectName,
   *                                  java.rmi.MarshalledObject,
   *                                  java.rmi.MarshalledObject,
   *                                  javax.security.auth.Subject)
   * @see #removeNotificationListeners(ObjectName, Integer[],
   *                                  javax.security.auth.Subject)
   * @see NotificationBroadcaster#addNotificationListener(NotificationListener,
   *                                                      NotificationFilter,
   *                                                      Object)
   */
  @SuppressWarnings("unchecked")
  Integer[] addNotificationListeners(ObjectName[] names, MarshalledObject[] filters,
				     Subject[] delegationSubjects)
    throws InstanceNotFoundException, IOException;

  /**
   * Closes the connection and unexports the RMI object implementing this
   * interface.  Following this call, future method calls to this instance
   * will fail.
   *
   * @throws IOException if there is an I/O error in transmitting the close
   *                     request via RMI, closing the connection, or unexporting
   *                     the RMI object.
   */
  void close()
    throws IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#createMBean(String, ObjectName,
   * Object[], String[])}.  The array of parameters is wrapped in
   * a {@link MarshalledObject} so that it is deserialised using the
   * bean's classloader.
   * </p>
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
   * @param name the name to register the new bean with.  This may
   *             be <code>null</code>.
   * @param params the parameters for the bean's constructor, encapsulated
   *               in a {@link MarshalledObject}.  If this parameter is
   *               <code>null</code>, it will be judged equivalent to an
   *               empty array.
   * @param sig the signature of the constructor to use.  If this parameter
   *            is <code>null</code>, it will be judged equivalent to an
   *            empty array.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
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
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */  
  @SuppressWarnings("unchecked")
  ObjectInstance createMBean(String className, ObjectName name,
			     MarshalledObject params, String[] sig,
			     Subject delegationSubject)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException, IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#createMBean(String, ObjectName,
   * ObjectName, Object[], String[])}.  The array of parameters is
   * wrapped in a {@link MarshalledObject} so that it is deserialised
   * using the bean's classloader.
   * </p>
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
   * @param name the name to register the new bean with.  This may
   *             be <code>null</code>.
   * @param loaderName the name of the class loader.
   * @param params the parameters for the bean's constructor, encapsulated
   *               in a {@link MarshalledObject}.  If this parameter is
   *               <code>null</code>, it will be judged equivalent to an
   *               empty array.
   * @param sig the signature of the constructor to use.  If this parameter
   *            is <code>null</code>, it will be judged equivalent to an
   *            empty array.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
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
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */
  @SuppressWarnings("unchecked")
  ObjectInstance createMBean(String className, ObjectName name,
			     ObjectName loaderName, MarshalledObject params,
			     String[] sig, Subject delegationSubject)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException, InstanceNotFoundException,
	   IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#createMBean(String, ObjectName,
   * ObjectName)} by instantiating a new instance of the specified
   * management bean using the default constructor and registering
   * it with the server under the supplied name.  The class is loaded
   * using the given class loader.  If this argument is <code>null</code>,
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
   * @param name the name to register the new bean with.  This may
   *             be <code>null</code>.
   * @param loaderName the name of the class loader.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
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
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #createMBean(String, ObjectName, ObjectName, MarshalledObject,
   *                   String[], Subject)
   */
  ObjectInstance createMBean(String className, ObjectName name, 
			     ObjectName loaderName, Subject delegationSubject)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException, InstanceNotFoundException,
	   IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#createMBean(String, ObjectName)} by
   * instantiating a new instance of the specified management bean
   * using the default constructor and registering it with the server
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
   * @param name the name to register the new bean with.  This may
   *             be <code>null</code>.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
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
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #createMBean(String, ObjectName, MarshalledObject, String[], Subject)
   */
  ObjectInstance createMBean(String className, ObjectName name,
			     Subject delegationSubject)
    throws ReflectionException, InstanceAlreadyExistsException,
	   MBeanRegistrationException, MBeanException,
	   NotCompliantMBeanException, IOException;

  /**
   * <p>
   * Retrieves any waiting notifications from the server.  When notifications
   * are requested using the {@link #addNotificationListeners(ObjectName[],
   * MarshalledObject[], Subject[])} method, the server sets up an internal
   * listener to receive notifications from the bean and buffer them.  When
   * this method is called, these buffered notifications can be retrieved.
   * </p>
   * <p>
   * The blocking behaviour of this method depends on the timeout value specified.
   * If there are no waiting notifications in the buffer, a value of 0 will cause
   * the method to return immediately.  Conversely, if the value is
   * {@link Long#MAX_VALUE}, then it will wait indefinitely until a notification
   * arrives. For any other value, it waits until a notification arrives or the
   * number of milliseconds specified by the timeout value is exceeded.  The
   * behaviour for a negative timeout value is undefined.
   * </p>
   * <p>
   * For a notification to be returned, the following criteria must be fulfilled:
   * </p>
   * <ul>
   * <li>the client must have previously requested notifications from at least
   * one bean</li>
   * <li>a bean from which notifications have been requested must have emitted
   * a notification since the last call to this method</li>
   * <li>the emitted notification must pass through any filters established
   * when notifications were requested</li>
   * <li>the sequence number of the notification must be greater than or equal
   * to the specified sequence number (if non-negative)</li>
   * </ul>
   *
   * @param sequenceNumber the sequence number of each notification returned
   *                       must be greater than or equal to this value.  If
   *                       the number is negative, this is interpreted as
   *                       meaning the sequence number of the next notification
   *                       and so all notifications are allowed through.
   * @param maxNotifications the maximum number of notifications to return.
   *                         This does not include any duplicates so the
   *                         number of actual notifications returned may
   *                         be larger.
   * @param timeout the number of milliseconds to wait for a notification
   *                if the buffer is empty.  <code>0</code> causes the
   *                method to return immediately even if there are no
   *                notifications available (non-blocking behaviour) while
   *                a value of {@link Long#MAX_VALUE} causes it to wait
   *                indefinitely (blocking behaviour).  The response to
   *                a negative value is undefined.
   * @return a {@link NotificationResult} object containing the buffered
   *         notifications.
   * @throws IOException if an I/O error occurs.
   */
  NotificationResult fetchNotifications(long sequenceNumber,
					int maxNotifications,
					long timeout)
    throws IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#getAttribute(ObjectName, String)},
   * returning the value of the supplied attribute from the specified
   * management bean.
   *
   * @param bean the bean to retrieve the value from.
   * @param name the name of the attribute to retrieve.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
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
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see DynamicMBean#getAttribute(String)
   */
  Object getAttribute(ObjectName bean, String name, Subject delegationSubject)
    throws MBeanException, AttributeNotFoundException,
	   InstanceNotFoundException, ReflectionException,
	   IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#getAttribute(ObjectName, String)},
   * returning the values of the named attributes from the specified
   * management bean.
   *
   * @param bean the bean to retrieve the value from.
   * @param names the names of the attributes to retrieve.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return the values of the attributes.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ReflectionException if an exception was thrown in trying
   *                             to invoke the bean's accessor.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean or attribute
   *                                    name.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see DynamicMBean#getAttributes(String[])
   */
  AttributeList getAttributes(ObjectName bean, String[] names,
			      Subject delegationSubject)
    throws InstanceNotFoundException, ReflectionException,
	   IOException;

  /**
   * Returns the unique identifier for this connection to the RMI
   * server.  
   *
   * @return the connection ID.
   * @throws IOException if an I/O error occurred.
   */
  String getConnectionId()
    throws IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#getDefaultDomain()} by returning the default
   * domain this server applies to beans that have no specified domain.
   *
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return the default domain.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */
  String getDefaultDomain(Subject delegationSubject)
    throws IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#getDomains()} by returning an array
   * containing all the domains used by beans registered with
   * this server.  The ordering of the array is undefined.
   *
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return the list of domains.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see ObjectName#getDomain()
   */
  String[] getDomains(Subject delegationSubject)
    throws IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#getMBeanCount()} by returning the number of
   * management beans registered with this server.
   *
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return the number of registered beans.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */
  Integer getMBeanCount(Subject delegationSubject)
    throws IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#getMBeanInfo(ObjectName)} by returning
   * information on the given management bean.
   *
   * @param name the name of the management bean.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return an instance of {@link MBeanInfo} for the bean.
   * @throws IntrospectionException if an exception occurs in examining
   *                                the bean.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ReflectionException if an exception occurs when trying
   *                             to invoke {@link DynamicMBean#getMBeanInfo()}
   *                             on the bean.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see DynamicMBean#getMBeanInfo()
   */
  MBeanInfo getMBeanInfo(ObjectName name, Subject delegationSubject)
    throws InstanceNotFoundException, IntrospectionException,
	   ReflectionException, IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#getObjectInstance(ObjectName)} by returning
   * the {@link ObjectInstance} created for the specified management
   * bean on registration.
   *
   * @param name the name of the bean.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return the corresponding {@link ObjectInstance} instance.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #createMBean(String, ObjectName, Subject)
   */
  ObjectInstance getObjectInstance(ObjectName name, Subject delegationSubject)
    throws InstanceNotFoundException, IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#invoke(ObjectName, String, Object[],
   * String[])}.  The array of parameters is wrapped in a
   * {@link MarshalledObject} so that it is deserialised
   * using the bean's classloader.
   * </p>
   * <p>
   * Invokes the supplied operation on the specified management
   * bean.  The class objects specified in the signature are loaded
   * using the same class loader as was used for the management bean.
   *
   * @param bean the management bean whose operation should be invoked.
   * @param name the name of the operation to invoke.
   * @param params the parameters for the bean's constructor, encapsulated
   *               in a {@link MarshalledObject}.  If this parameter is
   *               <code>null</code>, it will be judged equivalent to an
   *               empty array.
   * @param sig the signature of the constructor to use.  If this parameter
   *            is <code>null</code>, it will be judged equivalent to an
   *            empty array.  The class objects will be loaded using the
   *            bean's classloader.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return the return value of the method.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws MBeanException if the method invoked throws an exception.
   * @throws ReflectionException if an exception is thrown in invoking the
   *                             method.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see DynamicMBean#invoke(String, Object[], String[])
   */
  @SuppressWarnings("unchecked")
  Object invoke(ObjectName bean, String name, MarshalledObject params,
		String[] sig, Subject delegationSubject)
    throws InstanceNotFoundException, MBeanException,
	   ReflectionException, IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#isInstanceOf(ObjectName, String) by
   * returning true if the specified management bean is an instance
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
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return true if either B is directly an instance of the named class,
   *         or B is assignable to the class, given that both it and B's
   *         current class were loaded using the same class loader.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */
  boolean isInstanceOf(ObjectName name, String className,
		       Subject delegationSubject)
    throws InstanceNotFoundException, IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#isRegistered(ObjectName) by returning
   * true if the specified management bean is registered with
   * the server.
   *
   * @param name the name of the management bean.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return true if the bean is registered.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean name.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */
  boolean isRegistered(ObjectName name, Subject delegationSubject)
    throws IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#queryMBeans(ObjectName, QueryExp)}.
   * The query expression is wrapped in a {@link MarshalledObject}
   * so that it is deserialised using the bean's classloader.
   * </p>
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
   *              the given object name, encapsulated in a
   *              {@link MarshalledObject}.  If a <code>null</code> value is
   *              encapsulated, then the beans will only be filtered using
   *              pattern matching on the supplied {@link ObjectName}.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return a set of {@link ObjectInstance}s matching the filtered beans.
   *         This is empty if no beans survived the filters.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   */
  @SuppressWarnings("unchecked")
  Set<ObjectInstance> queryMBeans(ObjectName name, MarshalledObject query,
				  Subject delegationSubject)
    throws IOException;
  
  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#queryNames(ObjectName, QueryExp)}.
   * The query expression is wrapped in a {@link MarshalledObject}
   * so that it is deserialised using the bean's classloader.
   * </p>
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
   *              the given object name, encapsulated in a
   *              {@link MarshalledObject}.  If a <code>null</code> value is
   *              encapsulated, then the beans will only be filtered using
   *              pattern matching on the supplied {@link ObjectName}.
   * @param delegationSubject an instance of {@link javax.security.auth.Subject}
   *                          containing the delegation principles.  This may be
   *                          {@code null} is authentication is used instead.
   * @return a set of {@link ObjectName}s matching the filtered beans.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */
  @SuppressWarnings("unchecked")
  Set<ObjectName> queryNames(ObjectName name, MarshalledObject query,
			     Subject delegationSubject)
    throws IOException;

  /**
   * <p>
   * Handles {@link
   * MBeanServerConnection#removeNotificationListener(ObjectName,
   * ObjectName, NotificationFilter, Object)}. Both the filter and
   * the handback object are wrapped in a {@link MarshalledObject}
   * so that they are deserialised using the bean's classloader.
   * </p>
   * <p>
   * Removes the specified listener from the list of recipients
   * of notifications from the supplied bean.  Only the first instance with
   * the supplied filter and passback object is removed.
   * <code>null</code> is used as a valid value for these parameters,
   * rather than as a way to remove all registration instances for
   * the specified listener; for this behaviour instead, see
   * {@link #removeNotificationListener(ObjectName, NotificationListener)}.
   * </p>
   *
   * @param name the name of the management bean from which the
   *             listener should be removed.
   * @param listener the listener to remove.
   * @param filter a wrapper containing the filter of the listener
   *               to remove.
   * @param passback a wrapper containing the handback object of the
   *                 listener to remove.
   * @param delegationSubject a {@link javax.security.auth.Subject} instance
   *                          containing the delegation principles or
   *                          {@code null} if authentication is used.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with the bean.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #addNotificationListener(ObjectName, NotificationListener,
   *                               MarshalledObject, MarshalledObject, Subject)
   * @see NotificationEmitter#removeNotificationListener(NotificationListener,
   *                                                     NotificationFilter,
   *                                                     Object)
   */
  @SuppressWarnings("unchecked")
  void removeNotificationListener(ObjectName name,
				  ObjectName listener,
				  MarshalledObject filter,
				  MarshalledObject passback,
				  Subject delegationSubject)
    throws InstanceNotFoundException, ListenerNotFoundException,
	   IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#removeNotificationListener(ObjectName,
   * ObjectName)} by removing the specified listener from the list
   * of recipients of notifications from the supplied bean.  This
   * includes all combinations of filters and passback objects
   * registered for this listener.  For more specific removal of
   * listeners, see {@link #removeNotificationListener(ObjectName,
   * ObjectName,MarshalledObject,MarshalledObject,Subject)}
   *
   * @param name the name of the management bean from which the
   *             listener should be removed.
   * @param listener the name of the listener to remove.
   * @param delegationSubject a {@link javax.security.auth.Subject} instance
   *                          containing the delegation principles or
   *                          {@code null} if authentication is used.
   * @throws InstanceNotFoundException if a name doesn't match a registered
   *                                   bean.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with the bean.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #addNotificationListener(ObjectName, NotificationListener,
   *                               MarshalledObject, MarshalledObject, Subject)
   * @see NotificationBroadcaster#removeNotificationListener(NotificationListener)
   */
  void removeNotificationListener(ObjectName name, ObjectName listener,
				  Subject delegationSubject)
    throws InstanceNotFoundException, ListenerNotFoundException,
	   IOException;

  /**
   * Removes one or more {@link NotificationListener}s from the specified
   * management bean.  This method corresponds to
   * {@link #addNotificationListeners(ObjectName[], MarshalledObject[],
   * Subject)} and provides a different way of handling
   * MBeanServerConnection#removeNotificationListener(ObjectName,
   * ObjectName)} and
   * {@link MBeanServerConnection#removeNotificationListener(ObjectName,
   * ObjectName, NotificationFilter, Object)} by using the integer
   * identifiers provided by the
   * {@link #addNotificationListeners(ObjectName[], MarshalledObject[],
   * Subject)} method to select the listeners to remove.
   * 
   * @param name the name of the management bean from which the
   *             listeners should be removed.
   * @param listenerIds the identifiers of the listeners to remove.
   * @param delegationSubject a {@link javax.security.auth.Subject} instance
   *                          containing the delegation principles or
   *                          {@code null} if authentication is used.
   * @throws InstanceNotFoundException if a name doesn't match a registered
   *                                   bean.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with the bean.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @throws IllegalArgumentException if either <code>name</code>,
   *                                  <code>listenerIds</code> or an element
   *                                  of <code>listenerIds</code>
   *                                  is <code>null</code>.
   * @see #addNotificationListeners(ObjectName[], MarshalledObject[], Subject)
   */
  void removeNotificationListeners(ObjectName name, Integer[] listenerIds,
				   Subject delegationSubject)
    throws InstanceNotFoundException, ListenerNotFoundException,
	   IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#setAttribute(ObjectName, Attribute)}
   * by setting the value of the specified attribute of the supplied
   * management bean.  The attribute is wrapped in a
   * {@link MarshalledObject} so that it is deserialised using the
   * bean's classloader.
   *
   * @param name the name of the management bean.
   * @param attribute the attribute to set, encapsulated in a
   *                  {@link MarshalledObject}.
   * @param delegationSubject a {@link javax.security.auth.Subject} instance
   *                          containing the delegation principles or
   *                          {@code null} if authentication is used.
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
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #getAttribute(ObjectName, String, Subject)
   * @see javax.management.DynamicMBean#setAttribute(Attribute)
   */
  @SuppressWarnings("unchecked")
  void setAttribute(ObjectName name, MarshalledObject attribute,
		    Subject delegationSubject)
    throws InstanceNotFoundException, AttributeNotFoundException,
	   InvalidAttributeValueException, MBeanException,
	   ReflectionException, IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#setAttributes(ObjectName, AttributeList)}
   * by setting the value of each of the specified attributes
   * of the supplied management bean to that specified by
   * the {@link Attribute} object.  The returned list contains
   * the attributes that were set and their new values.
   * The attribute list is wrapped in a {@link MarshalledObject} so
   * that it is deserialised using the bean's classloader.
   *
   * @param name the name of the management bean.
   * @param attributes the attributes to set, encapsulated in a
   *                   {@link MarshalledObject}.
   * @param delegationSubject a {@link javax.security.auth.Subject} instance
   *                          containing the delegation principles or
   *                          {@code null} if authentication is used.
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
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   * @see #getAttributes(ObjectName, String[])
   * @see DynamicMBean#setAttributes(AttributeList)
   */
  @SuppressWarnings("unchecked")
  AttributeList setAttributes(ObjectName name, MarshalledObject attributes,
			      Subject delegationSubject)
    throws InstanceNotFoundException, ReflectionException,
	   IOException;

  /**
   * Handles {@link
   * MBeanServerConnection#unregisterMBean(ObjectName)} by unregistering
   * the specified management bean.  Following this operation,
   * the bean instance is no longer accessible from the server via this
   * name.  Prior to unregistering the bean, the
   * {@link MBeanRegistration#preDeregister()} method will be called if
   * the bean implements the {@link MBeanRegistration} interface.
   *
   * @param name the name of the management bean.
   * @param delegationSubject a {@link javax.security.auth.Subject} instance
   *                          containing the delegation principles or
   *                          {@code null} if authentication is used.
   * @throws InstanceNotFoundException if the bean can not be found.
   * @throws MBeanRegistrationException if an exception occurs in
   *                                    calling the preDeregister
   *                                    method.
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> bean name or a
   *                                    request being made to unregister the
   *                                    {@link MBeanServerDelegate} bean.
   * @throws SecurityException if the client or delegated subject (if any) does
   *                           not have permission to invoke this operation.
   * @throws IOException if an I/O error occurred in communicating with
   *                     the bean server.
   */ 
  void unregisterMBean(ObjectName name, Subject delegationSubject)
    throws InstanceNotFoundException, MBeanRegistrationException,
	   IOException;

}
