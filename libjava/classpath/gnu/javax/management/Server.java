/* Server.java -- A GNU Classpath management server.
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

package gnu.javax.management;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamClass;
import java.io.StreamCorruptedException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.ConcurrentHashMap;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.BadAttributeValueExpException;
import javax.management.BadBinaryOpValueExpException;
import javax.management.BadStringOperationException;
import javax.management.DynamicMBean;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.InvalidApplicationException;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MalformedObjectNameException;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanPermission;
import javax.management.MBeanRegistration;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MBeanServerDelegate;
import javax.management.MBeanServerNotification;
import javax.management.MBeanTrustPermission;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.OperationsException;
import javax.management.QueryExp;
import javax.management.ReflectionException;
import javax.management.RuntimeOperationsException;
import javax.management.StandardMBean;

import javax.management.loading.ClassLoaderRepository;

/**
 * This class provides an {@link javax.management.MBeanServer}
 * implementation for GNU Classpath.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class Server
  implements MBeanServer
{

  /**
   * The name of the delegate bean.
   */
  private static final ObjectName DELEGATE_NAME;

  /**
   * The registered beans, represented as a map of
   * {@link javax.management.ObjectName}s to
   * {@link gnu.javax.management.Server.ServerInfo}s.
   */
  private final ConcurrentHashMap<ObjectName,ServerInfo> beans =
    new ConcurrentHashMap<ObjectName,ServerInfo>();

  /**
   * The default domain.
   */
  private final String defaultDomain;

  /**
   * The outer server.
   */
  private final MBeanServer outer;

  /**
   * The class loader repository.
   */
  private ClassLoaderRepository repository;

  /**
   * The map of listener delegates to the true
   * listener.  We wrap this in an inner class
   * to delay initialisation until a listener
   * is actually added.
   */
  private static class LazyListenersHolder
  {
    private static final Map<NotificationListener,NotificationListener> listeners =
      new ConcurrentHashMap<NotificationListener,NotificationListener>();
  }

  /**
   * An MBean that emits notifications when an MBean is registered and
   * unregistered with this server.
   *
   */
  private final MBeanServerDelegate delegate;

  /**
   * Provides sequencing for notifications about registrations.
   */
  private static final AtomicLong sequenceNumber = new AtomicLong(1);

  /**
   * Initialise the delegate name.
   */
  static
  {
    try
      {
        DELEGATE_NAME =
          new ObjectName("JMImplementation:type=MBeanServerDelegate");
      }
    catch (MalformedObjectNameException e)
      {
        throw (Error)
          (new InternalError("Failed to construct " +
                             "the delegate's object name.").initCause(e));
      }
  }

  /**
   * Constructs a new management server using the specified
   * default domain, delegate bean and outer server.
   *
   * @param defaultDomain the default domain to use for beans constructed
   *               with no specified domain.
   * @param outer an {@link javax.management.MBeanServer} to pass
   *              to beans implementing the {@link MBeanRegistration}
   *              interface, or <code>null</code> if <code>this</code>
   *              should be passed.
   * @param delegate the delegate bean for this server.
   */
  public Server(String defaultDomain, MBeanServer outer,
                MBeanServerDelegate delegate)
  {
    this.defaultDomain = defaultDomain;
    this.outer = outer;
    this.delegate = delegate;
    try
      {
        registerMBean(delegate, DELEGATE_NAME);
      }
    catch (InstanceAlreadyExistsException e)
      {
        throw (Error)
          (new InternalError("The delegate bean is " +
                             "already registered.").initCause(e));
      }
    catch (MBeanRegistrationException e)
      {
        throw (Error)
          (new InternalError("The delegate bean's preRegister " +
                             "methods threw an exception.").initCause(e));
      }
    catch (NotCompliantMBeanException e)
      {
        throw (Error)
          (new InternalError("The delegate bean is " +
                             "not compliant.").initCause(e));
      }
  }

  /**
   * Checks for the necessary security privileges to perform an
   * operation.
   *
   * @param name the name of the bean being accessed.
   * @param member the name of the operation or attribute being
   *               accessed, or <code>null</code> if one is not
   *               involved.
   * @param action the action being performed.
   * @throws SecurityException if the action is denied.
   */
  private void checkSecurity(ObjectName name, String member,
                             String action)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      try
        {
          MBeanInfo info = null;
          if (name != null)
            {
              Object bean = getBean(name);
              Method method = bean.getClass().getMethod("getMBeanInfo");
              info = (MBeanInfo) method.invoke(bean);
            }
          sm.checkPermission(new MBeanPermission((info == null) ?
                                                 null : info.getClassName(),
                                                 member, name, action));
        }
      catch (InstanceNotFoundException e)
        {
          throw (Error)
            (new InternalError("Failed to get bean.").initCause(e));
        }
      catch (NoSuchMethodException e)
        {
          throw (Error)
            (new InternalError("Failed to get bean info.").initCause(e));
        }
      catch (IllegalAccessException e)
        {
          throw (Error)
            (new InternalError("Failed to get bean info.").initCause(e));
        }
      catch (IllegalArgumentException e)
        {
          throw (Error)
            (new InternalError("Failed to get bean info.").initCause(e));
        }
      catch (InvocationTargetException e)
        {
          throw (Error)
            (new InternalError("Failed to get bean info.").initCause(e));
        }
  }

  /**
   * Retrieves the specified bean.
   *
   * @param name the name of the bean.
   * @return the bean.
   * @throws InstanceNotFoundException if the name of the management bean
   *                                   could not be resolved.
   */
  private Object getBean(ObjectName name)
    throws InstanceNotFoundException
  {
    ServerInfo bean = beans.get(name);
    if (bean == null)
      throw new InstanceNotFoundException("The bean, " + name +
                                          ", was not found.");
    return bean.getObject();
  }

  /**
   * Registers the supplied listener with the specified management
   * bean.  Notifications emitted by the management bean are forwarded
   * to the listener via the server, which will convert an MBean
   * references in the source to a portable {@link ObjectName}
   * instance.  The notification is otherwise unchanged.
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
  public void addNotificationListener(ObjectName name, NotificationListener listener,
                                      NotificationFilter filter, Object passback)
    throws InstanceNotFoundException
  {
    Object bean = getBean(name);
    checkSecurity(name, null, "addNotificationListener");
    if (bean instanceof NotificationBroadcaster)
      {
        NotificationBroadcaster bbean = (NotificationBroadcaster) bean;
        NotificationListener indirection = new ServerNotificationListener(bean, name,
                                                                          listener);
        bbean.addNotificationListener(indirection, filter, passback);
        LazyListenersHolder.listeners.put(listener, indirection);
      }
  }

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
  public void addNotificationListener(ObjectName name, ObjectName listener,
                                      NotificationFilter filter, Object passback)
    throws InstanceNotFoundException
  {
    Object lbean = getBean(listener);
    if (!(lbean instanceof NotificationListener))
      {
        RuntimeException e =
          new IllegalArgumentException("The supplied listener name does not " +
                                       "correspond to a notification listener.");
        throw new RuntimeOperationsException(e);
      }
    addNotificationListener(name, ((NotificationListener) lbean), filter, passback);
  }

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
  public ObjectInstance createMBean(String className, ObjectName name)
    throws ReflectionException, InstanceAlreadyExistsException,
           MBeanRegistrationException, MBeanException,
           NotCompliantMBeanException
  {
    return createMBean(className, name, (Object[]) null, (String[]) null);
  }

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
  public ObjectInstance createMBean(String className, ObjectName name,
                                    Object[] params, String[] sig)
    throws ReflectionException, InstanceAlreadyExistsException,
           MBeanRegistrationException, MBeanException,
           NotCompliantMBeanException
  {
    return registerMBean(instantiate(className, params, sig), name);
  }

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
  public ObjectInstance createMBean(String className, ObjectName name,
                                    ObjectName loaderName)
    throws ReflectionException, InstanceAlreadyExistsException,
           MBeanRegistrationException, MBeanException,
           NotCompliantMBeanException, InstanceNotFoundException
  {
    return createMBean(className, name, loaderName, (Object[]) null,
                       (String[]) null);
  }

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
  public ObjectInstance createMBean(String className, ObjectName name,
                                    ObjectName loaderName, Object[] params,
                                    String[] sig)
    throws ReflectionException, InstanceAlreadyExistsException,
           MBeanRegistrationException, MBeanException,
           NotCompliantMBeanException, InstanceNotFoundException
  {
    return registerMBean(instantiate(className, loaderName, params, sig),
                         name);
  }

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
  public ObjectInputStream deserialize(ObjectName name, byte[] data)
    throws InstanceNotFoundException, OperationsException
  {
    try
      {
        return new ServerInputStream(new ByteArrayInputStream(data),
                                     getClassLoaderFor(name));
      }
    catch (IOException e)
      {
        throw new OperationsException("An I/O error occurred: " + e);
      }
  }

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
  public ObjectInputStream deserialize(String name, byte[] data)
    throws OperationsException, ReflectionException
  {
    try
      {
        Class<?> c = getClassLoaderRepository().loadClass(name);
        return new ServerInputStream(new ByteArrayInputStream(data),
                                           c.getClassLoader());
      }
    catch (IOException e)
      {
        throw new OperationsException("An I/O error occurred: " + e);
      }
    catch (ClassNotFoundException e)
      {
        throw new ReflectionException(e, "The class could not be found.");
      }
  }

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
  public ObjectInputStream deserialize(String name, ObjectName loader, byte[] data)
    throws InstanceNotFoundException, ReflectionException,
           OperationsException
  {
    try
      {
        Class<?> c = getClassLoader(loader).loadClass(name);
        return new ServerInputStream(new ByteArrayInputStream(data),
                                           c.getClassLoader());
      }
    catch (IOException e)
      {
        throw new OperationsException("An I/O error occurred: " + e);
      }
    catch (ClassNotFoundException e)
      {
        throw new ReflectionException(e, "The class could not be found.");
      }
  }

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
  public Object getAttribute(ObjectName bean, String name)
    throws MBeanException, AttributeNotFoundException,
           InstanceNotFoundException, ReflectionException
  {
    if (bean == null || name == null)
      {
        RuntimeException e =
          new IllegalArgumentException("One of the supplied arguments was null.");
        throw new RuntimeOperationsException(e);
      }
    Object abean = getBean(bean);
    checkSecurity(bean, name, "getAttribute");
    if (abean instanceof DynamicMBean)
      return ((DynamicMBean) abean).getAttribute(name);
    else
      try
        {
          return new StandardMBean(abean, null).getAttribute(name);
        }
      catch (NotCompliantMBeanException e)
        {
          throw (Error)
            (new InternalError("Failed to create dynamic bean.").initCause(e));
        }
  }


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
  public AttributeList getAttributes(ObjectName bean, String[] names)
    throws InstanceNotFoundException, ReflectionException
  {
    if (bean == null || names == null)
      {
        RuntimeException e =
          new IllegalArgumentException("One of the supplied arguments was null.");
        throw new RuntimeOperationsException(e);
      }
    Object abean = getBean(bean);
    checkSecurity(bean, null, "getAttribute");
    AttributeList list = new AttributeList(names.length);
    for (int a = 0; a < names.length; ++a)
      {
        if (names[a] == null)
          {
            RuntimeException e =
              new IllegalArgumentException("Argument " + a + " was null.");
            throw new RuntimeOperationsException(e);
          }
        checkSecurity(bean, names[a], "getAttribute");
        try
          {
            Object value;
            if (abean instanceof DynamicMBean)
              value = ((DynamicMBean) abean).getAttribute(names[a]);
            else
              try
                {
                  value = new StandardMBean(abean, null).getAttribute(names[a]);
                }
              catch (NotCompliantMBeanException e)
                {
                  throw (Error)
                    (new InternalError("Failed to create dynamic bean.").initCause(e));
                }
            list.add(new Attribute(names[a], value));
          }
        catch (AttributeNotFoundException e)
          {
            /* Ignored */
          }
        catch (MBeanException e)
          {
            /* Ignored */
          }
      }
    return list;
  }


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
  public ClassLoader getClassLoader(ObjectName name)
    throws InstanceNotFoundException
  {
    if (name == null)
      {
        checkSecurity(null, null, "getClassLoader");
        return getClass().getClassLoader();
      }
    Object bean = getBean(name);
    checkSecurity(name, null, "getClassLoader");
    return (ClassLoader) bean;
  }

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
  public ClassLoader getClassLoaderFor(ObjectName name)
    throws InstanceNotFoundException
  {
    Object bean = getBean(name);
    checkSecurity(name, null, "getClassLoaderFor");
    return bean.getClass().getClassLoader();
  }

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
  public ClassLoaderRepository getClassLoaderRepository()
  {
    return repository;
  }

  /**
   * Returns the default domain this server applies to beans that have
   * no specified domain.
   *
   * @return the default domain.
   */
  public String getDefaultDomain()
  {
    return defaultDomain;
  }

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
  public String[] getDomains()
  {
    checkSecurity(null, null, "getDomains");
    Set<String> domains = new HashSet<String>();
    Iterator<ObjectName> iterator = beans.keySet().iterator();
    while (iterator.hasNext())
      {
        String d = iterator.next().getDomain();
        try
          {
            checkSecurity(new ObjectName(d + ":x=x"), null, "getDomains");
            domains.add(d);
          }
        catch (MalformedObjectNameException e)
          {
            /* Ignored */
          }
      }
    return domains.toArray(new String[domains.size()]);
  }

  /**
   * Returns the number of management beans registered with this server.
   * This may be less than the real number if the caller's access is
   * restricted.
   *
   * @return the number of registered beans.
   */
  public Integer getMBeanCount()
  {
    return Integer.valueOf(beans.size());
  }

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
  public MBeanInfo getMBeanInfo(ObjectName name)
    throws InstanceNotFoundException, IntrospectionException,
           ReflectionException
  {
    Object bean = getBean(name);
    checkSecurity(name, null, "getMBeanInfo");
    try
      {
        Method method = bean.getClass().getMethod("getMBeanInfo");
        return (MBeanInfo) method.invoke(bean);
      }
    catch (NoSuchMethodException e)
      {
        try
          {
            return new StandardMBean(bean, null).getMBeanInfo();
          }
        catch (NotCompliantMBeanException ex)
          {
            throw new IntrospectionException("An error occurred in executing " +
                                             "getMBeanInfo on the bean: " + ex + ".");
          }
      }
    catch (IllegalAccessException e)
      {
        throw new ReflectionException(e, "Failed to call getMBeanInfo");
      }
    catch (IllegalArgumentException e)
      {
        throw new ReflectionException(e, "Failed to call getMBeanInfo");
      }
    catch (InvocationTargetException e)
      {
        throw new ReflectionException(e, "The method threw an exception");
      }
  }

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
  public ObjectInstance getObjectInstance(ObjectName name)
    throws InstanceNotFoundException
  {
    ServerInfo bean = beans.get(name);
    if (bean == null)
      throw new InstanceNotFoundException("The bean, " + name +
                                          ", was not found.");
    return bean.getInstance();
  }

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
  public Object instantiate(String name)
    throws ReflectionException, MBeanException
  {
    return instantiate(name, (Object[]) null, (String[]) null);
  }

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
  public Object instantiate(String name, Object[] params, String[] sig)
    throws ReflectionException, MBeanException
  {
    checkSecurity(null, null, "instantiate");
    if (name == null)
      {
        RuntimeException e =
          new IllegalArgumentException("The name was null.");
        throw new RuntimeOperationsException(e);
      }
    Class<?>[] sigTypes = new Class[sig.length];
    for (int a = 0; a < sigTypes.length; ++a)
      {
        try
          {
            sigTypes[a] = repository.loadClass(sig[a]);
          }
        catch (ClassNotFoundException e)
          {
            throw new ReflectionException(e, "The class, " + sigTypes[a] +
                                          ", in the method signature " +
                                          "could not be loaded.");
          }
      }
    try
      {
        Constructor<?> cons =
          repository.loadClass(name).getConstructor(sigTypes);
        return cons.newInstance(params);
      }
    catch (ClassNotFoundException e)
      {
        throw new ReflectionException(e, "The class, " + name +
                                      ", of the constructor " +
                                      "could not be loaded.");
      }
    catch (NoSuchMethodException e)
      {
        throw new ReflectionException(e, "The method, " + name +
                                      ", could not be found.");
      }
    catch (IllegalAccessException e)
      {
        throw new ReflectionException(e, "Failed to instantiate the object");
      }
    catch (InstantiationException e)
      {
        throw new ReflectionException(e, "Failed to instantiate the object");
      }
    catch (InvocationTargetException e)
      {
        throw new MBeanException((Exception) e.getCause(), "The constructor "
                                 + name + " threw an exception");
      }
  }

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
  public Object instantiate(String name, ObjectName loaderName)
    throws InstanceNotFoundException, ReflectionException,
           MBeanException
  {
    return instantiate(name, loaderName);
  }

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
  public Object instantiate(String name, ObjectName loaderName,
                            Object[] params, String[] sig)
    throws InstanceNotFoundException, ReflectionException,
           MBeanException
  {
    checkSecurity(null, null, "instantiate");
    if (name == null)
      {
        RuntimeException e =
          new IllegalArgumentException("The name was null.");
        throw new RuntimeOperationsException(e);
      }
    ClassLoader loader = getClassLoader(loaderName);
    Class<?>[] sigTypes = new Class[sig.length];
    for (int a = 0; a < sig.length; ++a)
      {
        try
          {
            sigTypes[a] = Class.forName(sig[a], true, loader);
          }
        catch (ClassNotFoundException e)
          {
            throw new ReflectionException(e, "The class, " + sig[a] +
                                          ", in the method signature " +
                                          "could not be loaded.");
          }
      }
    try
      {
        Constructor<?> cons =
          Class.forName(name, true, loader).getConstructor(sigTypes);
        return cons.newInstance(params);
      }
    catch (ClassNotFoundException e)
      {
        throw new ReflectionException(e, "The class, " + name +
                                      ", of the constructor " +
                                      "could not be loaded.");
      }
    catch (NoSuchMethodException e)
      {
        throw new ReflectionException(e, "The method, " + name +
                                      ", could not be found.");
      }
    catch (IllegalAccessException e)
      {
        throw new ReflectionException(e, "Failed to instantiate the object");
      }
    catch (InstantiationException e)
      {
        throw new ReflectionException(e, "Failed to instantiate the object");
      }
    catch (InvocationTargetException e)
      {
        throw new MBeanException((Exception) e.getCause(), "The constructor "
                                 + name + " threw an exception");
      }
  }

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
   * @throws RuntimeOperationsException if an {@link IllegalArgumentException}
   *                                    is thrown by the server due to a
   *                                    <code>null</code> name.
   * @throws ReflectionException if an exception is thrown in invoking the
   *                             method.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(className, name, bean,
   *                           "invoke")</code>}.
   * @see DynamicMBean#invoke(String, Object[], String[])
   */
  public Object invoke(ObjectName bean, String name, Object[] params, String[] sig)
    throws InstanceNotFoundException, MBeanException,
           ReflectionException
  {
    if (bean == null)
      {
        RuntimeException e =
          new IllegalArgumentException("The bean was null.");
        throw new RuntimeOperationsException(e);
      }
    Object abean = getBean(bean);
    checkSecurity(bean, name, "invoke");
    if (abean instanceof DynamicMBean)
      return ((DynamicMBean) abean).invoke(name, params, sig);
    else
      try
        {
          return new StandardMBean(abean, null).invoke(name, params, sig);
        }
      catch (NotCompliantMBeanException e)
        {
          throw (Error)
            (new InternalError("Failed to create dynamic bean.").initCause(e));
        }
  }

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
  public boolean isInstanceOf(ObjectName name, String className)
    throws InstanceNotFoundException
  {
    Object bean = getBean(name);
    checkSecurity(name, null, "isInstanceOf");
    MBeanInfo info;
    if (bean instanceof DynamicMBean)
      info = ((DynamicMBean) bean).getMBeanInfo();
    else
      try
        {
          info = new StandardMBean(bean, null).getMBeanInfo();
        }
      catch (NotCompliantMBeanException e)
        {
          throw (Error)
            (new InternalError("Failed to create dynamic bean.").initCause(e));
        }
    if (info.getClassName().equals(className))
      return true;
    Class<?> bclass = bean.getClass();
    try
      {
        Class<?> oclass = Class.forName(className);
        return (bclass.getClassLoader().equals(oclass.getClassLoader()) &&
                oclass.isAssignableFrom(bclass));
      }
    catch (ClassNotFoundException e)
      {
        return false;
      }
  }

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
  public boolean isRegistered(ObjectName name)
  {
    if (name == null)
      {
        RuntimeException e =
          new IllegalArgumentException("The name was null.");
        throw new RuntimeOperationsException(e);
      }
    return beans.containsKey(name);
  }

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
  public Set<ObjectInstance> queryMBeans(ObjectName name, QueryExp query)
  {
    checkSecurity(name, null, "queryMBeans");
    Set<ObjectInstance> results = new HashSet<ObjectInstance>();
    for (Map.Entry<ObjectName,ServerInfo> entry : beans.entrySet())
      {
        ObjectName nextName = entry.getKey();
        checkSecurity(name, nextName.toString(), "queryMBeans");
        try
          {
            if ((name == null || name.apply(nextName)) &&
                (query == null || query.apply(nextName)))
              results.add(entry.getValue().getInstance());
          }
        catch (BadStringOperationException e)
          {
            /* Ignored -- assume false result */
          }
        catch (BadBinaryOpValueExpException e)
          {
            /* Ignored -- assume false result */
          }
        catch (BadAttributeValueExpException e)
          {
            /* Ignored -- assume false result */
          }
        catch (InvalidApplicationException e)
          {
            /* Ignored -- assume false result */
          }
      }
    return results;
  }

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
  public Set<ObjectName> queryNames(ObjectName name, QueryExp query)
  {
    checkSecurity(name, null, "queryNames");
    Set<ObjectName> results = new HashSet<ObjectName>();
    for (ObjectName nextName : beans.keySet())
      {
        checkSecurity(name, nextName.toString(), "queryNames");
        try
          {
            if ((name == null || name.apply(nextName)) &&
                (query == null || query.apply(nextName)))
              results.add(nextName);
          }
        catch (BadStringOperationException e)
          {
            /* Ignored -- assume false result */
          }
        catch (BadBinaryOpValueExpException e)
          {
            /* Ignored -- assume false result */
          }
        catch (BadAttributeValueExpException e)
          {
            /* Ignored -- assume false result */
          }
        catch (InvalidApplicationException e)
          {
            /* Ignored -- assume false result */
          }
      }
    return results;
  }

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
   * @return an {@link ObjectInstance} containing the supplied
   *         {@link ObjectName} along with the name of the bean's class.
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
  public ObjectInstance registerMBean(Object obj, ObjectName name)
    throws InstanceAlreadyExistsException, MBeanRegistrationException,
           NotCompliantMBeanException
  {
    SecurityManager sm = System.getSecurityManager();
    Class<?> cl = obj.getClass();
    String className = cl.getName();
    if (sm != null)
      {
        sm.checkPermission(new MBeanPermission(className, null, name,
                                               "registerMBean"));
        if (!(cl.getProtectionDomain().implies(new MBeanTrustPermission("register"))))
          throw new SecurityException("The protection domain of the object's class" +
                                      "does not imply the trust permission," +
                                      "register");
      }
    if (obj == null)
      {
        RuntimeException e =
          new IllegalArgumentException("The object was null.");
        throw new RuntimeOperationsException(e);
      }
    MBeanRegistration register = null;
    if (obj instanceof MBeanRegistration)
      register = (MBeanRegistration) obj;
    if (name == null && register == null)
      {
        RuntimeException e =
          new IllegalArgumentException("The name was null and " +
                                       "the bean does not implement " +
                                       "MBeanRegistration.");
        throw new RuntimeOperationsException(e);
      }
    if (register != null)
      {
        try
          {
            name = register.preRegister(this, name);
            if (name == null)
              {
                RuntimeException e =
                  new NullPointerException("The name returned by " +
                                           "MBeanRegistration.preRegister() " +
                                           "was null");
                throw e;
              }
            if (sm != null)
              sm.checkPermission(new MBeanPermission(className, null, name,
                                                     "registerMBean"));
          }
        catch (SecurityException e)
          {
            register.postRegister(Boolean.FALSE);
            throw e;
          }
        catch (Exception e)
          {
            register.postRegister(Boolean.FALSE);
            throw new MBeanRegistrationException(e, "Pre-registration failed.");
          }
      }
    ObjectInstance obji = new ObjectInstance(name, className);
    if (beans.putIfAbsent(name, new ServerInfo(obji, obj)) != null)
      {
        if (register != null)
          register.postRegister(Boolean.FALSE);
        throw new InstanceAlreadyExistsException(name + "is already registered.");
      }
    if (register != null)
      register.postRegister(Boolean.TRUE);
    notify(name, MBeanServerNotification.REGISTRATION_NOTIFICATION);
    return obji;
  }

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
  public void removeNotificationListener(ObjectName name,
                                         NotificationListener listener)
    throws InstanceNotFoundException, ListenerNotFoundException
  {
    Object bean = getBean(name);
    checkSecurity(name, null, "removeNotificationListener");
    if (bean instanceof NotificationBroadcaster)
      {
        NotificationBroadcaster bbean = (NotificationBroadcaster) bean;
        bbean.removeNotificationListener(listener);
        LazyListenersHolder.listeners.remove(listener);
      }
  }

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
  public void removeNotificationListener(ObjectName name,
                                         NotificationListener listener,
                                         NotificationFilter filter,
                                         Object passback)
    throws InstanceNotFoundException, ListenerNotFoundException
  {
    Object bean = getBean(name);
    checkSecurity(name, null, "removeNotificationListener");
    if (bean instanceof NotificationEmitter)
      {
        NotificationEmitter bbean = (NotificationEmitter) bean;
        bbean.removeNotificationListener(listener, filter, passback);
        LazyListenersHolder.listeners.remove(listener);
      }
  }

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
  public void removeNotificationListener(ObjectName name, ObjectName listener)
    throws InstanceNotFoundException, ListenerNotFoundException
  {
    Object lbean = getBean(listener);
    if (!(lbean instanceof NotificationListener))
      {
        RuntimeException e =
          new IllegalArgumentException("The supplied listener name does not " +
                                       "correspond to a notification listener.");
        throw new RuntimeOperationsException(e);
      }
    removeNotificationListener(name, ((NotificationListener) lbean));
  }

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
  public void removeNotificationListener(ObjectName name,
                                  ObjectName listener,
                                  NotificationFilter filter,
                                  Object passback)
    throws InstanceNotFoundException, ListenerNotFoundException
  {
    Object lbean = getBean(listener);
    if (!(lbean instanceof NotificationListener))
      {
        RuntimeException e =
          new IllegalArgumentException("The supplied listener name does not " +
                                       "correspond to a notification listener.");
        throw new RuntimeOperationsException(e);
      }
    removeNotificationListener(name, ((NotificationListener) lbean), filter,
                               passback);
  }

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
  public void setAttribute(ObjectName name, Attribute attribute)
    throws InstanceNotFoundException, AttributeNotFoundException,
           InvalidAttributeValueException, MBeanException,
           ReflectionException
  {
    if (attribute == null || name == null)
      {
        RuntimeException e =
          new IllegalArgumentException("One of the supplied arguments was null.");
        throw new RuntimeOperationsException(e);
      }
    Object bean = getBean(name);
    checkSecurity(name, attribute.getName(), "setAttribute");
    if (bean instanceof DynamicMBean)
      ((DynamicMBean) bean).setAttribute(attribute);
    else
      try
        {
          new StandardMBean(bean, null).setAttribute(attribute);
        }
      catch (NotCompliantMBeanException e)
        {
          throw (Error)
            (new InternalError("Failed to create dynamic bean.").initCause(e));
        }
  }

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
  public AttributeList setAttributes(ObjectName name, AttributeList attributes)
    throws InstanceNotFoundException, ReflectionException
  {
    if (name == null || attributes == null)
      {
        RuntimeException e =
          new IllegalArgumentException("One of the supplied arguments was null.");
        throw new RuntimeOperationsException(e);
      }
    Object abean = getBean(name);
    checkSecurity(name, null, "setAttribute");
    AttributeList list = new AttributeList(attributes.size());
    Iterator<Object> it = attributes.iterator();
    while (it.hasNext())
      {
        try
          {
            Attribute attrib = (Attribute) it.next();
            if (attrib == null)
              {
                RuntimeException e =
                  new IllegalArgumentException("An attribute was null.");
                throw new RuntimeOperationsException(e);
              }
            checkSecurity(name, attrib.getName(), "setAttribute");
            if (abean instanceof DynamicMBean)
              ((DynamicMBean) abean).setAttribute(attrib);
            else
              try
                {
                  new StandardMBean(abean, null).setAttribute(attrib);
                }
              catch (NotCompliantMBeanException e)
                {
                  throw (Error)
                    (new InternalError("Failed to create dynamic bean.").initCause(e));
                }
            list.add(attrib);
          }
        catch (AttributeNotFoundException e)
          {
            /* Ignored */
          }
        catch (InvalidAttributeValueException e)
          {
            /* Ignored */
          }
        catch (MBeanException e)
          {
            /* Ignored */
          }
      }
    return list;
  }

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
  public void unregisterMBean(ObjectName name)
    throws InstanceNotFoundException, MBeanRegistrationException
  {
    if (name == null)
      {
        RuntimeException e =
          new IllegalArgumentException("The name was null.");
        throw new RuntimeOperationsException(e);
      }
    if (name.equals(DELEGATE_NAME))
      {
        RuntimeException e =
          new IllegalArgumentException("The delegate can not be unregistered.");
        throw new RuntimeOperationsException(e);
      }
    Object bean = getBean(name);
    checkSecurity(name, null, "unregisterMBean");
    MBeanRegistration register = null;
    if (bean instanceof MBeanRegistration)
      {
        register = (MBeanRegistration) bean;
        try
          {
            register.preDeregister();
          }
        catch (Exception e)
          {
            throw new MBeanRegistrationException(e, "Pre-deregistration failed.");
          }
      }
    beans.remove(name);
    notify(name, MBeanServerNotification.UNREGISTRATION_NOTIFICATION);
    if (register != null)
      register.postDeregister();
  }

  /**
   * Notifies the delegate of beans being registered
   * and unregistered.
   *
   * @param name the bean being registered.
   * @param type the type of notification;
   * {@code REGISTRATION_NOTIFICATION} or
   * {@code UNREGISTRATION_NOTIFICATION}.
   */
   private void notify(ObjectName name, String type)
   {
      delegate.sendNotification
        (new MBeanServerNotification
         (type, DELEGATE_NAME, sequenceNumber.getAndIncrement(), name));
   }

  /**
   * Input stream which deserializes using the given classloader.
   */
  private class ServerInputStream
    extends ObjectInputStream
  {

    private ClassLoader cl;

    public ServerInputStream(InputStream is, ClassLoader cl)
      throws IOException, StreamCorruptedException
    {
      super(is);
      this.cl = cl;
    }

    protected Class<?> resolveClass(ObjectStreamClass osc)
      throws ClassNotFoundException, IOException
    {
      try
        {
          return Class.forName(osc.getName(), true, cl);
        }
      catch (ClassNotFoundException e)
        {
          return super.resolveClass(osc);
        }
    }

  }

  /**
   * Holder for information on registered beans.
   */
  private class ServerInfo
  {
    private ObjectInstance instance;

    private Object object;

    public ServerInfo(ObjectInstance instance, Object object)
    {
      this.instance = instance;
      this.object = object;
    }

    public Object getObject()
    {
      return object;
    }

    public ObjectInstance getInstance()
    {
      return instance;
    }
  }

  /**
   * Notification listener which removes direct references
   * to beans.
   */
  private class ServerNotificationListener
    implements NotificationListener
  {

    /**
     * The bean from which notifications are emitted.
     */
    Object bean;

    /**
     * The {@link ObjectName} of the emitting bean.
     */
    ObjectName name;

    /**
     * The real {@link NotificationListener}.
     */
    NotificationListener listener;

    /**
     * Constructs a new {@link ServerNotificationListener} replacing
     * occurrences of <code>bean</code> with its name.
     *
     * @param bean the bean emitting notifications.
     * @param name the object name of the emitting bean.
     * @param listener the listener events eventually reach.
     */
    public ServerNotificationListener(Object bean, ObjectName name,
                                      NotificationListener listener)
    {
      this.bean = bean;
      this.name = name;
      this.listener = listener;
    }

    /**
     * Replace a direct reference to <code>bean</code> with its
     * object reference, if necessary, before calling the listener.
     *
     * @param notif the notification being emitted.
     * @param handback an object that will be returned to the notification
     *                 listener when an event occurs.
     */
    public void handleNotification(Notification notif, Object handback)
    {
      if (notif.getSource() == bean)
        notif.setSource(name);
      listener.handleNotification(notif, handback);
    }

  }

}
