/* ManagementFactory.java - Factory for obtaining system beans.
   Copyright (C) 2006 Free Software Foundation

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

package java.lang.management;

import gnu.classpath.SystemProperties;

import gnu.java.lang.management.ClassLoadingMXBeanImpl;
import gnu.java.lang.management.CompilationMXBeanImpl;
import gnu.java.lang.management.GarbageCollectorMXBeanImpl;
import gnu.java.lang.management.OperatingSystemMXBeanImpl;
import gnu.java.lang.management.MemoryMXBeanImpl;
import gnu.java.lang.management.MemoryManagerMXBeanImpl;
import gnu.java.lang.management.MemoryPoolMXBeanImpl;
import gnu.java.lang.management.RuntimeMXBeanImpl;
import gnu.java.lang.management.ThreadMXBeanImpl;

import java.io.IOException;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import java.util.logging.LogManager;

import javax.management.Attribute;
import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectName;

import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularData;

/**
 * <p>
 * Provides access to the system's management beans via a series
 * of static methods.
 * </p>
 * <p>
 * An instance of a system management bean can be obtained by
 * using one of the following methods:
 * </p>
 * <ol>
 * <li>Calling the appropriate static method of this factory.
 * </li>
 * <li>Using the platform {@link javax.management.MBeanServer}
 * to access the beans locally, or an
 * {@link javax.management.MBeanServerConnection} for remote
 * access.  The attributes and operations use the limited
 * range of data types specified below.</li>
 * </ol>
 * <h2>Open Data Types</h2>
 * <p>
 * The data types used by the management beans are restricted
 * to <emph>open</emph> data types to aid interoperability.  This
 * allows the beans to be accessed remotely, including from non-Java
 * clients.  Below is a table which lists the types used by the beans
 * on the left, and the types they are converted to when returned via
 * a bean server on the right.  Type information is provided for each
 * bean by obtaining its instance of {@link javax.management.MBeanInfo}.
 * </p>
 * <table>
 * <th><td>Data Type Used</td><td>Data Type Returned</td></th>
 * <tr>
 * <td>Primitive types (<code>int</code>, <code>char</code>, etc.)</td>
 * <td>Same</td>
 * </tr><tr>
 * <td>Wrapper classes ({@link{java.lang.Integer},
 * @link{java.lang.Character}, etc.)</td>
 * <td>Same</td>
 * </tr><tr>
 * <td>An {@link java.lang.Enum}</td>
 * <td>The <code>name</code> of the enumeration constant</td>
 * </tr><tr>
 * <td>An array of type <code>E</code></td>
 * <td>An array of the same dimensions with this mapping applied
 * to <code>E</code>.</td>
 * </tr><tr>
 * <td>A class with `getter' methods and a
 * <code>from({@link javax.management.openmbean.CompositeData})</code>
 * method.</td>
 * <td>The equivalent {@link javax.management.openmbean.CompositeData}
 * instance, specified by the <code>from</code> method.</td>
 * </tr><tr>
 * <td>A map with keys of type <code>K</code> and values of
 * type <code>V</code>.</td>
 * <td>A {@link javax.management.openmbean.TabularData} instance,
 * with the row type containing two items, <code>"key"</code> and
 * <code>"value"</code> with the types <code>K</code> and <code>V</code>
 * respectively (with translation applied).</td>
 * </tr><tr>
 * <td>A list of type <code>E</code>.</td>
 * <td>An array with this mapping applied to <code>E</code>.</td>
 * </tr></table>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ManagementFactory
{

  /**
   * The object name for the class loading bean.
   */
  public static final String CLASS_LOADING_MXBEAN_NAME =
    "java.lang:type=ClassLoading";

  /**
   * The object name for the compilation bean.
   */
  public static final String COMPILATION_MXBEAN_NAME =
    "java.lang:type=Compilation";

  /**
   * The domain for the garbage collecting beans.
   */
  public static final String GARBAGE_COLLECTOR_MXBEAN_DOMAIN_TYPE =
    "java.lang:type=GarbageCollector";

  /**
   * The domain for the memory manager beans.
   */
  public static final String MEMORY_MANAGER_MXBEAN_DOMAIN_TYPE =
    "java.lang:type=MemoryManager";

  /**
   * The object name for the memory bean.
   */
  public static final String MEMORY_MXBEAN_NAME =
    "java.lang:type=Memory";

  /**
   * The domain for the memory pool beans.
   */
  public static final String MEMORY_POOL_MXBEAN_DOMAIN_TYPE =
    "java.lang:type=MemoryPool";

  /**
   * The object name for the operating system bean.
   */
  public static final String OPERATING_SYSTEM_MXBEAN_NAME =
    "java.lang:type=OperatingSystem";

  /**
   * The object name for the runtime bean.
   */
  public static final String RUNTIME_MXBEAN_NAME =
    "java.lang:type=Runtime";

  /**
   * The object name for the threading bean.
   */
  public static final String THREAD_MXBEAN_NAME =
    "java.lang:type=Threading";

  /**
   * The operating system management bean.
   */
  private static OperatingSystemMXBean osBean;

  /**
   * The runtime management bean.
   */
  private static RuntimeMXBean runtimeBean;

  /**
   * The class loading management bean.
   */
  private static ClassLoadingMXBean classLoadingBean;

  /**
   * The thread bean.
   */
  private static ThreadMXBean threadBean;

  /**
   * The memory bean.
   */
  private static MemoryMXBean memoryBean;

  /**
   * The compilation bean (may remain null).
   */
  private static CompilationMXBean compilationBean;

  /**
   * The platform server.
   */
  private static MBeanServer platformServer;

  /**
   * Private constructor to prevent instance creation.
   */
  private ManagementFactory() {}

  /**
   * Returns the operating system management bean for the
   * operating system on which the virtual machine is running.
   *
   * @return an instance of {@link OperatingSystemMXBean} for
   *         the underlying operating system.
   */
  public static OperatingSystemMXBean getOperatingSystemMXBean()
  {
    if (osBean == null)
      try
        {
          osBean = new OperatingSystemMXBeanImpl();
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "operating system bean is not a " +
                                  "compliant management bean.");
        }
    return osBean;
  }

  /**
   * Returns the runtime management bean for the
   * running virtual machine.
   *
   * @return an instance of {@link RuntimeMXBean} for
   *         this virtual machine.
   */
  public static RuntimeMXBean getRuntimeMXBean()
  {
    if (runtimeBean == null)
      try
        {
          runtimeBean = new RuntimeMXBeanImpl();
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "runtime bean is not a compliant " +
                                  "management bean.");
        }
    return runtimeBean;
  }

  /**
   * Returns the class loading management bean for the
   * running virtual machine.
   *
   * @return an instance of {@link ClassLoadingMXBean} for
   *         this virtual machine.
   */
  public static ClassLoadingMXBean getClassLoadingMXBean()
  {
    if (classLoadingBean == null)
      try
        {
          classLoadingBean = new ClassLoadingMXBeanImpl();
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "class loading bean is not a " +
                                  "compliant management bean.");
        }
    return classLoadingBean;
  }

  /**
   * Returns the thread management bean for the running
   * virtual machine.
   *
   * @return an instance of {@link ThreadMXBean} for
   *         this virtual machine.
   */
  public static ThreadMXBean getThreadMXBean()
  {
    if (threadBean == null)
      try
        {
          threadBean = new ThreadMXBeanImpl();
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "thread bean is not a compliant " +
                                  "management bean.");
        }
    return threadBean;
  }

  /**
   * Returns the memory management bean for the running
   * virtual machine.
   *
   * @return an instance of {@link MemoryMXBean} for
   *         this virtual machine.
   */
  public static MemoryMXBean getMemoryMXBean()
  {
    if (memoryBean == null)
      try
        {
          memoryBean = new MemoryMXBeanImpl();
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "memory bean is not a compliant " +
                                  "management bean.");
        }
    return memoryBean;
  }

  /**
   * Returns the compilation bean for the running
   * virtual machine, if supported.  Otherwise,
   * it returns <code>null</code>.
   *
   * @return an instance of {@link CompilationMXBean} for
   *         this virtual machine, or <code>null</code>
   *         if the virtual machine doesn't include
   *         a Just-In-Time (JIT) compiler.
   */
  public static CompilationMXBean getCompilationMXBean()
  {
    if (compilationBean == null &&
        SystemProperties.getProperty("gnu.java.compiler.name") != null)
      try
        {
          compilationBean = new CompilationMXBeanImpl();
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "compilation bean is not a compliant " +
                                  "management bean.");
        }
    return compilationBean;
  }

  /**
   * Returns the memory pool beans for the running
   * virtual machine.  These may change during the course
   * of execution.
   *
   * @return a list of memory pool beans, one for each pool.
   */
  public static List<MemoryPoolMXBean> getMemoryPoolMXBeans()
  {
    List<MemoryPoolMXBean> poolBeans =
      new ArrayList<MemoryPoolMXBean>();
    String[] names = VMManagementFactory.getMemoryPoolNames();
    for (int a = 0; a < names.length; ++a)
      try
        {
          poolBeans.add(new MemoryPoolMXBeanImpl(names[a]));
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "memory pool bean, " + a + ", is " +
                                  "not a compliant management bean.");
        }
    return poolBeans;
  }

  /**
   * Returns the memory manager beans for the running
   * virtual machine.  These may change during the course
   * of execution.
   *
   * @return a list of memory manager beans, one for each manager.
   */
  public static List<MemoryManagerMXBean> getMemoryManagerMXBeans()
  {
    List<MemoryManagerMXBean> managerBeans =
      new ArrayList<MemoryManagerMXBean>();
    String[] names = VMManagementFactory.getMemoryManagerNames();
    for (int a = 0; a < names.length; ++a)
      try
        {
          managerBeans.add(new MemoryManagerMXBeanImpl(names[a]));
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "memory manager bean, " + a + ", is " +
                                  "not a compliant management bean.");
        }
    managerBeans.addAll(getGarbageCollectorMXBeans());
    return managerBeans;
  }

  /**
   * Returns the garbage collector beans for the running
   * virtual machine.  These may change during the course
   * of execution.
   *
   * @return a list of garbage collector beans, one for each pool.
   */
  public static List<GarbageCollectorMXBean> getGarbageCollectorMXBeans()
  {
    List<GarbageCollectorMXBean> gcBeans =
      new ArrayList<GarbageCollectorMXBean>();
    String[] names = VMManagementFactory.getGarbageCollectorNames();
    for (int a = 0; a < names.length; ++a)
      try
        {
          gcBeans.add(new GarbageCollectorMXBeanImpl(names[a]));
        }
      catch (NotCompliantMBeanException e)
        {
          throw new InternalError("The GNU implementation of the " +
                                  "garbage collector bean, " + a +
                                  ", is not a compliant management " +
                                  "bean.");
        }
    return gcBeans;
  }

  /**
   * <p>
   * Returns the platform {@link javax.management.MBeanServer}.  On the
   * first call to this method, a server instance is retrieved from
   * the {@link javax.management.MBeanServerFactory} and each of the
   * beans are registered with it.  Subsequent calls return the existing
   * instance.  If the property <code>javax.management.builder.initial</code>
   * is set, its value will be used as the name of the class which is used
   * to provide the server instance.
   * </p>
   * <p>
   * It is recommended that the platform server is used for other beans as
   * well, in order to simplify their discovery and publication.  Name conflicts
   * should be avoided.
   * </p>
   *
   * @return the platform {@link javax.management.MBeanServer}
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanServerPermission(String)}("createMBeanServer")
   * @see javax.management.MBeanServerFactory
   * @see javax.management.MBeanServerFactory#createMBeanServer()
   */
  public static MBeanServer getPlatformMBeanServer()
  {
    if (platformServer == null)
      {
        platformServer = MBeanServerFactory.createMBeanServer();
        try
          {
            platformServer.registerMBean(getOperatingSystemMXBean(),
                                         new ObjectName(OPERATING_SYSTEM_MXBEAN_NAME));
            platformServer.registerMBean(getRuntimeMXBean(),
                                         new ObjectName(RUNTIME_MXBEAN_NAME));
            platformServer.registerMBean(getClassLoadingMXBean(),
                                         new ObjectName(CLASS_LOADING_MXBEAN_NAME));
            platformServer.registerMBean(getThreadMXBean(),
                                         new ObjectName(THREAD_MXBEAN_NAME));
            platformServer.registerMBean(getMemoryMXBean(),
                                         new ObjectName(MEMORY_MXBEAN_NAME));
            CompilationMXBean compBean = getCompilationMXBean();
            if (compBean != null)
              platformServer.registerMBean(compBean,
                                           new ObjectName(COMPILATION_MXBEAN_NAME));
            Iterator beans = getMemoryPoolMXBeans().iterator();
            while (beans.hasNext())
              {
                MemoryPoolMXBean bean = (MemoryPoolMXBean) beans.next();
                platformServer.registerMBean(bean,
                                             new ObjectName(MEMORY_POOL_MXBEAN_DOMAIN_TYPE +
                                                            ",name=" +
                                                            bean.getName()));
              }
            beans = getMemoryManagerMXBeans().iterator();
            while (beans.hasNext())
              {
                MemoryManagerMXBean bean = (MemoryManagerMXBean) beans.next();
                platformServer.registerMBean(bean,
                                             new ObjectName(MEMORY_MANAGER_MXBEAN_DOMAIN_TYPE +
                                                            ",name=" +
                                                            bean.getName()));
              }
            beans = getGarbageCollectorMXBeans().iterator();
            while (beans.hasNext())
              {
                GarbageCollectorMXBean bean = (GarbageCollectorMXBean) beans.next();
                platformServer.registerMBean(bean,
                                             new ObjectName(GARBAGE_COLLECTOR_MXBEAN_DOMAIN_TYPE +
                                                            ",name=" +
                                                            bean.getName()));
              }
            platformServer.registerMBean(LogManager.getLoggingMXBean(),
                                         new ObjectName(LogManager.LOGGING_MXBEAN_NAME));
          }
        catch (InstanceAlreadyExistsException e)
          {
            throw (Error)
              (new InternalError("One of the management beans is " +
                                 "already registered.").initCause(e));
          }
        catch (MBeanRegistrationException e)
          {
            throw (Error)
              (new InternalError("One of the management beans' preRegister " +
                                 "methods threw an exception.").initCause(e));
          }
        catch (NotCompliantMBeanException e)
          {
            throw (Error)
              (new InternalError("One of the management beans is " +
                                 "not compliant.").initCause(e));
          }
        catch (MalformedObjectNameException e)
          {
            throw (Error)
              (new InternalError("The object name of a management bean is " +
                                 "not compliant.").initCause(e));
          }
      }
    return platformServer;
  }

  /**
   * <p>
   * Returns a proxy for the specified platform bean.  A proxy object is created
   * using <code>Proxy.newProxyInstance(mxbeanInterface.getClassLoader(),
   * new Class[] { mxbeanInterface }, handler)</code>.  The
   * {@link javax.management.NotificationEmitter} class is also added to the
   * array if the bean provides notifications.  <code>handler</code> refers
   * to the invocation handler which forwards calls to the connection, and
   * also provides translation between the Java data types used in the
   * bean interfaces and the open data types, as specified in the description
   * of this class.  It is this translation that makes the
   * usual {@link javax.management.MBeanServerInvocationHandler} inappropriate
   * for providing such a proxy.
   * </p>
   * <p>
   * <strong>Note</strong>: use of the proxy may result in
   * {@link java.io.IOException}s from the underlying {@link MBeanServerConnection}
   * and a {@link java.io.InvalidObjectException} if enum constants
   * used on the client and the server don't match.
   * </p>
   *
   * @param connection the server connection to use to access the bean.
   * @param mxbeanName the {@link javax.management.ObjectName} of the
   *                   bean to provide a proxy for.
   * @param mxbeanInterface the interface for the bean being proxied.
   * @return a proxy for the specified bean.
   * @throws IllegalArgumentException if <code>mxbeanName</code> is not a valid
   *                                  {@link javax.management.ObjectName},
   *                                  the interface and name do not match the
   *                                  same bean, the name does not refer to a
   *                                  platform bean or the bean is not registered
   *                                  with the server accessed by <code>connection</code>.
   * @throws IOException if the connection throws one.
   */
  public static <T> T newPlatformMXBeanProxy(MBeanServerConnection connection,
                                             String mxbeanName,
                                             Class<T> mxbeanInterface)
    throws IOException
  {
    if (!(mxbeanName.equals(CLASS_LOADING_MXBEAN_NAME) ||
          mxbeanName.equals(COMPILATION_MXBEAN_NAME) ||
          mxbeanName.startsWith(GARBAGE_COLLECTOR_MXBEAN_DOMAIN_TYPE) ||
          mxbeanName.startsWith(MEMORY_MANAGER_MXBEAN_DOMAIN_TYPE) ||
          mxbeanName.equals(MEMORY_MXBEAN_NAME) ||
          mxbeanName.startsWith(MEMORY_POOL_MXBEAN_DOMAIN_TYPE) ||
          mxbeanName.equals(OPERATING_SYSTEM_MXBEAN_NAME) ||
          mxbeanName.equals(RUNTIME_MXBEAN_NAME) ||
          mxbeanName.equals(THREAD_MXBEAN_NAME)))
      {
        throw new IllegalArgumentException("The named bean, " + mxbeanName +
                                           ", is not a platform name.");
      }
    if ((mxbeanName.equals(CLASS_LOADING_MXBEAN_NAME) &&
         mxbeanInterface != ClassLoadingMXBean.class) ||
        (mxbeanName.equals(COMPILATION_MXBEAN_NAME) &&
         mxbeanInterface != CompilationMXBean.class) ||
        (mxbeanName.startsWith(GARBAGE_COLLECTOR_MXBEAN_DOMAIN_TYPE) &&
         mxbeanInterface != GarbageCollectorMXBean.class) ||
        (mxbeanName.startsWith(MEMORY_MANAGER_MXBEAN_DOMAIN_TYPE) &&
         mxbeanInterface != MemoryManagerMXBean.class) ||
        (mxbeanName.equals(MEMORY_MXBEAN_NAME) &&
         mxbeanInterface != MemoryMXBean.class) ||
        (mxbeanName.startsWith(MEMORY_POOL_MXBEAN_DOMAIN_TYPE) &&
         mxbeanInterface != MemoryPoolMXBean.class) ||
        (mxbeanName.equals(OPERATING_SYSTEM_MXBEAN_NAME) &&
         mxbeanInterface != OperatingSystemMXBean.class) ||
        (mxbeanName.equals(RUNTIME_MXBEAN_NAME) &&
         mxbeanInterface != RuntimeMXBean.class) ||
        (mxbeanName.equals(THREAD_MXBEAN_NAME) &&
         mxbeanInterface != ThreadMXBean.class))
      throw new IllegalArgumentException("The interface, " + mxbeanInterface +
                                         ", does not match the bean, " + mxbeanName);
    ObjectName bean;
    try
      {
        bean = new ObjectName(mxbeanName);
      }
    catch (MalformedObjectNameException e)
      {
        throw new IllegalArgumentException("The named bean is invalid.");
      }
    if (!(connection.isRegistered(bean)))
      throw new IllegalArgumentException("The bean is not registered on this connection.");
    Class[] interfaces;
    if (mxbeanName.equals(MEMORY_MXBEAN_NAME))
      interfaces = new Class[] { mxbeanInterface, NotificationEmitter.class };
    else
      interfaces = new Class[] { mxbeanInterface };
    return (T) Proxy.newProxyInstance(mxbeanInterface.getClassLoader(),
                                      interfaces,
                                      new ManagementInvocationHandler(connection, bean));
  }

  /**
   * This invocation handler provides method calls for a platform bean
   * by forwarding them to a {@link MBeanServerConnection}.  Translation from
   * Java data types to open data types is performed as specified above.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static class ManagementInvocationHandler
    implements InvocationHandler
  {

    /**
     * The encapsulated connection.
     */
    private MBeanServerConnection conn;

    /**
     * The bean being proxied.
     */
    private ObjectName bean;

    /**
     * Constructs a new {@link InvocationHandler} which proxies
     * for the specified bean using the supplied connection.
     *
     * @param conn the connection on which to forward method calls.
     * @param bean the bean to proxy.
     */
    public ManagementInvocationHandler(MBeanServerConnection conn,
                                       ObjectName bean)
      throws IOException
    {
      this.conn = conn;
      this.bean = bean;
    }

    /**
     * Called by the proxy class whenever a method is called.  The method
     * is emulated by retrieving an attribute from, setting an attribute on
     * or invoking a method on the server connection as required.  Translation
     * between the Java data types supplied as arguments to the open types used
     * by the bean is provided, as well as translation of the return value back
     * in to the appropriate Java type.
     *
     * @param proxy the proxy on which the method was called.
     * @param method the method which was called.
     * @param args the arguments supplied to the method.
     * @return the return value from the method.
     * @throws Throwable if an exception is thrown in performing the
     *                   method emulation.
     */
    public Object invoke(Object proxy, Method method, Object[] args)
      throws Throwable
    {
      String name = method.getName();
      if (name.equals("toString"))
        return "Proxy for " + bean + " using " + conn;
      if (name.equals("addNotificationListener"))
        {
          conn.addNotificationListener(bean,
                                       (NotificationListener) args[0],
                                       (NotificationFilter) args[1],
                                       args[2]);
          return null;
        }
      if (name.equals("getNotificationInfo"))
        return conn.getMBeanInfo(bean).getNotifications();
      if (name.equals("removeNotificationListener"))
        {
          if (args.length == 1)
            conn.removeNotificationListener(bean,
                                            (NotificationListener)
                                            args[0]);
          else
            conn.removeNotificationListener(bean,
                                            (NotificationListener)
                                            args[0],
                                            (NotificationFilter)
                                            args[1], args[2]);
          return null;
        }
      String attrib = null;
      if (name.startsWith("get"))
        attrib = name.substring(3);
      else if (name.startsWith("is"))
        attrib = name.substring(2);
      if (attrib != null)
        return translate(conn.getAttribute(bean, attrib), method);
      else if (name.startsWith("set"))
        {
          conn.setAttribute(bean, new Attribute(name.substring(3),
                                                args[0]));
          return null;
        }
      else
        return translate(conn.invoke(bean, name, args, null), method);
    }

    /**
     * Translates the returned open data type to the value
     * required by the interface.
     *
     * @param otype the open type returned by the method call.
     * @param method the method that was called.
     * @return the equivalent return type required by the interface.
     * @throws Throwable if an exception is thrown in performing the
     *                   conversion.
     */
    private final Object translate(Object otype, Method method)
      throws Throwable
    {
      Class<?> returnType = method.getReturnType();
      if (returnType.isEnum())
        {
          String ename = (String) otype;
          Enum[] constants = (Enum[]) returnType.getEnumConstants();
          for (Enum c : constants)
            if (c.name().equals(ename))
              return c;
        }
      if (List.class.isAssignableFrom(returnType))
        {
          Object[] elems = (Object[]) otype;
          List l = new ArrayList(elems.length);
          for (Object elem : elems)
            l.add(elem);
          return l;
        }
      if (Map.class.isAssignableFrom(returnType))
        {
          TabularData data = (TabularData) otype;
          Map m = new HashMap(data.size());
          for (Object val : data.values())
            {
              CompositeData vals = (CompositeData) val;
              m.put(vals.get("key"), vals.get("value"));
            }
          return m;
        }
      try
        {
          Method m = returnType.getMethod("from",
                                          new Class[]
            { CompositeData.class });
          return m.invoke(null, (CompositeData) otype);
        }
      catch (NoSuchMethodException e)
        {
          /* Ignored; we expect this if this
             isn't a from(CompositeData) class */
        }
      return otype;
    }

  }
}
