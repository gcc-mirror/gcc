/* JMX.java -- Static methods pertaining to the management API.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

import java.lang.reflect.Proxy;

/**
 * Common static methods pertaining to the management
 * API.  There are no instances of this class.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public class JMX
{

  /**
   * The name of the defaultValue field.
   */
  public static final String DEFAULT_VALUE_FIELD = "defaultValue";

  /**
   * The name of the immutableInfo field.
   */
  public static final String  IMMUTABLE_INFO_FIELD = "immutableInfo";

  /**
   * The name of the interfaceClassName field.
   */
  public static final String  INTERFACE_CLASS_NAME_FIELD = "interfaceClassName";

  /**
   * The name of the legalValues field.
   */
  public static final String  LEGAL_VALUES_FIELD = "legalValues";

  /**
   * The name of the maxValue field.
   */
  public static final String  MAX_VALUE_FIELD = "maxValue";

  /**
   * The name of the minValue field.
   */
  public static final String  MIN_VALUE_FIELD = "minValue";

  /**
   * The name of the mxbean field.
   */
  public static final String  MXBEAN_FIELD = "mxbean";

  /**
   * The name of the openType field.
   */
  public static final String  OPEN_TYPE_FIELD = "openType";

  /**
   * The name of the originalType field.
   */
  public static final String  ORIGINAL_TYPE_FIELD = "originalType";

  /**
   * Prevent instance creation.
   */
  private JMX()
  {
  }

  /**
   * <p>
   * Returns true if the given class represents an {@link MXBean}
   * interface.  An interface is an {@link MXBean interface} if:
   * </p>
   * <ul>
   * <li>It is annotated with {@code @MXBean} or
   * {@code @MXBean(true)}</li>.
   * <li>Its name ends in {@code "MXBean"} and it does not
   * have an {@link MXBean} annotation.</li>
   * </ul>
   *
   * @param iface the interface class that is to be checked
   *              for {@link MXBean} status.
   * @return true if the interface represents an {@link MXBean}.
   * @throws NullPointerException if {@code iface} is {@code null}.
   */
  public static boolean isMXBeanInterface(Class<?> iface)
  {
    MXBean annotation = iface.getAnnotation(MXBean.class);
    if (annotation != null)
      return annotation.value();
    return iface.getName().endsWith("MXBean");
  }

  /**
   * <p>
   * Returns a proxy for a standard management bean, using
   * the specified connection to access the named implementation.
   * To create a proxy for the bean, {@code SomethingMBean}, a call to
   * {@code JMX.newMBeanProxy(server, name, SomethingMBean.class)}
   * is made, where {@code server} is a local or remote management
   * server, and {@code name} is the registered {@link ObjectName}
   * of the implementation of {@code SomethingMBean} to use.
   * </p>
   * <p>
   * The proxy redirects calls to the methods of the interface,
   * {@link SomethingMBean}, to the appropriate methods of the
   * management server.  If {@link SomethingMBean} is specified
   * as follows:
   * </p>
   * <pre>
   * public interface SomethingMBean
   * {
   *   String getName();
   *   void setName(String name);
   *   void doStuff();
   * }
   * </pre>
   * <p>
   * The proxy created above will provide these three methods
   * using an instance of {@link MBeanServerInvocationHandler}.
   * The two methods, {@code getName} and {@code setName} define
   * an attribute, {@code Name}, so a call to {@code getName()}
   * will return the value of {@code server.getAttribute(name,
   * "Name")}, while {@code setName(newName)} will result in a
   * call to {@code server.setAttribute(name, new Attribute("Name",
   * newName))}.  Finally, {@code doStuff()}, as an operation,
   * will cause the proxy to call {@link MBeanServer#invoke(ObjectName,
   * String, Object[], String[])} as
   * {@code server.invoke(name, "doStuff", null, null)}.
   * </p>
   * <p>
   * Calling this method is equivalent to calling
   * {@link #newMBeanProxy(MBeanServerConnection, ObjectName, Class,
   * boolean)}.
   * </p>
   *
   * @param conn the server connection over which to forward calls to
   *             the bean.
   * @param name the registered name of the bean to use to implement
   *             the given interface.
   * @param iface the interface to provide a proxy for.
   * @return a proxy implementing the specified interface using calls
   *         to the methods of the bean registered with the supplied
   *         server using the given name.
   * @see #newMBeanProxy(MBeanServerConnection, ObjectName, Class,
   *      boolean)
   */
  public static <T> T newMBeanProxy(MBeanServerConnection conn,
                                    ObjectName name, Class<T> iface)
  {
    return newMBeanProxy(conn, name, iface, false);
  }

  /**
   * Returns a proxy for a standard management bean, using
   * the specified connection to access the named implementation,
   * as with {@link #newMBeanProxy(MBeanServerConnection, ObjectName,
   * Class)}.  In addition, the proxy returned by this method will
   * also implement {@link NotificationEmitter} if {@code bcast} is
   * true, under the assumption that the implementation referenced by
   * {@code name} implements this interface.  Calls to the methods of
   * {@link NotificationEmitter} will be forwarded to the bean
   * implementation via the appropriate server methods.
   *
   * @param conn the server connection over which to forward calls to
   *             the bean.
   * @param name the registered name of the bean to use to implement
   *             the given interface.
   * @param iface the interface to provide a proxy for.
   * @param bcast true if the proxy should implement
   *              {@link NotificationEmitter}.
   * @return a proxy implementing the specified interface using calls
   *         to the methods of the bean registered with the supplied
   *         server using the given name.
   * @see #newMBeanProxy(MBeanServerConnection, ObjectName, Class)
   */
  public static <T> T newMBeanProxy(MBeanServerConnection conn,
                                    ObjectName name, Class<T> iface,
                                    boolean bcast)
  {
    return MBeanServerInvocationHandler.newProxyInstance(conn, name,
                                                         iface, bcast);
  }

  /**
   * <p>
   * Returns a proxy for a {@link MXBean}, using the specified
   * connection to access the named implementation.
   * To create a proxy for the bean, {@code SomethingMXBean}, a call to
   * {@code JMX.newMXBeanProxy(server, name, SomethingMXBean.class)}
   * is made, where {@code server} is a local or remote management
   * server, and {@code name} is the registered {@link ObjectName}
   * of the implementation of {@code SomethingMBean} to use.
   * </p>
   * <p>
   * The proxy redirects calls to the methods of the interface,
   * {@link SomethingMXBean}, to the appropriate methods of the
   * management server with appropriate conversion between
   * Java and open types, according to the MXBean rules.  If
   * {@link SomethingMXBean} is specified as follows:
   * </p>
   * <pre>
   * public interface SomethingMXBean
   * {
   *   String getName();
   *   void setName(String name);
   *   List<Double> getStatistics();
   *   void setStatistics(List<Double> statistics);
   *   List<Double> getNamedStatistics(String, Map<String,Integer>);
   * }
   * </pre>
   * <p>
   * The proxy created above will provide these five methods
   * using an instance of {@link MBeanServerInvocationHandler}.
   * The two methods, {@code getName} and {@code setName} define
   * an attribute, {@code Name}, so a call to {@code getName()}
   * will return the value of {@code server.getAttribute(name,
   * "Name")}, while {@code setName(newName)} will result in a
   * call to {@code server.setAttribute(name, new Attribute("Name",
   * newName))}.  As this uses a simple type, {@link String}, no
   * conversion is necessary.
   * </p>
   * <p>
   * The two methods, {@code getStatistics} and {@code setStatistics}
   * similarly define another attribute, {@code Statistics}.  Calling
   * {@code getStatistics()} will cause a call to the server to be
   * made as before, {@code server.getAttribute(name, "Statistics")}.
   * However, the type of the return value from this call will be
   * an array of {@link Double} objects, as per the {@link MXBean}
   * rules.  The proxy converts this back in to a {@link java.util.List}
   * of {@link Double} objects before returning it.
   * </p>
   * <p>
   * The same process is applied in reverse for
   * {@code setStatistics(newStats)}.  The list is converted into
   * an appropriate array before the call to
   * {@link MBeanServerConnection#setAttribute(ObjectName, Attribute)}
   * is made.  Finally, a call to {@code getNamedStatistics} will require
   * both a Java to open type conversion on the arguments, and then
   * an open type to Java conversion of the return value.  Thus, a proxy
   * enables an {@link MXBean} to be used in cases where the appropriate
   * Java types are available and the user wishes to access the bean
   * using the types directly defined in its interface, just as with
   * standard management beans.
   * </p>
   * <p>
   * Calling this method is equivalent to calling
   * {@link #newMXBeanProxy(MBeanServerConnection, ObjectName, Class,
   * boolean)}.
   * </p>
   *
   * @param conn the server connection over which to forward calls to
   *             the bean.
   * @param name the registered name of the bean to use to implement
   *             the given interface.
   * @param iface the interface to provide a proxy for.
   * @return a proxy implementing the specified interface using calls
   *         to the methods of the bean registered with the supplied
   *         server using the given name.
   * @see #newMXBeanProxy(MBeanServerConnection, ObjectName, Class,
   *      boolean)
   */
  public static <T> T newMXBeanProxy(MBeanServerConnection conn,
                                     ObjectName name, Class<T> iface)
  {
    return newMXBeanProxy(conn, name, iface, false);
  }

  /**
   * Returns a proxy for a {@link MXBean}, using
   * the specified connection to access the named implementation,
   * as with {@link #newMXBeanProxy(MBeanServerConnection, ObjectName,
   * Class)}.  In addition, the proxy returned by this method will
   * also implement {@link NotificationEmitter} if {@code bcast} is
   * true, under the assumption that the implementation referenced by
   * {@code name} implements this interface.  Calls to the methods of
   * {@link NotificationEmitter} will be forwarded to the bean
   * implementation via the appropriate server methods.
   *
   * @param conn the server connection over which to forward calls to
   *             the bean.
   * @param name the registered name of the bean to use to implement
   *             the given interface.
   * @param iface the interface to provide a proxy for.
   * @param bcast true if the proxy should implement
   *              {@link NotificationEmitter}.
   * @return a proxy implementing the specified interface using calls
   *         to the methods of the bean registered with the supplied
   *         server using the given name.
   * @see #newMXBeanProxy(MBeanServerConnection, ObjectName, Class)
   */
  // Suppress warnings as we know an instance of T will be returned.
  @SuppressWarnings("unchecked")
  public static <T> T newMXBeanProxy(MBeanServerConnection conn,
                                    ObjectName name, Class<T> iface,
                                    boolean bcast)
  {
    if (bcast)
      return (T) Proxy.newProxyInstance(iface.getClassLoader(),
                                        new Class[] { iface,
                                                      NotificationEmitter.class },
                                        new MBeanServerInvocationHandler(conn,name,true));
    else
      return (T) Proxy.newProxyInstance(iface.getClassLoader(),
                                        new Class[] { iface },
                                        new MBeanServerInvocationHandler(conn,name,true));
  }

}
