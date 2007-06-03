/* MBeanServerInvocationHandler.java -- Provides a proxy for a bean.
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

import gnu.javax.management.Translator;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * <p>
 * Provides a proxy for a management bean.  The methods
 * of the given interface are fulfilled by redirecting the
 * calls over an {@link MBeanServerConnection} to the bean
 * specified by the supplied {@link ObjectName}.
 * </p>
 * <p>
 * The {@link java.lang.reflect.InvocationHandler} also makes
 * provision for {@link MXBean}s by providing type conversion
 * according to the rules defined for these beans.  The input
 * parameters are converted to their equivalent open type before
 * calling the method, and then the return value is converted
 * back from its open type to the appropriate Java type.  For
 * example, a method that takes an {@link Enum} as input and
 * returns a {@link java.util.List} will have the input value
 * converted from an {@link Enum} to a {@link String}, while
 * the return value will be converted from its return type
 * (an appropriately typed array) to a {@link java.util.List}.
 * </p>
 * <p>
 * The proxy has special cases for the {@link Object#equals(Object)},
 * {@link Object#hashCode()} and {@link Object#toString()} methods.
 * Unless they are specified explictly by the interface, the
 * following behaviour is provided for these methods by the proxy:
 * </p>
 * <ul>
 * <li><code>equals(Object)</code> returns true if the other object
 * is an {@link MBeanServerInvocationHandler} with the same
 * {@link MBeanServerConnection} and {@link ObjectName}.  If an
 * interface class was specified on construction for one of the
 * proxies, then the same class must have also been specified
 * for the other.</li>
 * <li><code>hashCode()</code> returns the same value for
 * equivalent proxies.</li>
 * <li><code>toString()</code> returns a textual representation
 * of the proxy.</li>
 * </ul>
 * 
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanServerInvocationHandler
  implements InvocationHandler
{

  /**
   * The connection used to make the calls.
   */
  private MBeanServerConnection conn;

  /**
   * The name of the bean to perform operations on.
   */
  private ObjectName name;

  /**
   * True if this proxy is for an {@link MXBean}.
   */
  private boolean mxBean;

  /**
   * The interface class associated with the bean.
   */
  private Class<?> iface;

  /**
   * Constructs a new {@link MBeanServerInvocationHandler}
   * which forwards methods to the supplied bean via the
   * given {@link MBeanServerConnection}.  This constructor
   * is used in preference to
   * {@link JMX#newMBeanProxy(MBeanServerConnection, ObjectName,
   * Class<T>)} if you wish to make your own call to
   * {@link java.lang.reflect.Proxy#newInstance(ClassLoader,
   * Class[], java.lang.reflect.InvocationHandler)} with
   * a different {@link ClassLoader}.  Calling this constructor
   * is equivalent to <code>MBeanServerInvocationHandler(conn,
   * name, false)</code>.  The other constructor should be used
   * instead if the bean being proxied is an {@link MXBean}.
   *
   * @param conn the connection through which methods will
   *             be forwarded to the bean.
   * @param name the name of the bean to use to provide the
   *             actual calls.
   */
  public MBeanServerInvocationHandler(MBeanServerConnection conn,
				      ObjectName name)
  {
    this(conn, name, false);
  }

  /**
   * Constructs a new {@link MBeanServerInvocationHandler}
   * which forwards methods to the supplied bean via the
   * given {@link MBeanServerConnection}.  This constructor
   * is used in preference to
   * {@link JMX#newMBeanProxy(MBeanServerConnection, ObjectName,
   * Class<T>)} if you wish to make your own call to
   * {@link java.lang.reflect.Proxy#newInstance(ClassLoader,
   * Class[], java.lang.reflect.InvocationHandler)} with
   * a different {@link ClassLoader}.  
   *
   * @param conn the connection through which methods will
   *             be forwarded to the bean.
   * @param name the name of the bean to use to provide the
   *             actual calls.
   * @param mxBean true if the bean being proxied is an
   *               {@link MXBean}.
   * @since 1.6
   */
  public MBeanServerInvocationHandler(MBeanServerConnection conn,
				      ObjectName name, boolean mxBean)
  {
    this.conn = conn;
    this.name = name;
    this.mxBean = mxBean;
  }

  /**
   * Returns the connection through which the calls to the bean
   * will be made.
   * 
   * @return the connection being used to forward the calls to
   *         the bean.
   * @since 1.6
   */
  public MBeanServerConnection getMBeanServerConnection()
  {
    return conn;
  }

  /**
   * Returns the name of the bean to which method calls are made.
   *
   * @return the bean which provides the actual method calls.
   * @since 1.6
   */
  public ObjectName getObjectName()
  {
    return name;
  }

  /**
   * Called by the proxy class whenever a method is called.  The method
   * is emulated by retrieving an attribute from, setting an attribute on
   * or invoking a method on the server connection as required.  Translation
   * between the Java data types supplied as arguments to the open types used
   * by the bean is provided, as well as translation of the return value back
   * in to the appropriate Java type if the bean is an {@link MXBean}.
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
    String mName = method.getName();
    Class proxyClass = proxy.getClass();
    if (mName.equals("toString"))
      {
	if (inInterface(mName, proxyClass))
	  return conn.invoke(name,mName,null,null);
	else
	  return proxyClass.getName() + "[name=" + name 
	    + ", conn=" + conn + "]";
      }
    if (mName.equals("hashCode"))
      {
	if (inInterface(mName, proxyClass))
	  return conn.invoke(name,mName,null,null);
	else
	  return conn.hashCode() + name.hashCode()
	    + (iface == null ? 0 : iface.hashCode());
      }
    if (mName.equals("equals"))
      {
	if (inInterface(mName, proxyClass, Object.class))
	  return conn.invoke(name,mName,new Object[]{args[0]},
			     new String[]{"java.lang.Object"});
	else
	  {
	    if (args[0].getClass() != proxy.getClass())
	      return false;
	    InvocationHandler ih = Proxy.getInvocationHandler(args[0]);
	    if (ih instanceof MBeanServerInvocationHandler)
	      {
		MBeanServerInvocationHandler h =
		  (MBeanServerInvocationHandler) ih;
		return conn.equals(h.getMBeanServerConnection())
		  && name.equals(h.getObjectName())
		  && (iface == null ? h.iface == null 
		      : iface.equals(h.iface));
	      }
	    return false;
	  }
      }
    if (NotificationEmitter.class.isAssignableFrom(proxyClass))
      {
	if (mName.equals("addNotificationListener"))
	  {
	    conn.addNotificationListener(name,
					 (NotificationListener) args[0],
					 (NotificationFilter) args[1],
					 args[2]);
	    return null;
	  }
	if (mName.equals("getNotificationInfo"))
	  return conn.getMBeanInfo(name).getNotifications();
	if (mName.equals("removeNotificationListener"))
	  {
	    if (args.length == 1)
	      conn.removeNotificationListener(name, 
					      (NotificationListener)
					      args[0]);
	    else
	      conn.removeNotificationListener(name, 
					      (NotificationListener)
					      args[0],
					      (NotificationFilter)
					      args[1], args[2]);
	    return null;
	  }
      }
    String[] sigs;
    if (args == null)
      sigs = null;
    else
      {
	sigs = new String[args.length];
	for (int a = 0; a < args.length; ++a)
	  sigs[a] = args[a].getClass().getName();
      }
    String attrib = null;
    if (mName.startsWith("get"))
      attrib = mName.substring(3);
    else if (mName.startsWith("is"))
      attrib = mName.substring(2);
    if (attrib != null)
      {
	Object val = conn.getAttribute(name, attrib);
	if (mxBean)
	  return Translator.toJava(val, method);
	else
	  return val;
      }
    else if (mName.startsWith("set"))
      {
	Object arg;
	if (mxBean)
	  arg = Translator.fromJava(args, method)[0];
	else
	  arg = args[0];
	conn.setAttribute(name, new Attribute(mName.substring(3), arg));
	return null;
      }
    if (mxBean)
      return Translator.toJava(conn.invoke(name, mName, 
					   Translator.fromJava(args,method),
					   sigs), method);
    else
      return conn.invoke(name, mName, args, sigs);
  }

  /**
   * Returns true if this is a proxy for an {@link MXBean}
   * and conversions must be applied to input parameters
   * and return types, according to the rules for such beans.
   *
   * @return true if this is a proxy for an {@link MXBean}.
   * @since 1.6
   */
  public boolean isMXBean()
  {
    return mxBean;
  }

  /**
   * <p>
   * Returns a proxy for the specified bean.  A proxy object is created
   * using <code>Proxy.newProxyInstance(iface.getClassLoader(),
   * new Class[] { iface }, handler)</code>.  The
   * {@link javax.management.NotificationEmitter} class is included as the
   * second element of the array if <code>broadcaster</code> is true.
   * <code>handler</code> refers to the invocation handler which forwards
   * calls to the connection, which is created by a call to
   * <code>new MBeanServerInvocationHandler(conn, name)</code>. 
   * </p>
   * <p>
   * <strong>Note</strong>: use of the proxy may result in
   * {@link java.io.IOException}s from the underlying
   * {@link MBeanServerConnection}.
   * As of 1.6, the use of {@link JMX#newMBeanProxy(MBeanServerConnection,
   * ObjectName,Class)} and {@link JMX#newMBeanProxy(MBeanServerConnection,
   * ObjectName,Class,boolean)} is preferred.
   * </p>
   *
   * @param conn the server connection to use to access the bean.
   * @param name the {@link javax.management.ObjectName} of the
   *             bean to provide a proxy for.
   * @param iface the interface for the bean being proxied.
   * @param broadcaster true if the proxy should implement
   *                    {@link NotificationEmitter}.
   * @return a proxy for the specified bean.
   * @see JMX#newMBeanProxy(MBeanServerConnection,ObjectName,Class)
   */
  public static <T> T newProxyInstance(MBeanServerConnection conn,
				       ObjectName name, Class<T> iface,
				       boolean broadcaster)
  {
    if (broadcaster)
      return (T) Proxy.newProxyInstance(iface.getClassLoader(),
					new Class[] { iface,
						      NotificationEmitter.class },
					new MBeanServerInvocationHandler(conn,name));
    else
      return (T) Proxy.newProxyInstance(iface.getClassLoader(),
					new Class[] { iface },
					new MBeanServerInvocationHandler(conn,name));
  }

  /**
   * Returns true if the specified method is specified
   * by one of the proxy's interfaces.
   *
   * @param name the name of the method to search for.
   * @param proxyClass the class of the proxy.
   * @param args the arguments to the method.
   * @return true if one of the interfaces specifies the
   *         given method.
   */
  private boolean inInterface(String name, Class<?> proxyClass,
			      Class<?>... args)
  {
    for (Class<?> iface : proxyClass.getInterfaces())
      {
	try
	  {
	    iface.getMethod(name, args);
	    return true;
	  }
	catch (NoSuchMethodException e)
	  {
	    /* Ignored; this interface doesn't specify
	       the method. */
	  }
      }
    return false;
  }
  
}

