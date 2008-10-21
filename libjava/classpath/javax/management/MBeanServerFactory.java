/* MBeanServerFactory.java -- Manages server instances.
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

import gnu.classpath.SystemProperties;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.management.loading.ClassLoaderRepository;

/**
 * <p>
 * Creates and maintains a set of {@link MBeanServer} instances.
 * Server instances, as of JMX 1.2, are created using a subclass
 * of {@link MBeanServerBuilder}.  The exact class used is controlled
 * by the property <code>javax.management.builder.initial</code>,
 * and allows the instances created by {@link MBeanServerBuilder}
 * to be wrapped, thus providing additional functionality.
 * </p>
 * <p>
 * The property is used as follows:
 * </p>
 * <ol>
 * <li>If the property has no value, then an instance of
 * {@link MBeanServerBuilder} is used.</li>
 * <li>If a value is given, then:
 * <ol>
 * <li>The class is loaded using
 * <code>Thread.currentThread().getContextClassLoader()</code>, or,
 * if this is <code>null</code>, by <code>Class.forName()</code>.</li>
 * <li><code>Class.newInstance()</code> is used to create an instance
 * of the class.  The class must be public and have a public empty
 * constructor.  If an exception is thrown, it is propogated as
 * a {@link JMRuntimeException} and no new server instances may be
 * created until the property is set to a valid value.</li>
 * </ol></li>
 * <li>The value is checked on each successive request for a server.
 * If it differs from the class of the existing instance of
 * {@link MBeanServerBuilder}, then the value is used to create
 * a new instance.</li>
 * </ol>
 */
public class MBeanServerFactory
{

  /**
   * The last builder instance.
   */
  private static MBeanServerBuilder builder;

  /**
   * The map of registered servers (identifiers to servers).
   */
  private static final Map<Object,MBeanServer> servers =
    new HashMap<Object,MBeanServer>();

  /**
   * Private constructor to prevent instance creation.
   */
  private MBeanServerFactory() {}

  /**
   * Returns a server implementation using the default domain name
   * of <code>"DefaultDomain"</code>.  The default domain name is
   * used when the domain name specified by the user is <code>null</code.
   * A reference to the created server is retained, so that it can
   * be retrieved at a later date using {@link #findMBeanServer}.
   * Calling this method is equivalent to calling
   * {@link createMBeanServer(String)} with a <code>null</code> value.
   *
   * @return a new {@link MBeanServer} instance.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanServerPermission(String)}("createMBeanServer")
   * @throws JMRuntimeException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which either can not be
   *                     instantiated or provides an implementation that returns
   *                     <code>null</code> from either
   *                     {@link MBeanServerBuilder#newMBeanServerDelegate()}
   *                     or {@link MBeanServerBuilder#newMBeanServer()}
   * @throws ClassCastException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which is not a subclass
   *                     of {@link MBeanServerBuilder}.
   * @see #createMBeanServer(String)
   */
  public static MBeanServer createMBeanServer()
  {
    return createMBeanServer(null);
  }

  /**
   * Returns a server implementation using the default domain name
   * given, or <code>"DefaultDomain"</code> if this is <code>null</code>.
   * The default domain name is used when the domain name specified by
   * the user is <code>null</code.  A reference to the created server is
   * retained, so that it can be retrieved at a later date using
   * {@link #findMBeanServer}.  
   *
   * @param domain the default domain name of the server.
   * @return a new {@link MBeanServer} instance.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanServerPermission(String)}("createMBeanServer")
   * @throws JMRuntimeException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which either can not be
   *                     instantiated or provides an implementation that returns
   *                     <code>null</code> from either
   *                     {@link MBeanServerBuilder#newMBeanServerDelegate()}
   *                     or {@link MBeanServerBuilder#newMBeanServer()}
   * @throws ClassCastException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which is not a subclass
   *                     of {@link MBeanServerBuilder}.
   */
  public static MBeanServer createMBeanServer(String domain)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new MBeanServerPermission("createMBeanServer"));
    MBeanServer server = createServer(domain);
    try
      {
	ObjectName dn = new
	  ObjectName("JMImplementation:type=MBeanServerDelegate");
	servers.put(server.getAttribute(dn, "MBeanServerId"), server);
      }
    catch (MalformedObjectNameException e)
      {
	throw (Error) 
	  (new InternalError("Malformed delegate bean name.").initCause(e));
      }
    catch (MBeanException e)
      {
	throw (Error) 
	  (new InternalError("Exception in getMBeanServerId().").initCause(e));
      }
    catch (AttributeNotFoundException e)
      {
	throw (Error) 
	  (new InternalError("Could not find MBeanServerId attribute.").initCause(e));
      }
    catch (InstanceNotFoundException e)
      {
	throw (Error) 
	  (new InternalError("Could not find the delegate bean.").initCause(e));
      }
    catch (ReflectionException e)
      {
	throw (Error) 
	  (new InternalError("Could not call getMBeanServerId().").initCause(e));
      }
    return server;
  }

  /**
   * Returns the specified server, or, if <code>id</code> is <code>null</code>,
   * a list of all registered servers.  A registered server is one that
   * was created using {@link #createMBeanServer()} or
   * {@link #createMBeanServer(String)} and has not yet been released
   * using {@link releaseMBeanServer(MBeanServer)}.
   *
   * @param id the id of the server to retrieve, or <code>null</code>
   *           to return all servers.
   * @return a list of {@link MBeanServer}s.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanServerPermission(String)}("findMBeanServer")
   */
  public static ArrayList<MBeanServer> findMBeanServer(String id)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new MBeanServerPermission("findMBeanServer"));
    if (id == null)
      return new ArrayList<MBeanServer>(servers.values());
    ArrayList<MBeanServer> list = new ArrayList<MBeanServer>();
    MBeanServer server = servers.get(id);
    if (server != null)
      list.add(servers.get(id));
    return list;
  }

  /**
   * Returns the class loader repository used by the specified server.
   * This is equivalent to calling {@link MBeanServer#getClassLoaderRepository()}
   * on the given server.
   * 
   * @param server the server whose class loader repository should be
   *               retrieved.
   * @throws NullPointerException if <code>server</code> is <code>null</code>.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanPermission(String,String,ObjectName,String)
   *                           <code>MBeanPermission(null, null, null,
   *                           "getClassLoaderRepository")</code>
   */
  public static ClassLoaderRepository getClassLoaderRepository(MBeanServer server)
  {
    return server.getClassLoaderRepository();
  }

  /**
   * Returns a server implementation using the default domain name
   * of <code>"DefaultDomain"</code>.  The default domain name is
   * used when the domain name specified by the user is <code>null</code.
   * No reference to the created server is retained, so the server is
   * garbage collected when it is no longer used, but it can not be
   * retrieved at a later date using {@link #findMBeanServer}.   
   * Calling this method is equivalent to calling
   * {@link newMBeanServer(String)} with a <code>null</code> value.
   *
   * @return a new {@link MBeanServer} instance.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanServerPermission(String)}("newMBeanServer")
   * @throws JMRuntimeException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which either can not be
   *                     instantiated or provides an implementation that returns
   *                     <code>null</code> from either
   *                     {@link MBeanServerBuilder#newMBeanServerDelegate()}
   *                     or {@link MBeanServerBuilder#newMBeanServer()}
   * @throws ClassCastException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which is not a subclass
   *                     of {@link MBeanServerBuilder}.
   * @see #newMBeanServer(String)
   */
  public static MBeanServer newMBeanServer()
  {
    return newMBeanServer(null);
  }

  /**
   * Returns a server implementation using the default domain name
   * given, or <code>"DefaultDomain"</code> if this is <code>null</code>.
   * The default domain name is used when the domain name specified by
   * the user is <code>null</code.  No reference to the created server is
   * retained, so the server is garbage collected when it is no longer
   * used, but it can not be retrieved at a later date using
   * {@link #findMBeanServer}.
   *
   * @param domain the default domain name of the server.
   * @return a new {@link MBeanServer} instance.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanServerPermission(String)}("newMBeanServer")
   * @throws JMRuntimeException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which either can not be
   *                     instantiated or provides an implementation that returns
   *                     <code>null</code> from either
   *                     {@link MBeanServerBuilder#newMBeanServerDelegate()}
   *                     or {@link MBeanServerBuilder#newMBeanServer()}
   * @throws ClassCastException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which is not a subclass
   *                     of {@link MBeanServerBuilder}.
   */
  public static MBeanServer newMBeanServer(String domain)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new MBeanServerPermission("newMBeanServer"));
    return createServer(domain);
  }

  /**
   * Common method to create a server for the {@link #createMBeanServer(String)}
   * and {@link #newMBeanServer(String)} methods above.
   *
   * @param domain the default domain name of the server.
   * @throws JMRuntimeException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which either can not be
   *                     instantiated or provides an implementation that returns
   *                     <code>null</code> from either
   *                     {@link MBeanServerBuilder#newMBeanServerDelegate()}
   *                     or {@link MBeanServerBuilder#newMBeanServer()}
   * @throws ClassCastException if the property
   *                     <code>javax.management.builder.initial</code>
   *                     exists but names a class which is not a subclass
   *                     of {@link MBeanServerBuilder}.
   */
  private static MBeanServer createServer(String domain)
    {
    if (domain == null)
      domain = "DefaultDomain";
    String builderClass =
      SystemProperties.getProperty("javax.management.builder.initial");
    if (builderClass == null)
      {
	if (builder == null ||
	    builder.getClass() != MBeanServerBuilder.class)
	  builder = new MBeanServerBuilder();
      }
    else if (!(builder != null &&
	       builderClass.equals(builder.getClass().getName())))
      {
	ClassLoader cl = Thread.currentThread().getContextClassLoader();
	if (cl == null)
	  cl = MBeanServerFactory.class.getClassLoader();
	try
	  {
	    Class<?> bClass = Class.forName(builderClass, true, cl);
	    builder = (MBeanServerBuilder) bClass.newInstance();
	  }
	catch (ClassNotFoundException e)
	  {
	    throw (JMRuntimeException) (new JMRuntimeException("The builder class, " 
							       + builderClass +
							       ", could not be found."))
	      .initCause(e);
	  }
	catch (InstantiationException e)
	  {
	    throw (JMRuntimeException) (new JMRuntimeException("The builder class, " 
							       + builderClass +
							       ", could not be instantiated."))
	      .initCause(e);
	  }
	catch (IllegalAccessException e)
	  {
	    throw (JMRuntimeException) (new JMRuntimeException("The builder class, " 
							       + builderClass +
							       ", could not be accessed."))
	      .initCause(e);
	  }
      }
    MBeanServerDelegate delegate = builder.newMBeanServerDelegate();
    if (delegate == null)
      throw new JMRuntimeException("A delegate could not be created.");
    MBeanServer server = builder.newMBeanServer(domain, null, delegate);
    if (server == null)
      throw new JMRuntimeException("A server could not be created.");
    return server;
  }

  /**
   * Removes the reference to the specified server, thus allowing it to
   * be garbage collected.
   *
   * @param server the server to remove.
   * @throws IllegalArgumentException if a reference to the server is not
   *                                  held (i.e. it wasn't created by
   *                                  {@link #createMBeanServer(String)}
   *                                  or this method has already been called
   *                                  on it.
   * @throws SecurityException if a security manager exists and the
   *                           caller's permissions don't imply {@link
   *                           MBeanServerPermission(String)}("releaseMBeanServer")
   */
  public static void releaseMBeanServer(MBeanServer server)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new MBeanServerPermission("releaseMBeanServer"));
    Iterator<MBeanServer> i = servers.values().iterator();
    while (i.hasNext())
      {
	MBeanServer s = i.next();
	if (server == s)
	  {
	    i.remove();
	    return;
	  }
      }
    throw new IllegalArgumentException("The server given is not referenced.");
  }


}
