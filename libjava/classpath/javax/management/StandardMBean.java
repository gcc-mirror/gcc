/* StandardMBean.java -- A standard reflection-based management bean.
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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Provides a dynamic management bean by using reflection on an
 * interface and an implementing class.  By default, a bean instance
 * is paired up with its interface based on specific naming
 * conventions (if the implementation is called X, the interface must
 * be XMBean).  Using this class removes the need to use a specific
 * naming system to match up the two.  Instead, an instance of this
 * bean is created either via explicit construction or subclassing,
 * and this provides access to the attributes, constructors and
 * operations of the implementation via reflection.  Various hooks are
 * provided in order to allow customization of this process.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class StandardMBean
  implements DynamicMBean
{

  /**
   * The interface for this bean.
   */
  private Class<?> iface;

  /**
   * The implementation of the interface.
   */
  private Object impl;

  /**
   * Cached bean information.
   */
  private MBeanInfo info;

  /**
   * Constructs a new {@link StandardMBean} using the specified
   * interface and <code>this</code> as the instance.  This should
   * be used to create an instance via subclassing.
   * 
   * @param iface the interface this bean implements, or <code>null</code>
   *              if the interface should be determined using the naming
   *              convention (class X has interface XMBean).
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  protected StandardMBean(Class<?> iface)
    throws NotCompliantMBeanException
  {
    if (iface == null)
      {
	String className = getClass().getName();
	try
	  {
	    iface = Class.forName(className + "MBean");
	  }
	catch (ClassNotFoundException e)
	  {
	    for (Class<?> nextIface : getClass().getInterfaces())
	    {
	      if (JMX.isMXBeanInterface(nextIface))
		{
		  iface = nextIface;
		  break;
		}
	    }
	    if (iface == null)  
	      throw (NotCompliantMBeanException) 
		(new NotCompliantMBeanException("An interface for the class " 
						+ className +
						" was not found.").initCause(e));
	  }
      }
    if (!(iface.isInstance(this)))
      throw new NotCompliantMBeanException("The instance, " + impl + 
					   ", is not an instance of " + iface);
    impl = this;
    this.iface = iface;
  }

  /**
   * Constructs a new {@link StandardMBean} using the specified
   * interface and the supplied instance as the implementation.
   * 
   * @param impl the implementation.
   * @param iface the interface the bean implements, or <code>null</code>
   *              if the interface should be determined using the naming
   *              convention (class X has interface XMBean).
   * @throws IllegalArgumentException if <code>impl</code> is <code>null</code>.
   * @throws NotCompliantMBeanException if <code>impl</code> doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  public <T> StandardMBean(T impl, Class<T> iface)
    throws NotCompliantMBeanException
  {
    if (impl == null)
      throw new IllegalArgumentException("The specified implementation is null.");
    if (iface == null)
      {
	Class<?> implClass = impl.getClass();
	String className = implClass.getName();
	try
	  {
	    this.iface = Class.forName(className + "MBean", true,
				       implClass.getClassLoader());
	  }
	catch (ClassNotFoundException e)
	  {
	    for (Class<?> nextIface : implClass.getInterfaces())
	    {
	      if (JMX.isMXBeanInterface(nextIface))
		{
		  this.iface = nextIface;
		  break;
		}
	    }
	    if (this.iface == null)  
	      throw (NotCompliantMBeanException) 
		(new NotCompliantMBeanException("An interface for the class " +
						className +
						" was not found.").initCause(e));
	  }
      }
    else
      this.iface = iface;
    if (!(this.iface.isInstance(impl)))
      throw new NotCompliantMBeanException("The instance, " + impl + 
					   ", is not an instance of " + iface);
    this.impl = impl;
  }

  /**
   * Caches the {@link MBeanInfo} instance for this object.  This is a
   * customization hook, so that subclasses can choose the caching policy
   * used.  The default implementation caches the value in the instance
   * itself.  Subclasses may override this so as to not cache the data
   * at all, or so as to use a cache shared between multiple beans.
   *
   * @param info the {@link MBeanInfo} instance to cache, or <code>null</code>
   *             if there is no new value to cache.  When the value is not
   *             <code>null</code>, the cache should replace the current value
   *             with the value supplied here.
   * @see #getCachedMBeanInfo()
   */
  protected void cacheMBeanInfo(MBeanInfo info)
  {
    if (info != null)
      this.info = info;
  }

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
  public Object getAttribute(String name)
    throws AttributeNotFoundException, MBeanException,
	   ReflectionException
  {
    Method getter;
    try 
      {
	getter = iface.getMethod("get" + name, null);
      }
    catch (NoSuchMethodException e)
      {
	try 
	  {
	    getter = iface.getMethod("is" + name, null);
	  }
	catch (NoSuchMethodException ex)
	  {
	    throw ((AttributeNotFoundException) 
		   new AttributeNotFoundException("The attribute, " + name +
						  ", was not found.").initCause(ex));
	  }
      }
    Object result;
    try
      {
	result = getter.invoke(impl, null);
      }
    catch (IllegalAccessException e)
      {
	throw new ReflectionException(e, "Failed to retrieve " + name);
      }
    catch (IllegalArgumentException e)
      {
	throw new ReflectionException(e, "Failed to retrieve " + name);
      }
    catch (InvocationTargetException e)
      {
	throw new MBeanException((Exception) e.getCause(),
				 "The getter of " + name +
				 " threw an exception");
      }
    return result;
  }

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
  public AttributeList getAttributes(String[] names)
  {
    AttributeList list = new AttributeList(names.length);
    for (int a = 0; a < names.length; ++a)
      {
	try
	  {
	    Object value = getAttribute(names[a]);
	    list.add(new Attribute(names[a], value));
	  }
	catch (AttributeNotFoundException e)
	  {
	    /* Ignored */
	  }
	catch (ReflectionException e)
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
   * Returns the cached {@link MBeanInfo} instance for this object.  This is a
   * customization hook, so that subclasses can choose the caching policy
   * used.  The default implementation caches the value in the instance
   * itself, and returns this value on calls to this method.
   *
   * @return the cached {@link MBeanInfo} instance, or <code>null</code>
   *         if no value is cached.
   * @see #cacheMBeanInfo(javax.management.MBeanInfo)
   */
  protected MBeanInfo getCachedMBeanInfo()
  {
    return info;
  }

  /**
   * Returns the class name that will be used in the {@link MBeanInfo}
   * instance.  This is a customization hook, so that subclasses can
   * provide a custom class name.  By default, this returns the class
   * name from the supplied {@link MBeanInfo} instance.
   *
   * @param info the {@link MBeanInfo} instance constructed via
   *             reflection.
   * @return the class name to use in the instance.
   */
  protected String getClassName(MBeanInfo info)
  {
    return info.getClassName();
  }

  /**
   * Returns information on the constructors that will be used in
   * the {@link MBeanInfo} instance.  This is a customization hook,
   * so that subclasses can provide their own information on the
   * bean's constructors, if necessary.  By default, this method
   * returns <code>null</code> unless the implementation supplied
   * is either <code>null</code> or <code>this</code>.  This default
   * implementation prevents the use of
   * {@link MBeanServer#createMBean} in cases where the bean is
   * not created as a subclass of {@link StandardMBean}.
   *
   * @param constructors the constructor information created via
   *                     reflection.
   * @param impl the implementation, or <code>null</code> if this
   *             should be ignored.
   * @return the constructor information to use.
   */
  protected MBeanConstructorInfo[] getConstructors(MBeanConstructorInfo[]
						   constructors, Object impl)
  {
    if (impl == null || impl == this)
      return constructors;
    return null;
  }

  /**
   * Returns the description of the attribute that will be used in
   * the supplied {@link MBeanAttributeInfo} instance.  This is a
   * customization hook, so that subclasses can provide a custom
   * description.  By default, this calls
   * {@link #getDescription(MBeanFeatureInfo)} with the supplied
   * {@link MBeanAttributeInfo} instance.
   *
   * @param info the {@link MBeanAttributeInfo} instance constructed
   *             via reflection.
   * @return the description to use in the instance.
   */
  protected String getDescription(MBeanAttributeInfo info)
  {
    return getDescription((MBeanFeatureInfo) info);
  }

  /**
   * Returns the description of the constructor that will be used in
   * the supplied {@link MBeanConstructorInfo} instance.  This is a
   * customization hook, so that subclasses can provide a custom
   * description.  By default, this calls
   * {@link #getDescription(MBeanFeatureInfo)} with the supplied
   * {@link MBeanConstructorInfo} instance.
   *
   * @param info the {@link MBeanConstructorInfo} instance constructed
   *             via reflection.
   * @return the description to use in the instance.
   */
  protected String getDescription(MBeanConstructorInfo info)
  {
    return getDescription((MBeanFeatureInfo) info);
  }

  /**
   * Returns the description of the nth parameter of the constructor
   * that will be used in the supplied {@link MBeanParameterInfo}
   * instance.  This is a customization hook, so that subclasses
   * can provide a custom description.  By default, this calls
   * <code>param.getDescription()</code>.
   *
   * @param info the {@link MBeanConstructorInfo} instance constructed
   *             via reflection.
   * @param param the {@link MBeanParameterInfo} instance constructed
   *             via reflection.
   * @param n the number of the parameter, in order to link it to the
   *          information on the constructor.
   * @return the description to use in the instance.
   */
  protected String getDescription(MBeanConstructorInfo info,
				  MBeanParameterInfo param, int n)
  {
    return param.getDescription();
  }

  /**
   * Returns the description of the supplied feature that
   * will be used in the supplied {@link MBeanFeatureInfo}
   * instance.  This is a customization hook, so that subclasses
   * can provide a custom description.  By default, this calls
   * <code>info.getDescription()</code>.  This method is also called
   * by default for the more specific description methods for attributes,
   * constructors and operations.
   *
   * @param info the {@link MBeanFeatureInfo} instance constructed
   *             via reflection.
   * @return the description to use in the instance.
   */
  protected String getDescription(MBeanFeatureInfo info)
  {
    return info.getDescription();
  }

  /**
   * Returns the description of the bean that will be used in the
   * supplied {@link MBeanInfo} instance.  This is a customization
   * hook, so that subclasses can provide a custom description.  By
   * default, this calls <code>info.getDescription()</code>.
   *
   * @param info the {@link MBeanInfo} instance constructed
   *             via reflection.
   * @return the description to use in the instance.
   */
  protected String getDescription(MBeanInfo info)
  {
    return info.getDescription();
  }

  /**
   * Returns the description of the operation that will be used in
   * the supplied {@link MBeanOperationInfo} instance.  This is a
   * customization hook, so that subclasses can provide a custom
   * description.  By default, this calls
   * {@link #getDescription(MBeanFeatureInfo)} with the supplied
   * {@link MBeanOperationInfo} instance.
   *
   * @param info the {@link MBeanOperationInfo} instance constructed
   *             via reflection.
   * @return the description to use in the instance.
   */
  protected String getDescription(MBeanOperationInfo info)
  {
    return getDescription((MBeanFeatureInfo) info);
  }

  /**
   * Returns the description of the nth parameter of the operation
   * that will be used in the supplied {@link MBeanParameterInfo}
   * instance.  This is a customization hook, so that subclasses
   * can provide a custom description.  By default, this calls
   * <code>param.getDescription()</code>.
   *
   * @param info the {@link MBeanOperationInfo} instance constructed
   *             via reflection.
   * @param param the {@link MBeanParameterInfo} instance constructed
   *             via reflection.
   * @param n the number of the parameter, in order to link it to the
   *          information on the operation.
   * @return the description to use in the instance.
   */
  protected String getDescription(MBeanOperationInfo info,
				  MBeanParameterInfo param, int n)
  {
    return param.getDescription();
  }

  /**
   * Returns the impact of the operation that will be used in the
   * supplied {@link MBeanOperationInfo} instance.  This is a
   * customization hook, so that subclasses can provide a custom
   * impact flag.  By default, this returns
   * <code>info.getImpact()</code>.
   *
   * @param info the {@link MBeanOperationInfo} instance constructed
   *             via reflection.
   * @return the impact flag to use in the instance.
   */
  protected int getImpact(MBeanOperationInfo info)
  {
    return info.getImpact();
  }

  /**
   * Returns the instance that implements this bean.
   *
   * @return the implementation.
   */
  public Object getImplementation()
  {
    return impl;
  }

  /**
   * Returns the class of the instance that implements this bean.
   *
   * @return the implementation class.
   */
  public Class<?> getImplementationClass()
  {
    return impl.getClass();
  }

  /**
   * <p>
   * Returns an information object which lists the attributes
   * and actions associated with the management bean.  This
   * implementation proceeds as follows:
   * </p>
   * <ol>
   * <li>{@link #getCachedMBeanInfo()} is called to obtain
   * the cached instance.  If this returns a non-null value,
   * this value is returned.</li>
   * <li>If there is no cached value, then the method proceeds
   * to create one. During this process, the customization hooks
   * detailed in this class are called to allow the values used
   * to be overrided:
   * <ul>
   * <li>For each attribute, 
   * {@link #getDescription(MBeanAttributeInfo)} is called.</li>
   * <li>For each constructor,
   * {@link #getDescription(MBeanConstructorInfo)} is called,
   * along with {@link #getDescription(MBeanConstructorInfo,
   * MBeanParameterInfo, int)} and
   * {@link #getParameterName(MBeanConstructorInfo,
   * MBeanParameterInfo, int)} for each parameter.</li>
   * <li>The constructors may be replaced as a whole by
   * a call to
   * {@link #getConstructors(MBeanConstructorInfo[], Object)}.</li>
   * <li>For each operation,
   * {@link #getDescription(MBeanOperationInfo)} and
   * {@link #getImpact(MBeanOperationInfo)} are called,
   * along with {@link #getDescription(MBeanOperationInfo,
   * MBeanParameterInfo, int)} and
   * {@link #getParameterName(MBeanOperationInfo,
   * MBeanParameterInfo, int)} for each parameter.</li>
   * <li>{@link #getClassName(MBeanInfo)} and
   * {@link #getDescription(MBeanInfo)} are called to customise
   * the basic information about the class.</li>
   * </ul>
   * </li>
   * <li>Finally, {@link #cacheMBeanInfo(MBeanInfo)} is called
   * with the created instance before it is returned.</li>
   * </ol>
   *
   * @return a description of the management bean, including
   *         all exposed attributes and actions.
   */
  public MBeanInfo getMBeanInfo()
  {
    MBeanInfo info = getCachedMBeanInfo();
    if (info != null)
      return info;
    Method[] methods = iface.getMethods();
    Map attributes = new HashMap();
    List operations = new ArrayList();
    for (int a = 0; a < methods.length; ++a)
      {
	String name = methods[a].getName();
	if (((name.startsWith("get") &&
	      methods[a].getReturnType() != Void.TYPE) ||
	     (name.startsWith("is") &&
	      methods[a].getReturnType() == Boolean.TYPE)) &&
	    methods[a].getParameterTypes().length == 0)
	  {
	    Method[] amethods;
	    String attrib;
	    if (name.startsWith("is"))
	      attrib = name.substring(2);
	    else
	      attrib = name.substring(3);
	    if (attributes.containsKey(attrib))
	      amethods = (Method[]) attributes.get(attrib);
	    else
	      {
		amethods = new Method[2];
		attributes.put(attrib, amethods);
	      }
	    amethods[0] = methods[a];
	  }
	else if (name.startsWith("set") &&
		 methods[a].getReturnType() == Void.TYPE &&
		 methods[a].getParameterTypes().length == 1)
	  {
	    Method[] amethods;
	    String attrib = name.substring(3);
	    if (attributes.containsKey(attrib))
	      amethods = (Method[]) attributes.get(attrib);
	    else
	      {
		amethods = new Method[2];
		attributes.put(attrib, amethods);
	      }
	    amethods[1] = methods[a];
	  }
	else
	  operations.add(new MBeanOperationInfo(methods[a].getName(),
						methods[a]));
      }
    List attribs = new ArrayList(attributes.size());
    Iterator it = attributes.entrySet().iterator();
    while (it.hasNext())
      {
	Map.Entry entry = (Map.Entry) it.next();
	Method[] amethods = (Method[]) entry.getValue();
	try
	  {
	    attribs.add(new MBeanAttributeInfo((String) entry.getKey(),
					       (String) entry.getKey(),
					       amethods[0], amethods[1]));
	  }
	catch (IntrospectionException e)
	  {
	    /* Shouldn't happen; both shouldn't be null */
	    throw new IllegalStateException("The two methods passed to " +
					    "the MBeanAttributeInfo " +
					    "constructor for " + entry +
					    "were null.", e);
	  }
      }
    MBeanAttributeInfo[] ainfo = new MBeanAttributeInfo[attribs.size()];
    for (int a = 0; a < ainfo.length; ++a)
      {
	MBeanAttributeInfo oldInfo = (MBeanAttributeInfo) attribs.get(a);
	String desc = getDescription(oldInfo);
	ainfo[a] = new MBeanAttributeInfo(oldInfo.getName(),
					  oldInfo.getType(), desc,
					  oldInfo.isReadable(),
					  oldInfo.isWritable(),
					  oldInfo.isIs());
      }
    Constructor[] cons = impl.getClass().getConstructors();
    MBeanConstructorInfo[] cinfo = new MBeanConstructorInfo[cons.length];
    for (int a = 0; a < cinfo.length; ++a)
      {
	MBeanConstructorInfo oldInfo = new MBeanConstructorInfo(cons[a].getName(),
								cons[a]);
	String desc = getDescription(oldInfo);
	MBeanParameterInfo[] params = oldInfo.getSignature();
	MBeanParameterInfo[] pinfo = new MBeanParameterInfo[params.length];
	for (int b = 0; b < pinfo.length; ++b)
	  {
	    String pdesc = getDescription(oldInfo, params[b], b);
	    String pname = getParameterName(oldInfo, params[b], b);
	    pinfo[b] = new MBeanParameterInfo(pname, params[b].getType(),
					      pdesc);
	  }
	cinfo[a] = new MBeanConstructorInfo(oldInfo.getName(), desc,
					    pinfo);
      }
    cinfo = getConstructors(cinfo, impl);
    MBeanOperationInfo[] oinfo = new MBeanOperationInfo[operations.size()];
    for (int a = 0; a < oinfo.length; ++a)
      {
	MBeanOperationInfo oldInfo = (MBeanOperationInfo) operations.get(a);
	String desc = getDescription(oldInfo);
	int impact = getImpact(oldInfo);
	MBeanParameterInfo[] params = oldInfo.getSignature();
	MBeanParameterInfo[] pinfo = new MBeanParameterInfo[params.length];
	for (int b = 0; b < pinfo.length; ++b)
	  {
	    String pdesc = getDescription(oldInfo, params[b], b);
	    String pname = getParameterName(oldInfo, params[b], b);
	    pinfo[b] = new MBeanParameterInfo(pname, params[b].getType(),
					      pdesc);
	  }
	oinfo[a] = new MBeanOperationInfo(oldInfo.getName(), desc, pinfo,
					  oldInfo.getReturnType(), impact);
      }
    info = new MBeanInfo(impl.getClass().getName(), impl.getClass().getName(),
			 ainfo, cinfo, oinfo, null);
    String cname = getClassName(info);
    String desc = getDescription(info);
    MBeanNotificationInfo[] ninfo = null;
    if (impl instanceof NotificationBroadcaster)
      ninfo = ((NotificationBroadcaster) impl).getNotificationInfo();
    info = new MBeanInfo(cname, desc, ainfo, cinfo, oinfo, ninfo);
    cacheMBeanInfo(info);
    return info;
  }

  /**
   * Returns the interface for this management bean.
   *
   * @return the management interface.
   */
  public final Class<?> getMBeanInterface()
  {
    return iface;
  }

  /**
   * Returns the name of the nth parameter of the constructor
   * that will be used in the supplied {@link MBeanParameterInfo}
   * instance.  This is a customization hook, so that subclasses
   * can provide a custom name.  By default, this calls
   * <code>param.getName()</code>.
   *
   * @param info the {@link MBeanConstructorInfo} instance constructed
   *             via reflection.
   * @param param the {@link MBeanParameterInfo} instance constructed
   *             via reflection.
   * @param n the number of the parameter, in order to link it to the
   *          information on the constructor.
   * @return the name to use in the instance.
   */
  protected String getParameterName(MBeanConstructorInfo info,
				    MBeanParameterInfo param, int n)
  {
    return param.getName();
  }

  /**
   * Returns the name of the nth parameter of the operation
   * that will be used in the supplied {@link MBeanParameterInfo}
   * instance.  This is a customization hook, so that subclasses
   * can provide a custom name.  By default, this calls
   * <code>param.getName()</code>.
   *
   * @param info the {@link MBeanOperationInfo} instance constructed
   *             via reflection.
   * @param param the {@link MBeanParameterInfo} instance constructed
   *             via reflection.
   * @param n the number of the parameter, in order to link it to the
   *          information on the operation.
   * @return the name to use in the instance.
   */
  protected String getParameterName(MBeanOperationInfo info,
				    MBeanParameterInfo param, int n)
  {
    return param.getName();
  }

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
  public Object invoke(String name, Object[] params, String[] signature)
    throws MBeanException, ReflectionException
  {
    if (name.startsWith("get") || name.startsWith("is") ||
	name.startsWith("set"))
      throw new ReflectionException(new NoSuchMethodException(),
				    "Invocation of an attribute " +
				    "method is disallowed.");
    ClassLoader loader = getClass().getClassLoader();
    Class[] sigTypes;
    if (signature != null)
      {
	sigTypes = new Class[signature.length];
	for (int a = 0; a < signature.length; ++a)
	  try 
	    {
	      sigTypes[a] = Class.forName(signature[a], true, loader);
	    }
	  catch (ClassNotFoundException e)
	    {
	      throw new ReflectionException(e, "The class, " + signature[a] + 
					    ", in the method signature " +
					    "could not be loaded.");
	    }
      }
    else
      sigTypes = null;
    Method method;
    try
      {
	method = iface.getMethod(name, sigTypes);
      }
    catch (NoSuchMethodException e)
      {
	throw new ReflectionException(e, "The method, " + name +
				      ", could not be found.");
      }
    Object result;
    try
      {
	result = method.invoke(impl, params);
      }
    catch (IllegalAccessException e)
      {
	throw new ReflectionException(e, "Failed to call " + name);
      }
    catch (IllegalArgumentException e)
      {
	throw new ReflectionException(e, "Failed to call " + name);
      }
    catch (InvocationTargetException e)
      {
	throw new MBeanException((Exception) e.getCause(), "The method "
				 + name + " threw an exception");
      }
    return result;
  }

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
  public void setAttribute(Attribute attribute)
    throws AttributeNotFoundException, InvalidAttributeValueException,
	   MBeanException, ReflectionException
  {
    String name = attribute.getName();
    String attName = name.substring(0, 1).toUpperCase() + name.substring(1);
    Object val = attribute.getValue();   
    try
      {
	getMutator(attName, val.getClass()).invoke(impl, new Object[] { val });
      }
    catch (IllegalAccessException e)
      {
	throw new ReflectionException(e, "Failed to set " + name);
      }
    catch (IllegalArgumentException e)
      {
	throw ((InvalidAttributeValueException)
	       new InvalidAttributeValueException(attribute.getValue() +
						  " is an invalid value for " +
						  name).initCause(e));
      }
    catch (InvocationTargetException e)
      {
	throw new MBeanException(e, "The getter of " + name +
				 " threw an exception");
      }
  }

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
  public AttributeList setAttributes(AttributeList attributes)
  {
    AttributeList list = new AttributeList(attributes.size());
    Iterator it = attributes.iterator();
    while (it.hasNext())
      {
	try
	  {
	    Attribute attrib = (Attribute) it.next();
	    setAttribute(attrib);
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
	catch (ReflectionException e)
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
   * Replaces the implementation of the interface used by this
   * instance with the one specified.  The new implementation
   * must be non-null and implement the interface specified on
   * construction of this instance.
   *
   * @throws IllegalArgumentException if <code>impl</code> is <code>null</code>.
   * @throws NotCompliantMBeanException if <code>impl</code> doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  public void setImplementation(Object impl)
    throws NotCompliantMBeanException
  {
    if (impl == null)
      throw new IllegalArgumentException("The specified implementation is null.");
    if (!(iface.isInstance(impl)))
      throw new NotCompliantMBeanException("The instance, " + impl + 
					   ", is not an instance of " + iface);
    this.impl = impl;
  }

  /**
   * Returns the mutator method for a particular attribute name
   * with a parameter type matching that of the given value.
   *
   * @param name the name of the attribute.
   * @param type the type of the parameter.
   * @return the appropriate mutator method.
   * @throws AttributeNotFoundException if a method can't be found.
   */
  private Method getMutator(String name, Class<?> type)
    throws AttributeNotFoundException
  {
    String mutator = "set" + name;
    Exception ex = null;
    try 
      {
	return iface.getMethod(mutator, type);
      }
    catch (NoSuchMethodException e)
      {
	/* Ignored; we'll try harder instead */
	ex = e;
      }
    /* Special cases */
    if (type == Boolean.class)
      try
	{
	  return iface.getMethod(mutator, Boolean.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    if (type == Byte.class)
      try
	{
	  return iface.getMethod(mutator, Byte.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    if (type == Character.class)
      try
	{
	  return iface.getMethod(mutator, Character.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    if (type == Double.class)
      try
	{
	  return iface.getMethod(mutator, Double.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    if (type == Float.class)
      try
	{
	  return iface.getMethod(mutator, Float.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    if (type == Integer.class)
      try
	{
	  return iface.getMethod(mutator, Integer.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    if (type == Long.class)
      try
	{
	  return iface.getMethod(mutator, Long.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    if (type == Short.class)
      try
	{
	  return iface.getMethod(mutator, Short.TYPE);
	}
      catch (NoSuchMethodException e)
	{
	  throw ((AttributeNotFoundException) 
		 new AttributeNotFoundException("The attribute, " + name +
						", was not found.").initCause(e));
	}
    /* Superclasses and interfaces */
    for (Class<?> i : type.getInterfaces())
      try
	{
	  return getMutator(name, i);
	}
      catch (AttributeNotFoundException e)
	{
	  ex = e;
	}
    Class<?> sclass = type.getSuperclass();
    if (sclass != null && sclass != Object.class)
      try
	{
	  return getMutator(name, sclass);
	}
      catch (AttributeNotFoundException e)
	{
	  ex = e;
	}
    /* If we get this far, give up */
    throw ((AttributeNotFoundException) 
	   new AttributeNotFoundException("The attribute, " + name +
					  ", was not found.").initCause(ex)); 
  }

}
