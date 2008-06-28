/* BeanImpl.java - A common superclass for bean implementations.
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

package gnu.java.lang.management;

import gnu.javax.management.Translator;

import java.lang.management.ManagementPermission;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.management.AttributeNotFoundException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.ReflectionException;
import javax.management.StandardMBean;

import javax.management.openmbean.ArrayType;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenMBeanAttributeInfo;
import javax.management.openmbean.OpenMBeanAttributeInfoSupport;
import javax.management.openmbean.OpenMBeanConstructorInfo;
import javax.management.openmbean.OpenMBeanConstructorInfoSupport;
import javax.management.openmbean.OpenMBeanInfo;
import javax.management.openmbean.OpenMBeanInfoSupport;
import javax.management.openmbean.OpenMBeanOperationInfo;
import javax.management.openmbean.OpenMBeanOperationInfoSupport;
import javax.management.openmbean.OpenMBeanParameterInfo;
import javax.management.openmbean.OpenMBeanParameterInfoSupport;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

/**
 * A common superclass for bean implementations.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class BeanImpl
  extends StandardMBean
{

  /**
   * Cached open bean information.
   */
  private OpenMBeanInfo openInfo;

  /**
   * Constructs a new <code>BeanImpl</code>.
   *
   * @param iface the bean interface being implemented.
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  protected BeanImpl(Class iface)
    throws NotCompliantMBeanException
  {
    super(iface);
  }

  protected void cacheMBeanInfo(MBeanInfo info)
  {
    if (info == null)
      return;
    try
      {
	MBeanAttributeInfo[] oldA = info.getAttributes();
	OpenMBeanAttributeInfo[] attribs =
	  new OpenMBeanAttributeInfoSupport[oldA.length];
	for (int a = 0; a < oldA.length; ++a)
	  {
	    OpenMBeanParameterInfo param = Translator.translate(oldA[a].getType());
	    if (param.getMinValue() == null)
	      {
		Object[] lv;
		if (param.getLegalValues() == null)
		  lv = null;
		else
		  lv = param.getLegalValues().toArray();
		attribs[a] = new OpenMBeanAttributeInfoSupport(oldA[a].getName(),
							       oldA[a].getDescription(),
							       ((OpenType<Object>)
								param.getOpenType()),
							       oldA[a].isReadable(),
							       oldA[a].isWritable(),
							       oldA[a].isIs(),
							       param.getDefaultValue(),
							       lv);
	      }
	    else
	      attribs[a] = new OpenMBeanAttributeInfoSupport(oldA[a].getName(),
							     oldA[a].getDescription(),
							     ((OpenType<Object>)
							      param.getOpenType()),
							     oldA[a].isReadable(),
							     oldA[a].isWritable(),
							     oldA[a].isIs(),
							     param.getDefaultValue(),
							     ((Comparable<Object>)
							       param.getMinValue()),
							     ((Comparable<Object>)
							       param.getMaxValue()));
	  }
	MBeanConstructorInfo[] oldC = info.getConstructors();
	OpenMBeanConstructorInfo[] cons = new OpenMBeanConstructorInfoSupport[oldC.length];
	for (int a = 0; a < oldC.length; ++a)
	  cons[a] =
	    new OpenMBeanConstructorInfoSupport(oldC[a].getName(),
						oldC[a].getDescription(),
						translateSignature(oldC[a].getSignature()));
	MBeanOperationInfo[] oldO = info.getOperations();
	OpenMBeanOperationInfo[] ops = new OpenMBeanOperationInfoSupport[oldO.length];
	for (int a = 0; a < oldO.length; ++a)
	  ops[a] =
	new OpenMBeanOperationInfoSupport(oldO[a].getName(),
					  oldO[a].getDescription(),
					  translateSignature(oldO[a].getSignature()),
					  Translator.translate(oldO[a].getReturnType()).getOpenType(),
					  oldO[a].getImpact());
	openInfo = new OpenMBeanInfoSupport(info.getClassName(), info.getDescription(),
					    attribs, cons, ops, info.getNotifications());
      }
    catch (OpenDataException e)
      {
	throw (InternalError) (new InternalError("A problem occurred creating the open type " +
						 "descriptors.").initCause(e));
      }
  }

  protected void checkMonitorPermissions()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new ManagementPermission("monitor"));
  }

  protected void checkControlPermissions()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new ManagementPermission("control"));
  }

  public Object getAttribute(String attribute)
    throws AttributeNotFoundException, MBeanException,
	   ReflectionException
  {
    Object value = super.getAttribute(attribute);
    if (value instanceof Enum)
      return ((Enum) value).name();
    Class vClass = value.getClass();
    if (vClass.isArray())
      vClass = vClass.getComponentType();
    String cName = vClass.getName();
    String[] allowedTypes = OpenType.ALLOWED_CLASSNAMES;
    for (int a = 0; a < allowedTypes.length; ++a)
      if (cName.equals(allowedTypes[a]))
	return value;
    OpenMBeanInfo info = (OpenMBeanInfo) getMBeanInfo();
    MBeanAttributeInfo[] attribs =
      (MBeanAttributeInfo[]) info.getAttributes();
    OpenType type = null;
    for (int a = 0; a < attribs.length; ++a)
      if (attribs[a].getName().equals(attribute))
	type = ((OpenMBeanAttributeInfo) attribs[a]).getOpenType();
    if (value instanceof List)
      {
	try
	  {
	    Class e =
	      Class.forName(((ArrayType) type).getElementOpenType().getClassName());
	    List l = (List) value;
	    Object[] array = (Object[]) Array.newInstance(e, l.size());
	    return l.toArray(array);
	  }
	catch (ClassNotFoundException e)
	  {
	    throw (InternalError) (new InternalError("The class of the list " +
						     "element type could not " +
						     "be created").initCause(e));
	  }
      }
    if (value instanceof Map)
      {
	TabularType ttype = (TabularType) type;
	TabularData data = new TabularDataSupport(ttype);
	Iterator it = ((Map) value).entrySet().iterator();
	while (it.hasNext())
	  {
	    Map.Entry entry = (Map.Entry) it.next();
	    try 
	      {
		data.put(new CompositeDataSupport(ttype.getRowType(),
						  new String[] { 
						    "key", 
						    "value" 
						  },
						  new Object[] { 
						    entry.getKey(),
						    entry.getValue()
						  }));
	      }
	    catch (OpenDataException e)
	      {
		throw (InternalError) (new InternalError("A problem occurred " +
							 "converting the map " +
							 "to a composite data " +
							 "structure.").initCause(e));
	      }
	  }
	return data;
      }
    CompositeType cType = (CompositeType) type;
    Set names = cType.keySet();
    Iterator it = names.iterator();
    List values = new ArrayList(names.size());
    while (it.hasNext())
      {
	String field = (String) it.next();
	Method getter = null;
	try 
	  {
	    getter = vClass.getMethod("get" + field);
	  }
	catch (NoSuchMethodException e)
	  {
	    /* Ignored; the type tells us it's there. */
	  }
	try
	  {
	    values.add(getter.invoke(value));
	  }
	catch (IllegalAccessException e)
	  {
	    throw new ReflectionException(e, "Failed to retrieve " + field);
	  }
	catch (IllegalArgumentException e)
	  {
	    throw new ReflectionException(e, "Failed to retrieve " + field);
	  }
	catch (InvocationTargetException e)
	  {
	    throw new MBeanException((Exception) e.getCause(),
				     "The getter of " + field +
				     " threw an exception");
	  }
      }
    try
      {
	return new CompositeDataSupport(cType, 
					(String[]) 
					names.toArray(new String[names.size()]),
					values.toArray());
      }
    catch (OpenDataException e)
      {
	throw (InternalError) (new InternalError("A problem occurred " +
						 "converting the value " +
						 "to a composite data " +
						 "structure.").initCause(e));
      }
  }
  
  protected MBeanInfo getCachedMBeanInfo()
  {
    return (MBeanInfo) openInfo;
  }

  /**
   * Override this method so as to prevent the description of a constructor's
   * parameter being @code{null}.  Open MBeans can not have @code{null} descriptions,
   * but one will occur as the names of parameters aren't stored for reflection.
   * 
   * @param constructor the constructor whose parameter needs describing.
   * @param parameter the parameter to be described.
   * @param sequenceNo the number of the parameter to describe.
   * @return a description of the constructor's parameter.
   */
  protected String getDescription(MBeanConstructorInfo constructor,
				  MBeanParameterInfo parameter,
				  int sequenceNo)
  {
    String desc = parameter.getDescription();
    if (desc == null)
      return "param" + sequenceNo;
    else
      return desc;
  }

  /**
   * Override this method so as to prevent the description of an operation's
   * parameter being @code{null}.  Open MBeans can not have @code{null} descriptions,
   * but one will occur as the names of parameters aren't stored for reflection.
   * 
   * @param operation the operation whose parameter needs describing.
   * @param parameter the parameter to be described.
   * @param sequenceNo the number of the parameter to describe.
   * @return a description of the operation's parameter.
   */
  protected String getDescription(MBeanOperationInfo operation,
				  MBeanParameterInfo parameter,
				  int sequenceNo)
  {
    String desc = parameter.getDescription();
    if (desc == null)
      return "param" + sequenceNo;
    else
      return desc;
  }

  /**
   * Override this method so as to prevent the name of a constructor's
   * parameter being @code{null}.  Open MBeans can not have @code{null} names,
   * but one will occur as the names of parameters aren't stored for reflection.
   * 
   * @param constructor the constructor whose parameter needs a name.
   * @param parameter the parameter to be named.
   * @param sequenceNo the number of the parameter to name.
   * @return a description of the constructor's parameter.
   */
  protected String getParameterName(MBeanConstructorInfo constructor,
				    MBeanParameterInfo parameter,
				    int sequenceNo)
  {
    String name = parameter.getName();
    if (name == null)
      return "param" + sequenceNo;
    else
      return name;
  }

  /**
   * Override this method so as to prevent the name of an operation's
   * parameter being @code{null}.  Open MBeans can not have @code{null} names,
   * but one will occur as the names of parameters aren't stored for reflection.
   * 
   * @param operation the operation whose parameter needs a name.
   * @param parameter the parameter to be named.
   * @param sequenceNo the number of the parameter to name.
   * @return a description of the operation's parameter.
   */
  protected String getParameterName(MBeanOperationInfo operation,
				    MBeanParameterInfo parameter,
				    int sequenceNo)
  {
    String name = parameter.getName();
    if (name == null)
      return "param" + sequenceNo;
    else
      return name;
  }

  public MBeanInfo getMBeanInfo()
  {
    super.getMBeanInfo();
    return getCachedMBeanInfo();
  }

  private OpenMBeanParameterInfo[] translateSignature(MBeanParameterInfo[] oldS)
    throws OpenDataException
  {
    OpenMBeanParameterInfo[] sig = new OpenMBeanParameterInfoSupport[oldS.length];
    for (int a = 0; a < oldS.length; ++a)
      {
	OpenMBeanParameterInfo param = Translator.translate(oldS[a].getType());
	if (param.getMinValue() == null)
	  {
	    Object[] lv;
	    if (param.getLegalValues() == null)
	      lv = null;
	    else
	      lv = param.getLegalValues().toArray();
	    sig[a] = new OpenMBeanParameterInfoSupport(oldS[a].getName(),
						       oldS[a].getDescription(),
						       ((OpenType<Object>)
							param.getOpenType()),
						       param.getDefaultValue(),
						       lv);
	  }
	else
	  sig[a] = new OpenMBeanParameterInfoSupport(oldS[a].getName(),
						     oldS[a].getDescription(),
						     ((OpenType<Object>)
						      param.getOpenType()),
						     param.getDefaultValue(),
						     ((Comparable<Object>)
						      param.getMinValue()),
						     ((Comparable<Object>)
						      param.getMaxValue()));
      }
    return sig;
  }


}
