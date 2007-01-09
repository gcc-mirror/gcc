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

import java.lang.management.ManagementPermission;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.TypeVariable;

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
import javax.management.MBeanInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.ReflectionException;
import javax.management.StandardMBean;

import javax.management.openmbean.ArrayType;
import javax.management.openmbean.CompositeData;
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
import javax.management.openmbean.SimpleType;
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
	    OpenMBeanParameterInfo param = translate(oldA[a].getType());
	    if (param.getMinValue() == null)
	      {
		Object[] lv;
		if (param.getLegalValues() == null)
		  lv = null;
		else
		  lv = param.getLegalValues().toArray();
		attribs[a] = new OpenMBeanAttributeInfoSupport(oldA[a].getName(),
							       oldA[a].getDescription(),
							       param.getOpenType(),
							       oldA[a].isReadable(),
							       oldA[a].isWritable(),
							       oldA[a].isIs(),
							       param.getDefaultValue(),
							       lv);
	      }
	    else
	      attribs[a] = new OpenMBeanAttributeInfoSupport(oldA[a].getName(),
							     oldA[a].getDescription(),
							     param.getOpenType(),
							     oldA[a].isReadable(),
							     oldA[a].isWritable(),
							     oldA[a].isIs(),
							     param.getDefaultValue(),
							     param.getMinValue(),
							     param.getMaxValue());
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
					  translate(oldO[a].getReturnType()).getOpenType(),
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
      return value;
    String cName = vClass.getName();
    String[] allowedTypes = OpenType.ALLOWED_CLASSNAMES;
    for (int a = 0; a < allowedTypes.length; ++a)
      if (cName.equals(allowedTypes[a]))
	return value;
    if (value instanceof List)
      {
	List l = (List) value;
	Class e = null;
	TypeVariable[] vars = vClass.getTypeParameters();
	for (int a = 0; a < vars.length; ++a)
	  if (vars[a].getName().equals("E"))
	    e = (Class) vars[a].getGenericDeclaration();
	if (e == null)
	  e = Object.class;
	Object[] array = (Object[]) Array.newInstance(e, l.size());
	return l.toArray(array);
      }
    OpenMBeanInfo info = (OpenMBeanInfo) getMBeanInfo();
    OpenMBeanAttributeInfo[] attribs =
      (OpenMBeanAttributeInfo[]) info.getAttributes();
    OpenType type = null;
    for (int a = 0; a < attribs.length; ++a)
      if (attribs[a].getName().equals("attribute"))
	type = attribs[a].getOpenType();
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
	    getter = vClass.getMethod("get" + field, null);
	  }
	catch (NoSuchMethodException e)
	  {
	    /* Ignored; the type tells us it's there. */
	  }
	try
	  {
	    values.add(getter.invoke(value, null));
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

  public MBeanInfo getMBeanInfo()
  {
    super.getMBeanInfo();
    return getCachedMBeanInfo();
  }

  private OpenType getTypeFromClass(Class c)
    throws OpenDataException
  {
    return translate(c.getName()).getOpenType();
  }

  private OpenMBeanParameterInfo[] translateSignature(MBeanParameterInfo[] oldS)
    throws OpenDataException
  {
    OpenMBeanParameterInfo[] sig = new OpenMBeanParameterInfoSupport[oldS.length];
    for (int a = 0; a < oldS.length; ++a)
      {
	OpenMBeanParameterInfo param = translate(oldS[a].getType());
	if (param.getMinValue() == null)
	  {
	    Object[] lv;
	    if (param.getLegalValues() == null)
	      lv = null;
	    else
	      lv = param.getLegalValues().toArray();
	    sig[a] = new OpenMBeanParameterInfoSupport(oldS[a].getName(),
						       oldS[a].getDescription(),
						       param.getOpenType(),
						       param.getDefaultValue(),
						       lv);
	  }
	else
	  sig[a] = new OpenMBeanParameterInfoSupport(oldS[a].getName(),
						     oldS[a].getDescription(),
						     param.getOpenType(),
						     param.getDefaultValue(),
						     param.getMinValue(),
						     param.getMaxValue());
      }
    return sig;
  }

  private OpenMBeanParameterInfo translate(String type)
    throws OpenDataException
  {
    if (type.equals("boolean") || type.equals(Boolean.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.BOOLEAN,
					       null,
					       new Object[] {
						 Boolean.TRUE,
						 Boolean.FALSE
					       });
    if (type.equals("byte") || type.equals(Byte.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.BYTE,
					       null,
					       Byte.valueOf(Byte.MIN_VALUE),
					       Byte.valueOf(Byte.MAX_VALUE));
    if (type.equals("char") || type.equals(Character.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.CHARACTER,
					       null,
					       Character.valueOf(Character.MIN_VALUE),
					       Character.valueOf(Character.MAX_VALUE));
    if (type.equals("double") || type.equals(Double.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.DOUBLE,
					       null,
					       Double.valueOf(Double.MIN_VALUE),
					       Double.valueOf(Double.MAX_VALUE));
    if (type.equals("float") || type.equals(Float.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.FLOAT,
					       null,
					       Float.valueOf(Float.MIN_VALUE),
					       Float.valueOf(Float.MAX_VALUE));
    if (type.equals("int") || type.equals(Integer.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.INTEGER,
					       null,
					       Integer.valueOf(Integer.MIN_VALUE),
					       Integer.valueOf(Integer.MAX_VALUE));
    if (type.equals("long") || type.equals(Long.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.LONG,
					       null,
					       Long.valueOf(Long.MIN_VALUE),
					       Long.valueOf(Long.MAX_VALUE));
    if (type.equals("short") || type.equals(Short.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.SHORT,
					       null,
					       Short.valueOf(Short.MIN_VALUE),
					       Short.valueOf(Short.MAX_VALUE));
    if (type.equals(String.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.STRING);
    if (type.equals("void"))
      return new OpenMBeanParameterInfoSupport("TransParam",
					       "Translated parameter",
					       SimpleType.VOID);
    if (type.startsWith("java.util.Map"))
      {
	int lparam = type.indexOf("<");
	int comma = type.indexOf(",", lparam);
	int rparam = type.indexOf(">", comma);
	String key = type.substring(lparam + 1, comma).trim();
	OpenType k = translate(key).getOpenType();
	OpenType v = translate(type.substring(comma + 1, rparam).trim()).getOpenType(); 
 	CompositeType ctype = new CompositeType(Map.class.getName(), Map.class.getName(),
						new String[] { "key", "value" },
						new String[] { "Map key", "Map value"},
						new OpenType[] { k, v});
	TabularType ttype = new TabularType(key, key, ctype,
					    new String[] { "key" });
	return new OpenMBeanParameterInfoSupport("TransParam",
						 "Translated parameter",
						 ttype);
      }
    if (type.startsWith("java.util.List"))
      {
	int lparam = type.indexOf("<");
	int rparam = type.indexOf(">");
       	OpenType e = translate(type.substring(lparam + 1, rparam).trim()).getOpenType();
	return new OpenMBeanParameterInfoSupport("TransParam",
						 "Translated parameter",
						 new ArrayType(1, e)
						 );
      }	
    Class c;
    try
      {
	c = Class.forName(type);
      }
    catch (ClassNotFoundException e)
      {
	throw (InternalError)
	  (new InternalError("The class for a type used in a management bean " +
			     "could not be loaded.").initCause(e));
      }
    if (c.isEnum())
      {
	Object[] values = c.getEnumConstants();
	String[] names = new String[values.length];
	for (int a = 0; a < values.length; ++a)
	  names[a] = values[a].toString();
	return new OpenMBeanParameterInfoSupport("TransParam",
						 "Translated parameter",
						 SimpleType.STRING,
						 null,
						 (Object[]) names);
      }
    try
      {
	c.getMethod("from", new Class[] { CompositeData.class });
	Method[] methods = c.getMethods();
	List names = new ArrayList();
	List types = new ArrayList();
	for (int a = 0; a < methods.length; ++a)
	  {
	    String name = methods[a].getName();
	    if (name.startsWith("get"))
	      {
		names.add(name.substring(3));
		types.add(getTypeFromClass(methods[a].getReturnType()));
	      }
	  }
	String[] fields = (String[]) names.toArray();
	CompositeType ctype = new CompositeType(c.getName(), c.getName(),
						fields, fields,
						(OpenType[]) types.toArray());
	return new OpenMBeanParameterInfoSupport("TransParam",
						 "Translated parameter",
						 ctype);
      }
    catch (NoSuchMethodException e)
      {
	/* Ignored; we expect this if this isn't a from(CompositeData) class */
      }
    if (c.isArray())
      {
	int depth;
	for (depth = 0; c.getName().charAt(depth) == '['; ++depth);
	OpenType ot = getTypeFromClass(c.getComponentType());
	return new OpenMBeanParameterInfoSupport("TransParam",
						 "Translated parameter",
						 new ArrayType(depth, ot)
						 );
      }
    throw new InternalError("The type used does not have an open type translation.");
  }

}
