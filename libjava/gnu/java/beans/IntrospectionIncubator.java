/* gnu.java.beans.IntrospectionIncubator
   Copyright (C) 1998 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package gnu.java.beans;

import java.beans.*;
import java.util.*;
import java.lang.reflect.*;
import gnu.java.lang.*;

/**
 ** IntrospectionIncubator takes in a bunch of Methods, and
 ** Introspects only those Methods you give it.
 **
 ** @author John Keiser
 ** @version 1.1.0, 30 Jul 1998
 ** @see gnu.java.beans.ExplicitBeanInfo
 ** @see java.beans.BeanInfo
 **/

public class IntrospectionIncubator {
	Hashtable propertyMethods = new Hashtable();
	Hashtable listenerMethods = new Hashtable();
	Vector otherMethods = new Vector();

	Class propertyStopClass;
	Class eventStopClass;
	Class methodStopClass;

	public IntrospectionIncubator() {
	}

	/* Paving the way for automatic Introspection */
	public void addMethod(Method method) {
		if(Modifier.isPublic(method.getModifiers())) {
			String name = ClassHelper.getTruncatedName(method.getName());
			Class retType = method.getReturnType();
			Class[] params = method.getParameterTypes();
			boolean isVoid = retType.equals(java.lang.Void.TYPE);
			Class methodClass = method.getDeclaringClass();
			if(propertyStopClass == null || (propertyStopClass.isAssignableFrom(methodClass) && !propertyStopClass.equals(methodClass))) {
				if(name.startsWith("is")
				   && retType.equals(java.lang.Boolean.TYPE)
				   && params.length == 0) {
					addToPropertyHash(name,method,IS);
				} else if(name.startsWith("get") && !isVoid) {
					if(params.length == 0) {
						addToPropertyHash(name,method,GET);
					} else if(params.length == 1 && params[0].equals(java.lang.Integer.TYPE)) {
						addToPropertyHash(name,method,GET_I);
					} else {
						otherMethods.addElement(method);
					}
				} else if(name.startsWith("set") && isVoid) {
					if(params.length == 1) {
						addToPropertyHash(name,method,SET);
					} else if(params.length == 2 && params[0].equals(java.lang.Integer.TYPE)) {
						addToPropertyHash(name,method,SET_I);
					} else {
						otherMethods.addElement(method);
					}
				}
			}
			if(eventStopClass == null || (eventStopClass.isAssignableFrom(methodClass) && !eventStopClass.equals(methodClass))) {
				if(name.startsWith("add")
				          && isVoid
				          && params.length == 1
				          && java.util.EventListener.class.isAssignableFrom(params[0])) {
					addToListenerHash(name,method,ADD);
				} else if(name.startsWith("remove")
				          && isVoid
				          && params.length == 1
				          && java.util.EventListener.class.isAssignableFrom(params[0])) {
					addToListenerHash(name,method,REMOVE);
				}
			}
			if(methodStopClass == null || (methodStopClass.isAssignableFrom(methodClass) && !methodStopClass.equals(methodClass))) {
				otherMethods.addElement(method);
			}
		}
	}

	public void addMethods(Method[] m) {
		for(int i=0;i<m.length;i++) {
			addMethod(m[i]);
		}
	}

	public void setPropertyStopClass(Class c) {
		propertyStopClass = c;
	}

	public void setEventStopClass(Class c) {
		eventStopClass = c;
	}

	public void setMethodStopClass(Class c) {
		methodStopClass = c;
	}


	public BeanInfoEmbryo getBeanInfoEmbryo() throws IntrospectionException {
		BeanInfoEmbryo b = new BeanInfoEmbryo();
		findXXX(b,IS);
		findXXXInt(b,GET_I);
		findXXXInt(b,SET_I);
		findXXX(b,GET);
		findXXX(b,SET);
		findAddRemovePairs(b);
		for(int i=0;i<otherMethods.size();i++) {
			MethodDescriptor newMethod = new MethodDescriptor((Method)otherMethods.elementAt(i));
			if(!b.hasMethod(newMethod)) {
				b.addMethod(new MethodDescriptor((Method)otherMethods.elementAt(i)));
			}
		}
		return b;
	}

	public BeanInfo getBeanInfo() throws IntrospectionException {
		return getBeanInfoEmbryo().getBeanInfo();
	}


	void findAddRemovePairs(BeanInfoEmbryo b) throws IntrospectionException {
		Enumeration listenerEnum = listenerMethods.keys();
		while(listenerEnum.hasMoreElements()) {
			DoubleKey k = (DoubleKey)listenerEnum.nextElement();
			Method[] m = (Method[])listenerMethods.get(k);
			if(m[ADD] != null && m[REMOVE] != null) {
				EventSetDescriptor e = new EventSetDescriptor(Introspector.decapitalize(k.getName()),
				                                              k.getType(), k.getType().getMethods(),
				                                              m[ADD],m[REMOVE]);
				e.setUnicast(ArrayHelper.contains(m[ADD].getExceptionTypes(),java.util.TooManyListenersException.class));
				if(!b.hasEvent(e)) {
					b.addEvent(e);
				}
			}
		}
	}

	void findXXX(BeanInfoEmbryo b, int funcType) throws IntrospectionException {
		Enumeration keys = propertyMethods.keys();
		while(keys.hasMoreElements()) {
			DoubleKey k = (DoubleKey)keys.nextElement();
			Method[] m = (Method[])propertyMethods.get(k);
			if(m[funcType] != null) {
				PropertyDescriptor p = new PropertyDescriptor(Introspector.decapitalize(k.getName()),
				                                     m[IS] != null ? m[IS] : m[GET],
				                                     m[SET]);
				if(m[SET] != null) {
					p.setConstrained(ArrayHelper.contains(m[SET].getExceptionTypes(),java.beans.PropertyVetoException.class));
				}
				if(!b.hasProperty(p)) {
					b.addProperty(p);
				}
			}
		}
	}

	void findXXXInt(BeanInfoEmbryo b, int funcType) throws IntrospectionException {
		Enumeration keys = propertyMethods.keys();
		while(keys.hasMoreElements()) {
			DoubleKey k = (DoubleKey)keys.nextElement();
			Method[] m = (Method[])propertyMethods.get(k);
			if(m[funcType] != null) {
				boolean constrained;
				if(m[SET_I] != null) {
					constrained = ArrayHelper.contains(m[SET_I].getExceptionTypes(),java.beans.PropertyVetoException.class);
				} else {
					constrained = false;
				}

				/** Find out if there is an array type get or set **/
				Class arrayType = Array.newInstance(k.getType(),0).getClass();
				DoubleKey findSetArray = new DoubleKey(arrayType,k.getName());
				Method[] m2 = (Method[])propertyMethods.get(findSetArray);
				IndexedPropertyDescriptor p;
				if(m2 == null) {
					p = new IndexedPropertyDescriptor(Introspector.decapitalize(k.getName()),
				                                          null,null,
				                                          m[GET_I],m[SET_I]);
				} else {
					if(constrained && m2[SET] != null) {
						constrained = ArrayHelper.contains(m2[SET].getExceptionTypes(),java.beans.PropertyVetoException.class);
					}
					p = new IndexedPropertyDescriptor(Introspector.decapitalize(k.getName()),
				                                          m2[GET],m2[SET],
				                                          m[GET_I],m[SET_I]);
				}
				p.setConstrained(constrained);
				if(!b.hasProperty(p)) {
					b.addProperty(p);
				}
			}
		}
	}

	static final int IS=0;
	static final int GET_I=1;
	static final int SET_I=2;
	static final int GET=3;
	static final int SET=4;

	static final int ADD=0;
	static final int REMOVE=1;

	void addToPropertyHash(String name, Method method, int funcType) {
		String newName;
		Class type;

		switch(funcType) {
			case IS:
				type = java.lang.Boolean.TYPE;
				newName = name.substring(2);
				break;
			case GET_I:
				type = method.getReturnType();
				newName = name.substring(3);
				break;
			case SET_I:
				type = method.getParameterTypes()[1];
				newName = name.substring(3);
				break;
			case GET:
				type = method.getReturnType();
				newName = name.substring(3);
				break;
			case SET:
				type = method.getParameterTypes()[0];
				newName = name.substring(3);
				break;
			default:
				return;
		}
		newName = capitalize(newName);

		DoubleKey k = new DoubleKey(type,newName);
		Method[] methods = (Method[])propertyMethods.get(k);
		if(methods == null) {
			methods = new Method[5];
			propertyMethods.put(k,methods);
		}
		methods[funcType] = method;
	}


	void addToListenerHash(String name, Method method, int funcType) {
		String newName;
		Class type;

		switch(funcType) {
			case ADD:
				type = method.getParameterTypes()[0];
				newName = name.substring(3,name.length()-8);
				break;
			case REMOVE:
				type = method.getParameterTypes()[0];
				newName = name.substring(6,name.length()-8);
				break;
			default:
				return;
		}
		newName = capitalize(newName);

		DoubleKey k = new DoubleKey(type,newName);
		Method[] methods = (Method[])listenerMethods.get(k);
		if(methods == null) {
			methods = new Method[2];
			listenerMethods.put(k,methods);
		}
		methods[funcType] = method;
	}

	static String capitalize(String name) {
		try {
			if(Character.isUpperCase(name.charAt(0))) {
				return name;
			} else {
				char[] c = name.toCharArray();
				c[0] = Character.toLowerCase(c[0]);
				return new String(c);
			}
		} catch(StringIndexOutOfBoundsException E) {
			return name;
		} catch(NullPointerException E) {
			return null;
		}
	}
}

class DoubleKey {
	Class type;
	String name;

	DoubleKey(Class type, String name) {
		this.type = type;
		this.name = name;
	}

	Class getType() {
		return type;
	}

	String getName() {
		return name;
	}

	public boolean equals(Object o) {
		if(o instanceof DoubleKey) {
			DoubleKey d = (DoubleKey)o;
			return d.type.equals(type) && d.name.equals(name);
		} else {
			return false;
		}
	}

	public int hashCode() {
		return type.hashCode() ^ name.hashCode();
	}
}
