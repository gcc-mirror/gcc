/* gnu.java.beans.BeanInfoEmbryo
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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package gnu.java.beans;

import java.beans.*;
import java.util.*;
import gnu.java.lang.*;
import java.lang.reflect.*;

/**
 ** A BeanInfoEmbryo accumulates information about a Bean
 ** while it is in the process of being created, and then
 ** when you are done accumulating the information, the
 ** getBeanInfo() method may be called to create a BeanInfo
 ** object based on the information.<P>
 **
 ** This class is not well-synchronized.  (It can be, it
 ** just isn't yet.)
 **
 ** @author John Keiser
 ** @version 1.1.0, 30 Jul 1998
 ** @see java.beans.BeanInfo
 **/

public class BeanInfoEmbryo {
	Hashtable properties = new Hashtable();
	Hashtable events = new Hashtable();
	Vector methods = new Vector();

	BeanDescriptor beanDescriptor;
	BeanInfo[] additionalBeanInfo;
	java.awt.Image[] im;
	String defaultPropertyName;
	String defaultEventName;

	public BeanInfoEmbryo() {
	}

	public BeanInfo getBeanInfo() {
		int defaultProperty = -1;
		int defaultEvent = -1;

		PropertyDescriptor[] Aproperties = new PropertyDescriptor[properties.size()];
		int i = 0;
		Enumeration enum = properties.elements();
		while(enum.hasMoreElements()) {
			Aproperties[i] = (PropertyDescriptor)enum.nextElement();
			if(defaultPropertyName != null && Aproperties[i].getName().equals(defaultPropertyName)) {
				defaultProperty = i;
			}
			i++;
		}

		EventSetDescriptor[] Aevents = new EventSetDescriptor[events.size()];
		i = 0;
		enum = events.elements();
		while(enum.hasMoreElements()) {
			Aevents[i] = (EventSetDescriptor)enum.nextElement();
			if(defaultEventName != null && Aevents[i].getName().equals(defaultEventName)) {
				defaultEvent = i;
			}
			i++;
		}

		MethodDescriptor[] Amethods = new MethodDescriptor[methods.size()];
		methods.copyInto(Amethods);

		return new ExplicitBeanInfo(beanDescriptor,additionalBeanInfo,Aproperties,defaultProperty,Aevents,defaultEvent,Amethods,im);
	}

	public void setBeanDescriptor(BeanDescriptor b) {
		beanDescriptor = b;
	}

	public void setAdditionalBeanInfo(BeanInfo[] b) {
		additionalBeanInfo = b;
	}

	public boolean hasProperty(PropertyDescriptor p) {
		return properties.get(p.getName()) != null;
	}
	public void addProperty(PropertyDescriptor p) {
		properties.put(p.getName(),p);
	}
	public void addIndexedProperty(IndexedPropertyDescriptor p) {
		properties.put(p.getName(),p);
	}

	public boolean hasEvent(EventSetDescriptor e) {
		return events.get(e.getName()) != null;
	}
	public void addEvent(EventSetDescriptor e) {
		events.put(e.getName(),e);
	}

	public boolean hasMethod(MethodDescriptor m) {
		for(int i=0;i<methods.size();i++) {
			Method thisMethod = ((MethodDescriptor)methods.elementAt(i)).getMethod();
			if(m.getMethod().getName().equals(thisMethod.getName())
			   && ArrayHelper.equalsArray(m.getMethod().getParameterTypes(), thisMethod.getParameterTypes())) {
				return true;
			}
		}
		return false;
	}
	public void addMethod(MethodDescriptor m) {
		methods.addElement(m);
	}

	public void setDefaultPropertyName(String defaultPropertyName) {
		this.defaultPropertyName = defaultPropertyName;
	}

	public void setDefaultEventName(String defaultEventName) {
		this.defaultEventName = defaultEventName;
	}

	public void setIcons(java.awt.Image[] im) {
		this.im = im;
	}
}
