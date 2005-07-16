/* ExplicitBeanInfo.java --
   Copyright (C) 1998, 2004  Free Software Foundation, Inc.

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


package gnu.java.beans;

import java.awt.Image;
import java.beans.BeanDescriptor;
import java.beans.BeanInfo;
import java.beans.EventSetDescriptor;
import java.beans.MethodDescriptor;
import java.beans.PropertyDescriptor;

/**
 ** ExplicitBeanInfo lets you specify in the constructor
 ** all the various parts of the BeanInfo.
 **
 ** @author John Keiser
 ** @version 1.1.0, 30 Jul 1998
 ** @see java.beans.BeanInfo
 **/

public class ExplicitBeanInfo implements BeanInfo {
	/** The BeanDescriptor returned by getBeanDescriptor. **/
	protected BeanDescriptor beanDescriptor;

	/** The EventSetDescriptor array returned by
	 ** getEventSetDescriptors().
	 **/
	protected EventSetDescriptor[] eventSetDescriptors = new EventSetDescriptor[0];

	/** The PropertyDescriptor array returned by
	 ** getPropertyDescriptors().
	 **/
	protected PropertyDescriptor[] propertyDescriptors = new PropertyDescriptor[0];

	/** The MethodDescriptor array returned by
	 ** getMethodDescriptors().
	 **/
	protected MethodDescriptor[]   methodDescriptors;

	/** The default property index. **/
	protected int defaultPropertyIndex;

	/** The default event index. **/
	protected int defaultEventIndex;

	/** The BeanInfo array returned by
	 ** getAdditionalBeanInfo().
	 **/
	protected BeanInfo[] additionalBeanInfo;

	/** The set of icons. **/
	protected Image[] icons;

	public ExplicitBeanInfo(BeanDescriptor beanDescriptor,
	                        BeanInfo[] additionalBeanInfo,
	                        PropertyDescriptor[] propertyDescriptors,
				int defaultPropertyIndex,
	                        EventSetDescriptor[] eventSetDescriptors,
				int defaultEventIndex,
	                        MethodDescriptor[] methodDescriptors,
				Image[] icons) {
		this.beanDescriptor = beanDescriptor;
		this.additionalBeanInfo = additionalBeanInfo;
		this.propertyDescriptors = propertyDescriptors;
		this.defaultPropertyIndex = defaultPropertyIndex;
		this.eventSetDescriptors = eventSetDescriptors;
		this.defaultEventIndex = defaultEventIndex;
		this.methodDescriptors = methodDescriptors;
		this.icons = icons;
	}

	/** Get Bean descriptor. **/
	public BeanDescriptor getBeanDescriptor() {
		return beanDescriptor;
	}

	/** Get Bean events. **/
	public EventSetDescriptor[] getEventSetDescriptors() {
		return eventSetDescriptors;
	}

	/** Get default event set. **/
	public int getDefaultEventIndex() {
		return defaultEventIndex;
	}

	/** Get Bean properties. **/
	public PropertyDescriptor[] getPropertyDescriptors() {
		return propertyDescriptors;
	}

	/** Get "default" property. **/
	public int getDefaultPropertyIndex() {
		return defaultPropertyIndex;
	}

	/** Get Bean methods. **/
	public MethodDescriptor[] getMethodDescriptors() {
		return methodDescriptors;
	}

	/** Get additional Bean info. **/
	public BeanInfo[] getAdditionalBeanInfo() {
		return additionalBeanInfo;
	}

	/** Get Bean icons.
	 ** @param iconType the type of icon
	 **/
	public Image getIcon(int iconType) {
		return icons != null ? icons[iconType - 1] : null;
	}
}
