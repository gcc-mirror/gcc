/* java.beans.SimpleBeanInfo
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


package java.beans;

import java.awt.*;

/**
 ** SimpleBeanInfo is a class you may extend to more easily
 ** provide select information to the Introspector.  It
 ** implements all of the methods in BeanInfo by returning
 ** null and forces the Introspector to behave exactly as
 ** if there were no BeanInfo class at all (Introspecting
 ** everything).<P>
 **
 ** Overriding one or two of these functions
 ** to give explicit information on only those things you
 ** wish to give explicit information is perfectly safe,
 ** and even desirable.<P>
 **
 ** See the BeanInfo class for information on what the
 ** various methods actually do.
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 29 Jul 1998
 ** @see java.beans.BeanInfo
 **/

public class SimpleBeanInfo implements BeanInfo {
	/** Force Introspection of the general bean info.
	 ** @return <CODE>null</CODE>.
	 **/
	public BeanDescriptor getBeanDescriptor() {
		return null;
	}

	/** Force Introspection of the events this Bean type
	 ** fires.
	 ** @return <CODE>null</CODE>
	 **/
	public EventSetDescriptor[] getEventSetDescriptors() {
		return null;
	}

	/** Say that there is no "default" event set.
	 ** @return <CODE>-1</CODE>.
	 **/
	public int getDefaultEventIndex() {
		return -1;
	}

	/** Force Introspection of the Bean properties.
	 ** @return <CODE>null</CODE>.
	 **/
	public PropertyDescriptor[] getPropertyDescriptors() {
		return null;
	}

	/** Say that there is no "default" property.
	 ** @return <CODE>-1</CODE>.
	 **/
	public int getDefaultPropertyIndex() {
		return -1;
	}

	/** Force Introspection of the Bean's methods.
	 ** @return <CODE>null</CODE>.
	 **/
	public MethodDescriptor[] getMethodDescriptors() {
		return null;
	}

	/** Tell the Introspector to go look for other BeanInfo
	 ** itself.
	 ** @return <CODE>null</CODE>.
	 **/
	public BeanInfo[] getAdditionalBeanInfo() {
		return null;
	}

	/** Say that this Bean has no icons.
	 ** @param iconType the type of icon
	 ** @return <CODE>null</CODE>.
	 **/
	public Image getIcon(int iconType) {
		return null;
	}

	/** Helper method to load an image using the Bean class
	 ** getResource() method on the BeanInfo class (using
	 ** getClass(), since you'll extend this class to get
	 ** the BeanInfo).  Basically it's assumed that the Bean
	 ** and its BeanInfo are both loaded by the same
	 ** ClassLoader, generally a reasonable assumption.
	 ** @param location the URL relative
	 ** @return the Image in question.
	 **/
	public Image loadImage(String location) {
		return Toolkit.getDefaultToolkit().getImage(getClass().getResource(location));
	}
}

