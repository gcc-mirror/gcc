/* java.beans.BeanInfo
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


package java.beans;

/**
 ** BeanInfo can be implemented in order to provide explicit information to the Introspector.
 **
 ** When you write a BeanInfo class, you implement this interface
 ** and provide explicit information by returning a non-null
 ** value from the appropriate method.  If you wish the
 ** Introspector to determine certain information in the normal
 ** way, just return null (or in the case of int methods, return
 ** -1).  There is a class called SimpleBeanInfo which returns
 ** null from all methods, which you may extend and only
 ** override the methods you wish to override.<P>
 **
 ** When you have written the class, give it the name
 ** <CODE>&lt;Bean Class Name&gt;BeanInfo</CODE> and place it in
 ** the same package as the Bean, or in the bean info search path
 ** (see Introspector for information on search paths).<P>
 **
 ** A simple note about the way the Introspector interacts with
 ** BeanInfo.  Introspectors look at a Bean class and determine
 ** if there is a BeanInfo class with it.  If there is not a
 ** BeanInfo class, it will behave as if the BeanInfo class
 ** provided was a SimpleBeanInfo class (i.e. it will determine
 ** all information automatically).<P>If there is a BeanInfo
 ** class, then any methods that do *not* return null are
 ** regarded as providing definitive information about the class
 ** and all of its superclasses for those information types.
 ** Even if a parent BeanInfo class explicitly returns that
 ** information, it will not be used.
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 28 Jul 1998
 **/

public interface BeanInfo {
	/** Use this as a parameter for the getIcon() command to retrieve a certain type of icon. **/
	public static int ICON_COLOR_16x16 = 1;
	/** Use this as a parameter for the getIcon() command to retrieve a certain type of icon. **/
	public static int ICON_COLOR_32x32 = 2;
	/** Use this as a parameter for the getIcon() command to retrieve a certain type of icon. **/
	public static int ICON_MONO_16x16 = 3;
	/** Use this as a parameter for the getIcon() command to retrieve a certain type of icon. **/
	public static int ICON_MONO_32x32 = 4;

	/** Get the general description of this Bean type.
	 ** @return the BeanDescriptor for the Bean, or null if
	 **         the BeanDescriptor should be obtained by
	 **         Introspection.
	 **/
	public abstract BeanDescriptor getBeanDescriptor();

	/** Get the events this Bean type fires.
	 ** @return the EventDescriptors representing events this
	 **         Bean fires.  Returns <CODE>null</CODE> if the
	 **         events are to be acquired by Introspection.
	 **/
	public abstract EventSetDescriptor[] getEventSetDescriptors();

	/** Get the "default" event, basically the one a RAD tool
	 ** user is most likely to select.
	 ** @return the index into the getEventSetDescriptors()
	 **         that the user is most likely to use.  Returns
	 **         <CODE>-1</CODE> if there is no default event.
	 **/
	public abstract int getDefaultEventIndex();

	/** Get the properties (get/set method pairs) this Bean
	 ** type supports.
	 ** @return the PropertyDescriptors representing the
	 **         properties this Bean type supports.
	 **         Returns <CODE>null</CODE> if the properties
	 **         are to be obtained by Introspection.
	 **/
	public abstract PropertyDescriptor[] getPropertyDescriptors();

	/** Get the "default" property, basically the one a RAD
	 ** tool user is most likely to select.
	 ** @return the index into the getPropertyDescriptors()
	 **         that the user is most likely to use.  Returns
	 **         <CODE>-1</CODE> if there is no default event.
	 **/
	public abstract int getDefaultPropertyIndex();

	/** Get the methods this Bean type supports.
	 ** @return the MethodDescriptors representing the
	 **         methods this Bean type supports.  Returns
	 **         <CODE>null</CODE> if the methods are to be
	 **         obtained by Introspection.
	 **/
	public abstract MethodDescriptor[] getMethodDescriptors();

	/** Get additional BeanInfos representing this Bean.
	 ** In this version of JavaBeans, this method is used so
	 ** that space and time can be saved by reading a BeanInfo
	 ** for each class in the hierarchy (super, super(super),
	 ** and so on).<P>
	 **
	 ** The order of precedence when two pieces of BeanInfo
	 ** conflict (such as two PropertyDescriptors that have
	 ** the same name), in order from highest precedence to
	 ** lowest, is:
	 ** <OL>
	 ** <LI>This BeanInfo object.</LI>
	 ** <LI><CODE>getAdditionalBeanInfo()[getAdditionalBeanInfo().length]</CODE></LI>
	 ** <LI> ... </LI>
	 ** <LI><CODE>getAdditionalBeanInfo()[1]</CODE></LI>
	 ** <LI><CODE>getAdditionalBeanInfo()[0]</CODE></LI>
	 ** </OL><P>
	 **
	 ** <STRONG>Spec Note:</STRONG> It is possible that
	 ** returning <CODE>null</CODE> from this method could
	 ** stop Introspection in its tracks, but it is unclear
	 ** from the spec whether this is the case.
	 **
	 ** @return additional BeanInfos representing this Bean.
	 **         <CODE>null</CODE> may be returned (see Spec
	 **         Note, above).
	 **/
	public abstract BeanInfo[] getAdditionalBeanInfo();

	/** Get a visual icon for this Bean.
	 ** A Bean does not have to support icons, and if it does
	 ** support icons, it does not have to support every single
	 ** type.  Sun recommends that if you only support one
	 ** type, you support 16x16 color.  Sun also notes that you
	 ** should try to use a type (like GIF) that allows for
	 ** transparent pixels, so that the background of the RAD
	 ** tool can show through.<P>
	 **
	 ** <STRONG>Spec Note:</STRONG> If you do not support the
	 ** type of icon that is being asked for, but you do
	 ** support another type, it is unclear whether you should
	 ** return the other type or not.  I would presume not.
	 **
	 ** @param iconType the type of icon to get (see the
	 **        ICON_* constants in this class).
	 ** @return the icon, or null if that type of icon is
	 **         unsupported by this Bean.
	 **/
	public abstract java.awt.Image getIcon(int iconType);
}
