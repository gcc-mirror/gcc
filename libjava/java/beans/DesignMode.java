/* java.beans.DesignMode
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/**
 * <code>BeanContextChild</code> implementors implement this to get information about whether they are in a design time or runtime environment.
 * The reason this is restricted to <code>BeanContextChild</code>ren is that
 * only things in the <code>BeanContext</code> hierarchy are given this
 * information in the first place.
 *
 * @author John Keiser
 * @since JDK1.2
 * @see java.beans.beancontext.BeanContextChild
 */

public interface DesignMode {
	/**
	 * Use this name when firing <code>PropertyChangeEvent</code>s from your Bean.  
	 * @fixme Check whether PROPERTYNAME is set to same value as Sun.
	 */
	public static final String PROPERTYNAME = "designTime";

	/**
	 * The environment will call this method on your
	 * <code>BeanContextChild</code> when it is registered in a parent
	 * <code>BeanContext</code> or when behavior needs to switch from
	 * design time to runtime behavior (or vice versa).
	 * <P>
	 *
	 * <code>BeanContext</code>s are required to fire
	 * <code>PropertyChangeEvent</code>s when properties change.
	 * <code>designTime</code> is a property, and therefore when you
	 * implement <code>setDesignTime()</code>, you need to fire a
	 * <code>PropertyChangeEvent</code> with the old value, the new
	 * value and using <code>PROPERTYNAME</code> as the property name.
	 *
	 * @param designTime the new value of design time,
	 *        <code>true</code> if it is design time,
	 *        <code>false</code> if it is runtime.
	 *
	 * @fixme I'm frankly not really sure whether it's the case that
	 *        the BeanContext can <em>change</em> the status of the Bean from
	 *        design time to runtime.  But it appears that it may be so.
	 *
	 * @see java.util.PropertyChangeEvent
	 * @see java.beans.beancontext.BeanContext
	 * @see #PROPERTYNAME
	 */
	public void setDesignTime(boolean designTime);

	/**
	 * This method should tell whether it is design time or runtime.
	 * @return <code>true</code> if design time, <code>false</code> if
	 *         runtime.
	 */
	public boolean isDesignTime();
}
