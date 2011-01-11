/* java.beans.DesignMode
   Copyright (C) 1999, 2006, Free Software Foundation, Inc.

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


package java.beans;

/**
 * <code>BeanContextChild</code> implementors implement this to get information
 * about whether they are in a design time or runtime environment.
 * The reason this is restricted to <code>BeanContextChild</code>ren is that
 * only things in the <code>BeanContext</code> hierarchy are given this
 * information in the first place.
 *
 * @author John Keiser
 * @since JDK1.2
 * @see java.beans.beancontext.BeanContextChild
 */
public interface DesignMode
{

  /**
   * Use this name when firing <code>PropertyChangeEvent</code>s from your Bean.
   */
  String PROPERTYNAME = "designTime";

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
   * @see java.beans.PropertyChangeEvent
   * @see java.beans.beancontext.BeanContext
   * @see #PROPERTYNAME
   */
  void setDesignTime(boolean designTime);

  /**
   * This method should tell whether it is design time or runtime.
   * @return <code>true</code> if design time, <code>false</code> if
   *         runtime.
   */
  boolean isDesignTime();

}
