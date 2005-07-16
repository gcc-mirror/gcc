/* java.beans.beancontext.BeanContextProxy
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


package java.beans.beancontext;

/**
 * Beans that wish to have a <code>BeanContextChild</code> or <code>BeanContext</code> associated with them
 * but do not wish to implement those interfaces directly, can implement this interface.
 * <P>
 *
 * Don't shoot yourself in the foot: if you already implement
 * <code>BeanContextChild</code>, directly or indirectly, the whole
 * workings of this package will be unpredictable because it is
 * indeterminate as to whether the <code>BeanContextChild</code> is used
 * in preference to its proxy or vice versa.
 *
 * @author John Keiser
 * @since JDK1.2
 */

public interface BeanContextProxy {
	/**
	 * Return the <code>BeanContextChild</code> associated with this
	 * <code>Object</code>.
	 *
	 * @return the <code>BeanContextChild</code> associated with this
	 * <code>Object</code>.
	 */
	BeanContextChild getBeanContextProxy();
}
