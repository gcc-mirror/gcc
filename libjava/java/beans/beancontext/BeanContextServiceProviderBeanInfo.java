/* java.beans.beancontext.BeanContextServiceProviderBeanInfo
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


package java.beans.beancontext;

import java.beans.BeanInfo;

/**
 * <code>BeanContextServiceProvider</code>s implement this to provide information about all of the services they provide.
 * <P>
 *
 * This is apparently so that you can import a bunch of services into a
 * RAD tool and it will know about all of them and export them to the
 * user in a readable manner.
 *
 * @author John Keiser
 * @since JDK1.2
 */
public interface BeanContextServiceProviderBeanInfo extends BeanInfo {
	/**
	 * Get <code>BeanInfo</code>s for all of the service classes of this <code>BeanInfoServiceProvider</code>.
	 * @return <code>BeanInfo</code>s for all provided service classes.
	 */
	public BeanInfo[] getServicesBeanInfo();
}
