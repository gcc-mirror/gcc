/* gnu.java.beans.EmptyBeanInfo
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

/**
 ** EmptyBeanInfo is a BeanInfo that discloses no
 ** information about the Bean and does not allow
 ** Introspection.  The Introspector uses instances of this
 ** class to create empty BeanInfos, but it could also be
 ** used as a base class for BeanInfos that do not allow
 ** Introspection and provide only a little bit of
 ** information.<P>
 **
 ** @author John Keiser
 ** @version 1.1.0, 30 Jul 1998
 ** @see gnu.java.beans.ExplicitBeanInfo
 ** @see java.beans.BeanInfo
 **/

public class EmptyBeanInfo extends ExplicitBeanInfo {
	/** Create a new EmptyBeanInfo. **/
	public EmptyBeanInfo(Class beanClass) {
		super(new BeanDescriptor(beanClass,null),
		      new BeanInfo[0],
		      new PropertyDescriptor[0],
		      -1,
		      new EventSetDescriptor[0],
		      -1,
		      new MethodDescriptor[0],
		      null);
	}
}
