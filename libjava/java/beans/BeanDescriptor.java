/* java.beans.BeanDescriptor
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

import java.util.*;

/**
 ** BeanDescriptor describes general information about a Bean, plus
 ** stores the Bean's Class and it's customizer's Class.<P>
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 31 May 1998
 **/

public class BeanDescriptor extends FeatureDescriptor {
	Class beanClass;
	Class customizerClass;

	/** Create a new BeanDescriptor with the given beanClass and
	 ** no customizer class.
	 ** @param beanClass the class of the Bean.
	 **/
	public BeanDescriptor(Class beanClass) {
		this(beanClass,null);
	}

	/** Create a new BeanDescriptor with the given bean class and
	 ** customizer class.
	 ** @param beanClass the class of the Bean.
	 ** @param customizerClass the class of the Bean's Customizer.
	 **/
	public BeanDescriptor(Class beanClass, Class customizerClass) {
		this.beanClass = beanClass;
		this.customizerClass = customizerClass;
	}

	/** Get the Bean's class. **/
	public Class getBeanClass() {
		return beanClass;
	}

	/** Get the Bean's customizer's class. **/
	public Class getCustomizerClass() {
		return customizerClass;
	}
}
