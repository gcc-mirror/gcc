/* java.beans.MethodDescriptor
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

import java.lang.reflect.Method;

/** MethodDescriptor describes information about a JavaBeans method.
 ** It's a fairly straightforward class (at least something in this
 ** package is straightforward!).
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 26 Jul 1998
 **/
public class MethodDescriptor extends FeatureDescriptor {
	private Method m;
	private ParameterDescriptor[] parameterDescriptors;

	/** Create a new MethodDescriptor.
	 ** This method sets the name to the name of the method (Method.getName()).
	 ** @param m the method it will represent.
	 **/
	public MethodDescriptor(Method m) {
		setName(m.getName());
		this.m = m;
	}

	/** Create a new MethodDescriptor.
	 ** This method sets the name to the name of the method (Method.getName()).
	 ** @param m the method it will represent.
	 ** @param parameterDescriptors descriptions of the parameters (especially names).
	 **/
	public MethodDescriptor(Method m, ParameterDescriptor[] parameterDescriptors) {
		setName(m.getName());
		this.m = m;
		this.parameterDescriptors = parameterDescriptors;
	}

	/** Get the parameter descriptors from this method.
	 ** Since MethodDescriptor has no way of determining what
	 ** the parameter names were, this defaults to null.
	 **/
	public ParameterDescriptor[] getParameterDescriptors() {
		return parameterDescriptors;
	}

	/** Get the method this MethodDescriptor represents. **/
	public Method getMethod() {
		return m;
	}
}

