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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.beans;

import java.lang.reflect.*;

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

