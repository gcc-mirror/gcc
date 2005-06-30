/* gnu.java.beans.decoder.MethodFinder
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.java.beans.decoder;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.HashMap;

class MethodFinder
{
	/** Provides a mapping between a wrapper class and its corresponding primitive's type. */
	private static HashMap typeMapping = new HashMap();

	static {
		typeMapping.put(Byte.class, Byte.TYPE);
		typeMapping.put(Short.class, Short.TYPE);
		typeMapping.put(Integer.class, Integer.TYPE);
		typeMapping.put(Long.class, Long.TYPE);
		typeMapping.put(Float.class, Float.TYPE);
		typeMapping.put(Double.class, Double.TYPE);

		typeMapping.put(Character.class, Character.TYPE);
		typeMapping.put(Boolean.class, Boolean.TYPE);
	}

	private MethodFinder()
	{
	}

	/** Searches a Method which can accept the given arguments.
	 *
	 * @param klass
	 * @param name
	 * @param arguments
	 * @return
	 * @throws NoSuchMethodException
	 */
	static Method getMethod(Class klass, String name, Object[] arguments)
		throws NoSuchMethodException
	{
		// prepares array containing the types of the arguments
		Class[] argumentTypes = getArgumentTypes(arguments);

		Method[] methods = klass.getMethods();

		// iterates over all public methods
		for (int i = 0; i < methods.length; i++)
		{
			if (methods[i].getName().equals(name))
			{
				if (matchingArgumentTypes(methods[i].getParameterTypes(),
					argumentTypes))
					return methods[i];
			}
		}

		throw new NoSuchMethodException(
			"Could not find a matching method named "
				+ name
				+ "() in class "
				+ klass);
	}

	static Constructor getConstructor(Class klass, Object[] arguments)
		throws NoSuchMethodException
	{
		Class[] argumentTypes = getArgumentTypes(arguments);
		Constructor[] constructors = klass.getConstructors();

		// iterates over all public methods
		for (int i = 0; i < constructors.length; i++)
		{
			if (matchingArgumentTypes(constructors[i].getParameterTypes(),
				argumentTypes))
				return constructors[i];
		}

		throw new NoSuchMethodException(
			"Could not find a matching constructor in class " + klass);
	}

	/** Transforms an array of argument objects into an array of argument types.
	 * For each argument being null the argument is null, too. An argument type
	 * being null means: Accepts everything (although this can be ambigous).
	 * 
	 * @param arguments
	 * @return
	 */
	private static Class[] getArgumentTypes(Object[] arguments)
	{
		if (arguments == null)
			return new Class[0];

		// prepares array containing the types of the arguments
		Class[] argumentTypes = new Class[arguments.length];
		for (int i = 0; i < arguments.length; i++)
			argumentTypes[i] =
				(arguments[i] == null) ? null : arguments[i].getClass();
		return argumentTypes;
	}

	/** Tests whether the argument types supplied to the method argument types
	 * are assignable. In addition to the assignment specifications this method
	 * handles the primitive's wrapper classes as if they were of their
	 * primitive type (e.g Boolean.class equals Boolean.TYPE).
	 * When a supplied argument type is null it is assumed that no argument
	 * object was supplied for it and the test for this particular parameter will
	 * pass.
	 *
	 * @param methodArgTypes
	 * @param suppliedArgTypes
	 * @return
	 */
	private static boolean matchingArgumentTypes(
		Class[] methodArgTypes,
		Class[] suppliedArgTypes)
	{
		if (methodArgTypes.length != suppliedArgTypes.length)
			return false;

		for (int i = 0; i < methodArgTypes.length; i++)
		{
			if (suppliedArgTypes[i] == null)
			{
				// by definition a non-existant argument type (null) can be converted to everything
				continue;
			}
			else if (typeMapping.containsKey(suppliedArgTypes[i]))
			{
				Class primitiveType =
					(Class) typeMapping.get(suppliedArgTypes[i]);
				if (!(methodArgTypes[i].isAssignableFrom(suppliedArgTypes[i])
					|| methodArgTypes[i].isAssignableFrom(primitiveType)))
					return false;
			}
			else if (!methodArgTypes[i].isAssignableFrom(suppliedArgTypes[i]))
				return false;
		}

		return true;
	}
}
