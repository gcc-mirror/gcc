/* java.beans.Statement
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

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 * class Statement
 *
 * A Statement captures the execution of an object method.  It stores
 * the object, the method to call, and the arguments to the method and
 * provides the ability to execute the method on the object, using the
 * provided arguments.
 *
 * @since 1.4
 */
public class Statement
{
  private Object target;
  private String methodName;
  private Object[] arguments;

  // One or the other of these will get a value after execute is
  // called once, but not both.
  private transient Method method;
  private transient Constructor ctor;

  /**
   * Constructs a statement representing the invocation of
   * object.methodName(arg[0], arg[1], ...);
   *
   * @param target The object to invoke the method on.
   * @param methodName The object method to invoke.
   * @param arguments An array of arguments to pass to the method.
   */
  public Statement(Object target, String methodName, Object[] arguments)
  {
    this.target = target;
    this.methodName = methodName;
    this.arguments = arguments;
  }

  /**
   * Execute the statement.
   *
   * Finds the specified method in the target object and calls it with
   * the arguments given in the constructor.
   *
   * The most specific method according to the JLS(15.11) is used when
   * there are multiple methods with the same name.
   *
   * Execute performs some special handling for methods and
   * parameters:
   *
   * Static methods can be executed by providing the class as a
   * target.
   *
   * The method name new is reserved to call the constructor 
   * new() will construct an object and return it.  Not useful unless
   * an expression :-)
   *
   * If the target is an array, get and set as defined in
   * java.util.List are recognized as valid methods and mapped to the
   * methods of the same name in java.lang.reflect.Array.
   *
   * The native datatype wrappers Boolean, Byte, Character, Double,
   * Float, Integer, Long, and Short will map to methods that have
   * native datatypes as parameters, in the same way as Method.invoke.
   * However, these wrappers also select methods that actually take
   * the wrapper type as an argument.
   *
   * The Sun spec doesn't deal with overloading between int and
   * Integer carefully.  If there are two methods, one that takes an
   * Integer and the other taking an int, the method chosen is not
   * specified, and can depend on the order in which the methods are
   * declared in the source file.
   *
   * @throws Exception if an exception occurs while locating or
   * 		       invoking the method.
   */
  public void execute() throws Exception
  {
    doExecute();
  }
  
  private static Class wrappers[] = 
    {
      Boolean.class, Byte.class, Character.class, Double.class, Float.class,
      Integer.class, Long.class, Short.class
    };

  private static Class natives[] = 
    {
      Boolean.TYPE, Byte.TYPE, Character.TYPE, Double.TYPE, Float.TYPE,
      Integer.TYPE, Long.TYPE, Short.TYPE
    };

  // Given a wrapper class, return the native class for it.  For
  // example, if c is Integer, Integer.TYPE is returned.
  private Class unwrap(Class c)
  {
    for (int i = 0; i < wrappers.length; i++)
      if (c == wrappers[i])
	return natives[i];
    return null;
  }

  // Return true if all args can be assigned to params, false
  // otherwise.  Arrays are guaranteed to be the same length.
  private boolean compatible(Class[] params, Class[] args)
  {
    for (int i = 0; i < params.length; i++)
      {
	// Treat Integer like int if appropriate
	Class nativeType = unwrap(args[i]);
	if (nativeType != null && params[i].isPrimitive()
	    && params[i].isAssignableFrom(nativeType))
	  continue;
	if (params[i].isAssignableFrom(args[i]))
	  continue;

	return false;
      }
    return true;
  }

  /**
   * Return true if the method arguments in first are more specific
   * than the method arguments in second, i.e. all args in first can
   * be assigned to those in second.
   *
   * A method is more specific if all parameters can also be fed to
   * the less specific method, because, e.g. the less specific method
   * accepts a base class of the equivalent argument for the more
   * specific one.
   *
   * @param first a <code>Class[]</code> value
   * @param second a <code>Class[]</code> value
   * @return a <code>boolean</code> value
   */
  private boolean moreSpecific(Class[] first, Class[] second)
  {
    for (int j=0; j < first.length; j++)
      {
	if (second[j].isAssignableFrom(first[j]))
	  continue;
	return false;
      }
    return true;
  }

  final Object doExecute() throws Exception
  {
    Class klazz = (target instanceof Class)
	? (Class) target : target.getClass();
    Object args[] = (arguments == null) ? new Object[0] : arguments;
    Class argTypes[] = new Class[args.length];
    for (int i = 0; i < args.length; i++)
      argTypes[i] = args[i].getClass();

    if (target.getClass().isArray())
      {
	// FIXME: invoke may have to be used.  For now, cast to Number
	// and hope for the best.  If caller didn't behave, we go boom
	// and throw the exception.
	if (methodName.equals("get") && argTypes.length == 1)
	  return Array.get(target, ((Number)args[0]).intValue());
	if (methodName.equals("set") && argTypes.length == 2)
	  {
	    Object obj = Array.get(target, ((Number)args[0]).intValue());
	    Array.set(target, ((Number)args[0]).intValue(), args[1]);
	    return obj;
	  }
	throw new NoSuchMethodException("No matching method for statement " + toString());
      }

    // If we already cached the method, just use it.
    if (method != null)
      return method.invoke(target, args);
    else if (ctor != null)
      return ctor.newInstance(args);

    // Find a matching method to call.  JDK seems to go through all
    // this to find the method to call.

    // if method name or length don't match, skip
    // Need to go through each arg
    // If arg is wrapper - check if method arg is matchable builtin
    //  or same type or super
    //  - check that method arg is same or super

    if (methodName.equals("new") && target instanceof Class)
      {
	Constructor ctors[] = klazz.getConstructors();
	for (int i = 0; i < ctors.length; i++)
	  {
	    // Skip methods with wrong number of args.
	    Class ptypes[] = ctors[i].getParameterTypes();
	    System.out.println("ptypeslen = " + ptypes.length);
	    System.out.println("ptypes = " + ptypes);
	    System.out.println("ctor = " + ctors[i].getName());
	    for (int j=0; j < ptypes.length; j++) {
	      System.out.println("param = " + ptypes[i].getName());
     
	    }
	      
	    
	    if (ptypes.length != args.length)
	      continue;

	    // Check if method matches
	    if (!compatible(ptypes, argTypes))
	      continue;

	    // Use method[i] if it is more specific. 
	    // FIXME: should this check both directions and throw if
	    // neither is more specific?
	    if (ctor == null)
	      {
		ctor = ctors[i];
		continue;
	      }
	    Class mptypes[] = ctor.getParameterTypes();
	    if (moreSpecific(ptypes, mptypes))
	      ctor = ctors[i];
	  }
	if (ctor == null)
	  throw new InstantiationException("No matching constructor for statement " + toString());
	return ctor.newInstance(args);
      }

    Method methods[] = klazz.getMethods();

    for (int i = 0; i < methods.length; i++)
      {
	// Skip methods with wrong name or number of args.
	if (!methods[i].getName().equals(methodName))
	  continue;
	Class ptypes[] = methods[i].getParameterTypes();
	if (ptypes.length != args.length)
	  continue;

	// Check if method matches
	if (!compatible(ptypes, argTypes))
	  continue;

	// Use method[i] if it is more specific. 
	// FIXME: should this check both directions and throw if
	// neither is more specific?
	if (method == null)
	  {
	    method = methods[i];
	    continue;
	  }
	Class mptypes[] = method.getParameterTypes();
	if (moreSpecific(ptypes, mptypes))
	  method = methods[i];
      }
    if (method == null)
      throw new NoSuchMethodException("No matching method for statement " + toString());
    return method.invoke(target, args);
  }

  

  /** Return the statement arguments. */
  public Object[] getArguments() { return arguments; }

  /** Return the statement method name. */
  public String getMethodName() { return methodName; }

  /** Return the statement object. */
  public Object getTarget() { return target; }

  /** Return a string representation. */
  public String toString()
  {
    String result = target.getClass().getName() + "." + methodName + "(";
    String sep = "";
    for (int i = 0; i < arguments.length; i++)
      {
	result = result + sep + arguments[i].getClass().getName();
	sep = ", ";
      }
    result = result + ")";
    return result;
  }
}
