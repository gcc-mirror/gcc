// Method.java - Represent method of class or interface.

/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang.reflect;

import gnu.gcj.RawData;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date December 12, 1998
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Complete, but not correct: access checks aren't done.
 */

public final class Method extends AccessibleObject implements Member
{
  public boolean equals (Object obj)
    {
      if (! (obj instanceof Method))
	return false;
      Method m = (Method) obj;
      return declaringClass == m.declaringClass && offset == m.offset;
    }

  public Class getDeclaringClass ()
    {
      return declaringClass;
    }

  public Class[] getExceptionTypes ()
    {
      return (Class[]) exception_types.clone();
    }

  public native int getModifiers ();

  public native String getName ();

  private native void getType ();

  public Class[] getParameterTypes ()
    {
      if (parameter_types == null)
	getType();
      return (Class[]) parameter_types.clone();
    }

  public Class getReturnType ()
    {
      if (return_type == null)
	getType();
      return return_type;
    }

  public int hashCode ()
    {
      // FIXME.
      return name.hashCode() + declaringClass.getName().hashCode();
    }

  public native Object invoke (Object obj, Object[] args)
    throws IllegalAccessException, IllegalArgumentException,
           InvocationTargetException;

  public String toString ()
    {
      if (parameter_types == null)
	getType ();

      StringBuffer b = new StringBuffer ();
      b.append(Modifier.toString(getModifiers()));
      b.append(" ");
      b.append(return_type.toString());
      b.append(" ");
      b.append(declaringClass.toString());
      b.append(".");
      b.append(name);
      b.append("(");
      for (int i = 0; i < parameter_types.length; ++i)
	{
	  b.append(parameter_types[i].toString());
	  if (i < parameter_types.length - 1)
	    b.append(",");
	}
      b.append(")");
      if (exception_types.length > 0)
	{
	  b.append(" throws ");
	  for (int i = 0; i < exception_types.length; ++i)
	    {
	      b.append(exception_types[i].toString());
	      if (i < exception_types.length - 1)
		b.append(",");
	    }
	}
      return b.toString();
    }

  private Method ()
  {
  }

  // Declaring class.
  private Class declaringClass;

  // Exception types.
  private Class[] exception_types;
  // Name cache.  (Initially null.)
  private String name;
  // Parameter types.
  private Class[] parameter_types;
  // Return type.
  private Class return_type;

  // Offset in bytes from the start of declaringClass's methods array.
  private int offset;
}
