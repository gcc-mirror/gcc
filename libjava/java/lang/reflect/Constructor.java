// Constructor.java - Represents a constructor for a class.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang.reflect;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date December 12, 1998
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Incomplete: needs a private constructor, and
 *          newInstance() needs to be written.
 */

public final class Constructor extends AccessibleObject implements Member
{
  public boolean equals (Object obj)
    {
      if (! (obj instanceof Constructor))
	return false;
      Constructor c = (Constructor) obj;
      return declaringClass == c.declaringClass && offset == c.offset;
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

  public String getName ()
  {
    return declaringClass.getName();
  }

  public Class[] getParameterTypes ()
    {
      if (parameter_types == null)
	getType ();
      return (Class[]) parameter_types.clone();
    }

  public int hashCode ()
    {
      // FIXME.
      return getName().hashCode() + declaringClass.getName().hashCode();
    }

  // Update cached values from method descriptor in class.
  private native void getType ();

  public native Object newInstance (Object[] args)
    throws InstantiationException, IllegalAccessException,
           IllegalArgumentException, InvocationTargetException;

  public String toString ()
    {
      if (parameter_types == null)
	getType ();
      StringBuffer b = new StringBuffer ();
      b.append(Modifier.toString(getModifiers()));
      b.append(" ");
      b.append(getName());
      b.append("(");
      for (int i = 0; i < parameter_types.length; ++i)
	{
	  b.append(parameter_types[i].toString());
	  if (i < parameter_types.length - 1)
	    b.append(",");
	}
      b.append(")");
      return b.toString();
    }

  // Can't create these.
  private Constructor ()
    {
    }

  // Declaring class.
  private Class declaringClass;

  // Exception types.
  private Class[] exception_types;
  // Parameter types.
  private Class[] parameter_types;

  // Offset in bytes from the start of declaringClass's methods array.
  private int offset;
}
