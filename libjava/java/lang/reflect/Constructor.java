// Constructor.java - Represents a constructor for a class.

/* Copyright (C) 1998, 1999  Cygnus Solutions

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
      return decl_class == c.decl_class && index == c.index;
    }

  public Class getDeclaringClass ()
    {
      return decl_class;
    }

  public Class[] getExceptionTypes ()
    {
      return (Class[]) exception_types.clone();
    }

  public int getModifiers ()
    {
      return modifiers;
    }

  public String getName ()
    {
      return decl_class.getName();
    }

  public Class[] getParameterTypes ()
    {
      return (Class[]) parameter_types.clone();
    }

  public int hashCode ()
    {
      // FIXME.
      return getName().hashCode();
    }

  // FIXME: this must be native.  Should share implementation with
  // Method.invoke.
  public Object newInstance (Object[] args)
    throws InstantiationException, IllegalAccessException,
           IllegalArgumentException, InvocationTargetException
    {
      return null;
    }

  public String toString ()
    {
      StringBuffer b = new StringBuffer ();
      b.append(Modifier.toString(modifiers));
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

  // Can't create these.  FIXME.
  private Constructor ()
    {
    }

  // Declaring class.
  private Class decl_class;
  // Exception types.
  private Class[] exception_types;
  // Modifiers.
  private int modifiers;
  // Parameter types.
  private Class[] parameter_types;
  // Index of this method in declaring class' method table.
  private int index;
}
