/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang;

import java.io.Serializable;
 
/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date September 3, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public final class Boolean extends Object implements Serializable
{
  public static final Boolean FALSE = new Boolean(false);
  public static final Boolean TRUE = new Boolean(true);

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = boolean.class;

  /* The boolean value of the instance. */
  private boolean value;

  public Boolean(boolean boolVal)
  {
    value = boolVal;
  }

  public Boolean(String strVal)
  {
    value = (strVal == null ? false : strVal.equalsIgnoreCase("true"));
  }

  public boolean booleanValue()
  {
    return value;
  }

  public boolean equals(Object obj)
  {
    /* Don't need to compare obj to null as instanceof will do this. */
    if (obj instanceof Boolean)
      return value == ((Boolean) obj).value;
    return false;
  }

  public static boolean getBoolean(String property)
  {
    /* TBD: If a security manager exists and it doesn't permit accessing
     * the property, it will throw an exception.  Should we catch it?
     */
    try
      {
	String val = System.getProperty(property);
	return val == null ? false : val.equalsIgnoreCase("true");
      }
    catch (SecurityException e)
      {
        return false;
      }
  }

  public int hashCode()
  {
    /* These values are from the Java Lang. Spec. (Sec 20.4.7).
     * TBD: They could be made private static final fields but they're only
     * used here (and shouldn't be used anywhere else), though it might be
     * useful to grep on something like JAVA_HASH_* values for us as
     * developers.
     */
    return value ? 1231 : 1237;
  }

  public String toString()
  {
    return value ? "true" : "false";
  }

  public static Boolean valueOf(String str)
  {
    if (str == null)
      return FALSE;
    else
      /* This returns a Boolean (big B), not a boolean (little b). */
      return str.equalsIgnoreCase("true") ? TRUE : FALSE;
  }
}
