/* Copyright (C) 2001 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date May 16, 2001
 */
public class Binding extends NameClassPair
{
  public Binding (String name, Object obj)
  {
    super (name, null);
    boundObj = obj;
  }

  public Binding (String name, Object obj, boolean isRelative)
  {
    super (name, null, isRelative);
    boundObj = obj;
  }

  public Binding (String name, String className, Object obj)
  {
    super (name, className);
    boundObj = obj;
  }

  public Binding (String name, String className, Object obj,
		  boolean isRelative)
  {
    super (name, className, isRelative);
    boundObj = obj;
  }

  public String getClassName ()
  {
    String r = super.getClassName ();
    if (r != null)
      return r;
    return boundObj == null ? null : boundObj.getClass ().getName ();
  }

  public Object getObject ()
  {
    return boundObj;
  }

  public void setObject (Object obj)
  {
    boundObj = obj;
  }

  public String toString ()
  {
    // Format specified by the documentation.
    return super.toString () + ":" + boundObj.toString ();
  }

  // This name is fixed by the serialization spec.
  private Object boundObj;
}
