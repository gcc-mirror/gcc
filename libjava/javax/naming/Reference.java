/* Copyright (C) 2000, 2001 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date May 16, 2001
 */
public class Reference implements Cloneable, Serializable
{
  public Reference (String className)
  {
    this.className = className;
    addrs = new Vector ();
  }

  public Reference (String className, RefAddr addr)
  {
    this.className = className;
    addrs = new Vector ();
    addrs.add (addr);
  }

  public Reference (String className, String factory, String factoryLocation)
  {
    this.className = className;
    this.classFactory = factory;
    this.classFactoryLocation = factoryLocation;
    addrs = new Vector ();
  }

  public Reference (String className, RefAddr addr,
		    String factory, String factoryLocation)
  {
    this.className = className;
    this.classFactory = factory;
    this.classFactoryLocation = factoryLocation;
    addrs = new Vector ();
    addrs.add (addr);
  }

  public void add (int posn, RefAddr addr)
  {
    addrs.add (posn, addr);
  }

  public void add (RefAddr addr)
  {
    addrs.add (addr);
  }

  public void clear ()
  {
    addrs.clear ();
  }

  public Object clone ()
  {
    Reference r = new Reference (className, classFactory,
				 classFactoryLocation);
    r.addrs = (Vector) addrs.clone ();
    return r;
  }

  // Convenience function.
  private boolean equals (String a, String b)
  {
    return (a == null) ? (b == null) : a.equals (b);
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof Reference))
      return false;
    Reference r = (Reference) obj;
    return (equals (classFactory, r.classFactory)
	    && equals (classFactoryLocation, r.classFactoryLocation)
	    && equals (className, r.className)
	    && addrs.equals (r.addrs));
  }

  public RefAddr get (int posn)
  {
    return (RefAddr) addrs.get (posn);
  }

  public RefAddr get (String addrType)
  {
    for (int i = 0; i < addrs.size (); ++i)
      {
	RefAddr r = (RefAddr) addrs.get (i);
	if (addrType.equals (r.getType ()))
	  return r;
      }
    return null;
  }

  public Enumeration getAll ()
  {
    return addrs.elements ();
  }

  public String getClassName ()
  {
    return className;
  }

  public String getFactoryClassLocation ()
  {
    return classFactoryLocation;
  }

  public String getFactoryClassName ()
  {
    return classFactory;
  }

  public int hashCode ()
  {
    // The spec says the hash code is the sum of the hash codes of the
    // addresses.  It does not mention the other fields.
    int h = 0;
    for (int i = 0; i < addrs.size (); ++i)
      h += addrs.get (i).hashCode ();
    return h;
  }

  public Object remove (int posn)
  {
    return addrs.remove (posn);
  }

  public int size ()
  {
    return addrs.size ();
  }

  public String toString ()
  {
    String x = getClass ().toString () + "[";
    for (int i = 0; i < addrs.size (); ++i)
      {
	if (i > 0)
	  x += ",";
	x += addrs.get (i).toString ();
      }
    return x + "]";
  }

  protected Vector addrs;
  protected String classFactory;
  protected String classFactoryLocation;
  protected String className;
}
