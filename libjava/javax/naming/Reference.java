/* Reference.java --
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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
