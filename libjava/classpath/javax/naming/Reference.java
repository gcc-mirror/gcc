/* Reference.java --
   Copyright (C) 2000, 2001, 2005, 2006  Free Software Foundation, Inc.

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


package javax.naming;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Vector;

/**
 * This class represents a reference to an object that is located outside of the
 * naming/directory system.
 * 
 * @see Referenceable
 * 
 * @author Tom Tromey (tromey@redhat.com)
 */
public class Reference implements Cloneable, Serializable
{
  private static final long serialVersionUID = - 1673475790065791735L;
  
  /**
   * The list of addresses, stored in this reference. The object may be 
   * have by several different addresses.
   */
  protected Vector<RefAddr> addrs;
  
  /**
   * The name of the class factory to create an instance of the object,
   * referenced by this reference.
   */
  protected String classFactory;
  
  /**
   * The location, from where the class factory should be loaded.
   */
  protected String classFactoryLocation;
  
  /**
   * The name of the class of the object, to that this reference refers.
   */
  protected String className;
  
  /**
   * Create a new reference that is referencting to the object of the
   * specified class.
   */
  public Reference (String className)
  {
    this.className = className;
    addrs = new Vector<RefAddr> ();
  }
  
  /**
   * Create a new reference that is referencing to the object of the
   * specified class with the given address.
   */
  public Reference (String className, RefAddr addr)
  {
    this.className = className;
    addrs = new Vector<RefAddr> ();
    addrs.add (addr);
  }
   
  /**
   * Create a new reference that is referencing to the object of the
   * specified class, specifying the class and location of the factory that
   * produces these objects.
   * 
   * @param className the object class name
   * @param factoryClassName the object factory class name
   * @param factoryLocation the object factory location
   */
  public Reference (String className, String factoryClassName, 
                    String factoryLocation)
  {
    this.className = className;
    this.classFactory = factoryClassName;
    this.classFactoryLocation = factoryLocation;
    addrs = new Vector<RefAddr> ();
  }

  /**
   * Create a new reference that is referencing to the object of the
   * specified class, specifying the class and location of the factory that
   * produces these objects and also the address of this object.
   * 
   * @param className the object class name
   * @param addr the address of the object
   * @param factoryClassName the object factory class name
   * @param factoryLocation the object factory location
   */
  public Reference (String className, RefAddr addr,
		    String factoryClassName, String factoryLocation)
  {
    this.className = className;
    this.classFactory = factoryClassName;
    this.classFactoryLocation = factoryLocation;
    addrs = new Vector<RefAddr> ();
    addrs.add (addr);
  }

  /**
   * Add the new address for this object at the given position of the 
   * address list.
   */
  public void add (int posn, RefAddr addr)
  {
    addrs.add (posn, addr);
  }
  
  /**
   * Appends the new object address to the end of the address list.
   */
  public void add (RefAddr addr)
  {
    addrs.add (addr);
  }
  
  /**
   * Removes all defined addresses of the object.
   */
  public void clear ()
  {
    addrs.clear ();
  }

  public Object clone ()
  {
    Reference r = new Reference (className, classFactory,
				 classFactoryLocation);
    r.addrs = (Vector<RefAddr>) addrs.clone ();
    return r;
  }

  // Convenience function.
  private boolean equals (String a, String b)
  {
    return (a == null) ? (b == null) : a.equals (b);
  }
  
  /**
   * Compares two addresses for equality, by value.
   */
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
  
  /**
   * Get the address of this object at the given position.
   */
  public RefAddr get (int posn)
  {
    return addrs.get (posn);
  }
  
  /**
   * Get the given type of address for this object.
   * 
   * @param addrType the needed type of address
   * 
   * @return the address of this object, having the specified type. If there
   *           is no address of such type, null is returned.
   */
  public RefAddr get (String addrType)
  {
    for (int i = 0; i < addrs.size (); ++i)
      {
	RefAddr r = addrs.get (i);
	if (addrType.equals (r.getType ()))
	  return r;
      }
    return null;
  }
  
  /**
   * Get the enumeration over all defined addresses of the object.
   */
  public Enumeration<RefAddr> getAll ()
  {
    return addrs.elements ();
  }
  
  /**
   * Get the name of the class of the referenced object.
   * 
   * @see #className
   */
  public String getClassName ()
  {
    return className;
  }
  
  /**
   * Get the location of the factory class of the referenced object.
   * 
   * @see #classFactoryLocation
   */
  public String getFactoryClassLocation ()
  {
    return classFactoryLocation;
  }

  /**
   * Get the name of the factory class of the referenced object
   * 
   * @see #classFactory
   */
  public String getFactoryClassName ()
  {
    return classFactory;
  }
  
  /**
   * Get the hashcode of this reference. 
   * 
   * @return the sum of the hash codes of the addresses.  
   */
  public int hashCode ()
  {
    // The spec says the hash code is the sum of the hash codes of the
    // addresses.  It does not mention the other fields.
    int h = 0;
    for (int i = 0; i < addrs.size (); ++i)
      h += addrs.get (i).hashCode ();
    return h;
  }
  
  /**
   * Remove the address at the given position.
   * 
   * @param posn the position of the address to remove
   * 
   * @return the removed address
   */
  public Object remove (int posn)
  {
    return addrs.remove (posn);
  }
  
  /**
   * Return the number of the defined addresses.
   */
  public int size ()
  {
    return addrs.size ();
  }
  
  /**
   * Return the string representation.
   */
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

}
