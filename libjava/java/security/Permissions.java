/* Permissions.java -- A collection of permission collections
   Copyright (C) 1998, 2001 Free Software Foundation, Inc.

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

package java.security;

import java.io.Serializable;
import java.util.Hashtable;
import java.util.Enumeration;
import java.util.NoSuchElementException;

/**
 * This class is a heterogeneous collection of permissions.  It is 
 * organized as a collection of <code>PermissionCollection</code>'s stored
 * in a hashtable.  Each individual <code>PermissionCollection</code>
 * contains permissions of a single type.  If a specific type of 
 * <code>Permission</code> does not provide a collection type to use
 * via its <code>newPermissionCollection</code> method, then a default
 * collection type which stores its permissions in a hash table will be
 * used.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public final class Permissions
  extends PermissionCollection
  implements Serializable
{
  /**
   * Holds instances of <code>AllPermission</code>.
   */
  private PermissionCollection allPermission;

  /**
   * This is the <code>Hashtable</code> that contains our collections.
   */
  Hashtable perms = new Hashtable();

  /**
   * This method initializes a new instance of <code>Permissions</code>.
   */
  public Permissions()
  {
  }

  /**
   * This method adds a new <code>Permission</code> to this collection.  It
   * will be stored in a <code>PermissionCollection</code> of the appropriate
   * type, as determined by calling <code>newPermissionCollection</code> on
   * the specified permission (if an appropriate collection does not already
   * exist).  If this object does not specify a particular type of collection,
   * a default collection which stores in permissions in a hash table will
   * be used.
   *
   * @param perm The <code>Permission</code> object to be added to this collection.
   *
   * @exception SecurityException If this collection is marked as read only.
   * @exception IllegalArgumentException If the specified <code>Permission</code> cannot be added to this collection
   */
  public void add(Permission perm)
    throws SecurityException, IllegalArgumentException
  {
    if (isReadOnly())
      throw new SecurityException("PermissionCollection is read only");

    if (perm instanceof AllPermission)
      {
	if (allPermission == null)
	  {
	    allPermission = new
	      DefaultPermissionCollection("java.security.AllPermission");

	    perms.put("java.security.AllPermission", allPermission);
	  }
      }
    else
      {
	Object obj = perms.get(perm.getClass().getName());
	if (obj != null)
	  {
	    if (!(obj instanceof PermissionCollection))
	      throw new RuntimeException("Internal error in Permissions");

	    ((PermissionCollection) obj).add(perm);
	  }
	else
	  {
	    PermissionCollection pc = perm.newPermissionCollection();
	    if (pc == null)
	      pc = new DefaultPermissionCollection(perm.getClass().getName());

	    pc.add(perm);

	    perms.put(perm.getClass().getName(), pc);
	  }
      }
  }

  /**
   * This method tests whether or not the specified <code>Permission</code>
   * is implied by this <code>PermissionCollection</code>.
   *
   * @param perm The <code>Permission</code> to test.
   *
   * @return <code>true</code> if the specified permission is implied by this <code>PermissionCollection</code>, or <code>false</code> otherwise.
   */
  public boolean implies(Permission perm)
  {
    if (allPermission != null)
      return (true);

    Object obj = perms.get(perm.getClass().getName());
    if (obj == null)
      return (false);

    if (!(obj instanceof PermissionCollection))
      return (false);

    return (((PermissionCollection) obj).implies(perm));
  }

  /**
   * This method returns an <code>Enumeration</code> which contains a
   * list of all <code>Permission</code> objects contained in this
   * collection.
   *
   * @return An <code>Enumeration</code> of this collection's elements.
   */
  public Enumeration elements()
  {
    return new Enumeration()
    {
      Enumeration main_enum = perms.elements();
      Enumeration sub_enum;

      public boolean hasMoreElements()
      {
	if (sub_enum == null)
	  if (main_enum == null)
	    return (false);
	  else
	    {
	      if (!main_enum.hasMoreElements())
		return (false);
	      else
		{
		  try
		    {
		      PermissionCollection pc =
			(PermissionCollection) main_enum.nextElement();
		      sub_enum = pc.elements();
		    }
		  catch (NoSuchElementException e)
		    {
		      return (false);
		    }
		}
	    }
	else if (!sub_enum.hasMoreElements())
	  {
	    sub_enum = null;
	    return (hasMoreElements());
	  }

	return (true);
      }

      public Object nextElement() throws NoSuchElementException
      {
	if (!hasMoreElements())
	  throw new NoSuchElementException();

	return (sub_enum.nextElement());
      }
    };
  }
  
  static class DefaultPermissionCollection extends PermissionCollection
    implements Serializable
  {

    // Type of Permission we can store
    private Class permcls;

    // Hashtable where we store permissions.
    private Hashtable perms = new Hashtable();

    DefaultPermissionCollection(String permtype) throws IllegalArgumentException
    {
      try
	{
	  permcls = Class.forName(permtype);
	}
      catch(ClassNotFoundException e)
	{
	  throw new IllegalArgumentException(e.getMessage());
	}
    }

    public void add(Permission perm) 
      throws SecurityException, IllegalArgumentException
    {
      if (isReadOnly())
	throw new SecurityException("PermissionCollection is read only");

      if (!permcls.isInstance(perm))
	throw new IllegalArgumentException("Wrong permission type: " + 
                                	   perm.getClass().getName());

      if (perms.get(perm.getName()) != null)
	throw new IllegalArgumentException("Duplicate permission: " +
                                	   perm.getName());

      perms.put(perm.getName(), perm);
    }

    public boolean implies(Permission perm)
    {
      Object obj = perms.get(perm.getName());
      if (obj == null)
	return(false);

      if (!(obj instanceof Permission))
	return(false);

      Permission p = (Permission)obj;

      return(p.implies(perm));
    }

    public Enumeration elements()
    {
      return(perms.elements());
    }
  }
}
