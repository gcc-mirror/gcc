/* PropertyPermissionCollection.java -- a collection of PropertyPermissions
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package java.util;

import java.security.Permission;
import java.security.PermissionCollection;

/**
 * This class provides the implementation for
 * <code>PropertyPermission.newPermissionCollection()</code>. It only accepts
 * PropertyPermissions, and correctly implements <code>implies</code>. It
 * is synchronized, as specified in the superclass.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @status an undocumented class, but this matches Sun's serialization
 */
class PropertyPermissionCollection extends PermissionCollection
{
  /**
   * Compatible with JDK 1.4.
   */
  private static final long serialVersionUID = 7015263904581634791L;

  /**
   * The permissions.
   *
   * @serial the table of permissions in the collection
   */
  private final Hashtable permissions = new Hashtable();

  /**
   * A flag to detect if "*" is in the collection.
   *
   * @serial true if "*" is in the collection 
   */
  private boolean all_allowed;

  /**
   * Adds a PropertyPermission to this collection.
   *
   * @param permission the permission to add
   * @throws IllegalArgumentException if permission is not a PropertyPermission
   * @throws SecurityException if collection is read-only
   */
  public void add(Permission permission)
  {
    if (isReadOnly())
      throw new SecurityException("readonly");
    if (! (permission instanceof PropertyPermission))
      throw new IllegalArgumentException();
    PropertyPermission pp = (PropertyPermission) permission;
    String name = pp.getName();
    if (name.equals("*"))
        all_allowed = true;
    PropertyPermission old = (PropertyPermission) permissions.get(name);
    if (old != null)
      {
        if ((pp.actions | old.actions) == old.actions)
          pp = old; // Old implies pp.
        else if ((pp.actions | old.actions) != pp.actions)
          // Here pp doesn't imply old; the only case left is both actions.
          pp = new PropertyPermission(name, "read,write");
      }
    permissions.put(name, pp);
  }

  /**
   * Returns true if this collection implies the given permission. This even
   * returns true for this case:
   * <p>
<pre>collection.add(new PropertyPermission("a.*", "read"));
collection.add(new PropertyPermission("a.b.*", "write"));
collection.implies(new PropertyPermission("a.b.c", "read,write"));</pre>
   *
   * @param permission the permission to check
   * @return true if it is implied by this
   */
  public boolean implies(Permission permission)
  {
    if (! (permission instanceof PropertyPermission))
      return false;
    PropertyPermission toImply = (PropertyPermission) permission;
    int actions = toImply.actions;

    if (all_allowed)
      {
        int all_actions = ((PropertyPermission) permissions.get("*")).actions;
        actions &= ~all_actions;
        if (actions == 0)
          return true;
      }

    String name = toImply.getName();
    if (name.equals("*"))
      return false;

    int prefixLength = name.length();
    if (name.endsWith("*"))
      prefixLength -= 2;

    while (true)
      {
        PropertyPermission forName =
          (PropertyPermission) permissions.get(name);
        if (forName != null)
          {
            actions &= ~forName.actions;
            if (actions == 0)
              return true;
          }

        prefixLength = name.lastIndexOf('.', prefixLength);
        if (prefixLength < 0)
          return false;
        name = name.substring(0, prefixLength + 1) + '*';
      }
  }

  /**
   * Enumerate over the collection.
   *
   * @return an enumeration of the collection contents
   */
  public Enumeration elements()
  {
    return permissions.elements();
  }
}
