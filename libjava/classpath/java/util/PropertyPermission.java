/* PropertyPermission.java -- permission to get and set System properties
   Copyright (C) 1999, 2000, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamField;
import java.security.BasicPermission;
import java.security.Permission;
import java.security.PermissionCollection;

/**
 * This class represents the permission to access and modify a property.<br>
 *
 * The name is the name of the property, e.g. xxx.  You can also
 * use an asterisk "*" as described in BasicPermission.<br>
 *
 * The action string is a comma-separated list of keywords.  There are
 * two possible actions:
 * <dl>
 * <dt>read</dt>
 * <dd>Allows to read the property via <code>System.getProperty</code>.</dd>
 * <dt>write</dt>
 * <dd>Allows to write the property via <code>System.setProperty</code>.</dd>
 * </dl>
 *
 * The action string is case insensitive (it is converted to lower case).
 *
 * @see Permission
 * @see BasicPermission
 * @see SecurityManager
 * @author Jochen Hoenicke
 * @since 1.2
 * @status updated to 1.4
 */
public final class PropertyPermission extends BasicPermission
{
  /**
   * PropertyPermission uses a more efficient representation than the
   * serialized form; this documents the difference.
   *
   * @serialField action String the action string
   */
  private static final ObjectStreamField[] serialPersistentFields =
  {
    new ObjectStreamField("action", String.class)
  };

  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 885438825399942851L;

  /** Permission to read. */
  private static final int READ = 1;
  /** Permission to write. */
  private static final int WRITE = 2;

  /** The set of actions permitted. */
  // Package visible for use by PropertyPermissionCollection.
  transient int actions;

  /**
   * The String forms of the actions permitted.
   */
  private static final String actionStrings[] =
  {
    "", "read", "write", "read,write"
  };

  /**
   * Constructs a PropertyPermission with the specified property.  Possible
   * actions are read and write, comma-separated and case-insensitive.
   *
   * @param name the name of the property
   * @param actions the action string
   * @throws NullPointerException if name is null
   * @throws IllegalArgumentException if name string contains an
   *         illegal wildcard or actions string contains an illegal action
   *         (this includes a null actions string)
   */
  public PropertyPermission(String name, String actions)
  {
    super(name);
    if (actions == null)
      throw new IllegalArgumentException();
    setActions(actions);
  }

  /**
   * Parse the action string and convert actions from external to internal
   * form.  This will set the internal actions field.
   *
   * @param str the action string
   * @throws IllegalArgumentException if actions string contains an
   *         illegal action
   */
  private void setActions(String str)
  {
    // Initialising the class java.util.Locale ...
    //    tries to initialise the Locale.defaultLocale static
    //    which calls System.getProperty,
    //    which calls SecurityManager.checkPropertiesAccess,
    //    which creates a PropertyPermission with action "read,write",
    //    which calls setActions("read,write").
    // If we now were to call toLowerCase on 'str',
    //    this would call Locale.getDefault() which returns null
    //       because Locale.defaultLocale hasn't been set yet
    //    then toLowerCase will fail with a null pointer exception.
    //
    // The solution is to take a punt on 'str' being lower case, and
    // test accordingly.  If that fails, we convert 'str' to lower case
    // and try the tests again.
    if ("read".equals(str))
      actions = READ;
    else if ("write".equals(str))
      actions = WRITE;
    else if ("read,write".equals(str) || "write,read".equals(str))
      actions = READ | WRITE;
    else
      {
        String lstr = str.toLowerCase();
        if ("read".equals(lstr))
          actions = READ;
        else if ("write".equals(lstr))
          actions = WRITE;
        else if ("read,write".equals(lstr) || "write,read".equals(lstr))
          actions = READ | WRITE;
        else
          throw new IllegalArgumentException("illegal action " + str);
      }
  }

  /**
   * Reads an object from the stream. This converts the external to the
   * internal representation.
   *
   * @param s the stream to read from
   * @throws IOException if the stream fails
   * @throws ClassNotFoundException if reserialization fails
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    ObjectInputStream.GetField fields = s.readFields();
    setActions((String) fields.get("actions", null));
  }

  /**
   * Writes an object to the stream. This converts the internal to the
   * external representation.
   *
   * @param s the stram to write to
   * @throws IOException if the stream fails
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    ObjectOutputStream.PutField fields = s.putFields();
    fields.put("actions", getActions());
    s.writeFields();
  }

  /**
   * Check if this permission implies p.  This returns true iff all of
   * the following conditions are true:
   * <ul>
   * <li> p is a PropertyPermission </li>
   * <li> this.getName() implies p.getName(),
   *  e.g. <code>java.*</code> implies <code>java.home</code> </li>
   * <li> this.getActions is a subset of p.getActions </li>
   * </ul>
   *
   * @param p the permission to check
   * @return true if this permission implies p
   */
  public boolean implies(Permission p)
  {
    // BasicPermission checks for name and type.
    if (super.implies(p))
      {
        // We have to check the actions.
        PropertyPermission pp = (PropertyPermission) p;
        return (pp.actions & ~actions) == 0;
      }
    return false;
  }

  /**
   * Check to see whether this object is the same as another
   * PropertyPermission object; this is true if it has the same name and
   * actions.
   *
   * @param obj the other object
   * @return true if the two are equivalent
   */
  public boolean equals(Object obj)
  {
    return super.equals(obj) && actions == ((PropertyPermission) obj).actions;
  }

  /**
   * Returns the hash code for this permission.  It is equivalent to
   * <code>getName().hashCode()</code>.
   *
   * @return the hash code
   */
  public int hashCode()
  {
    return super.hashCode();
  }

  /**
   * Returns the action string.  Note that this may differ from the string
   * given at the constructor:  The actions are converted to lowercase and
   * may be reordered.
   *
   * @return one of "read", "write", or "read,write"
   */
  public String getActions()
  {
    return actionStrings[actions];
  }

  /**
   * Returns a permission collection suitable to take
   * PropertyPermission objects.
   *
   * @return a new empty PermissionCollection
   */
  public PermissionCollection newPermissionCollection()
  {
    return new PropertyPermissionCollection();
  }
}
