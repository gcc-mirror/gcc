/* java.util.PropertyPermission
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
import java.security.BasicPermission;
import java.security.PermissionCollection;
import java.io.ObjectStreamField;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;

/**
 * This class represents the permission to access and modify a property.<br>
 *
 * The name is the name of the property, e.g. xxx.  You can also
 * use an asterisk "*" as described in BasicPermission <br>
 *
 * The action string is a comma-separated list if keywords.  There are
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
 * @author Jochen Hoenicke 
 */
public final class PropertyPermission extends BasicPermission
{
  /**
   * @serialField action String
   *   The action string.
   */
  private static final ObjectStreamField[] serialPersistentFields =
  {
    new ObjectStreamField("action", String.class)
  };

  private static final long serialVersionUID = 885438825399942851L;

  private static final int READ = 1;
  private static final int WRITE = 2;
  private transient int actions;

  private static final String actionStrings[] =
  {
    "", "read", "write", "read,write"
  };

  /**
   * Constructs a PropertyPermission witha he specified property.  Possible
   * actions are read and write.
   * @param name the name of the property.
   * @param actions the action string.
   * @exception IllegalArgumentException if name string contains an
   * illegal wildcard or actions string contains an illegal action
   */
  public PropertyPermission(String name, String actions)
  {
    super(name);
    setActions(actions.toLowerCase());
  }

  /**
   * Parse the action string and convert actions from external to internal
   * form.  This will set the internal actions field.
   * @param actions the action string.
   * @exception IllegalArgumentException if actions string contains an
   * illegal action */
  private void setActions(String actions)
  {
    this.actions = 0;
    StringTokenizer actionTokenizer = new StringTokenizer(actions, ",");
    while (actionTokenizer.hasMoreElements())
      {
	String anAction = actionTokenizer.nextToken();
	if ("read".equals(anAction))
	  this.actions |= READ;
	else if ("write".equals(anAction))
	  this.actions |= WRITE;
	else
	  throw new IllegalArgumentException("illegal action " + anAction);
      }
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
   */
  public boolean implies(Permission p)
  {
    if (!(p instanceof PropertyPermission))
      return false;

    // We have to check the actions.
    PropertyPermission pp = (PropertyPermission) p;
    if ((pp.actions & ~actions) != 0)
      return false;

    // BasicPermission checks for name.
    if (!super.implies(p))
      return false;

    return true;
  }

  /**
   * Returns the action string.  Note that this may differ from the string
   * given at the constructor:  The actions are converted to lowercase and
   * may be reordered.
   */
  public String getActions()
  {
    return actionStrings[actions];
  }

  /**
   * Check to see whether this object is the same as another
   * PropertyPermission object.
   *
   * @param obj The other object
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof PropertyPermission))
      return false;
    PropertyPermission p = (PropertyPermission) obj;
    return actions == p.actions && super.equals (p);
  }

  /**
   * Reads an object from the stream. This converts the external to the
   * internal representation.
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
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    ObjectOutputStream.PutField fields = s.putFields();
    fields.put("actions", getActions());
    s.writeFields();
  }

  /**
   * Returns a permission collection suitable to take
   * PropertyPermission objects.
   * @return a new empty PermissionCollection.  
   */
  public PermissionCollection newPermissionCollection()
  {
    return new PermissionCollection()
    {
      Hashtable permissions = new Hashtable();
      int allActions = 0;

      public void add(Permission permission)
      {
	if (isReadOnly())
	  throw new IllegalStateException("readonly");

	// also check that permission is of correct type.
	PropertyPermission pp = (PropertyPermission) permission;
	String name = pp.getName();
	if (name.equals("*"))
	  allActions |= pp.actions;
	permissions.put(name, pp);
      }

      public boolean implies(Permission permission)
      {
	if (!(permission instanceof PropertyPermission))
	  return false;

	PropertyPermission toImply = (PropertyPermission) permission;
	if ((toImply.actions & ~allActions) == 0)
	  return true;

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
	    if (forName != null && (toImply.actions & ~forName.actions) == 0)
	      return true;

	    prefixLength = name.lastIndexOf('.', prefixLength);
	    if (prefixLength < 0)
	      return false;
	    name = name.substring(0, prefixLength + 1) + '*';
	  }
      }

      public Enumeration elements()
      {
	return permissions.elements();
      }
    };
  }
}
