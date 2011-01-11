/* MBeanServerPermission.java -- Permissions controlling server creation.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

import java.security.BasicPermission;
import java.security.Permission;
import java.security.PermissionCollection;

import java.util.Enumeration;
import java.util.NoSuchElementException;

/**
 * <p>
 * Represents the permissions required to perform
 * operations provided by the {@link MBeanServerFactory}.
 * As with all {@link java.security.Permission} objects, an
 * instance of this class either represents a permission
 * already held or one that is required to access a
 * particular service.  In the case of {@link MBeanServerPermission}s,
 * implication checks are made using an instance of this class
 * when a user requests an operation from the factory, and a
 * {@link SecurityManager} is in place.
 * </p>
 * <p>
 * The permission is defined by its name, which may be
 * either a <code>'*'</code> (to allow all) or one or
 * more of the following, separated by a <code>','</code>:
 * </p>
 * <ul>
 * <li><code>createMBeanServer</code> -- allows a registered
 * instance of a server to be obtained from the factory.</li>
 * <li><code>findMBeanServer</code> -- allows all or one
 * particular server instance to be retrieved from the factory.</li>
 * <li><code>newMBeanServer</code> -- allows an unregistered
 * instance of a server to be obtained from the factory.</li>
 * <li><code>releaseMBeanServer</code> -- allows a reference to
 * a server instance to be removed from the factory.</li>
 * </ul>
 * <p>
 * The names may be surrounded by arbitrary amounts of whitespace.
 * <code>createMBeanServer</code> implies <code>newMBeanServer</code>.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanServerPermission
  extends BasicPermission
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -5661980843569388590L;

  /**
   * <p>
   * Constructs a new {@link MBeanServerPermission} with
   * the given name.  The name must not be <code>null</code>
   * and must be equal to either <code>"*"</code> or a
   * comma-separated list of valid permissions.  The four
   * valid constraints are:
   * </p>
   * <ol>
   * <li><code>createMBeanServer</code></li>
   * <li><code>findMBeanServer</code></li>
   * <li><code>newMBeanServer</code></li>
   * <li><code>releaseMBeanServer</code></li>
   * </ol>
   * <p>
   * Calling this constructor is equivalent to calling
   * <code>MBeanPermission(name, null)</code>.
   * </p>
   *
   * @param name the name of this permission.
   * @throws NullPointerException if <code>name</code>
   *                              is <code>null</code>.
   * @throws IllegalArgumentException if <code>name</code>
   *                                  is not either equal to
   *                                  <code>"*"</code> or forms
   *                                  a comma-separated list of
   *                                  valid constraints.
   * @see #MBeanServerPermission(String,String)
   */
  public MBeanServerPermission(String name)
  {
    this(name, null);
  }

  /**
   * <p>
   * Constructs a new {@link MBeanServerPermission} with
   * the given name and actions.  The actions are unused,
   * and must be either <code>null</code> or the empty
   * string.  The name must not be <code>null</code>
   * and must be equal to either <code>"*"</code> or a
   * comma-separated list of valid permissions.  The four
   * valid constraints are:
   * </p>
   * <ol>
   * <li><code>createMBeanServer</code></li>
   * <li><code>findMBeanServer</code></li>
   * <li><code>newMBeanServer</code></li>
   * <li><code>releaseMBeanServer</code></li>
   * </ol>
   * <p>
   * Calling this constructor is equivalent to calling
   * <code>MBeanPermission(name, null)</code>.
   * </p>
   *
   * @param name the name of this permission.
   * @throws NullPointerException if <code>name</code>
   *                              is <code>null</code>.
   * @throws IllegalArgumentException if <code>name</code>
   *                                  is not either equal to
   *                                  <code>"*"</code> or forms
   *                                  a comma-separated list of
   *                                  valid constraints, or if
   *                                  <code>actions</code> is not
   *                                  <code>null</code> or the
   *                                  empty string.
   * @see #MBeanServerPermission(String,String)
   */
  public MBeanServerPermission(String name, String actions)
  {
    super(checkName(name), actions);
    if (actions != null && actions.length() > 0)
      throw new IllegalArgumentException("The supplied action list " +
                                         "was not equal to null or the " +
                                         "empty string.");
  }

  /**
   * Returns true if the given object is also an {@link MBeanServerPermission}
   * with the same name.
   *
   * @param obj the object to compare with this one.
   * @return true if the object is an {@link MBeanPermission}
   *         with the same name.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof MBeanServerPermission)
      {
        MBeanServerPermission o = (MBeanServerPermission) obj;
        return o.getName().equals(getName());
      }
    return false;
  }

  /**
   * Returns a unique hash code for this permission.
   * This is simply the hashcode of {@link BasicPermission#getName()}.
   *
   * @return the hashcode of this permission.
   */
  public int hashCode()
  {
    return getName().hashCode();
  }

  /**
   * Returns true if this {@link MBeanServerPermission} implies
   * the given permission.  This occurs if the given permission
   * is also an {@link MBeanServerPermission} and its target names
   * are a subset of the target names of this permission.  Note that
   * the name <code>createMBeanServer</code> implies
   * <code>newMBeanServer</code>.
   *
   * @param p the permission to check for implication.
   * @return true if this permission implies <code>p</code>.
   */
  public boolean implies(Permission p)
  {
    if (p instanceof MBeanServerPermission)
      {
        if (getName().equals("*"))
          return true;
        MBeanServerPermission msp = (MBeanServerPermission) p;
        String[] thisCaps = getName().split(",");
        String[] mspCaps = msp.getName().split(",");
        for (int a = 0; a < mspCaps.length; ++a)
          {
            boolean found = false;
            String mc = mspCaps[a].trim();
            for (int b = 0; b < thisCaps.length; ++b)
              {
                String tc = thisCaps[b].trim();
                if (tc.equals(mc))
                  found = true;
                if (tc.equals("createMBeanServer") &&
                    mc.equals("newMBeanServer"))
                  found = true;
              }
            if (!found)
              return false;
          }
        return true;
      }
    return false;
  }

  /**
   * Returns a {@link PermissionCollection} which stores
   * a series of {@link MBeanServerPermission}s as the union
   * of their capabilities.
   *
   * @return a collection for {@link MBeanServerPermission}s.
   */
  public PermissionCollection newPermissionCollection()
  {
    return new MBeanServerPermissionCollection();
  }

  /**
   * A collection of {@link MBeanServerPermission}s, stored
   * as a single permission with the union of the capabilities
   * as its capabilities.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private class MBeanServerPermissionCollection
    extends PermissionCollection
  {

    /**
     * Compatible with JDK 1.5
     */
    private static final long serialVersionUID = -5661980843569388590L;

    /**
     * The collected permission.  This is <code>null</code> or
     * the union of the permissions held by all the collected
     * permissions.
     */
    private MBeanServerPermission collectionPermission;

    /**
     * Adds a new permission by unifying it with the existing
     * collection permission.
     *
     * @param p the permission to add.
     * @throws SecurityException if the collection is read only.
     * @see #isReadOnly()
     * @see #setReadOnly(boolean)
     */
    @Override
    public void add(Permission p)
    {
      if (isReadOnly())
        throw new SecurityException("This collection is read only.");
      if (p instanceof MBeanServerPermission)
        {
          MBeanServerPermission msp = (MBeanServerPermission) p;
          if (collectionPermission == null)
            collectionPermission = msp;
          else
            {
              String finalString = collectionPermission.getName();
              String[] cp = finalString.split(",");
              String[] np = msp.getName().split(",");
              int createms = finalString.indexOf("createMBeanServer");
              int newms = finalString.indexOf("newMBeanServer");
              for (int a = 0; a < np.length; ++a)
                {
                  boolean found = false;
                  String nps = np[a].trim();
                  for (int b = 0; b < cp.length; ++b)
                    {
                      String cps = cp[b].trim();
                      if (cps.equals(nps))
                        found = true;
                      if (nps.equals("newMBeanServer")
                          && createms != -1)
                        found = true;
                      if (nps.equals("createMBeanServer")
                          && newms != -1)
                        finalString =
                          finalString.replace("newMBeanServer",
                                              "createMBeanServer");
                    }
                  if (!found)
                    finalString += "," + nps;
                }
              collectionPermission =
                new MBeanServerPermission(finalString);
            }
        }
    }

    /**
     * Returns an enumeration over the single permission.
     *
     * @return an enumeration over the collection permission.
     */
    @Override
    public Enumeration<Permission> elements()
    {
      return new
        MBeanServerPermissionEnumeration(collectionPermission);
    }

    /**
     * Provides an enumeration over a comma-separated list
     * of capabilities.
     *
     * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
     * @since 1.5
     */
    private class MBeanServerPermissionEnumeration
      implements Enumeration<Permission>
    {

      /**
       * The collected permission.
       */
      private MBeanServerPermission p;

      /**
       * True if we have returned the permission.
       */
      private boolean done;

      /**
       * Constructs a new {@link MBeanServerPermissionEnumeration}
       * using the given collected permission.
       *
       * @param p the collected permission.
       */
      public MBeanServerPermissionEnumeration(MBeanServerPermission p)
      {
        this.p = p;
        done = false;
      }

      /**
       * Returns true if there are more capabilities to return.
       *
       * @return true if there are more capabilities available.
       */
      public boolean hasMoreElements()
      {
        return !done;
      }

      /**
       * Returns the next capability.
       *
       * @return the next capability.
       */
      public Permission nextElement()
      {
        if (hasMoreElements())
          {
            done = true;
            return p;
          }
        else
          throw new NoSuchElementException("No more elements are available.");
      }

    }

    /**
     * Returns true if the collected {@link MBeanServerPermission}
     * implies the given permission.  This occurs if the given permission
     * is also an {@link MBeanServerPermission} and its target names
     * are a subset of the target names of this permission.  Note that
     * the name <code>createMBeanServer</code> implies
     * <code>newMBeanServer</code>.
     *
     * @param p the permission to check for implication.
     * @return true if this permission implies <code>p</code>.
     */
    @Override
    public boolean implies(Permission p)
    {
      return collectionPermission.implies(p);
    }
  }

  /**
   * Checks the name is valid, including removing
   * the <code>newMBeanServer</code> permission when
   * <code>createMBeanServer</code> is present.
   *
   * @param name the name to check.
   * @throws NullPointerException if <code>name</code>
   *                              is <code>null</code>.
   * @throws IllegalArgumentException if <code>name</code>
   *                                  is not either equal to
   *                                  <code>"*"</code> or forms
   *                                  a comma-separated list of
   *                                  valid constraints.
   */
  private static String checkName(String name)
  {
    if (!(name.equals("*")))
      {
        String[] constraints = name.split(",");
        name = "";
        boolean seenCreate = false;
        boolean seenNew = false;
        boolean start = true;
        for (int a = 0; a < constraints.length; ++a)
          {
            String next = constraints[a].trim();
            if (!(next.equals("createMBeanServer") ||
                  next.equals("findMBeanServer") ||
                  next.equals("newMBeanServer") ||
                  next.equals("releaseMBeanServer")))
              throw new IllegalArgumentException("An invalid constraint, " +
                                                 next + ", was specified.");
            if (next.equals("newMBeanServer"))
              seenNew = true;
            else if (next.equals("createMBeanServer"))
              seenCreate = true;
            else
              {
                if (!start)
                  name += ",";
                name += next;
                start = false;
              }
          }
        if (seenNew && !seenCreate)
          name += (start ? "" : ",") + "newMBeanServer";
        else if (seenCreate)
          name += (start ? "" : ",") + "createMBeanServer";
      }
    return name;
  }

}
