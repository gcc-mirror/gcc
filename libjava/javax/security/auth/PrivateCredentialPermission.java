/* PrivateCredentialPermission.java -- permissions governing private credentials.
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.security.auth;

import java.io.Serializable;

import java.security.Permission;
import java.security.PermissionCollection;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;

/**
 * A permission governing access to a private credential. The action of this
 * permission is always "read" -- meaning that the private credential
 * information can be read from an object.
 *
 * <p>The target of this permission is formatted as follows:</p>
 *
 * <p><code>CredentialClassName ( PrinicpalClassName PrincipalName )*</code></p>
 *
 * <p><i>CredentialClassName</i> is either the name of a private credential
 * class name, or a wildcard character (<code>'*'</code>).
 * <i>PrinicpalClassName</i> is the class name of a principal object, and
 * <i>PrincipalName</i> is a string representing the principal, or the
 * wildcard character.</p>
 */
public final class PrivateCredentialPermission extends Permission
  implements Serializable
{
  /**
   * For compatability with Sun's JDK 1.4.2 rev. 5
   */
  private static final long serialVersionUID = 5284372143517237068L;

  // Fields.
  // -------------------------------------------------------------------------

  /**
   * @serial The credential class name.
   */
  private final String credentialClass;

  /**
   * @serial The principals, a set of CredOwner objects (an undocumented
   *  inner class of this class).
   */
  private final Set principals;

  /**
   * @serial Who knows?
   */
  private final boolean testing;

  // Constructor.
  // -------------------------------------------------------------------------

  /**
   * Create a new private credential permission.
   *
   * @param name The permission target name.
   * @param actions The list of actions, which, for this class, must be
   *  <code>"read"</code>.
   */
  public PrivateCredentialPermission (final String name, String actions)
  {
    super(name);
    actions = actions.trim().toLowerCase();
    if (!"read".equals (actions))
      {
        throw new IllegalArgumentException("actions must be \"read\"");
      }
    StringTokenizer st = new StringTokenizer (name, " \"'");
    principals = new HashSet();
    if (st.countTokens() < 3 || (st.countTokens() & 1) == 0)
      {
        throw new IllegalArgumentException ("badly formed credential name");
      }
    credentialClass = st.nextToken();
    while (st.hasMoreTokens())
      {
        principals.add (new CredOwner (st.nextToken(), st.nextToken()));
      }
    testing = false; // WTF ever.
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public boolean equals (Object o)
  {
    if (! (o instanceof PrivateCredentialPermission))
      {
        return false;
      }
    PrivateCredentialPermission that = (PrivateCredentialPermission) o;
    if (!that.getActions().equals (getActions()))
      {
        return false;
      }
    if (!that.getCredentialClass().equals (getCredentialClass()))
      {
        return false;
      }

    final String[][] principals = getPrincipals();
    final String[][] that_principals = that.getPrincipals();
    if (that_principals == null)
      {
        return false;
      }
    if (that_principals.length != principals.length)
      {
        return false;
      }
    for (int i = 0; i < principals.length; i++)
      {
        if (!principals[i][0].equals (that_principals[i][0]) ||
            !principals[i][1].equals (that_principals[i][1]))
          {
            return false;
          }
      }
    return true;
  }

  /**
   * Returns the actions this permission encompasses. For private credential
   * permissions, this is always the string <code>"read"</code>.
   *
   * @return The list of actions.
   */
  public String getActions()
  {
    return "read";
  }

  /**
   * Returns the credential class name that was embedded in this permission's
   * target name.
   *
   * @return The credential class name.
   */
  public String getCredentialClass()
  {
    return credentialClass;
  }

  /**
   * Returns the principal list that was embedded in this permission's target
   * name.
   *
   * <p>Each element of the returned array is a pair; the first element is the
   * principal class name, and the second is the principal name.
   *
   * @return The principal list.
   */
  public String[][] getPrincipals()
  {
    String[][] ret = new String[principals.size()][];
    Iterator it = principals.iterator();
    for (int i = 0; i < principals.size() && it.hasNext(); i++)
      {
        CredOwner co = (CredOwner) it.next();
        ret[i] = new String[] { co.getPrincipalClass(), co.getPrincipalName() };
      }
    return ret;
  }

  public int hashCode()
  {
    return credentialClass.hashCode() + principals.hashCode();
  }

  /**
   * Test if this permission implies another. This method returns true if:
   *
   * <ol>
   * <li><i>p</i> is an instance of PrivateCredentialPermission</li>.
   * <li>The credential class name of this instance matches that of <i>p</i>,
   * and one of the principals of <i>p</i> is contained in the principals of
   * this class. Thus,
   *   <ul>
   *   <li><code>[ * P "foo" ]  implies [ C P "foo" ]</code></li>
   *   <li><code>[ C P1 "foo" ] implies [ C P1 "foo" P2 "bar" ]</code></li>
   *   <li><code>[ C P1 "*" ]   implies [ C P1 "foo" ]</code></li>
   *   </ul>
   * </ol>
   *
   * @param p The permission to check.
   * @return True if this permission implies <i>p</i>.
   */
  public boolean implies (Permission p)
  {
    if (! (p instanceof PrivateCredentialPermission))
      {
        return false;
      }
    PrivateCredentialPermission that = (PrivateCredentialPermission) p;
    if (!credentialClass.equals ("*")
        && !credentialClass.equals (that.getCredentialClass()))
      {
        return false;
      }
    String[][] principals = getPrincipals();
    String[][] that_principals = that.getPrincipals();
    if (that_principals == null)
      {
        return false;
      }
    for (int i = 0; i < principals.length; i++)
      {
        for (int j = 0; j < that_principals.length; j++)
          {
            if (principals[i][0].equals (that_principals[j][0]) &&
                (principals[i][1].equals ("*") ||
                 principals[i][1].equals (that_principals[j][1])))
              {
                return true;
              }
          }
      }
    return false;
  }

  /**
   * This method is not necessary for this class, thus it always returns null.
   *
   * @return null.
   */
  public PermissionCollection newPermissionCollection()
  {
    return null;
  }

  // Inner class.
  // -------------------------------------------------------------------------

  /**
   * An undocumented inner class present for serialization compatibility.
   */
  private static class CredOwner implements Serializable
  {

    // Fields.
    // -----------------------------------------------------------------------

    private final String principalClass;
    private final String principalName;

    // Constructor.
    // -----------------------------------------------------------------------

    CredOwner (final String principalClass, final String principalName)
    {
      this.principalClass = principalClass;
      this.principalName = principalName;
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public boolean equals (Object o)
    {
      if (!(o instanceof CredOwner))
        {
          return false;
        }
      return principalClass.equals (((CredOwner) o).getPrincipalClass()) &&
        principalName.equals (((CredOwner) o).getPrincipalName());
    }

    public int hashCode()
    {
      return principalClass.hashCode() + principalName.hashCode();
    }

    public String getPrincipalClass()
    {
      return principalClass;
    }

    public String getPrincipalName()
    {
      return principalName;
    }
  }
}
