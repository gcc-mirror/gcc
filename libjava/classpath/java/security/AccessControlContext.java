/* AccessControlContext.java --- Access Control Context Class
   Copyright (C) 1999, 2004 Free Software Foundation, Inc.

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

package java.security;

import java.util.HashSet;

/**
 * AccessControlContext makes system resource access decsion
 * based on permission rights.
 *
 * It is used for a specific context and has only one method
 * checkPermission. It is similar to AccessController except
 * that it makes decsions based on the current context instead
 * of the the current thread.
 *
 * It is created by call AccessController.getContext method.
 *
 * @author Mark Benvenuto
 * @since 1.2
 */
public final class AccessControlContext
{
  private final ProtectionDomain[] protectionDomains;
  private final DomainCombiner combiner;

  /**
   * Construct a new AccessControlContext with the specified
   * ProtectionDomains. <code>context</code> must not be
   * null and duplicates will be removed.
   *
   * @param context The ProtectionDomains to use
   */
  public AccessControlContext(ProtectionDomain[] context)
  {
    HashSet domains = new HashSet (context.length);
    for (int i = 0; i < context.length; i++)
      domains.add (context[i]);
    protectionDomains = (ProtectionDomain[])
      domains.toArray (new ProtectionDomain[domains.size()]);
    combiner = null;
  }

  /**
   * Construct a new AccessControlContext with the specified
   * {@link ProtectionDomain}s and {@link DomainCombiner}.
   *
   * <p>Code calling this constructor must have a {@link
   * SecurityPermission} of <i>createAccessControlContext</i>.</p>
   *
   * @throws SecurityException If the caller does not have permission
   * to create an access control context.
   * @since 1.3
   */
  public AccessControlContext(AccessControlContext acc,
                              DomainCombiner combiner)
  {
    AccessControlContext acc2 = null;
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      {
        Permission perm =
          new SecurityPermission ("createAccessControlContext");

        // The default SecurityManager.checkPermission(perm) just calls
        // AccessController.checkPermission(perm) which in turn just
        // calls AccessController.getContext().checkPermission(perm).
        // This means AccessController.getContext() is called twice,
        // once for the security check and once by us.  It's a very
        // expensive call (on gcj at least) so if we're using the
        // default security manager we avoid this duplication.
        if (sm.getClass() == SecurityManager.class)
          {
            acc2 = AccessController.getContext ();
            acc2.checkPermission (perm);
          }
        else
          sm.checkPermission (perm);
      }
    if (acc2 == null)
      acc2 = AccessController.getContext ();
    protectionDomains = combiner.combine (acc2.protectionDomains,
                                          acc.protectionDomains);
    this.combiner = combiner;
  }

  AccessControlContext (ProtectionDomain[] domains, AccessControlContext acc,
                        DomainCombiner combiner)
  {
    protectionDomains = combiner.combine (domains, acc.protectionDomains);
    this.combiner = combiner;
  }

  /**
   * Returns the Domain Combiner associated with the AccessControlContext
   *
   * @return the DomainCombiner
   */
  public DomainCombiner getDomainCombiner()
  {
    return combiner;
  }

  /**
   * Determines whether or not the specific permission is granted
   * depending on the context it is within.
   *
   * @param perm a permission to check
   *
   * @throws AccessControlException if the permssion is not permitted
   */
  public void checkPermission(Permission perm) throws AccessControlException
  {
    if (protectionDomains.length == 0)
      throw new AccessControlException ("permission "
                                        + perm
                                        + " not granted: no protection domains");

    for (int i = 0; i < protectionDomains.length; i++)
      {
        final ProtectionDomain domain = protectionDomains[i];
        if (!domain.implies(perm))
          throw new AccessControlException ("permission "
                                            + perm
                                            + " not granted: "
                                            + domain
                                            + " does not imply it.");
      }
  }

  /**
   * Checks if two AccessControlContexts are equal.
   *
   * It first checks if obj is an AccessControlContext class, and
   * then checks if each ProtectionDomain matches.
   *
   * @param obj The object to compare this class to
   *
   * @return true if equal, false otherwise
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof AccessControlContext)
      {
        AccessControlContext acc = (AccessControlContext) obj;

        if (acc.protectionDomains.length != protectionDomains.length)
          return false;

        int i, j;
        for (i = 0; i < protectionDomains.length; i++)
          {
            for (j = 0; j < acc.protectionDomains.length; j++)
              {
                if (acc.protectionDomains[j].equals (protectionDomains[i]))
                  break;
              }
            if (j == acc.protectionDomains.length)
              return false;
          }
        return true;
      }
    return false;
  }

  /**
   * Computes a hash code of this class
   *
   * @return a hash code representing this class
   */
  public int hashCode()
  {
    int h = 0;
    for (int i = 0; i < protectionDomains.length; i++)
      h ^= protectionDomains[i].hashCode();

    return h;
  }

  ProtectionDomain[] getProtectionDomains ()
  {
    return protectionDomains;
  }
}
