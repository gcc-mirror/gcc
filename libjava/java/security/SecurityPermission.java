/* SecurityPermission.java -- Class for named security permissions
   Copyright (C) 1998 Free Software Foundation, Inc.

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

/**
 * This class provides a mechanism for specified named permissions 
 * related to the Java security framework.  These permissions have no
 * associated actions list.  They are either granted or no granted.
 * <p>
 * The list of valid permission names is:
 * <p><ul>
 * <li>getPolicy - Allows retrieval of the system security policy.
 * <li>setPolicy - Allows the security policy to be changed.
 * <li>getProperty.&lt;key&gt; - Allows retrieval of the value of the named
 * property or all properties if &lt;key&gt is a '*'.
 * <li>setProperty.&lt;key&gt; - Allows changing of the value of the named
 * property or all properties if &lt;key&gt is a '*'.
 * <li>insertProvider.&lt;key&gt; - Allows the named provider to be added,
 * or any provide if the key is '*'.
 * <li>removeProvider.&lt;key&gt; - Allows the named provider to be removed,
 * or any provide if the key is '*'.
 * <li>setSystemScope - Allows the system identity scope to be set.
 * <li>setIdentityPublicKey - Allows the public key of an Identity to be set.
 * <li>SetIdentityInfo - Allows the description of an Identity to be set.
 * <li>addIdentityCertificate - Allows a certificate to be set for the public
 * key of an identity.
 * <li>removeIdentityCertificate - Allows a certificate to be removed from the 
 * public key of an identity.
 * <li>clearProviderProperties.&lt;key%gt; - Allows the properties of the
 * named provider to be cleared, or all providers if key is '*'.
 * <li>putProviderProperty.&lt;key%gt; - Allows the properties of the
 * named provider to be changed, or all providers if key is '*'.
 * <li>removeProviderProperty.&lt;key%gt; - Allows the properties of the
 * named provider to be deleted, or all providers if key is '*'.
 * <li>getSignerPrivateKey - Allows the retrieval of the private key for
 * a signer.
 * <li>setSignerKeyPair - Allows the public and private key of a Signer to
 * be changed.
 * </ul>
 * <p>
 * There is some degree of security risk in granting any of these permissions.
 * Some of them can completely compromise system security.  Please exercise
 * extreme caution in granting these permissions.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public final class SecurityPermission extends BasicPermission
{
  /**
   * This method initializes a new instance of <code>SecurityPermission</code>
   * to have the specified name.
   *
   * @param name The name to assign to this permission.
   */
  public SecurityPermission(String name)
  {
    super(name);
  }

  /**
   * This method initializes a new instance of <code>SecurityPermission</code>
   * to have the specified name.  The actions parameter is ignored in this
   * class.
   *
   * @param name The name to assign to this permission.
   * @param actions The action list for this permission - ignored.
   */
  public SecurityPermission(String name, String actions)
  {
    super(name, actions);
  }
}
