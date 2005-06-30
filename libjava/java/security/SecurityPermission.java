/* SecurityPermission.java -- Class for named security permissions
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

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

/**
 * This class provides a mechanism for specified named permissions
 * related to the Java security framework.  These permissions have no
 * associated actions list.  They are either granted or not granted.
 *
 * <p>The list of valid permission names is:<br>
 * <table border=1>
 * <tr><th>Permission Name</th><th>Permission Allows</th><th>Risks</th</tr>
 * <tr>
 *   <td><code>createAccessControlContext</code></td>
 *   <td>Allows creation of an AccessControlContext</td>
 *   <td>The new control context can have a rogue DomainCombiner, leading
 *       to a privacy leak</td></tr>
 * <tr>
 *   <td><code>getDomainCombiner</code></td>
 *   <td>Get a DomainCombiner from an AccessControlContext</td>
 *   <td>Access to a DomainCombiner can lead to a privacy leak</td></tr>
 * <tr>
 *   <td><code>getPolicy</code></td>
 *   <td>Allows retrieval of the system security policy</td>
 *   <td>Malicious code can use information from the policy to better plan
 *       an attack</td></tr>
 * <tr>
 *   <td><code>setPolicy</code></td>
 *   <td>Allows the security policy to be changed</td>
 *   <td>Malicious code can give itself any permission it wants</td></tr>
 * <tr>
 *   <td><code>getProperty.</code><em>key</em></td>
 *   <td>Retrieve the property specified by the key</td>
 *   <td>Malicious code can use information from the property to better plan
 *       an attack</td></tr>
 * <tr>
 *   <td><code>setProperty.</code><em>key</em></td>
 *   <td>Allows changing of the value of all properties implied by key</td>
 *   <td>Malicious code can insert rogue classes to steal keys or recreate
 *       the security policy with whatever permissions it desires</td></tr>
 * <tr>
 *   <td><code>insertProvider.</code><em>key</em></td>
 *   <td>Allows the named provider to be added</td>
 *   <td>Malicious code can insert rogue providers that steal data</td></tr>
 * <tr>
 *   <td><code>removeProvider.</code><em>key</em></td>
 *   <td>Allows the named provider to be removed</td>
 *   <td>A missing provider can cripple code that relies on it</td></tr>
 * <tr>
 *   <td><code>setSystemScope</code></td>
 *   <td>Allows the system identity scope to be set</td>
 *   <td>Malicious code can add certificates not available in the original
 *       identity scope, to gain more permissions</td></tr>
 * <tr>
 *   <td><code>setIdentityPublicKey</code></td>
 *   <td>Allows the public key of an Identity to be set</td>
 *   <td>Malicious code can install its own key to gain permissions not
 *       allowed by the original identity scope</td></tr>
 * <tr>
 *   <td><code>SetIdentityInfo</code></td>
 *   <td>Allows the description of an Identity to be set</td>
 *   <td>Malicious code can spoof users into trusting a fake identity</td></tr>
 * <tr>
 *   <td><code>addIdentityCertificate</code></td>
 *   <td>Allows a certificate to be set for the public key of an identity</td>
 *   <td>The public key can become trusted to a wider audience than originally
 *       intended</td></tr>
 * <tr>
 *   <td><code>removeIdentityCertificate</code></td>
 *   <td>Allows removal of a certificate from an identity's public key</td>
 *   <td>The public key can become less trusted than it should be</td></tr>
 * <tr>
 *   <td><code>printIdentity</code></td>
 *   <td>View the name of the identity and scope, and whether they are
 *       trusted</td>
 *   <td>The scope may include a filename, which provides an entry point for
 *       further security breaches</td></tr>
 * <tr>
 *   <td><code>clearProviderProperties.</code><em>key</em></td>
 *   <td>Allows the properties of the named provider to be cleared</td>
 *   <td>This can disable parts of the program which depend on finding the
 *       provider</td></tr>
 * <tr>
 *   <td><code>putProviderProperty.</code><em>key</em></td>
 *   <td>Allows the properties of the named provider to be changed</td>
 *   <td>Malicious code can replace the implementation of a provider</td></tr>
 * <tr>
 *   <td><code>removeProviderProperty.</code><em>key</em></td>
 *   <td>Allows the properties of the named provider to be deleted</td>
 *   <td>This can disable parts of the program which depend on finding the
 *       provider</td></tr>
 * <tr>
 *   <td><code>getSignerPrivateKey</code></td>
 *   <td>Allows the retrieval of the private key for a signer</td>
 *   <td>Anyone that can access the private key can claim to be the
 *       Signer</td></tr>
 * <tr>
 *   <td><code>setSignerKeyPair</code></td>
 *   <td>Allows the public and private key of a Signer to be changed</td>
 *   <td>The replacement might be a weaker encryption, or the attacker
 *       can use knowledge of the replaced key to decrypt an entire
 *       communication session</td></tr>
 * </table>
 *
 * <p>There is some degree of security risk in granting any of these
 * permissions. Some of them can completely compromise system security.
 * Please exercise extreme caution in granting these permissions.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see Permission
 * @see SecurityManager
 * @since 1.1
 * @status updated to 1.4
 */
public final class SecurityPermission extends BasicPermission
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 5236109936224050470L;

  /**
   * Create a new instance with the specified name.
   *
   * @param name the name to assign to this permission
   */
  public SecurityPermission(String name)
  {
    super(name);
  }

  /**
   * Create a new instance with the specified name. As SecurityPermission
   * carries no actions, the second parameter is ignored.
   *
   * @param name the name to assign to this permission
   * @param actions ignored
   */
  public SecurityPermission(String name, String actions)
  {
    super(name);
  }
} // class SecurityPermission
