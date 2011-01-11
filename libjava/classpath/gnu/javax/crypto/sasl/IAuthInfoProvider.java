/* IAuthInfoProvider.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.sasl;

import java.util.Map;

import javax.security.sasl.AuthenticationException;

/**
 * The visible methods of any authentication information provider.
 */
public interface IAuthInfoProvider
{
  /**
   * Activates (initialises) this provider instance. SHOULD be the first method
   * invoked on the provider.
   *
   * @param context a collection of name-value bindings describing the
   *          activation context.
   * @throws AuthenticationException if an exception occurs during the
   *           operation.
   */
  void activate(Map context) throws AuthenticationException;

  /**
   * Passivates (releases) this provider instance. SHOULD be the last method
   * invoked on the provider. Once it is done, no other method may be invoked on
   * the same instance before it is <i>activated</i> agains.
   *
   * @throws AuthenticationException if an exception occurs during the
   *           operation.
   */
  void passivate() throws AuthenticationException;

  /**
   * Checks if a user with a designated name is known to this provider.
   *
   * @param userName the name of a user to check.
   * @return <code>true</code> if the user with the designated name is known
   *         to this provider; <code>false</code> otherwise.
   * @throws AuthenticationException if an exception occurs during the
   *           operation.
   */
  boolean contains(String userName) throws AuthenticationException;

  /**
   * Returns a collection of information about a designated user. The contents
   * of the returned map is provider-specific of name-to-value mappings.
   *
   * @param userID a map of name-to-value bindings that fully describe a user.
   * @return a collection of information about the designated user.
   * @throws AuthenticationException if an exception occurs during the
   *           operation.
   */
  Map lookup(Map userID) throws AuthenticationException;

  /**
   * Updates the credentials of a designated user.
   *
   * @param userCredentials a map of name-to-value bindings that fully describe
   *          a user, including per new credentials.
   * @throws AuthenticationException if an exception occurs during the
   *           operation.
   */
  void update(Map userCredentials) throws AuthenticationException;

  /**
   * A provider may operate in more than mode; e.g. SRP-II caters for user
   * credentials computed in more than one message digest algorithm. This method
   * returns the set of name-to-value bindings describing the mode of the
   * provider.
   *
   * @param mode a unique identifier describing the operational mode.
   * @return a collection of name-to-value bindings describing the designated
   *         mode.
   * @throws AuthenticationException if an exception occurs during the
   *           operation.
   */
  Map getConfiguration(String mode) throws AuthenticationException;
}
