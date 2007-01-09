/* SaslClientFactory.java
   Copyright (C) 2003, 2005, Free Software Foundation, Inc.

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


package javax.security.sasl;

import java.util.Map;

import javax.security.auth.callback.CallbackHandler;

/**
 * <p>An interface for creating instances of {@link SaslClient}. A class that
 * implements this interface must be thread-safe and handle multiple
 * simultaneous requests. It must also have a public constructor that accepts
 * no arguments.</p>
 *
 * <p>This interface is not normally accessed directly by a client, which will
 * use the {@link Sasl} static methods to create a client instance instead.
 * However, a particular environment may provide and install a new or different
 * <code>SaslClientFactory</code>.</p>
 *
 * @see SaslClient
 * @see Sasl
 *
 * @since 1.5
 */
public interface SaslClientFactory
{

  /**
   * Creates a {@link SaslClient} using the parameters supplied.
   *
   * @param mechanisms the non-null list of mechanism names to try. Each is the
   * IANA-registered name of a SASL mechanism (e.g. "GSSAPI", "CRAM-MD5").
   * @param authorizationID the possibly null protocol-dependent identification
   * to be used for authorization. If <code>null</code> or empty, the server
   * derives an authorization ID from the client's authentication credentials.
   * When the SASL authentication completes successfully, the specified entity
   * is granted access.
   * @param protocol the non-null string name of the protocol for which the
   * authentication is being performed (e.g. "ldap").
   * @param serverName the non-null fully qualified host name of the server to
   * authenticate to.
   * @param props the possibly <code>null</code> set of properties used to
   * select the SASL mechanism and to configure the authentication exchange of
   * the selected mechanism. See the {@link Sasl} class for a list of standard
   * properties. Other, possibly mechanism-specific, properties can be included.
   * Properties not relevant to the selected mechanism are ignored.
   * @param cbh the possibly <code>null</code> callback handler to used by the
   * SASL mechanisms to get further information from the application/library to
   * complete the authentication. For example, a SASL mechanism might require
   * the authentication ID, password and realm from the caller. The
   * authentication ID is requested by using a
   * {@link javax.security.auth.callback.NameCallback}. The password is
   * requested by using a {@link javax.security.auth.callback.PasswordCallback}.
   * The realm is requested by using a {@link RealmChoiceCallback} if there is
   * a list of realms to choose from, and by using a {@link RealmCallback} if
   * the realm must be entered.
   * @return a possibly <code>null</code> {@link SaslClient} created using the
   * parameters supplied. If <code>null</code>, this factory cannot produce a
   * {@link SaslClient} using the parameters supplied.
   * @throws SaslException if a {@link SaslClient} instance cannot be created
   * because of an error.
   */
  SaslClient createSaslClient(String[] mechanisms, String authorizationID,
                              String protocol, String serverName,
                              Map<String, ?> props, CallbackHandler cbh)
    throws SaslException;

  /**
   * Returns an array of names of mechanisms that match the specified mechanism
   * selection policies.
   *
   * @param props the possibly <code>null</code> set of properties used to
   * specify the security policy of the SASL mechanisms. For example, if props
   * contains the {@link Sasl#POLICY_NOPLAINTEXT} property with the value
   * <code>"true"</code>, then the factory must not return any SASL mechanisms
   * that are susceptible to simple plain passive attacks. See the {@link Sasl}
   * class for a complete list of policy properties. Non-policy related
   * properties, if present in props, are ignored.
   * @return a non-null array containing IANA-registered SASL mechanism names.
   */
  String[] getMechanismNames(Map<String, ?> props);
}
