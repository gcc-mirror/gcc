/* X509KeyManager.java -- X.509 key manager interface.
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


package javax.net.ssl;

import java.net.Socket;

import java.security.Principal;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;

/**
 * A key manager for X.509 certificates and their associated private keys.
 */
public interface X509KeyManager extends KeyManager
{

  /**
   * Choose an alias for client-side authentication.
   *
   * @param keyTypes A list of acceptable key types.
   * @param issuers A list of acceptable certificate issuers.
   * @param socket The connecting socket.
   * @return The chosen alias.
   */
  String chooseClientAlias(String[] keyTypes, Principal[] issuers,
                           Socket socket);

  /**
   * Choose an alias for server-side authentication.
   *
   * @param keyType The desired certificate type.
   * @param issuers A list of acceptable certificate issuers.
   * @param socket The connecting socket.
   * @return The chosen alias.
   */
  String chooseServerAlias(String keyType, Principal[] issuers,
                           Socket socket);

  /**
   * Gets the X.509 certificate chain associated with the given alias.
   *
   * @param alias The alias.
   * @return The certificate chain.
   */
  X509Certificate[] getCertificateChain(String alias);

  /**
   * Returns all client aliases that support the given key type.
   *
   * @param keyType The desired key type.
   * @param issuers A list of acceptable certificate issuers.
   * @return The (possibly empty) list of aliases.
   */
  String[] getClientAliases(String keyType, Principal[] issuers);

  /**
   * Gets the private key associated with the given alias.
   *
   * @param alias The alias.
   * @return The private key.
   */
  PrivateKey getPrivateKey(String alias);

  /**
   * Returns all server aliases that support the given key type.
   *
   * @param keyType The desired key type.
   * @param issuers A list of acceptable certificate issuers.
   * @return The (possibly empty) list of aliases.
   */
  String[] getServerAliases(String keyType, Principal[] issuers);
}
