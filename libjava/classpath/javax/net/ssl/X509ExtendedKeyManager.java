/* X509ExtendedKeyManager.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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


package javax.net.ssl;

import java.security.Principal;

/**
 * An extended {@link X509KeyManager} for use with {@link SSLEngine}.
 *
 * @since 1.5
 * @author Casey Marshall (csm@gnu.org)
 */
public abstract class X509ExtendedKeyManager implements X509KeyManager
{

  /**
   * Default constructor.
   */
  protected X509ExtendedKeyManager ()
  {
  }

  /**
   * Return a client alias given a list of key types, a list of
   * allowable issuers, and the SSLEngine being used.
   *
   * <p>This implementation always returns <code>null</code>.
   *
   * @param keyTypes The list of desired key types.
   * @param issuers The list of desired key issuers.
   * @param engine This client's SSLEngine.
   * @return A key alias that matches the given parameters, or
   * <code>null</code> if the parameters were not matched.
   */
  public String chooseEngineClientAlias (final String[] keyTypes,
                                         final Principal[] issuers,
                                         final SSLEngine engine)
  {
    return null;
  }

  /**
   * Return a server alias given a key type, a list of allowable
   * issuers, and the SSLEngine being used.
   *
   * <p>This implementation always returns <code>null</code>.
   *
   * @param keyType The desired key type.
   * @param issuers The list of desired key issuers.
   * @param engine The server's SSLEngine.
   * @return A key alias that matches the given parameters, or
   * <code>null</code> if the parameters were not matched.
   */
  public String chooseEngineServerAlias (final String keyType,
                                         final Principal[] issuers,
                                         final SSLEngine engine)
  {
    return null;
  }
}
