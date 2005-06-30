/* SSLContextSpi.java -- SPI for SSL contexts.
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


package javax.net.ssl;

import java.security.KeyManagementException;
import java.security.SecureRandom;

/**
 * The <i>Service Provider Interface</i> (<b>SPI</b>) for SSLContext
 * objects.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public abstract class SSLContextSpi
{

  // Constructor.
  // -------------------------------------------------------------------

  /**
   * Create a new SSLContextSpi.
   */
  public SSLContextSpi()
  {
    super();
  }

  // Abstract methods.
  // -------------------------------------------------------------------

  /**
   * Returns the set of SSL sessions available for client connections.
   *
   * @return The set of SSL sessions available for client connections.
   */
  protected abstract SSLSessionContext engineGetClientSessionContext();

  /**
   * Returns the set of SSL sessions available for server connections.
   *
   * @return The set of SSL sessions available for server connections.
   */
  protected abstract SSLSessionContext engineGetServerSessionContext();

  /**
   * Returns the SSL server socket factory.
   *
   * @return The SSL server socket factory.
   */
  protected abstract SSLServerSocketFactory engineGetServerSocketFactory();

  /**
   * Returns the SSL client socket factory.
   *
   * @return The SSL client socket factory.
   */
  protected abstract SSLSocketFactory engineGetSocketFactory();

  /**
   * Initialize this context with key and trust managers, and a source
   * of randomness. All of the parameters are optional.
   *
   * @param keyManagers The set of key managers.
   * @param trustManagers The set of trust managers.
   * @param random The source of randomness.
   * @throws KeyManagementException If this context cannot be
   *   initialized with these parameters.
   */
  protected abstract void engineInit(KeyManager[] keyManagers,
                                     TrustManager[] trustManagers,
                                     SecureRandom random)
    throws KeyManagementException;
}
