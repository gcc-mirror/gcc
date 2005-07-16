/* KeyManagerFactorySpi.java -- SPI for key manager factories.
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

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;

/**
 * The <i>Service Provider Interface</i> (<b>SPI</b>) for key manager
 * factories.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public abstract class KeyManagerFactorySpi
{

  // Constructor.
  // ------------------------------------------------------------------

  public KeyManagerFactorySpi()
  {
    super();
  }

  // Abstract methods.
  // ------------------------------------------------------------------

  /**
   * Engine method for retrieving this factory's key managers.
   *
   * @return The key managers.
   */
  protected abstract KeyManager[] engineGetKeyManagers();

  /**
   * Engine method for initializing this factory with some
   * algorithm-specific parameters.
   *
   * @param params The factory parameters.
   * @throws InvalidAlgorithmParameterException If the supplied parameters
   *   are inappropriate for this instance.
   */
  protected abstract void engineInit(ManagerFactoryParameters params)
    throws InvalidAlgorithmParameterException;

  /**
   * Engine method for initializing this factory with a key store and a
   * password for private keys. Either parameter may be <code>null</code>,
   * in which case some default parameters (possibly derived from system
   * properties) should be used.
   *
   * @param store The key store.
   * @param passwd The private key password.
   * @throws KeyStoreException If the key store cannot be accessed.
   * @throws NoSuchAlgorithmException If some of the data from the key
   *   store cannot be retrieved.
   * @throws UnrecoverableKeyException If a private key cannot be retrieved,
   *   likely from a wrong password.
   */
  protected abstract void engineInit(KeyStore store, char[] passwd)
    throws KeyStoreException, NoSuchAlgorithmException,
           UnrecoverableKeyException;
}
