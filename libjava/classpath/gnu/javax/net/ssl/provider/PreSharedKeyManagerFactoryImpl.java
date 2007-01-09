/* PreSharedKeyManagerFactory.java -- 
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
exception statement from your version. */


package gnu.javax.net.ssl.provider;

import gnu.javax.net.ssl.PreSharedKeyManager;
import gnu.javax.net.ssl.PreSharedKeyManagerParameters;

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.util.Iterator;

import javax.crypto.SecretKey;
import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactorySpi;
import javax.net.ssl.ManagerFactoryParameters;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class PreSharedKeyManagerFactoryImpl
  extends KeyManagerFactorySpi
{
  PreSharedKeyManagerParameters params;

  /* (non-Javadoc)
   * @see javax.net.ssl.KeyManagerFactorySpi#engineGetKeyManagers()
   */
  @Override protected KeyManager[] engineGetKeyManagers()
  {
    if (params == null)
      throw new IllegalStateException("not initialized");
    return new KeyManager[] { new Manager() };
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.KeyManagerFactorySpi#engineInit(javax.net.ssl.ManagerFactoryParameters)
   */
  @Override protected void engineInit(ManagerFactoryParameters params)
    throws InvalidAlgorithmParameterException
  {
    if (!(params instanceof PreSharedKeyManagerParameters))
      throw new InvalidAlgorithmParameterException("only supports gnu.javax.net.ssl.PreSharedKeyManagerParameters");
    params = (PreSharedKeyManagerParameters) params;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.KeyManagerFactorySpi#engineInit(java.security.KeyStore, char[])
   */
  @Override protected void engineInit(KeyStore store, char[] passwd)
    throws KeyStoreException, NoSuchAlgorithmException,
           UnrecoverableKeyException
  {
    // XXX Could implement this.
  }

  class Manager implements PreSharedKeyManager
  {
    Manager()
    {
    }

    /* (non-Javadoc)
     * @see gnu.javax.net.ssl.PreSharedKeyManager#getKey(java.lang.String)
     */
    public SecretKey getKey(String name) throws KeyManagementException
    {
      return params.getKey(name);
    }
    
    public String chooseIdentityHint()
    {
      Iterator<String> it = params.identities();
      if (it.hasNext())
        return it.next();
      return null;
    }
  }
}
