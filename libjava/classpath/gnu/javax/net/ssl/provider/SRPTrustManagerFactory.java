/* SRPTrustManagerFactory.java -- trust manager for SRP.
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


package gnu.javax.net.ssl.provider;

import java.io.IOException;
import java.math.BigInteger;

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyPair;
import java.security.KeyStore;
import java.util.HashMap;

import javax.net.ssl.ManagerFactoryParameters;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactorySpi;

import gnu.java.security.key.IKeyPairGenerator;
import gnu.javax.crypto.key.srp6.SRPKeyPairGenerator;
import gnu.javax.crypto.sasl.srp.PasswordFile;
import gnu.javax.crypto.sasl.srp.SRP;

import gnu.javax.net.ssl.SRPManagerParameters;
import gnu.javax.net.ssl.SRPTrustManager;

/**
 * This is an implementation of a {@link javax.net.ssl.TrustManagerFactory}
 * engine for the ``SRP'' algorithm. You must initialize instances of this
 * algorithm with {@link SRPManagerParameters}.
 */
public class SRPTrustManagerFactory extends TrustManagerFactorySpi
{

  // Field.
  // -------------------------------------------------------------------------

  private Manager current;

  // Constructor.
  // -------------------------------------------------------------------------

  public SRPTrustManagerFactory()
  {
    super();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  protected TrustManager[] engineGetTrustManagers()
  {
    if (current == null)
      throw new IllegalStateException("not initialized");
    return new TrustManager[] { current };
  }

  protected void engineInit(KeyStore ks)
  {
    throw new IllegalArgumentException("only accepts SRPManagerParameters");
  }

  protected void engineInit(ManagerFactoryParameters params)
    throws InvalidAlgorithmParameterException
  {
    if (params == null)
      {
        try
          {
            String srpPasswd = Util.getSecurityProperty("jessie.srp.password.file");
            if (srpPasswd == null)
              {
                current = new Manager(new PasswordFile());
                return;
              }
            String srpPasswd2 = Util.getSecurityProperty("jessie.srp.password.file2");
            if (srpPasswd2 == null)
              srpPasswd2 = srpPasswd + "2";
            String srpConfig = Util.getSecurityProperty("jessie.srp.config");
            if (srpConfig == null)
              srpConfig = srpPasswd + ".conf";
            current = new Manager(new PasswordFile(srpPasswd, srpPasswd2, srpConfig));
            return;
          }
        catch (IOException ioe)
          {
            throw new InvalidAlgorithmParameterException("default initialization failed: "
                                                         + ioe.toString());
          }
      }
    if (params instanceof SRPManagerParameters)
      {
        current = new Manager(((SRPManagerParameters) params).getPasswordFile());
        return;
      }
    throw new InvalidAlgorithmParameterException();
  }

  // Inner class.
  // -------------------------------------------------------------------------

  private class Manager implements SRPTrustManager
  {

    // Field.
    // -----------------------------------------------------------------------

    private final PasswordFile file;

    // Constructor.
    // -----------------------------------------------------------------------

    Manager(PasswordFile file)
    {
      this.file = file;
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public boolean contains(String user)
    {
      try
        {
          return file.contains(user);
        }
      catch (IOException ioe) { }
      return false;
    }

    public KeyPair getKeyPair(String user)
    {
      try
        {
          if (file.contains(user))
            {
              SRP srp = SRP.instance("SHA");
              String[] ent = file.lookup(user, "SHA");
              String[] cnf = file.lookupConfig(ent[2]);
              BigInteger v, N, g;
              v = new BigInteger(1, gnu.java.security.util.Util.fromBase64(ent[0]));
              N = new BigInteger(1, gnu.java.security.util.Util.fromBase64(cnf[0]));
              g = new BigInteger(1, gnu.java.security.util.Util.fromBase64(cnf[1]));
              IKeyPairGenerator kpg = new SRPKeyPairGenerator();
              HashMap attr = new HashMap();
              attr.put(SRPKeyPairGenerator.SHARED_MODULUS, N);
              attr.put(SRPKeyPairGenerator.GENERATOR, g);
              attr.put(SRPKeyPairGenerator.USER_VERIFIER, v);
              kpg.setup(attr);
              return kpg.generate();
            }
        }
      catch (IOException ioe) { }
      return null;
    }

    public byte[] getSalt(String user)
    {
      try
        {
          if (file.contains(user))
            {
              return gnu.java.security.util.Util.fromBase64(file.lookup(user, "SHA")[1]);
            }
        }
      catch (IOException ioe) { }
      return null;
    }

    public BigInteger getVerifier(String user)
    {
      try
        {
          if (file.contains(user))
            {
              return new BigInteger(1,
                gnu.java.security.util.Util.fromBase64(file.lookup(user, "SHA")[0]));
            }
        }
      catch (IOException ioe) { }
      return null;
    }

    public PasswordFile getPasswordFile()
    {
      return file;
    }
  }
}
