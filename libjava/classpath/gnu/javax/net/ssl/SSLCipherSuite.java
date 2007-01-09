/* SSLCipherSuite.java -- an SSL cipher suite.
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


package gnu.javax.net.ssl;

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.nio.ByteBuffer;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;

/**
 * An SSL cipher suite.
 */
public abstract class SSLCipherSuite
{
  private static final String SERVICE = "SSLCipherSuite";
  private final String algorithm;
  private final byte[] id;
  private final SSLProtocolVersion version;
  private Provider provider;
  
  protected SSLCipherSuite (final String algorithm, final byte[] id,
                            final SSLProtocolVersion version)
  {
    this.algorithm = algorithm;
    if (id.length != 2)
      throw new IllegalArgumentException ("cipher suite ID must be two bytes");
    this.id = (byte[]) id.clone ();
    this.version = version;
  }
  
  public static final SSLCipherSuite getInstance (SSLProtocolVersion version, byte[] id)
    throws NoSuchAlgorithmException
  {
    return getInstance (version + "-" + ((id[0] & 0xFF) + "/" + (id[1] & 0xFF)));
  }
  
  public static final SSLCipherSuite getInstance (SSLProtocolVersion version,
                                                  byte[] id, Provider provider)
    throws NoSuchAlgorithmException
  {
    return getInstance (version + "-" + (id[0] & 0xFF) + "/" + (id[1] & 0xFF), provider);
  }
  
  public static final SSLCipherSuite getInstance (String name)
    throws NoSuchAlgorithmException
  {
    Provider[] providers = Security.getProviders ();
    for (int i = 0; i < providers.length; i++)
      {
        try
          {
            return getInstance (name, providers[i]);
          }
        catch (NoSuchAlgorithmException nsae)
          {
            // Ignore.
          }
      }
      
    throw new NoSuchAlgorithmException (SERVICE + ": " + name);
  }
  
  public static final SSLCipherSuite getInstance (String name, Provider provider)
    throws NoSuchAlgorithmException
  {
    SSLCipherSuite suite = null;
    try
      {
        suite = (SSLCipherSuite) Engine.getInstance (SERVICE, name, provider);
        suite.provider = provider;
      }
    catch (InvocationTargetException ite)
      {
        // XXX
        NoSuchAlgorithmException nsae = new NoSuchAlgorithmException (name);
        nsae.initCause (ite);
        throw nsae;
      }
    return suite;
  }
  
  public final String getAlgorithm ()
  {
    return algorithm;
  }
  
  public final byte[] getId ()
  {
    return (byte[]) id.clone ();
  }
  
  public final Provider getProvider ()
  {
    return provider;
  }
  
  public final SSLProtocolVersion getProtocolVersion ()
  {
    return version;
  }
  
  public abstract void encipher (ByteBuffer in, ByteBuffer out);
}
