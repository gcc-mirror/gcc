/* Jessie.java -- JESSIE's JSSE provider.
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

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.Provider;

/**
 * This is the security provider for Jessie. It implements the following
 * algorithms:
 *
 * <pre>
 * {@link javax.net.ssl.SSLContext}.SSLv3
 * {@link javax.net.ssl.SSLContext}.SSL
 * {@link javax.net.ssl.SSLContext}.TLSv1
 * {@link javax.net.ssl.SSLContext}.TLS
 * {@link javax.net.ssl.KeyManagerFactory}.JessieX509
 * {@link javax.net.ssl.TrustManagerFactory}.JessieX509
 * {@link javax.net.ssl.TrustManagerFactory}.SRP
 * </pre>
 *
 */
public class Jessie extends Provider
{
  private static final long serialVersionUID = -1;

  public static final String VERSION = "2.0.0";
  public static final double VERSION_DOUBLE = 2.0;

  public Jessie()
  {
    super("Jessie", VERSION_DOUBLE,
          "Implementing TLSv1.1, with SSLv3, TLSv1.0 compatibility modes; "
          + "X.509 Key Manager Factory; "
          + "X.509 Trust Manager Factory; "
          + "SSLv3 MD5 and SHA Mac.");

    AccessController.doPrivileged(new PrivilegedAction<Object>()
      {
        public Object run()
        {
          put("SSLContext.TLSv1.1", SSLContextImpl.class.getName());
          put("Alg.Alias.SSLContext.SSLv3",   "TLSv1.1");
          put("Alg.Alias.SSLContext.TLSv1",   "TLSv1.1");
          put("Alg.Alias.SSLContext.TLSv1.0", "TLSv1.1");
          put("Alg.Alias.SSLContext.TLS",     "TLSv1.1");
          put("Alg.Alias.SSLContext.SSL",     "TLSv1.1");

          put("KeyManagerFactory.JessieX509",   X509KeyManagerFactory.class.getName());
          put("TrustManagerFactory.JessieX509", X509TrustManagerFactory.class.getName());
          put("KeyManagerFactory.JessiePSK",    PreSharedKeyManagerFactoryImpl.class.getName());
          //put("TrustManagerFactory.SRP",        SRPTrustManagerFactory.class.getName());

          put("Mac.SSLv3HMac-MD5", SSLv3HMacMD5Impl.class.getName());
          put("Mac.SSLv3HMac-SHA", SSLv3HMacSHAImpl.class.getName());

          put("Signature.TLSv1.1-RSA", SSLRSASignatureImpl.class.getName());
          put("Alg.Alias.Signature.TLSv1-RSA", "TLSv1.1-RSA");
          put("Alg.Alias.Signature.SSLv3-RSA", "TLSv1.1-RSA");

          return null;
        }
      });
  }
}
