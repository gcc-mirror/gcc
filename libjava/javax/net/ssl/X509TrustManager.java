/* X509TrustManager.java -- X.509 trust manager interface.
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

import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

/**
 * A trust manager for dealing with X.509 certificates.
 */
public interface X509TrustManager extends TrustManager
{

  /**
   * Checks if a certificate chain sent by the client is trusted.
   *
   * @param chain The certificate chain to check.
   * @param authType The authentication type.
   * @throws CertificateException If the client's certificates are not trusted.
   */
  void checkClientTrusted(X509Certificate[] chain, String authType)
    throws CertificateException;

  /**
   * Checks if a certificate chain sent by the server is trusted.
   *
   * @param chain The certificate chain to check.
   * @param authType The authentication type.
   * @throws CertificateException If the server's certificates are not trusted.
   */
  void checkServerTrusted(X509Certificate[] chain, String authType)
    throws CertificateException;

  /**
   * Returns the list of trusted issuer certificates currently in use.
   *
   * @return The list of trusted issuer certificates.
   */
  X509Certificate[] getAcceptedIssuers();
}
