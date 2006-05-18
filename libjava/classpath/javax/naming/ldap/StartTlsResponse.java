/* StartTlsResponse.java -- extended ldap TLS response
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.naming.ldap;

import java.io.IOException;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;

/**
 * @since 1.4
 */
public abstract class StartTlsResponse
    implements ExtendedResponse
{
  private static final long serialVersionUID = 8372842182579276418L;

  /**
   * The assigned object identifier for this response.
   */
  public static final String OID = "1.3.6.1.4.1.1466.20037";

  /**
   * Create a new instance.
   */
  protected StartTlsResponse()
  {
  }

  /**
   * Return the response identifier.  This is simply the value
   * of the {@link #OID} field.
   */
  public String getID()
  {
    return OID;
  }

  /**
   * Return the encoded value.  This implementation always returns null.
   */
  public byte[] getEncodedValue()
  {
    return null;
  }

  /**
   * Set the list of cipher suites to use.
   * @param cipherSuites the list of suites
   * @see SSLSocketFactory#getSupportedCipherSuites()
   */
  public abstract void setEnabledCipherSuites(String[] cipherSuites);

  /**
   * Set the hostname verifier to use.  This must be called before
   * {@link #negotiate()}.
   * @param verifier the hostname verifier
   */
  public abstract void setHostnameVerifier(HostnameVerifier verifier);

  /**
   * Negotiate the TLS session using the default SSL socket factory.
   * @return the SSL session
   * @throws IOException if communication fails for some reason
   */
  public abstract SSLSession negotiate() throws IOException;

  /**
   * Negotiate the TLS session using the supplied SSL socket factory.
   * @param factory the socket factory to use
   * @return the SSL session
   * @throws IOException if communication fails for some reason
   */
  public abstract SSLSession negotiate(SSLSocketFactory factory)
    throws IOException;

  /**
   * Close the connection.
   * @throws IOException if communication fails for some reason
   */
  public abstract void close() throws IOException;
}
