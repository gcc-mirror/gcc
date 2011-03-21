/* SslRMIClientSocketFactory.java --
   Copyright (C) 2006 Free Software Foundation

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

package javax.rmi.ssl;

import java.io.IOException;
import java.io.Serializable;

import java.util.StringTokenizer;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.SSLSocket;
import java.net.Socket;
import java.rmi.server.RMIClientSocketFactory;

/**
 * SslRMIClientSocketFactory
 *
 * This class implements an RMIClientSocketFactory for SSL sockets.
 * it uses the default SSLClientSocketFactory.
 *
 * This class can optionally use the following system properties, if set:
 * <code>javax.rmi.ssl.client.enabledCipherSuites</code>
 * <code>javax.rmi.ssl.client.enabledProtocols</code>
 *
 * These properties will specify a list of SSL/TLS cipher suites and protocols,
 * respectively, to enable on the created sockets.
 *
 * Both properties should consist of a comma-separated list.
 *
 * @author Sven de Marothy
 * @since 1.5
 */
public class SslRMIClientSocketFactory
  implements RMIClientSocketFactory, Serializable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -8310631444933958385L;

  private String[] enabledCipherSuites, enabledProtocols;

  /**
   * The SSL Socket factory.
   */
  private static SSLSocketFactory socketFactory =
    (SSLSocketFactory)SSLSocketFactory.getDefault();

  /**
   * Creates a new SslRMIClientSocketFactory
   */
  public SslRMIClientSocketFactory()
  {
    enabledCipherSuites = getProp("javax.rmi.ssl.client.enabledCipherSuites");
    enabledProtocols = getProp("javax.rmi.ssl.client.enabledProtocols");
  }

  private String[] getProp(String p)
  {
    String o;
    try
      {
        o = System.getProperty(p);
      }
    catch(SecurityException se)
      {
        return null;
      }

    if (o == null)
      return null;
    StringTokenizer st = new StringTokenizer( o, "," );
    int n = st.countTokens();
    if( n < 1 )
      return null;
    String[] strs = new String[ n ];
    for( int i = 0; i < n; i++ )
      strs[i] = st.nextToken().trim();

    return strs;
  }

  /**
   * Creates an SSLSocket on a given port
   *
   * @throws IOException if an error occurs on socket creation.
   */
  public Socket createSocket(String host, int port) throws IOException
  {
    SSLSocket socket = (SSLSocket)socketFactory.
      createSocket( host, port );
    if( enabledCipherSuites != null )
      socket.setEnabledCipherSuites( enabledCipherSuites );
    if( enabledProtocols != null )
      socket.setEnabledProtocols( enabledProtocols );
    return socket;
  }

  /**
   * Compare two SslRMIServerSocketFactor instances
   */
  public boolean equals(Object obj)
  {
    if( !(obj instanceof SslRMIClientSocketFactory) )
      return false;
    SslRMIClientSocketFactory s = (SslRMIClientSocketFactory)obj;

    if(!SslRMIServerSocketFactory.
       cmpStrArray(enabledCipherSuites, s.enabledCipherSuites))
      return false;

    if(!SslRMIServerSocketFactory.
       cmpStrArray(enabledProtocols, s.enabledProtocols))
      return false;

    return true;
  }

  /**
   * Returns the hash code of this object.
   */
  public int hashCode()
  {
    int hash = 0;
    if( enabledCipherSuites != null )
      for(int i = 0; i < enabledCipherSuites.length; i++ )
        hash = hash ^ enabledCipherSuites[i].hashCode();
    if( enabledProtocols != null )
      for(int i = 0; i < enabledProtocols.length; i++ )
        hash = hash ^ enabledProtocols[i].hashCode();
    return hash;
 }
}
