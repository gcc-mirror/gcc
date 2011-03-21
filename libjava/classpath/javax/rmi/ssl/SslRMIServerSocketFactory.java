/* SslRMIServerSocketFactory.java --
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
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLServerSocket;
import java.net.ServerSocket;
import java.rmi.server.RMIServerSocketFactory;

/**
 * SslRMIServerSocketFactory
 *
 * This class implements an RMIServerSocketFactory for SSL sockets.
 * it uses the defeult SSLServerSocketFactory.
 *
 * @author Sven de Marothy
 * @since 1.5
 */
public class SslRMIServerSocketFactory implements RMIServerSocketFactory
{
  private String[] enabledCipherSuites, enabledProtocols;
  private boolean needClientAuth;

  /**
   * The SSL ServerSocket factory.
   */
  private static SSLServerSocketFactory socketFactory =
    (SSLServerSocketFactory)SSLServerSocketFactory.getDefault();

  /**
   * Creates a new SslRMIServerSocketFactory with the default socket
   * cipher suites and protocols, and without requiring client authorisation.
   */
  public SslRMIServerSocketFactory()
  {
    enabledCipherSuites = enabledProtocols = null;
    needClientAuth = false;
  }

  /**
   * Creates a new SslRMIServerSocketFactory with a given set of socket
   * cipher suites and protocols. needClientAuth specifies if client
   * authorization is required.
   *
   * @param enabledCipherSuites - the cypher suites to enable
   * or <code>null</code> for the defauls.
   * @param enabledCipherSuites - the protocols to enable,
   * or <code>null</code> for the defauls.
   * @param needClientAuth - specify client authorization requirement.
   * @throws IllegalArgumentException if any of the ciphers or protocols
   *  specified are not available.
   */
  public SslRMIServerSocketFactory(String[] enabledCipherSuites,
                                   String[] enabledProtocols,
                                   boolean needClientAuth)
  {
    this.enabledCipherSuites = enabledCipherSuites;
    this.enabledProtocols = enabledProtocols;
    this.needClientAuth = needClientAuth;
    try
      {
        if( enabledProtocols != null || enabledCipherSuites != null )
          createServerSocket( 0 ); // stupid way to test the parameters
      }
    catch(IOException e)
      {
        // Can this happen? FIXME.
        throw new IllegalArgumentException();
      }
  }

  /**
   * Creates an SSLServerSocket on a given port
   *
   * @throws IOException if an error occurs on socket creation.
   */
  public ServerSocket createServerSocket(int port) throws IOException
  {
    SSLServerSocket socket = (SSLServerSocket)socketFactory.
      createServerSocket( port );
    if( enabledCipherSuites != null )
      socket.setEnabledCipherSuites( enabledCipherSuites );
    if( enabledProtocols != null )
      socket.setEnabledProtocols( enabledProtocols );
    socket.setNeedClientAuth( needClientAuth );
    return socket;
  }

  /**
   * Compare two SslRMIServerSocketFactor instances
   */
  public boolean equals(Object obj)
  {
    if( !(obj instanceof SslRMIServerSocketFactory) )
      return false;
    SslRMIServerSocketFactory s = (SslRMIServerSocketFactory)obj;
    if( needClientAuth != s.needClientAuth )
      return false;

    if(!cmpStrArray(enabledCipherSuites, s.enabledCipherSuites))
      return false;

    if(!cmpStrArray(enabledProtocols, s.enabledProtocols))
      return false;

    return true;
  }

  /**
   * Compare two string arrays.
   */
  static boolean cmpStrArray(String[] a, String[] b)
  {
    if( ( a == null || b == null ) && a != b )
      return false;

    if( a != null )
      {
        if( a.length != b.length )
          return false;
        for( int i = 0; i < a.length; i++ )
          if(!a[i].equals(b[i]))
            return false;
      }

    return true;
  }

  /**
   * Returns the enabled cipher suites, or <code>null</code>
   * if the defaults are to be used.
   * @returns a string array of cipher suite names
   */
  public String[] getEnabledCipherSuites()
  {
    if( enabledCipherSuites == null )
      return null;
    return (String[])enabledCipherSuites.clone();
  }

  /**
   * Returns the enabled protocols, or <code>null</code> if the defaults are
   * to be used.
   *
   * @returns a string array of protocol names
   */
  public String[] getEnabledProtocols()
  {
    if( enabledProtocols == null )
      return null;
    return (String[])enabledProtocols.clone();
  }

  /**
   * Returns whether client authorization is needed.
   */
  public boolean getNeedClientAuth()
  {
    return needClientAuth;
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
    hash = ( needClientAuth ) ? (hash^0xFFFF) : hash;
    return hash;
 }
}
