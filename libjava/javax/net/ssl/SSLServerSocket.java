/* SSLServerSocket.java -- a server socket for SSL connections.
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

import java.io.IOException;

import java.net.InetAddress;
import java.net.ServerSocket;

/**
 * A server socket that allows clients to connect via the SSL protocol.
 */
public abstract class SSLServerSocket extends ServerSocket
{

  // Constructors.
  // -------------------------------------------------------------------------

  protected SSLServerSocket() throws IOException
  {
    super();
    //super(0);
    //throw new UnsupportedOperationException("1.4 socket methods not enabled");
  }

  protected SSLServerSocket(int port) throws IOException
  {
    super(port);
  }

  protected SSLServerSocket(int port, int backlog) throws IOException
  {
    super(port, backlog);
  }

  protected SSLServerSocket(int port, int backlog, InetAddress bindAddress)
    throws IOException
  {
    super(port, backlog, bindAddress);
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the list of cihper suites that are currently enabled in this
   * server socket. Sockets accepted by this server socket will only have
   * these suites enabled.
   *
   * @return The enabled cipher suites.
   */
  public abstract String[] getEnabledCipherSuites();

  /**
   * Sets the list enabled cipher suites.
   *
   * @param suites The cipher suites to enable.
   */
  public abstract void setEnabledCipherSuites(String[] suites);

  /**
   * Returns the list of enabled protocols, such as "SSLv3" and "TLSv1".
   *
   * @return The enabled protocols.
   */
  public abstract String[] getEnabledProtocols();

  /**
   * Sets the list of enabled protocols.
   *
   * @param protocols The list of protocols to enable.
   */
  public abstract void setEnabledProtocols(String[] protocols);

  /**
   * Returns whether or not sessions will be created, i.e., whether or not
   * this server socket will allow SSL session resumption.
   *
   * @return True if sessions will be created.
   */
  public abstract boolean getEnableSessionCreation();

  /**
   * Sets whether or not sessions will be created.
   *
   * @param enabled The new enabled value.
   */
  public abstract void setEnableSessionCreation(boolean enabled);

  /**
   * Returns whether or not this server socket will require clients to
   * authenticate themselves, such as through a certificate.
   *
   * @return True if clients must authenticate themselves.
   */
  public abstract boolean getNeedClientAuth();

  /**
   * Enabled or disables the requirement that clients authenticate themselves.
   * When this is set to <code>true</code>, connections will be rejected if
   * connecting clients do not provide proper authentication.
   *
   * @param needAuth The new need auth value.
   */
  public abstract void setNeedClientAuth(boolean needAuth);

  /**
   * Returns whether or not sockets accepted by this server socket will do
   * their handshake as the client-side. The default is false.
   *
   * @return True if client mode will be used.
   */
  public abstract boolean getUseClientMode();

  /**
   * Sets whether or not sockets accepted by this server socket will be
   * created in client mode.
   *
   * @param clientMode The new client mode value.
   */
  public abstract void setUseClientMode(boolean clientMode);

  /**
   * Returns whether or not this socket will ask for, but not require, that
   * connecting clients authenticate themselves. Clients that do not
   * provide authentication they will still be allowed to connect.
   *
   * @return True if this server socket wants client authentication.
   */
  public abstract boolean getWantClientAuth();

  /**
   * Sets whether or not this server socket will want client authentication.
   *
   * @param wantAuth The new want auth value.
   */
  public abstract void setWantClientAuth(boolean wantAuth);

  /**
   * Returns a list of cipher suites that this server socket supports.
   *
   * @return The list of supported suites.
   */
  public abstract String[] getSupportedCipherSuites();

  /**
   * Returns a list of SSL protocols supported by this server socket.
   *
   * @return The list of supported protocols.
   */
  public abstract String[] getSupportedProtocols();
}
