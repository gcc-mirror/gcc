/* SSLSocket.java -- an SSL client socket.
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
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * A socket that communicates over the secure socket layer protocol.
 */
public abstract class SSLSocket extends Socket
{

  // Constructors.
  // -------------------------------------------------------------------------

  protected SSLSocket()
  {
    super();
  }

  protected SSLSocket(String host, int port)
    throws IOException, UnknownHostException
  {
    super(host, port);
  }

  protected SSLSocket(InetAddress address, int port) throws IOException
  {
    super(address, port);
  }

  protected SSLSocket(String host, int port,
                      InetAddress localAddr, int localPort)
    throws IOException, UnknownHostException
  {
    super(host, port, localAddr, localPort);
  }

  protected SSLSocket(InetAddress address, int port,
                      InetAddress localAddr, int localPort)
    throws IOException
  {
    super(address, port, localAddr, localPort);
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  /**
   * Adds a handshake completed listener that wants to be notified when the
   * SSL handshake completes.
   *
   * @param listener The listener to add.
   */
  public abstract void
    addHandshakeCompletedListener(HandshakeCompletedListener listener);

  /**
   * Removes a handshake listener from this socket.
   *
   * @param listener The listener to remove.
   */
  public abstract void
    removeHandshakeCompletedListener(HandshakeCompletedListener listener);

  /**
   * Returns the list of currently enabled cipher suites.
   *
   * @return The list of enabled cipher suites.
   */
  public abstract String[] getEnabledCipherSuites();

  /**
   * Sets the list of enabled cipher suites.
   *
   * @param suites The list of suites to enable.
   */
  public abstract void setEnabledCipherSuites(String[] suites);

  /**
   * Returns the list of enabled SSL protocols.
   *
   * @return The list of enabled protocols.
   */
  public abstract String[] getEnabledProtocols();

  /**
   * Sets the list of enabled SSL protocols.
   *
   * @param protocols The list of protocols to enable.
   */
  public abstract void setEnabledProtocols(String[] protocols);

  /**
   * Returns whether or not sessions will be created by this socket, and thus
   * allow sessions to be continued later.
   *
   * @return Whether or not sessions will be created.
   */
  public abstract boolean getEnableSessionCreation();

  /**
   * Sets whether or not sessions will be created by this socket.
   *
   * @param enable The new value.
   */
  public abstract void setEnableSessionCreation(boolean enable);

  /**
   * Returns whether or not this socket will require connecting clients to
   * authenticate themselves. This value only applies to sockets in server
   * mode.
   *
   * @return Whether or not this socket requires client authentication.
   */
  public abstract boolean getNeedClientAuth();

  /**
   * Sets whether or not this socket will require connecting clients to
   * authenticate themselves. This value only applies to sockets in server
   * mode.
   *
   * @param needAuth The new need auth value.
   */
  public abstract void setNeedClientAuth(boolean needAuth);

  /**
   * Returns this socket's session object.
   *
   * @return The session.
   */
  public abstract SSLSession getSession();

  /**
   * Returns the list of cipher suites supported by this socket.
   *
   * @return The list of supported cipher suites.
   */
  public abstract String[] getSupportedCipherSuites();

  /**
   * Returns the list of protocols supported by this socket.
   *
   * @return The list of supported protocols.
   */
  public abstract String[] getSupportedProtocols();

  /**
   * Returns whether or not this socket will connect in client mode.
   *
   * @return True if this is a client socket.
   */
  public abstract boolean getUseClientMode();

  /**
   * Sets whether or not this socket will connect in client mode.
   *
   * @param clientMode The new value.
   */
  public abstract void setUseClientMode(boolean clientMode);

  /**
   * Returns whether or not this socket will request that connecting clients
   * authenticate themselves. This value only applies to sockets in server
   * mode.
   *
   * @return The want client auth value.
   */
  public abstract boolean getWantClientAuth();

  /**
   * Sets whether or not this socket will request that connecting clients
   * authenticate themselves. This value only applies to sockets in server
   * mode.
   *
   * @param wantAuth The new want auth value.
   */
  public abstract void setWantClientAuth(boolean wantAuth);

  /**
   * Explicitly begins the handshake, or, if the handshake has already
   * completed, requests that the handshake be repeated.
   *
   * <p>The handshake will begin implicitly when any attempt to read or
   * write to the socket is made.</p>
   *
   * @throws IOException If an I/O or SSL error occurs.
   */
  public abstract void startHandshake() throws IOException;
}
