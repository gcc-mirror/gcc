/* HandshakeCompletedEvent.java -- SSL handshake completed.
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

import java.security.cert.Certificate;

import javax.security.cert.X509Certificate;

/**
 * An event raised by a SSLSocket and passed to the {@link
 * HandshakeCompletedListener#handshakeCompleted(HandshakeCompletedEvent)}
 * method of all registered listeners when a SSL handshake in a SSL
 * protocol is completed.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class HandshakeCompletedEvent extends java.util.EventObject
{
  // Fields.
  // -------------------------------------------------------------------

  /** Serialization constant. */
  private static final long serialVersionUID = 7914963744257769778L;

  /** The session. */
  private final transient SSLSession session;

  // Constructor.
  // -------------------------------------------------------------------

  /**
   * Creates a new handshake completed event.
   *
   * @param socket The socket (also the source) creating this event.
   * @param session The associated session object.
   * @throws NullPointerException If <i>session</i> is null.
   */
  public HandshakeCompletedEvent(SSLSocket socket, SSLSession session)
  {
    super(socket);
    if (session == null)
      throw new NullPointerException();
    this.session = session;
  }

  // Instance methods.
  // --------------------------------------------------------------------

  /**
   * Returns the name of the cipher that was negotiated in this
   * connection.
   *
   * @return The negotiated cipher name.
   */
  public String getCipherSuite()
  {
    if (session != null)
      return session.getCipherSuite();
    return null;
  }

  /**
   * Returns the local certificates being used in this connection.
   *
   * @return The local certificates.
   */
  public Certificate[] getLocalCertificates()
  {
    if (session != null)
      return session.getLocalCertificates();
    return null;
  }

  /**
   * Returns the peer's certificates being used in this connection.
   *
   * @return The peer's certificates.
   * @throws SSLPeerUnverifiedException If the peer has not been
   *   verified.
   */
  public Certificate[] getPeerCertificates() throws SSLPeerUnverifiedException
  {
    if (session != null)
      return session.getPeerCertificates();
    return null;
  }

  public X509Certificate[] getPeerCertificateChain() throws SSLPeerUnverifiedException
  {
    if (session != null)
      return session.getPeerCertificateChain();
    return null;
  }

  /**
   * Returns the SSL session object associated with this connection.
   *
   * @return The session object.
   */
  public SSLSession getSession()
  {
    return session;
  }

  /**
   * Returns the socket over which this connection is being
   * negotiated. This method is equivalent to the {@link
   * java.util.EventObject#getSource()} method.
   *
   * @return The socket.
   */
  public SSLSocket getSocket()
  {
    return (SSLSocket) getSource();
  }
}
