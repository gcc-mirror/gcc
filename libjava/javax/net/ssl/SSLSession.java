/* SSLSession.java -- an SSL session.
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


package javax.net.ssl;

import java.security.cert.Certificate;

import javax.security.cert.X509Certificate;

/**
 * An SSL session is a mechanism through which connections can be established
 * by re-using previously negotiated handshakes.
 */
public interface SSLSession
{
  /**
   * Returns this session's cihper suite.
   *
   * @return The cipher suite.
   */
  String getCipherSuite();

  /**
   * Returns the time in milliseconds since midnight GMT, 1 January 1970, that
   * this session was created.
   *
   * @return The creation time.
   */
  long getCreationTime();

  /**
   * Returns this session's unique identifier, a arbitrary byte array of up
   * to 32 bytes.
   *
   * @return The session identifier.
   */
  byte[] getId();

  /**
   * Returns the last time this session was accessed.
   *
   * @return The lest time this session was accessed.
   */
  long getLastAccessedTime();

  /**
   * Returns the chain of certificates that the local side used in the
   * handshake, or null if none were used.
   *
   * @return The local certificate chain.
   */
  Certificate[] getLocalCertificates();

  /**
   * Returns the chain of certificates that the remote side used in
   * the handshake, or null if none were used.
   *
   * @return The peer's certificate chain.
   * @throws SSLPeerUnverifiedException If the identity of the peer has
   *   not been verified.
   */
  Certificate[] getPeerCertificates() throws SSLPeerUnverifiedException;

  /**
   * Returns the chain of certificates that the remote side used in
   * the handshake, or null if none were used.
   *
   * @return The peer's certificate chain.
   * @throws SSLPeerUnverifiedException If the identity of the peer has
   *   not been verified.
   */
  X509Certificate[] getPeerCertificateChain()
    throws SSLPeerUnverifiedException;

  /**
   * Returns the remote host's name.
   *
   * @return The name of the remote host.
   */
  String getPeerHost();

  /**
   * Returns the protocol this session uses.
   *
   * @return The protocol.
   */
  String getProtocol();

  /**
   * Returns this session's session context object.
   *
   * @return The session context.
   * @throws SecurityException If the caller does not have the
   *   {@link SSLPermission} "getSessionContext".
   */
  SSLSessionContext getSessionContext();

  /**
   * Returns the names of all values bound to this session.
   *
   * @return The list of bound names.
   */
  String[] getValueNames();

  /**
   * Returns the object bound to the given name.
   *
   * @param name The name of the value to get.
   * @return The object bound by that name, or null.
   */
  Object getValue(String name);

  /**
   * Invalidates this session, ensuring that it will not be continued by
   * another socket.
   */
  void invalidate();

  /**
   * Binds a value to this session, with the given name.
   *
   * @param name The name to bind the object with.
   * @param value The value to bind.
   */
  void putValue(String name, Object value);

  /**
   * Un-binds a value.
   *
   * @param name The name of the value to un-bind.
   */
  void removeValue(String name);
}
