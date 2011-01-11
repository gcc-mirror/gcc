/* SSLEngine.java -- advanced, generic utility for manipulating SSL messages.
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


package javax.net.ssl;

import java.nio.ByteBuffer;

/**
 * A class for low-level message wrapping and unwrapping of SSL
 * messages.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.5
 */
public abstract class SSLEngine
{
  private final String peerHost;
  private final int peerPort;

  /**
   * Creates a new SSLEngine with no peer host name or port number.
   */
  protected SSLEngine ()
  {
    this (null, -1);
  }

  /**
   * Creates a new SSLEngine with the specified peer host name and
   * port number.
   *
   * @param peerHost The peer's host name.
   * @param peerPort The peer's port number.
   */
  protected SSLEngine (String peerHost, int peerPort)
  {
    this.peerHost = peerHost;
    this.peerPort = peerPort;
  }



  /**
   * Begin, or restart, the SSL handshake.
   *
   * @throws SSLException
   */
  public abstract void beginHandshake () throws SSLException;

  /**
   * Close the inbound state.
   *
   * @throws SSLException
   */
  public abstract void closeInbound () throws SSLException;

  /**
   * Close the outbound state.
   */
  public abstract void closeOutbound ();

  /**
   *
   */
  public abstract Runnable getDelegatedTask ();

  /**
   * Returns the peer host name this SSL session is connected to, or
   * <code>null</code> if this value was not set.
   *
   * @return The peer host's name.
   */
  public String getPeerHost ()
  {
    return peerHost;
  }

  /**
   * Returns the peer IP port number this SSL session in communicating
   * on, or -1 if this value was not set.
   *
   * @return The peer's port number.
   */
  public int getPeerPort ()
  {
    return peerPort;
  }

  /**
   * Returns a list of SSL cipher suite names this SSLEngine is
   * configured to use.
   *
   * @return The list of enabled cipher suite names.
   */
  public abstract String[] getEnabledCipherSuites();

  /**
   * Returns a list of SSL protocol version names this SSLEngine is
   * configured to use.
   *
   * @return The list of enabled protocol names.
   */
  public abstract String[] getEnabledProtocols ();

  /**
   * Tells if sessions will be created by this engine, and therefore
   * may be resumed at a later time.
   *
   * @return True if sessions will be created.
   */
  public abstract boolean getEnableSessionCreation();

  /**
   * Return the current handshake status.
   *
   * @return The current handshake status.
   */
  public abstract SSLEngineResult.HandshakeStatus getHandshakeStatus ();

  /**
   * Tells if this SSLEngine is configured to require client
   * authentication when in server mode.
   *
   * @return True iff client authentication is required.
   */
  public abstract boolean getNeedClientAuth ();

  /**
   * Return the {@link SSLSession} object this connection represents.
   *
   * @return The SSL session.
   */
  public abstract SSLSession getSession ();

  /**
   * Returns a list of SSL cipher suite names this SSLEngine
   * implementation supports.
   *
   * @return The list of cipher suite names supported by this
   * implementation.
   */
  public abstract String[] getSupportedCipherSuites ();

  /**
   * Returns a list of SSL protocol version names this SSLEngine
   * implementation supports. SSL protocol names include things like
   * "SSLv3" or "TLSv1".
   *
   * @return The list of SSL protocol names
   */
  public abstract String[] getSupportedProtocols ();

  /**
   * Tells if this SSLEngine is a "client" session.
   *
   * @return True iff this session is configured for client mode.
   */
  public abstract boolean getUseClientMode ();

  /**
   * Tells if client authentication is requested, but not required,
   * for sessions in server mode. If true, a server session will
   * request an authentication message from connecting clients, but
   * will still allow clients to connect if they cannot be
   * authenticated.
   *
   * @return True iff client authentication is requested.
   */
  public abstract boolean getWantClientAuth ();

  /**
   * Tells if the incoming data stream is finished, and thus if no
   * more data will be available to be unwrapped.
   *
   * @return True if no more data is to be unwrapped.
   */
  public abstract boolean isInboundDone ();

  /**
   * Tells if the outgoing data stream is finished, and thus if no
   * more data may be wrapped.
   *
   * @return True if no more data may be wrapped.
   */
  public abstract boolean isOutboundDone ();

  /**
   * Sets the list of enabled cipher suites. The argument is an array
   * of strings of the canonical suite names.
   *
   * @param suites The cipher suites to enable.
   * @throws IllegalArgumentException If any of the specified suite
   * strings is not supported by this implementation, or if the
   * argument is null.
   */
  public abstract void setEnabledCipherSuites (String[] suites);

  /**
   * Sets the list of enabled protocol versions. The argument is an
   * array of strings of the canonical protocol version names, such as
   * "TLSv1".
   *
   * @param protocols The protocol versions to enable.
   * @throws IllegalArgumentException If any of the specified
   * protocols are not supported, or if the argument is null.
   */
  public abstract void setEnabledProtocols (String[] protocols);

  /**
   * Enables or disables session creation. If enabled, each connection
   * will create session that may be resumed by another connection.
   *
   * @param create Whether or not to enable session creation.
   */
  public abstract void setEnableSessionCreation (boolean create);

  /**
   * Enables client or server mode. If the argument is true, this
   * engine will run in client mode; if false, server mode.
   *
   * @param clientMode Whether or not to use client mode.
   */
  public abstract void setUseClientMode (boolean clientMode);

  /**
   * Enables or disables required client authentication. If enabled,
   * clients may only connect if they provide proper identification.
   *
   * <p>This parameter is only used in server mode.
   *
   * @param needAuth Whether or not client authentication is required.
   */
  public abstract void setNeedClientAuth (boolean needAuth);

  /**
   * Enables or disables requested client authentication. If enabled,
   * clients will be asked to provide proper identification, but will
   * still be allowed to connect if they do not provide it.
   *
   * <p>This parameter is only used in server mode.
   *
   * @param wantAuth Whether or not client authentication will be
   * requested, but not required.
   */
  public abstract void setWantClientAuth (boolean wantAuth);

  /**
   * Unwraps a byte buffer recieved from the network, storing the
   * decrypted, unwrapped bytes into the given buffer.
   *
   * <p>This call is exactly equivalent to <code>unwrap (source, new
   * ByteBuffer[] { sink }, 0, 1)</code>.
   *
   * @param source The source bytes, coming from the network.
   * @param sink The buffer to hold the unwrapped message.
   * @return An engine result object for the operation.
   * @throws SSLException If an SSL message parsing error occurs.
   * @throws java.nio.ReadOnlyBufferException If 'sink' is not
   * writable.
   * @throws IllegalArgumentException If either 'source' or 'sink' is
   * null.
   * @throws IllegalStateException If this engine has not been put
   * into client or server mode.
   */
  public SSLEngineResult unwrap (ByteBuffer source, ByteBuffer sink)
    throws SSLException
  {
    return unwrap (source, new ByteBuffer[] { sink }, 0, 1);
  }

  /**
   * Unwraps a byte buffer recieved from the network, storing the
   * decrypted, unwrapped bytes into the given buffers.
   *
   * <p>This call is exactly equivalent to <code>unwrap (source,
   * sinks, 0, sinks.length)</code>.
   *
   * @param source The source bytes, coming from the network.
   * @param sinks The buffers to hold the unwrapped message.
   * @return An engine result object for the operation.
   * @throws SSLException If an SSL message parsing error occurs.
   * @throws java.nio.ReadOnlyBufferException If any buffer in 'sinks'
   * is not writable.
   * @throws IllegalArgumentException If either 'source' or 'sinks' is
   * null.
   * @throws IllegalStateException If this engine has not been put
   * into client or server mode.
   */
  public SSLEngineResult unwrap (ByteBuffer source, ByteBuffer[] sinks)
    throws SSLException
  {
    return unwrap (source, sinks, 0, sinks.length);
  }

  /**
   * Unwraps a byte buffer received from the network, storing the
   * decrypted, unwrapped bytes into the given buffers. After
   * unwrapping, the bytes placed into the sink buffers are ready for
   * consumption by the application.
   *
   * <p>This method may place no bytes in the destination buffer; for
   * example, if this engine is still performing the SSL handshake,
   * only handshake data will be consumed, and no application data.
   *
   * <p>It is stated that this method may modify the source buffer,
   * and that it must not be passed to another SSLEngine (SSL
   * connections are independent, so another SSLEngine will not have
   * the parameters or state to handle messages meant for this
   * engine).
   *
   * @param source The source bytes, coming from the network.
   * @param sinks The buffers to hold the unwrapped message.
   * @param offset The index of the first buffer in 'sinks' to use.
   * @param length The number of buffers in 'sinks' to use.
   * @return An engine result object for the operation.
   * @throws SSLException If an SSL message parsing error occurs.
   * @throws java.nio.ReadOnlyBufferException If any buffer in 'sinks'
   * is not writable.
   * @throws IllegalArgumentException If either 'source' or 'sinks' is
   * null.
   * @throws IllegalStateException If this engine has not been put
   * into client or server mode.
   * @throws IndexOutOfBoundsException If 'offset' or 'length' is
   * negative, or if 'length+offset' is greater than 'sinks.length'.
   */
  public abstract SSLEngineResult unwrap (ByteBuffer source,
                                          ByteBuffer[] sinks, int offset,
                                          int length)
    throws javax.net.ssl.SSLException;

  /**
   * Wraps a byte buffer into an SSL message, for preparation to send
   * it over the network.
   *
   * <p>This method is exactly equivalent to <code>wrap (new
   * ByteBuffer[] { source }, 0, 1, sink)</code>.
   *
   * @param source The source buffer with application data.
   * @param sink The buffer to hold the wrapped data.
   * @return An engine result object for the operation.
   * @throws SSLException If an SSL error occurs.
   * @throws java.nio.ReadOnlyBufferException If 'sink' is read-only.
   * @throws IllegalArgumentException If either 'source' or 'sink' is
   * null.
   * @throws IllegalStateException If this engine has not been put
   * into client or server mode.
   */
  public SSLEngineResult wrap (ByteBuffer source, ByteBuffer sink)
    throws SSLException
  {
    return wrap (new ByteBuffer[] { source }, 0, 1, sink);
  }

  /**
   * Wraps byte buffers into an SSL message, for preparation to send
   * them over the network.
   *
   * <p>This method is exactly equivalent to <code>wrap (sources, 0,
   * 1, sink)</code>.
   *
   * @param sources The source buffers with application data.
   * @param sink The buffer to hold the wrapped data.
   * @return An engine result object for the operation.
   * @throws SSLException If an SSL error occurs.
   * @throws java.nio.ReadOnlyBufferException If 'sink' is read-only.
   * @throws IllegalArgumentException If either 'sources' or 'sink' is
   * null.
   * @throws IllegalStateException If this engine has not been put
   * into client or server mode.
   */
  public SSLEngineResult wrap (ByteBuffer[] sources, ByteBuffer sink)
    throws SSLException
  {
    return wrap (sources, 0, sources.length, sink);
  }

  /**
   * Wraps byte buffers into an SSL message, for preparation to send
   * them over the network. After wrapping, the data in the sink
   * buffer is ready to be sent over the transport layer.
   *
   * <p>This method may consume no data from the source buffers, and
   * yet still produce output that should be sent accross the wire;
   * for example if this engine has not yet completed the SSL
   * handshake, the sink buffer will be filled with handshake
   * messages.
   *
   * @param sources The source buffers with application data.
   * @param offset The offset into the source buffers to start reading
   * application data.
   * @param length The number of buffers to read from 'sources'.
   * @param sink The buffer to hold the wrapped data.
   * @return An engine result object for the operation.
   * @throws SSLException If an SSL error occurs.
   * @throws java.nio.ReadOnlyBufferException If 'sink' is read-only.
   * @throws IllegalArgumentException If either 'sources' or 'sink' is
   * null.
   * @throws IllegalStateException If this engine has not been put
   * into client or server mode.
   * @throws IndexOutOfBoundsException If 'offset' or 'length' is
   * negative, or if 'length+offset' is greater than 'sources.length'.
   */
  public abstract SSLEngineResult wrap (ByteBuffer[] sources, int offset,
                                        int length, ByteBuffer sink)
    throws SSLException;

}
