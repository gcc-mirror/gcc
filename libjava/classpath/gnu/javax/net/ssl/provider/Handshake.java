/* Handshake.java -- SSL Handshake message.
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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import java.nio.ByteBuffer;

import java.security.PublicKey;

import java.util.ArrayList;
import java.util.Collections;

import javax.net.ssl.SSLProtocolException;

/**
 * An SSL handshake message. SSL handshake messages have the following
 * form:
 *
 * <pre>
struct
{
  HandshakeType msg_type;
  uint24        length;
  select (msg_type)
  {
    case hello_request:       HelloRequest;
    case client_hello:        ClientHello;
    case server_hello:        ServerHello;
    case certificate:         Certificate;
    case server_key_exchange: ServerKeyExchange;
    case certificate_request: CertificateRequest;
    case server_hello_done:   ServerHelloDone;
    case certificate_verify:  CertificateVerify;
    case client_key_exchange: ClientKeyExchange;
    case finished:            Finished;
  } body;
};</pre>
 */
public final class Handshake implements Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  private final ByteBuffer buffer;
  private final CipherSuite suite;
  private final ProtocolVersion version;

  // Constructors.
  // -------------------------------------------------------------------------

  public Handshake (final ByteBuffer buffer)
  {
    this (buffer, null, ProtocolVersion.TLS_1_1);
  }

  public Handshake (final ByteBuffer buffer, final CipherSuite suite,
                    final ProtocolVersion version)
  {
    this.buffer = buffer;
    this.suite = suite;
    this.version = version;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the handshake type.
   *
   * @return The handshake type.
   */
  public Type type()
  {
    return Type.forInteger (buffer.get (0) & 0xFF);
  }

  /**
   * Returns the message length.
   *
   * @return The message length.
   */
  public int length ()
  {
    // Length is a uint24.
    return buffer.getInt (0) & 0xFFFFFF;
  }

  /**
   * Returns the handshake message body. Depending on the handshake
   * type, some implementation of the Body interface is returned.
   *
   * @return The handshake body.
   */
  public Body body()
  {
    Type type = type ();
    ByteBuffer bodyBuffer = bodyBuffer ();
    switch (type)
      {
      case HELLO_REQUEST:
        return new HelloRequest ();

      case CLIENT_HELLO:
        return new ClientHello (bodyBuffer);

      case SERVER_HELLO:
        return new ServerHello (bodyBuffer);

      case CERTIFICATE:
        return new Certificate (bodyBuffer, CertificateType.X509);

      case SERVER_KEY_EXCHANGE:
        return new ServerKeyExchange (bodyBuffer, suite);

      case CERTIFICATE_REQUEST:
        return new CertificateRequest (bodyBuffer);

      case SERVER_HELLO_DONE:
        return new ServerHelloDone ();

      case CERTIFICATE_VERIFY:
        return new CertificateVerify (bodyBuffer, suite.signatureAlgorithm ());

      case CLIENT_KEY_EXCHANGE:
        return new ClientKeyExchange (bodyBuffer, suite, version);

      case FINISHED:
        return new Finished (bodyBuffer, version);

      case CERTIFICATE_URL:
      case CERTIFICATE_STATUS:
        throw new UnsupportedOperationException ("FIXME");
      }
    throw new IllegalArgumentException ("unknown handshake type " + type);
  }

  /**
   * Returns a subsequence of the underlying buffer, containing only
   * the bytes that compose the handshake body.
   *
   * @return The body's byte buffer.
   */
  public ByteBuffer bodyBuffer ()
  {
    int length = length ();
    return ((ByteBuffer) buffer.position (4).limit (4 + length)).slice ();
  }

  /**
   * Sets the handshake body type.
   *
   * @param type The handshake type.
   */
  public void setType (final Type type)
  {
    buffer.put (0, (byte) type.getValue ());
  }

  /**
   * Sets the length of the handshake body.
   *
   * @param length The handshake body length.
   * @throws java.nio.ReadOnlyBufferException If the underlying buffer
   * is not writable.
   * @throws IllegalArgumentException of <code>length</code> is not
   * between 0 and 16777215, inclusive.
   */
  public void setLength (final int length)
  {
    if (length < 0 || length > 0xFFFFFF)
      throw new IllegalArgumentException ("length " + length + " out of range;"
                                          + " must be between 0 and 16777215");
    buffer.put (1, (byte) (length >>> 16));
    buffer.put (2, (byte) (length >>>  8));
    buffer.put (3, (byte)  length);
  }

  public String toString()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null) out.print (prefix);
    out.println("struct {");
    if (prefix != null) out.print (prefix);
    out.print ("  type: ");
    out.print (type ());
    out.println (";");
    Body body = body ();
    out.println (body.toString (prefix != null ? (prefix + "  ") : "  "));
    if (prefix != null) out.print (prefix);
    out.print ("} Handshake;");
    return str.toString();
  }

  // Inner class.
  // -------------------------------------------------------------------------

  public static interface Body extends Constructed
  {
    int length ();

    String toString (String prefix);
  }

  public static enum Type
  {
    HELLO_REQUEST       ( 0),
    CLIENT_HELLO        ( 1),
    SERVER_HELLO        ( 2),
    CERTIFICATE         (11),
    SERVER_KEY_EXCHANGE (12),
    CERTIFICATE_REQUEST (13),
    SERVER_HELLO_DONE   (14),
    CERTIFICATE_VERIFY  (15),
    CLIENT_KEY_EXCHANGE (16),
    FINISHED            (20),
    CERTIFICATE_URL     (21),
    CERTIFICATE_STATUS  (22);

    private final int value;

    private Type(int value)
    {
      this.value = value;
    }

    // Class methods.
    // -----------------------------------------------------------------------

    /**
     * Convert a raw handshake type value to a type enum value.
     * 
     * @return The corresponding enum value for the raw integer value.
     * @throws IllegalArgumentException If the value is not a known handshake
     *  type.
     */
    public static Type forInteger (final int value)
    {
      switch (value & 0xFF)
        {
        case 0:  return HELLO_REQUEST;
        case 1:  return CLIENT_HELLO;
        case 2:  return SERVER_HELLO;
        case 11: return CERTIFICATE;
        case 12: return SERVER_KEY_EXCHANGE;
        case 13: return CERTIFICATE_REQUEST;
        case 14: return SERVER_HELLO_DONE;
        case 15: return CERTIFICATE_VERIFY;
        case 16: return CLIENT_KEY_EXCHANGE;
        case 20: return FINISHED;
        case 21: return CERTIFICATE_URL;
        case 22: return CERTIFICATE_STATUS;
        default: throw new IllegalArgumentException ("unsupported value type " + value);
        }
    }

    public int getValue()
    {
      return value;
    }
  }
}
