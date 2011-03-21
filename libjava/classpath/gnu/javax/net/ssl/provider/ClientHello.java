/* ClientHello.java -- SSL ClientHello message.
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

import java.io.PrintWriter;
import java.io.StringWriter;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * A ClientHello handshake message.
 *
 * <pre>
struct
{
  ProtocolVersion   client_version;                // 2
  Random            random;                        // 32
  SessionID         session_id;                    // 1 + 0..32
  CipherSuite       cipher_suites&lt;2..2^16-1&gt;
  CompressionMethod compression_methods&lt;1..2^8-1&gt;
  Extension         client_hello_extension_list&lt;0..2^16-1&gt;
} ClientHello;
</pre>
 */
public class ClientHello implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  // To help track offsets into the message:
  // The location of the 'random' field.
  protected static final int RANDOM_OFFSET = 2;
  // The location of the sesion_id length.
  protected static final int SESSID_OFFSET = 32 + RANDOM_OFFSET;
  // The location of the session_id bytes (if any).
  protected static final int SESSID_OFFSET2 = SESSID_OFFSET + 1;

  protected ByteBuffer buffer;
  protected boolean disableExtensions;

  // Constructor.
  // -------------------------------------------------------------------------

  public ClientHello (final ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
    disableExtensions = false;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length()
  {
    int len = SESSID_OFFSET2 + buffer.get(SESSID_OFFSET);
    len += (buffer.getShort(len) & 0xFFFF) + 2;
    len += (buffer.get(len) & 0xFF) + 1;
    if (!disableExtensions && len + 1 < buffer.capacity())
      len += (buffer.getShort(len) & 0xFFFF) + 2;
    return len;
  }

  /**
   * Gets the protocol version field.
   *
   * @return The protocol version field.
   */
  public ProtocolVersion version()
  {
    return ProtocolVersion.getInstance (buffer.getShort (0));
  }

  /**
   * Gets the SSL nonce.
   *
   * @return The nonce.
   */
  public Random random()
  {
    ByteBuffer randomBuf =
      ((ByteBuffer) buffer.duplicate ().position (RANDOM_OFFSET)
       .limit (SESSID_OFFSET)).slice ();
    return new Random (randomBuf);
  }

  public byte[] sessionId()
  {
    int idlen = buffer.get (SESSID_OFFSET) & 0xFF;
    byte[] sessionId = new byte[idlen];
    buffer.position (SESSID_OFFSET2);
    buffer.get (sessionId);
    return sessionId;
  }

  public CipherSuiteList cipherSuites()
  {
    int offset = getCipherSuitesOffset ();

    // We give the CipherSuiteList all the remaining bytes to play with,
    // since this might be an in-construction packet that will fill in
    // the length field itself.
    ByteBuffer listBuf = ((ByteBuffer) buffer.duplicate ().position (offset)
                          .limit (buffer.capacity ())).slice ();
    return new CipherSuiteList (listBuf, version ());
  }

  public CompressionMethodList compressionMethods()
  {
    int offset = getCompressionMethodsOffset ();
    ByteBuffer listBuf = ((ByteBuffer) buffer.duplicate ().position (offset)
                          .limit (buffer.capacity ())).slice ();
    return new CompressionMethodList (listBuf);
  }

  public boolean hasExtensions()
  {
    int offset = getExtensionsOffset();
    return (offset + 1 < buffer.limit());
  }

  public ExtensionList extensions()
  {
    int offset = getExtensionsOffset ();
    if (offset + 1 >= buffer.limit())
      return null;
    int len = buffer.getShort(offset) & 0xFFFF;
    if (len == 0)
      len = buffer.limit() - offset - 2;
    ByteBuffer ebuf = ((ByteBuffer) buffer.duplicate().position(offset)
                       .limit(offset + len + 2)).slice ();
    return new ExtensionList(ebuf);
  }

  public int extensionsLength()
  {
    if (hasExtensions())
      return 0;
    return buffer.getShort(getExtensionsOffset()) & 0xFFFF;
  }

  protected int getCipherSuitesOffset ()
  {
    return (SESSID_OFFSET2 + (buffer.get (SESSID_OFFSET) & 0xFF));
  }

  protected int getCompressionMethodsOffset ()
  {
    int csOffset = getCipherSuitesOffset ();
    int csLen = buffer.getShort (csOffset) & 0xFFFF;
    return csOffset + csLen + 2;
  }

  protected int getExtensionsOffset ()
  {
    int cmOffset = getCompressionMethodsOffset ();
    return (buffer.get (cmOffset) & 0xFF) + cmOffset + 1;
  }

  public String toString ()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);
    String subprefix = "  ";
    if (prefix != null)
      subprefix += prefix;
    if (prefix != null)
      out.print (prefix);
    out.println ("struct {");
    if (prefix != null)
      out.print (prefix);
    out.print ("  version: ");
    out.print (version ());
    out.println (";");
    out.print (subprefix);
    out.println ("random:");
    out.print (random ().toString (subprefix));
    if (prefix != null)
      out.print (prefix);
    out.print ("  sessionId: ");
    out.print (Util.toHexString (sessionId (), ':'));
    out.println (";");
    out.print (subprefix);
    out.println ("cipher_suites:");
    out.println (cipherSuites ().toString (subprefix));
    out.print (subprefix);
    out.println ("compression_methods:");
    out.println (compressionMethods ().toString (subprefix));
    out.print (subprefix);
    out.print ("extensions: ");
    ExtensionList el = extensions();
    out.println (el != null ? el.toString(subprefix+"  ") : "(nil)");
    if (prefix != null)
      out.print (prefix);
    out.print ("} ClientHello;");
    return str.toString();
  }
}
