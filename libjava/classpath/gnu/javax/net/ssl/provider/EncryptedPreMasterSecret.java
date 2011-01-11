/* EncryptedPreMasterSecret.java -- RSA encrypted secret.
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

/**
 * The client's RSA-encrypted pre-master secret.
 *
 * <pre>
struct {
  public-key-encrypted PreMasterSecret pre_master_secret;
} EncryptedPreMasterSecret;</pre>
 */
public final class EncryptedPreMasterSecret extends ExchangeKeys implements Builder
{
  private final ProtocolVersion version;

  public EncryptedPreMasterSecret(ByteBuffer buffer, ProtocolVersion version)
  {
    super(buffer);
    version.getClass();
    this.version = version;
  }

  public EncryptedPreMasterSecret(byte[] encryptedSecret, ProtocolVersion version)
  {
    this(ByteBuffer.allocate(version == ProtocolVersion.SSL_3
                             ? encryptedSecret.length
                             : encryptedSecret.length + 2), version);
    ByteBuffer b = buffer.duplicate();
    if (version != ProtocolVersion.SSL_3)
      b.putShort((short) encryptedSecret.length);
    b.put(encryptedSecret);
  }

  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().rewind();
  }

  public byte[] encryptedSecret()
  {
    byte[] secret;
    if (version == ProtocolVersion.SSL_3)
      {
        buffer.position (0);
        secret = new byte[buffer.limit ()];
        buffer.get(secret);
      }
    else
      {
        int len = buffer.getShort(0) & 0xFFFF;
        secret = new byte[len];
        buffer.position(2);
        buffer.get(secret);
      }
    return secret;
  }

  public void setEncryptedSecret(final byte[] secret, final int offset, final int length)
  {
    if (version == ProtocolVersion.SSL_3)
      {
        buffer.position(0);
        buffer.put(secret, offset, length);
        buffer.rewind();
      }
    else
      {
        buffer.putShort(0, (short) length);
        buffer.position(2);
        buffer.put(secret, offset, length);
        buffer.rewind();
      }
  }

  public int length ()
  {
    if (version == ProtocolVersion.SSL_3)
      {
        return buffer.capacity();
      }
    else
      {
        return (buffer.getShort(0) & 0xFFFF) + 2;
      }
  }

  public String toString ()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null) out.print(prefix);
    out.println("struct {");
    if (prefix != null) out.print(prefix);
    out.println("  pre_master_secret = ");
    out.print(Util.hexDump(encryptedSecret(), prefix != null ? prefix + "    "
                                                             : "    "));
    if (prefix != null) out.print(prefix);
    out.print("} EncryptedPreMasterSecret;");
    return str.toString();
  }
}
