/* Finished.java -- SSL Finished message.
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

final class Finished implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  private final ByteBuffer buffer;
  private final ProtocolVersion version;

  // Constructor.
  // -------------------------------------------------------------------------

  Finished (final ByteBuffer buffer, final ProtocolVersion version)
  {
    buffer.getClass ();
    version.getClass ();
    this.buffer = buffer;
    this.version = version;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length ()
  {
    if (version.compareTo(ProtocolVersion.TLS_1) >= 0)
      return 12;
    if (version == ProtocolVersion.SSL_3)
      return 36;
    throw new IllegalArgumentException ("length for this version unknown");
  }

  byte[] verifyData()
  {
    if (version.compareTo(ProtocolVersion.TLS_1) >= 0)
      {
        byte[] verify = new byte[12];
        buffer.position (0);
        buffer.get (verify);
        return verify;
      }
    throw new IllegalArgumentException ("not TLSv1.0 or later");
  }

  byte[] md5Hash()
  {
    if (version == ProtocolVersion.SSL_3)
      {
        byte[] md5 = new byte[16];
        buffer.position (0);
        buffer.get (md5);
        return md5;
      }
    throw new IllegalArgumentException ("not SSLv3");
  }

  byte[] shaHash()
  {
    if (version == ProtocolVersion.SSL_3)
      {
        byte[] sha = new byte[20];
        buffer.position (16);
        buffer.get (sha);
        return sha;
      }
    throw new IllegalArgumentException ("not SSLv3");
  }

  void setVerifyData (final byte[] verifyData, final int offset)
  {
    if (version == ProtocolVersion.SSL_3)
      throw new IllegalArgumentException ("not TLSv1");
    buffer.position (0);
    buffer.put (verifyData, offset, 12);
  }

  void setMD5Hash (final byte[] md5, final int offset)
  {
    if (version != ProtocolVersion.SSL_3)
      throw new IllegalArgumentException ("not SSLv3");
    buffer.position (0);
    buffer.put (md5, offset, 16);
  }

  void setShaHash (final byte[] sha, final int offset)
  {
    if (version != ProtocolVersion.SSL_3)
      throw new IllegalArgumentException ("not SSLv3");
    buffer.position (16);
    buffer.put (sha, offset, 20);
  }

  public String toString ()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);
    if (prefix != null)
      out.print (prefix);
    out.println ("struct {");
    if (prefix != null)
      out.print (prefix);
    if (version.compareTo(ProtocolVersion.TLS_1) >= 0)
      {
        out.print ("  verifyData = ");
        out.print (Util.toHexString (verifyData (), ':'));
      }
    else if (version == ProtocolVersion.SSL_3)
      {
        out.print ("  md5 = ");
        out.print (Util.toHexString (md5Hash (), ':'));
        out.println (';');
        if (prefix != null)
          out.print (prefix);
        out.print ("  sha = ");
        out.print (Util.toHexString (shaHash (), ':'));
      }
    out.println (';');
    if (prefix != null)
      out.print (prefix);
    out.print ("} Finished;");
    return str.toString ();
  }
}
