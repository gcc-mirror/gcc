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

import java.io.DataInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

final class Finished implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  /** TLSv1.x verify data. */
  private final byte[] verifyData;

  /** SSLv3 message digest pair. */
  private final byte[] md5, sha;

  // Constructor.
  // -------------------------------------------------------------------------

  Finished(byte[] verifyData)
  {
    this.verifyData = verifyData;
    md5 = sha = null;
  }

  Finished(byte[] md5, byte[] sha)
  {
    this.md5 = md5;
    this.sha = sha;
    verifyData = null;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static Finished read(InputStream in, CipherSuite suite)
    throws IOException
  {
    DataInputStream din = new DataInputStream(in);
    if (suite.getVersion().equals(ProtocolVersion.SSL_3))
      {
        byte[] md5 = new byte[16];
        byte[] sha = new byte[20];
        din.readFully(md5);
        din.readFully(sha);
        return new Finished(md5, sha);
      }
    else
      {
        byte[] buf = new byte[12];
        din.readFully(buf);
        return new Finished(buf);
      }
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    if (verifyData != null)
      out.write(verifyData);
    else
      {
        out.write(md5);
        out.write(sha);
      }
  }

  byte[] getVerifyData()
  {
    return verifyData;
  }

  byte[] getMD5Hash()
  {
    return md5;
  }

  byte[] getSHAHash()
  {
    return sha;
  }

  public String toString()
  {
    String nl = System.getProperty("line.separator");
    if (verifyData != null)
      {
        return "struct {" + nl +
          "  verifyData = " + Util.toHexString(verifyData, ':') + ";" + nl +
          "} Finished;" + nl;
      }
    else
      {
        return "struct {" + nl +
          "  md5Hash = " + Util.toHexString(md5, ':') + ";" + nl +
          "  shaHash = " + Util.toHexString(sha, ':') + ";" + nl +
          "} Finished;" + nl;
      }
  }
}
