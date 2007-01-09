/* ServerKeyExchange.java -- SSL ServerKeyExchange message.
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
 * The server key exchange message.
 *
 * <pre>
struct
{
  select (KeyExchangeAlgorithm)
  {
    case diffie_hellman:
      ServerDHParams params;
      Signature signed_params;
    case rsa:
      ServerRSAParams params;
      Signature signed_params;
    case srp:
      ServerSRPParams params;
      Signature signed_params;
  };
} ServerKeyExchange;
</pre>
 */
public class ServerKeyExchange implements Handshake.Body
{

  protected ByteBuffer buffer;
  protected final CipherSuite suite;

  public ServerKeyExchange(final ByteBuffer buffer, final CipherSuite suite)
  {
    suite.getClass();
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
    this.suite = suite;
  }

  public int length ()
  {
    if (suite.keyExchangeAlgorithm ().equals (KeyExchangeAlgorithm.NONE))
      return 0;
    int len = 0;
    ServerKeyExchangeParams params = params();
    Signature sig = signature();
    if (params != null)
      len += params.length();
    if (sig != null)
      len += sig.length();
    return len;
  }

  /**
   * Returns the server's key exchange parameters. The value returned will
   * depend on the key exchange algorithm this object was created with.
   *
   * @return The server's key exchange parameters.
   */
  public ServerKeyExchangeParams params ()
  {
    KeyExchangeAlgorithm kex = suite.keyExchangeAlgorithm ();
    if (kex == KeyExchangeAlgorithm.RSA)
      return new ServerRSAParams(buffer.duplicate ());
    else if (kex == KeyExchangeAlgorithm.DHE_DSS
             || kex == KeyExchangeAlgorithm.DHE_RSA
             || kex == KeyExchangeAlgorithm.DH_anon)
      return new ServerDHParams(buffer.duplicate());
//     else if (kex.equals (KeyExchangeAlgorithm.SRP))
//       return new ServerSRPParams (buffer.duplicate ());
    else if (kex == KeyExchangeAlgorithm.NONE)
      return null;
    else if (kex == KeyExchangeAlgorithm.DHE_PSK)
      return new ServerDHE_PSKParameters(buffer.duplicate());
    else if (kex == KeyExchangeAlgorithm.PSK)
      return new ServerPSKParameters(buffer.duplicate());
    else if (kex == KeyExchangeAlgorithm.RSA_PSK)
      return new ServerPSKParameters(buffer.duplicate());
    throw new IllegalArgumentException ("unsupported key exchange: " + kex);
  }

  /**
   * Returns the digital signature made over the key exchange parameters.
   *
   * @return The signature.
   */
  public Signature signature ()
  {
    KeyExchangeAlgorithm kex = suite.keyExchangeAlgorithm();
    if (kex == KeyExchangeAlgorithm.NONE
        || kex == KeyExchangeAlgorithm.DH_anon
        || kex == KeyExchangeAlgorithm.DHE_PSK
        || kex == KeyExchangeAlgorithm.PSK
        || kex == KeyExchangeAlgorithm.RSA_PSK)
      return null;
    ServerKeyExchangeParams params = params();
    ByteBuffer sigbuf = ((ByteBuffer) buffer.position(params.length ())).slice ();
    return new Signature (sigbuf, suite.signatureAlgorithm ());
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
    out.print ("  algorithm: ");
    out.print (suite.keyExchangeAlgorithm ());
    out.println (";");
    if (!suite.keyExchangeAlgorithm ().equals (KeyExchangeAlgorithm.NONE))
      {
        if (prefix != null) out.print (prefix);
        out.println ("  parameters:");
        out.println (params ().toString (prefix != null ? prefix+"  " : "  "));
      }
    if (!suite.signatureAlgorithm ().equals (SignatureAlgorithm.ANONYMOUS))
      {
        if (prefix != null) out.print (prefix);
        out.println ("  signature:");
        out.println (signature ().toString (prefix != null ? prefix+"  " : "  "));
      }
    if (prefix != null) out.print (prefix);
    out.print ("} ServerKeyExchange;");
    return str.toString();
  }
}
