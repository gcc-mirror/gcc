/* ClientHelloV2.java -- a hello message from SSLv2.
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
import java.util.ArrayList;
import java.util.List;

/**
 * A client hello message from SSLv2. In SSLv3 and later, clients can
 * send an SSLv2 client hello message, but set the protocol version
 * for a later version.
 *
 * <p>The format of a version 2 client hello is:
 *
 * <pre>
    char MSG-CLIENT-HELLO          // equals 1
    char CLIENT-VERSION-MSB
    char CLIENT-VERSION-LSB
    char CIPHER-SPECS-LENGTH-MSB
    char CIPHER-SPECS-LENGTH-LSB
    char SESSION-ID-LENGTH-MSB
    char SESSION-ID-LENGTH-LSB
    char CHALLENGE-LENGTH-MSB
    char CHALLENGE-LENGTH-LSB
    char CIPHER-SPECS-DATA[(MSB&lt;&lt;8)|LSB]
    char SESSION-ID-DATA[(MSB&lt;&lt;8)|LSB]
    char CHALLENGE-DATA[(MSB&lt;&lt;8)|LSB]</pre>
 */
class ClientHelloV2 implements Constructed
{
  private final ByteBuffer buffer;

  ClientHelloV2 (final ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
  }

  public int length ()
  {
    return 9 + cipherSpecsLength () + sessionIdLength () + challengeLength ();
  }

  ProtocolVersion version ()
  {
    return ProtocolVersion.getInstance (buffer.getShort (1));
  }

  int cipherSpecsLength ()
  {
    return buffer.getShort (3) & 0xFFFF;
  }

  int sessionIdLength ()
  {
    return buffer.getShort (5) & 0xFFFF;
  }

  int challengeLength ()
  {
    return buffer.getShort (7) & 0xFFFF;
  }

  public List<CipherSuite> cipherSpecs ()
  {
    int n = cipherSpecsLength ();
    List<CipherSuite> l = new ArrayList<CipherSuite>(n / 3);
    ByteBuffer b = (ByteBuffer) buffer.duplicate ().position (9);
    for (int i = 0; i < n; i += 3)
      {
        if (b.get () == 0)
          l.add (CipherSuite.forValue(b.getShort()).resolve());
        else
          b.getShort ();
      }
    return l;
  }

  byte[] sessionId ()
  {
    byte[] id = new byte[sessionIdLength ()];
    ((ByteBuffer) buffer.duplicate ().position (9 + cipherSpecsLength ())).get (id);
    return id;
  }

  byte[] challenge ()
  {
    byte[] challenge = new byte[challengeLength ()];
    ((ByteBuffer) buffer.duplicate ().position (9 + cipherSpecsLength () + sessionIdLength ())).get (challenge);
    return challenge;
  }

  public String toString ()
  {
    return toString (null);
  }

  public String toString (String prefix)
  {
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);

    if (prefix != null) out.print (prefix);
    out.println ("CLIENT-HELLO-MSG");
    if (prefix != null) out.print (prefix);
    out.print ("  version: ");
    out.println (version ());
    if (prefix != null) out.print (prefix);
    out.println ("  suites: ");
    out.println (cipherSpecs ());
    if (prefix != null) out.print (prefix);
    out.print ("  sessionId: ");
    out.println (Util.toHexString (sessionId (), ':'));
    if (prefix != null) out.print (prefix);
    out.print ("  challenge: ");
    out.println (Util.toHexString (challenge (), ':'));
    return str.toString ();
  }
}
