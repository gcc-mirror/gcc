/* ServerRSAParams.java -- The server's RSA parameters.
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
import java.math.BigInteger;
import java.nio.ByteBuffer;

/**
 * The ServerRSAParams structure.
 *
 * <pre>
struct
{
  opaque rsa_modulus&lt;1..2^16-1&gt;;
  opaque rsa_exponent&lt;1..2^16-1&gt;;
} ServerRSAParams;
</pre>
 */
public class ServerRSAParams implements ServerKeyExchangeParams
{

  private final ByteBuffer buffer;

  public ServerRSAParams (final ByteBuffer buffer)
  {
    this.buffer = buffer;
  }

  public KeyExchangeAlgorithm algorithm ()
  {
    return KeyExchangeAlgorithm.RSA;
  }

  public int length ()
  {
    int offset = buffer.getShort (0) & 0xFFFF;
    return (buffer.getShort (offset + 2) & 0xFFFF) + offset + 4;
  }

  /**
   * Gets the modulus field.
   *
   * @return The modulus.
   */
  public BigInteger modulus ()
  {
    int len = buffer.getShort (0) & 0xFFFF;
    byte[] buf = new byte[len];
    buffer.position (2);
    buffer.get (buf);
    return new BigInteger (1, buf);
  }

  /**
   * Returns the exponent field.
   *
   * @return The exponent.
   */
  public BigInteger exponent ()
  {
    int off = (buffer.getShort (0) & 0xFFFF) + 2;
    int len = buffer.getShort (off) & 0xFFFF;
    byte[] buf = new byte[len];
    buffer.position (off + 2);
    buffer.get (buf);
    return new BigInteger (1, buf);
  }

  /**
   * Sets the modulus.
   *
   * @param modulus The modulus.
   * @throws java.nio.ReadOnlyBufferException If the underlying buffer
   * is not writable.
   */
  public void setModulus (final BigInteger modulus)
  {
    byte[] buf = modulus.toByteArray ();
    int length = (buf[0] == 0x00 ? buf.length - 1 : buf.length);
    int offset = (buf[0] == 0x00 ? 1 : 0);
    buffer.putShort (0, (short) length);
    buffer.position (2);
    buffer.put (buf, offset, length);
  }

  /**
   * Sets the exponent.
   *
   * @param exponent The exponent.
   * @throws java.nio.ReadOnlyBufferException If the underlying buffer
   * is not writeable.
   */
  public void setExponent (final BigInteger exponent)
  {
    byte[] buf = exponent.toByteArray ();
    int length = (buf[0] == 0x00 ? buf.length -1 : buf.length);
    int offset = (buf[0] == 0x00 ? 1 : 0);
    int where = (buffer.getShort (0) & 0xFFFF) + 2;
    buffer.putShort (where, (short) length);
    buffer.position (where + 2);
    buffer.put (buf, offset, length);
  }

  public String toString ()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);
    if (prefix != null) out.print (prefix);
    out.println ("struct {");
    if (prefix != null) out.print (prefix);
    out.print ("  rsa_modulus:  ");
    out.println (modulus ().toString (16));
    if (prefix != null) out.print (prefix);
    out.print ("  rsa_exponent: ");
    out.println (exponent ());
    if (prefix != null) out.print (prefix);
    out.print ("} ServerRSAParams;");
    return str.toString ();
  }
}
