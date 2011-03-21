/* OutputBuffer.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;

/**
 * The implementation of an outgoing SASL buffer.
 * <p>
 * The data elements this class caters for are described in [1].
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://www.ietf.org/internet-drafts/draft-burdis-cat-srp-sasl-09.txt">
 * Secure Remote Password Authentication Mechanism</a>;<br/>
 * draft-burdis-cat-srp-sasl-09,<br/> <a
 * href="mailto:keith@rucus.ru.ac.za">Keith Burdis</a> and <a
 * href="mailto:raif@forge.com.au">Ra&iuml;f S. Naffah</a>.</li>
 * </ol>
 */
public class OutputBuffer
{
  /** The internal output stream. */
  private ByteArrayOutputStream out;

  public OutputBuffer()
  {
    super();

    out = new ByteArrayOutputStream();
  }

  /**
   * Encodes a SASL scalar quantity, <code>count</code>-octet long, to the
   * current buffer.
   *
   * @param count number of octets to encode <code>b</code> with.
   * @param b the scalar quantity.
   * @throws SaslEncodingException if an encoding size constraint is violated.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public void setScalar(int count, int b) throws IOException
  {
    if (count < 0 || count > 4)
      throw new SaslEncodingException("Invalid SASL scalar octet count: "
                                      + String.valueOf(count));
    byte[] element = new byte[count];
    for (int i = count; --i >= 0; b >>>= 8)
      element[i] = (byte) b;
    out.write(element);
  }

  /**
   * Encodes a SASL OS to the current buffer.
   *
   * @param b the OS element.
   * @throws SaslEncodingException if an encoding size constraint is violated.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public void setOS(byte[] b) throws IOException
  {
    final int length = b.length;
    if (length > Registry.SASL_ONE_BYTE_MAX_LIMIT)
      throw new SaslEncodingException("SASL octet-sequence too long");
    out.write(length & 0xFF);
    out.write(b);
  }

  /**
   * Encodes a SASL EOS to the current buffer.
   *
   * @param b the EOS element.
   * @throws SaslEncodingException if an encoding size constraint is violated.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public void setEOS(byte[] b) throws IOException
  {
    final int length = b.length;
    if (length > Registry.SASL_TWO_BYTE_MAX_LIMIT)
      throw new SaslEncodingException("SASL extended octet-sequence too long");
    byte[] lengthBytes = { (byte)(length >>> 8), (byte) length };
    out.write(lengthBytes);
    out.write(b);
  }

  /**
   * Encodes a SASL MPI to the current buffer.
   *
   * @param val the MPI element.
   * @throws SaslEncodingException if an encoding size constraint is violated.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public void setMPI(BigInteger val) throws IOException
  {
    byte[] b = Util.trim(val);
    final int length = b.length;
    if (length > Registry.SASL_TWO_BYTE_MAX_LIMIT)
      throw new SaslEncodingException("SASL multi-precision integer too long");
    byte[] lengthBytes = { (byte)(length >>> 8), (byte) length };
    out.write(lengthBytes);
    out.write(b);
  }

  /**
   * Encodes a SASL Text to the current buffer.
   *
   * @param str the Text element.
   * @throws SaslEncodingException if an encoding size constraint is violated.
   * @throws SaslEncodingException if the UTF-8 encoding is not supported on
   *           this platform.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public void setText(String str) throws IOException
  {
    byte[] b = str.getBytes("UTF8");
    final int length = b.length;
    if (length > Registry.SASL_TWO_BYTE_MAX_LIMIT)
      throw new SaslEncodingException("SASL text too long");
    byte[] lengthBytes = { (byte)(length >>> 8), (byte) length };
    out.write(lengthBytes);
    out.write(b);
  }

  /**
   * Returns the encoded form of the current buffer including the 4-byte length
   * header.
   *
   * @throws SaslEncodingException if an encoding size constraint is violated.
   */
  public byte[] encode() throws SaslEncodingException
  {
    byte[] buffer = wrap();
    final int length = buffer.length;
    byte[] result = new byte[length + 4];
    result[0] = (byte)(length >>> 24);
    result[1] = (byte)(length >>> 16);
    result[2] = (byte)(length >>> 8);
    result[3] = (byte) length;
    System.arraycopy(buffer, 0, result, 4, length);
    return result;
  }

  /**
   * Returns the encoded form of the current buffer excluding the 4-byte length
   * header.
   *
   * @throws SaslEncodingException if an encoding size constraint is violated.
   */
  public byte[] wrap() throws SaslEncodingException
  {
    final int length = out.size();
    if (length > Registry.SASL_BUFFER_MAX_LIMIT || length < 0)
      throw new SaslEncodingException("SASL buffer too long");
    return out.toByteArray();
  }
}
