/* InputBuffer.java -- 
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigInteger;

/**
 * The implementation of an incoming SASL buffer.
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
public class InputBuffer
{
  /** The internal buffer stream containing the buffer's contents. */
  protected ByteArrayInputStream in;
  /** The length of the buffer, according to its header. */
  protected int length;

  /**
   * Constructs a SASL buffer given the buffer's encoded form, including its
   * header bytes.
   * 
   * @param frame the encoded form, including the header bytes, of a SASL
   *          buffer.
   * @throws SaslEncodingException if the buffer is malformed.
   */
  public InputBuffer(byte[] frame) throws SaslEncodingException
  {
    this();

    if (frame.length < 4)
      throw new SaslEncodingException("SASL buffer header too short");
    length = (frame[0] & 0xFF) << 24
           | (frame[1] & 0xFF) << 16
           | (frame[2] & 0xFF) << 8
           | (frame[3] & 0xFF);
    if (length > Registry.SASL_BUFFER_MAX_LIMIT || length < 0)
      throw new SaslEncodingException("SASL buffer size limit exceeded");
    in = new ByteArrayInputStream(frame, 4, length);
  }

  /** Trivial private constructor for use by the class method. */
  private InputBuffer()
  {
    super();
  }

  /**
   * Returns an instance of a SASL buffer given the buffer's encoded contents,
   * excluding the buffer's header bytes.
   * <p>
   * Calls the method with the same name and three arguments as:
   * <code>getInstance(raw, 0, raw.length)</code>.
   * 
   * @param raw the encoded form, excluding the header bytes, of a SASL buffer.
   * @return a new instance of {@link InputBuffer}.
   */
  public static InputBuffer getInstance(byte[] raw)
  {
    return getInstance(raw, 0, raw.length);
  }

  /**
   * Returns an instance of a SASL buffer given the buffer's encoded contents,
   * excluding the buffer's header bytes.
   * 
   * @param raw the encoded form, excluding the header bytes, of a SASL buffer.
   * @param offset offset where to start using raw bytes from.
   * @param len number of bytes to use.
   * @return a new instance of {@link InputBuffer}.
   */
  public static InputBuffer getInstance(byte[] raw, int offset, int len)
  {
    InputBuffer result = new InputBuffer();
    result.in = new ByteArrayInputStream(raw, offset, len);
    return result;
  }

  /**
   * Converts two octets into the number that they represent.
   * 
   * @param b the two octets.
   * @return the length.
   */
  public static int twoBytesToLength(byte[] b) throws SaslEncodingException
  {
    final int result = (b[0] & 0xFF) << 8 | (b[1] & 0xFF);
    if (result > Registry.SASL_TWO_BYTE_MAX_LIMIT)
      throw new SaslEncodingException("SASL MPI/Text size limit exceeded");
    return result;
  }

  public boolean hasMoreElements()
  {
    return (in.available() > 0);
  }

  /**
   * Decodes a SASL scalar quantity, <code>count</code>-octet long, from the
   * current buffer.
   * 
   * @param count the number of octets of this scalar quantity.
   * @return a native representation of a SASL scalar (unsigned integer)
   *         quantity.
   * @throws SaslEncodingException if an encoding exception occurs during the
   *           operation.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public long getScalar(int count) throws IOException
  {
    if (count < 0 || count > 4)
      throw new SaslEncodingException("Invalid SASL scalar octet count: "
                                      + String.valueOf(count));
    if (! hasMoreElements())
      throw new SaslEncodingException("Not enough bytes for a scalar in buffer");
    if (in.available() < count)
      throw new SaslEncodingException("Illegal SASL scalar encoding");
    byte[] element = new byte[count];
    in.read(element);
    long result = 0L;
    for (int i = 0; i < count; i++)
      {
        result <<= 8;
        result |= element[i] & 0xFFL;
      }
    return result;
  }

  /**
   * Decodes a SASL OS from the current buffer.
   * 
   * @return a native representation of a SASL OS.
   * @throws SaslEncodingException if an encoding exception occurs during the
   *           operation.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public byte[] getOS() throws IOException
  {
    if (! hasMoreElements())
      throw new SaslEncodingException(
          "Not enough bytes for an octet-sequence in buffer");
    final int elementLength = in.read();
    if (elementLength > Registry.SASL_ONE_BYTE_MAX_LIMIT)
      throw new SaslEncodingException("SASL octet-sequence size limit exceeded");
    if (in.available() < elementLength)
      throw new SaslEncodingException("Illegal SASL octet-sequence encoding");
    byte[] result = new byte[elementLength];
    in.read(result);
    return result;
  }

  /**
   * Decodes a SASL EOS from the current buffer.
   * 
   * @return a native representation of a SASL EOS.
   * @throws SaslEncodingException if an encoding exception occurs during the
   *           operation.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public byte[] getEOS() throws IOException
  {
    if (in.available() < 2)
      throw new SaslEncodingException(
          "Not enough bytes for an extended octet-sequence in buffer");
    byte[] elementLengthBytes = new byte[2];
    in.read(elementLengthBytes);
    final int elementLength = twoBytesToLength(elementLengthBytes);
    if (in.available() < elementLength)
      throw new SaslEncodingException(
          "Illegal SASL extended octet-sequence encoding");
    byte[] result = new byte[elementLength];
    in.read(result);
    return result;
  }

  /**
   * Decodes a SASL MPI from the current buffer.
   * 
   * @return a native representation of a SASL MPI.
   * @throws SaslEncodingException if an encoding exception occurs during the
   *           operation.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public BigInteger getMPI() throws IOException
  {
    if (in.available() < 2)
      throw new SaslEncodingException("Not enough bytes for an MPI in buffer");
    byte[] elementLengthBytes = new byte[2];
    in.read(elementLengthBytes);
    final int elementLength = twoBytesToLength(elementLengthBytes);
    if (in.available() < elementLength)
      throw new SaslEncodingException(
          "Illegal SASL multi-precision integer encoding");
    byte[] element = new byte[elementLength];
    in.read(element);
    return new BigInteger(1, element);
  }

  /**
   * Decodes a SASL Text from the current buffer.
   * 
   * @return a native representation of a SASL Text.
   * @throws SaslEncodingException if an encoding exception occurs during the
   *           operation.
   * @throws SaslEncodingException if the UTF-8 character encoding is not
   *           supported on this platform.
   * @throws IOException if any other I/O exception occurs during the operation.
   */
  public String getText() throws IOException
  {
    if (in.available() < 2)
      throw new SaslEncodingException("Not enough bytes for a text in buffer");
    byte[] elementLengthBytes = new byte[2];
    in.read(elementLengthBytes);
    final int elementLength = twoBytesToLength(elementLengthBytes);
    if (in.available() < elementLength)
      throw new SaslEncodingException("Illegal SASL text encoding");
    byte[] element = new byte[elementLength];
    in.read(element);
    return new String(element, "UTF8");
  }
}
