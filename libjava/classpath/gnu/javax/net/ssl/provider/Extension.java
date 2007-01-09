/* Extension.java -- A TLS hello extension.
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
 * An SSL hello extension.
 * 
 * <pre>
 * struct {
 *   ExtensionType extension_type;
 *   opaque extension_data<0..2^16-1>;
 * } Extension;</pre>
 * 
 * @author csm@gnu.org
 */
public final class Extension implements Builder, Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  private ByteBuffer buffer;

  // Constructor.
  // -------------------------------------------------------------------------

  public Extension(final ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
  }
  
  public Extension(final Type type, final Value value)
  {
    ByteBuffer valueBuffer = value.buffer();
    int length = 2 + 2 + valueBuffer.remaining();
    buffer = ByteBuffer.allocate(length);
    buffer.putShort((short) type.getValue());
    buffer.putShort((short) valueBuffer.remaining());
    buffer.put(valueBuffer);
    buffer.rewind();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length ()
  {
    return (buffer.getShort (2) & 0xFFFF) + 4;
  }
  
  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().limit(length());
  }

  public Type type()
  {
    return Type.forValue (buffer.getShort (0) & 0xFFFF);
  }

  public byte[] valueBytes()
  {
    int len = buffer.getShort (2) & 0xFFFF;
    byte[] value = new byte[len];
    ((ByteBuffer) buffer.duplicate ().position (4)).get (value);
    return value;
  }
  
  public ByteBuffer valueBuffer()
  {
    int len = buffer.getShort(2) & 0xFFFF;
    return ((ByteBuffer) buffer.duplicate().position(4).limit(len+4)).slice();
  }
  
  public Value value()
  {
    switch (type ())
      {
        case SERVER_NAME:
          return new ServerNameList(valueBuffer());
          
        case MAX_FRAGMENT_LENGTH:
          switch (valueBuffer().get() & 0xFF)
            {
              case 1: return MaxFragmentLength.LEN_2_9;
              case 2: return MaxFragmentLength.LEN_2_10;
              case 3: return MaxFragmentLength.LEN_2_11;
              case 4: return MaxFragmentLength.LEN_2_12;
              default:
                throw new IllegalArgumentException("invalid max_fragment_len");
            }
          
        case TRUNCATED_HMAC:
          return new TruncatedHMAC();

        case CLIENT_CERTIFICATE_URL:
          return new CertificateURL(valueBuffer());
          
        case TRUSTED_CA_KEYS:
          return new TrustedAuthorities(valueBuffer());
          
        case STATUS_REQUEST:
          return new CertificateStatusRequest(valueBuffer());
          
        case SRP:
        case CERT_TYPE:
      }
    return new UnresolvedExtensionValue(valueBuffer());
  }
  
  public void setLength (final int newLength)
  {
    if (newLength < 0 || newLength > 65535)
      throw new IllegalArgumentException ("length is out of bounds");
    buffer.putShort (2, (short) newLength);
  }
  
  public void setType (final Type type)
  {
    buffer.putShort(0, (short) type.getValue());
  }

  public void setValue (byte[] value)
  {
    setValue (value, 0, value.length);
  }
  
  public void setValue (final byte[] value, final int offset, final int length)
  {
    if (length != length ())
      throw new IllegalArgumentException ("length is different than claimed length");
    ((ByteBuffer) buffer.duplicate().position(4)).put(value, offset, length);
  }
  
  public String toString()
  {
    return toString(null);
  }

  public String toString(String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null) out.print (prefix);
    out.println("struct {");
    if (prefix != null) out.print (prefix);
    out.println("  type = " + type () + ";");
    if (prefix != null) out.print (prefix);
    String subprefix = "  ";
    if (prefix != null) subprefix = prefix + subprefix;
    out.println("  value =");
    out.println(value().toString(subprefix));
    if (prefix != null) out.print (prefix);
    out.print("} Extension;");
    return str.toString();
  }

  // Inner classes.
  // -------------------------------------------------------------------------

  public static enum Type
  {
    SERVER_NAME            (0),
    MAX_FRAGMENT_LENGTH    (1),
    CLIENT_CERTIFICATE_URL (2),
    TRUSTED_CA_KEYS        (3),
    TRUNCATED_HMAC         (4),
    STATUS_REQUEST         (5),
    SRP                    (6),
    CERT_TYPE              (7);

    private final int value;

    private Type(int value)
    {
      this.value = value;
    }

    public static Type forValue (final int value)
    {
      switch (value & 0xFFFF)
        {
          case 0: return SERVER_NAME;
          case 1: return MAX_FRAGMENT_LENGTH;
          case 2: return CLIENT_CERTIFICATE_URL;
          case 3: return TRUSTED_CA_KEYS;
          case 4: return TRUNCATED_HMAC;
          case 5: return STATUS_REQUEST;
          case 6: return SRP;
          case 7: return CERT_TYPE;
          default: return null;
        }
    }
    
    public int getValue()
    {
      return value;
    }
  }
  
  public static abstract class Value implements Builder, Constructed
  {
  }
}
