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

import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

final class Extension implements Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  private final Type type;
  private final byte[] value;

  // Constructor.
  // -------------------------------------------------------------------------

  Extension(Type type, byte[] value)
  {
    if (type == null || value == null)
      {
        throw new NullPointerException();
      }
    this.type = type;
    this.value = value;
  }

  // Class method.
  // -------------------------------------------------------------------------

  static Extension read(InputStream in) throws IOException
  {
    Type t = Type.read(in);
    int len = (in.read() & 0xFF) << 8 | (in.read() & 0xFF);
    byte[] v = new byte[len];
    int count = 0;
    while (count < len)
      {
        int l = in.read(v, count, len - count);
        if (l == -1)
          {
            throw new EOFException("unexpected end of extension");
          }
        count += l;
      }
    return new Extension(t, v);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    out.write(type.getEncoded());
    out.write(value.length >>> 8 & 0xFF);
    out.write(value.length & 0xFF);
    out.write(value);
  }

  Type getType()
  {
    return type;
  }

  byte[] getValue()
  {
    return value;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    out.println("  type = " + type + ";");
    out.println("  value =");
    out.println(Util.hexDump(value, "    "));
    out.println("} Extension;");
    return str.toString();
  }

  // Inner class.
  // -------------------------------------------------------------------------

  static final class Type implements Enumerated
  {

    // Constants and fields.
    // -----------------------------------------------------------------------

    static final Type SERVER_NAME            = new Type(0);
    static final Type MAX_FRAGMENT_LENGTH    = new Type(1);
    static final Type CLIENT_CERTIFICATE_URL = new Type(2);
    static final Type TRUSTED_CA_KEYS        = new Type(3);
    static final Type TRUNCATED_HMAC         = new Type(4);
    static final Type STATUS_REQUEST         = new Type(5);
    static final Type SRP                    = new Type(6);
    static final Type CERT_TYPE              = new Type(7);

    private final int value;

    // Constructor.
    // -----------------------------------------------------------------------

    private Type(int value)
    {
      this.value = value;
    }

    // Class methods.
    // -----------------------------------------------------------------------

    static Type read(InputStream in) throws IOException
    {
      int i = in.read();
      if (i == -1)
        {
          throw new EOFException("unexpected end of input stream");
        }
      int value = (i & 0xFF) << 8;
      i = in.read();
      if (i == -1)
        {
          throw new EOFException("unexpected end of input stream");
        }
      value |= i & 0xFF;
      switch (value)
        {
        case 0: return SERVER_NAME;
        case 1: return MAX_FRAGMENT_LENGTH;
        case 2: return CLIENT_CERTIFICATE_URL;
        case 3: return TRUSTED_CA_KEYS;
        case 4: return TRUNCATED_HMAC;
        case 5: return STATUS_REQUEST;
        case 6: return SRP;
        case 7: return CERT_TYPE;
        default: return new Type(value);
        }
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public byte[] getEncoded()
    {
      return new byte[] {
        (byte) (value >>> 8 & 0xFF), (byte) (value & 0xFF)
      };
    }

    public int getValue()
    {
      return value;
    }

    public String toString()
    {
      switch (value)
        {
        case 0: return "server_name";
        case 1: return "max_fragment_length";
        case 2: return "client_certificate_url";
        case 3: return "trusted_ca_keys";
        case 4: return "truncated_hmac";
        case 5: return "status_request";
        case 6: return "srp";
        case 7: return "cert_type";
        default: return "unknown(" + value + ")";
        }
    }
  }
}
