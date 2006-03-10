/* Handshake.java -- SSL handshake message.
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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import java.security.PublicKey;

import java.util.ArrayList;
import java.util.Collections;

import javax.net.ssl.SSLProtocolException;

final class Handshake implements Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  private static final buffer BUF = new buffer();

  private final Type type;
  private final Body body;

  // Constructors.
  // -------------------------------------------------------------------------

  Handshake(Type type, Body body)
  {
    this.type = type;
    this.body = body;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static Handshake read(byte[] buffer) throws IOException
  {
    return read(new ByteArrayInputStream(buffer));
  }

  static Handshake read(byte[] buffer, CipherSuite suite, PublicKey key)
    throws IOException
  {
    return read(new ByteArrayInputStream(buffer), suite, key);
  }

  static Handshake read(InputStream in) throws IOException
  {
    return read(in, null, null);
  }

  static Handshake read(InputStream in, CipherSuite suite, PublicKey key)
    throws IOException
  {
    return read(in, suite, key, null);
  }

  static Handshake read(InputStream in, CertificateType certType)
    throws IOException
  {
    return read(in, null, null, certType);
  }

  static Handshake read(InputStream in, CipherSuite suite, PublicKey key,
                        CertificateType certType)
    throws IOException
  {
    Type type = Type.read(in);
    byte[] lenbuf = new byte[3];
    in.read(lenbuf);
    int len = (lenbuf[0] & 0xFF) << 16 | (lenbuf[1] & 0xFF) << 8
            | (lenbuf[2] & 0xFF);
    Body body = null;
    if (type == Type.HELLO_REQUEST)
      {
        body = null;
      }
    else if (type == Type.CLIENT_HELLO)
      {
        // Most likely a V2 hello. If the first byte is 0x30, and if this
        // is not a V2 client hello, then it is a V3 client hello with
        // at least 1.5 million cipher specs, which is unlikely.
        if (lenbuf[0] == 3 && (lenbuf[1] >= 0 && lenbuf[1] <= 2))
          {
            ProtocolVersion vers = null;
            switch (lenbuf[1])
              {
              case 0:
                vers = ProtocolVersion.SSL_3;
                break;
              case 1:
                vers = ProtocolVersion.TLS_1;
                break;
              case 2:
                vers = ProtocolVersion.TLS_1_1;
                break;
              }
            int specLen = (lenbuf[2] & 0xFF) << 8 | (in.read() & 0xFF);
            int idLen   = (in.read() & 0xFF) << 8 | (in.read() & 0xFF);
            int chalLen = (in.read() & 0xFF) << 8 | (in.read() & 0xFF);

            ArrayList suites = new ArrayList(specLen / 3);
            for (int i = 0; i < specLen; i += 3)
              {
                if (in.read() == 0)
                  {
                    suites.add(CipherSuite.read(in).resolve(vers));
                  }
                else
                  {
                    in.read();
                    in.read();
                  }
              }
            byte[] id = new byte[idLen];
            in.read(id);
            byte[] challenge = new byte[chalLen];
            in.read(challenge);
            if (challenge.length > 32)
              challenge = Util.trim(challenge, 32);
            else if (challenge.length < 32)
              {
                byte[] b = new byte[32];
                System.arraycopy(challenge, 0, b, b.length - challenge.length,
                                 challenge.length);
                challenge = b;
              }
            int time = (challenge[0] & 0xFF) << 24 | (challenge[1] & 0xFF) << 16
                     | (challenge[2] & 0xFF) <<  8 | (challenge[3] & 0xFF);
            Random rand = new Random(time, Util.trim(challenge, 4, 28));
            return new Handshake(Handshake.Type.CLIENT_HELLO,
              new ClientHello(vers, rand, id, suites,
                              Collections.singletonList(CompressionMethod.NULL)));
          }
        // Since hello messages may contain extensions, we read the whole
        // thing here.
        byte[] buf = new byte[len];
        int count = 0;
        while (count < len)
          {
            int l = in.read(buf, count, len - count);
            if (l == -1)
              {
                throw new EOFException("unexpected end of input stream");
              }
            count += l;
          }
        body = ClientHello.read(new ByteArrayInputStream(buf));
      }
    else if (type == Type.SERVER_HELLO)
      {
        byte[] buf = new byte[len];
        int count = 0;
        while (count < len)
          {
            int l = in.read(buf, count, len - count);
            if (l == -1)
              {
                throw new EOFException("unexpected end of input stream");
              }
            count += l;
          }
        body = ServerHello.read(new ByteArrayInputStream(buf));
      }
    else if (type == Type.CERTIFICATE)
      {
        body = Certificate.read(in, certType);
      }
    else if (type == Type.SERVER_KEY_EXCHANGE)
      {
        body = ServerKeyExchange.read(in, suite, key);
      }
    else if (type == Type.CERTIFICATE_REQUEST)
      {
        body = CertificateRequest.read(in);
      }
    else if (type == Type.CERTIFICATE_VERIFY)
      {
        body = (CertificateVerify) CertificateVerify.read(in, suite, key);
      }
    else if (type == Type.CLIENT_KEY_EXCHANGE)
      {
        body = ClientKeyExchange.read(in, suite, key);
      }
    else if (type == Type.SERVER_HELLO_DONE)
      {
        body = null;
      }
    else if (type == Type.FINISHED)
      {
        body = Finished.read(in, suite);
      }
    else
      {
        throw new SSLProtocolException("unknown HandshakeType: " +
                                       type.getValue());
      }

    return new Handshake(type, body);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out)
  {
    throw new UnsupportedOperationException();
  }

  public int write(OutputStream out, ProtocolVersion version)
    throws IOException
  {
    out.write(type.getValue());
    if (body == null)
      {
        out.write(0);
        out.write(0);
        out.write(0);
        return 4;
      }
    else
      {
        ByteArrayOutputStream bout = BUF.getBuffer();
        bout.reset();
        if (body instanceof ServerKeyExchange)
          {
            ((ServerKeyExchange) body).write(bout, version);
          }
        else if (body instanceof ClientKeyExchange)
          {
            ((ClientKeyExchange) body).write(bout, version);
          }
        else if (body instanceof CertificateVerify)
          {
            ((CertificateVerify) body).write(bout, version);
          }
        else
          {
            body.write(bout);
          }
        out.write(bout.size() >>> 16 & 0xFF);
        out.write(bout.size() >>>  8 & 0xFF);
        out.write(bout.size() & 0xFF);
        bout.writeTo(out);
        return 4 + bout.size();
      }
  }

  Type getType()
  {
    return type;
  }

  Body getBody()
  {
    return body;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    String nl = System.getProperty("line.separator");
    StringBuffer buf = new StringBuffer();
    out.println("struct {");
    out.println("  type = " + type + ";");
    if (body != null)
      {
        BufferedReader r = new BufferedReader(new StringReader(body.toString()));
        String s;
        try
          {
            while ((s = r.readLine()) != null)
              {
                out.print("  ");
                out.println(s);
              }
          }
        catch (IOException ignored)
          {
          }
      }
    out.println("} Handshake;");
    return str.toString();
  }

  // Inner class.
  // -------------------------------------------------------------------------

  static interface Body extends Constructed
  {
  }

  static class Type implements Enumerated
  {

    // Constants and fields.
    // -----------------------------------------------------------------------

    public static final Type
      HELLO_REQUEST       = new Type( 0), CLIENT_HELLO        = new Type( 1),
      SERVER_HELLO        = new Type( 2), CERTIFICATE         = new Type(11),
      SERVER_KEY_EXCHANGE = new Type(12), CERTIFICATE_REQUEST = new Type(13),
      SERVER_HELLO_DONE   = new Type(14), CERTIFICATE_VERIFY  = new Type(15),
      CLIENT_KEY_EXCHANGE = new Type(16), FINISHED            = new Type(20),
      CERTIFICATE_URL     = new Type(21), CERTIFICATE_STATUS  = new Type(22);

    private final int value;

    // Constructor.
    // -----------------------------------------------------------------------

    private Type(int value)
    {
      this.value = value;
    }

    // Class methods.
    // -----------------------------------------------------------------------

    public static Type read(InputStream in) throws IOException
    {
      int i = in.read();
      if (i == -1)
        {
          throw new EOFException("unexpected end of input stream");
        }
      switch (i & 0xFF)
        {
        case  0: return HELLO_REQUEST;
        case  1: return CLIENT_HELLO;
        case  2: return SERVER_HELLO;
        case 11: return CERTIFICATE;
        case 12: return SERVER_KEY_EXCHANGE;
        case 13: return CERTIFICATE_REQUEST;
        case 14: return SERVER_HELLO_DONE;
        case 15: return CERTIFICATE_VERIFY;
        case 16: return CLIENT_KEY_EXCHANGE;
        case 20: return FINISHED;
        case 21: return CERTIFICATE_URL;
        case 22: return CERTIFICATE_STATUS;
        default: return new Type(i);
        }
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public byte[] getEncoded()
    {
      return new byte[] { (byte) value };
    }

    public int getValue()
    {
      return value;
    }

    public String toString()
    {
      switch (value)
        {
        case  0: return "hello_request";
        case  1: return "client_hello";
        case  2: return "server_hello";
        case 11: return "certificate";
        case 12: return "server_key_exchange";
        case 13: return "certificate_request";
        case 14: return "server_hello_done";
        case 15: return "certificate_verify";
        case 16: return "client_key_exchange";
        case 20: return "finished";
        case 21: return "certificate_url";
        case 22: return "certificate_status";
        default: return "unknown(" + value + ")";
        }
    }
  }

  private static class buffer extends ThreadLocal
  {
    static final int SIZE = 2048;

    protected Object initialValue()
    {
      return new ByteArrayOutputStream(SIZE);
    }

    ByteArrayOutputStream getBuffer()
    {
      return (ByteArrayOutputStream) get();
    }
  }
}
