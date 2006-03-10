/* ServerHello.java -- SSL ServerHello message.
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
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.net.ssl.SSLProtocolException;

class ServerHello implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  private final ProtocolVersion version;
  private final Random random;
  private final byte[] sessionId;
  private final CipherSuite suite;
  private final CompressionMethod comp;
  private final List extensions;

  // Constructor.
  // -------------------------------------------------------------------------

  ServerHello(ProtocolVersion version, Random random,
              byte[] sessionId, CipherSuite suite,
              CompressionMethod comp)
  {
    this(version, random, sessionId, suite, comp, null);
  }

  ServerHello(ProtocolVersion version, Random random,
              byte[] sessionId, CipherSuite suite,
              CompressionMethod comp, List extensions)
  {
    this.version = version;
    this.random = random;
    this.sessionId = sessionId;
    this.suite = suite;
    this.comp = comp;
    this.extensions = extensions;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static ServerHello read(InputStream in) throws IOException
  {
    ProtocolVersion vers = ProtocolVersion.read(in);
    Random rand = Random.read(in);
    byte[] id = new byte[in.read() & 0xFF];
    in.read(id);
    CipherSuite suite = CipherSuite.read(in).resolve(vers);
    CompressionMethod comp = CompressionMethod.read(in);
    List ext = null;
    if (in.available() > 0)
      {
        ext = new LinkedList();
        int len = (in.read() >>> 8 & 0xFF) | (in.read() & 0xFF);
        int count = 0;
        while (count < len)
          {
            Extension e = Extension.read(in);
            ext.add(e);
            count += e.getValue().length + 4;
          }
      }
    return new ServerHello(vers, rand, id, suite, comp, ext);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    version.write(out);
    random.write(out);
    out.write(sessionId.length);
    out.write(sessionId);
    suite.write(out);
    out.write(comp.getValue());
    if (extensions != null)
      {
        ByteArrayOutputStream out2 = new ByteArrayOutputStream();
        for (Iterator i = extensions.iterator(); i.hasNext(); )
          ((Extension) i.next()).write(out2);
        out.write(out2.size() >>> 8 & 0xFF);
        out.write(out2.size() & 0xFF);
        out2.writeTo(out);
      }
  }

  ProtocolVersion getVersion()
  {
    return version;
  }

  Random getRandom()
  {
    return random;
  }

  byte[] getSessionId()
  {
    return (byte[]) sessionId.clone();
  }

  CipherSuite getCipherSuite()
  {
    return suite;
  }

  CompressionMethod getCompressionMethod()
  {
    return comp;
  }

  List getExtensions()
  {
    return extensions;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    out.println("  version = " + version + ";");
    BufferedReader r = new BufferedReader(new StringReader(random.toString()));
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
    out.println("  sessionId = " + Util.toHexString(sessionId, ':') + ";");
    out.println("  cipherSuite = " + suite + ";");
    out.println("  compressionMethod = " + comp + ";");
    if (extensions != null)
      {
        out.println("  extensions = {");
        for (Iterator i = extensions.iterator(); i.hasNext(); )
          {
            r = new BufferedReader(new StringReader(i.next().toString()));
            try
              {
                while ((s = r.readLine()) != null)
                  {
                    out.print("    ");
                    out.println(s);
                  }
              }
            catch (IOException ignored)
              {
              }
          }
        out.println("  };");
      }
    out.println("} ServerHello;");
    return str.toString();
  }
}
