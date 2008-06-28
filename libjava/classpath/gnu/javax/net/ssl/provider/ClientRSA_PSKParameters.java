/* ClientRSA_PSKParameters.java -- 
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
exception statement from your version. */


package gnu.javax.net.ssl.provider;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class ClientRSA_PSKParameters extends ExchangeKeys implements Builder, Constructed
{
  public ClientRSA_PSKParameters(ByteBuffer buffer)
  {
    super(buffer);
  }

  public ClientRSA_PSKParameters(String identity, ByteBuffer epms)
  {
    super(null);
    Charset utf8 = Charset.forName("UTF-8");
    ByteBuffer idBuf = utf8.encode(identity);
    buffer = ByteBuffer.allocate(2 + idBuf.remaining() + epms.remaining());
    buffer.putShort((short) idBuf.remaining());
    buffer.put(idBuf);
    buffer.put(epms);
    buffer.rewind();
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.Builder#buffer()
   */
  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().rewind().limit(length());
  }
  
  public String identity()
  {
    Charset utf8 = Charset.forName("UTF-8");
    return utf8.decode((ByteBuffer) buffer.duplicate().position(2).limit
                       (identityLength())).toString();
  }
  
  private int identityLength()
  {
    return (buffer.getShort(0) & 0xFFFF) + 2;
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.Constructed#length()
   */
  public int length()
  {
    return identityLength() + secret().length();
  }
  
  public EncryptedPreMasterSecret secret()
  {
    return new EncryptedPreMasterSecret
      (((ByteBuffer) buffer.duplicate().position(identityLength())
        .limit(buffer.capacity())).slice(), ProtocolVersion.TLS_1);
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.Constructed#toString(java.lang.String)
   */
  public String toString(String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null) out.print(prefix);
    out.println("struct {");
    if (prefix != null) out.print(prefix);
    out.print("  identity = ");
    out.print(identity());
    if (prefix != null) out.print(prefix);
    out.println("  encrypted_pre_master_secret =");
    out.println(secret().toString(prefix != null ? prefix + "    " : "    "));
    if (prefix != null) out.print(prefix);
    out.print("} ClientRSA_PSKParameters;");
    return str.toString();
  }
}
