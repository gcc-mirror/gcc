/* Extensions.java -- various static extension utilities.
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.security.auth.x500.X500Principal;

import gnu.java.security.x509.X500DistinguishedName;

final class Extensions
{

  // Constants.
  // -------------------------------------------------------------------------

  private static final Integer _512  = new Integer(512),
    _1024 = new Integer(1024), _2048 = new Integer(2048),
    _4096 = new Integer(4096);

  // Class methods only.
  private Extensions() { }

  // Class methods.
  // -------------------------------------------------------------------------

  static List getServerName(Extension ex)
  {
    LinkedList l = new LinkedList();
    byte[] buf = ex.getValue();
    int pos = 0;
    try
      {
        while (pos < buf.length)
          {
            if (buf[pos++] != 0)
              break;
            int len = (buf[pos++] & 0xFF) << 8;
            len |= buf[pos++] & 0xFF;
            l.add(new String(buf, pos, len, "UTF-8"));
            pos += len;
          }
      }
    catch (Exception x)
      {
      }
    return Collections.unmodifiableList(l);
  }

  static List getClientCertTypes(Extension ex) throws IOException
  {
    List l = new LinkedList();
    ByteArrayInputStream in = new ByteArrayInputStream(ex.getValue());
    final int len = in.read() & 0xFF;
    for (int i = 0; i < len; i++)
      {
        l.add(CertificateType.read(in));
      }
    return Collections.unmodifiableList(l);
  }

  static CertificateType getServerCertType(Extension ex) throws IOException
  {
    return CertificateType.read(new ByteArrayInputStream(ex.getValue()));
  }

  static Integer getMaxFragmentLength(Extension ex)
  {
    switch (ex.getValue()[0] & 0xFF)
      {
      case 1: return _512;
      case 2: return _1024;
      case 3: return _2048;
      case 4: return _4096;
      }
    throw new IllegalArgumentException();
  }

  static Object[] getTrustedCA(Extension ex)
  {
    byte[] buf = ex.getValue();
    int type = buf[0] & 0xFF;
    try
      {
        switch (type)
          {
          case 0:
            return new Object[] { new Integer(type), null };
          case 1:
          case 3:
            return new Object[] { new Integer(type),
                                  Util.trim(buf, 1, 20) };
          case 2:
            return new Object[] { new Integer(type),
                                  new X500Principal(Util.trim(buf, 1, 20)) };
          }
      }
    catch (Exception x)
      {
      }
    throw new IllegalArgumentException();
  }

  static String getSRPUsername(Extension ex)
  {
    int len = ex.getValue()[0] & 0xFF;
    if (len > ex.getValue().length - 1)
      throw new IllegalArgumentException();
    try
      {
        return new String(ex.getValue(), 1, len, "UTF-8");
      }
    catch (UnsupportedEncodingException uee)
      {
        throw new Error(uee.toString());
      }
  }
}
