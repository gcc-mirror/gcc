/* ProtocolVersion.java -- An SSL version number.
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

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

final class ProtocolVersion implements Comparable, Constructed
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  static final ProtocolVersion SSL_3 = new ProtocolVersion(3, 0);
  static final ProtocolVersion TLS_1 = new ProtocolVersion(3, 1);
  static final ProtocolVersion TLS_1_1 = new ProtocolVersion(3, 2);

  private final int major;
  private final int minor;

  // Constructor.
  // -------------------------------------------------------------------------

  private ProtocolVersion(int major, int minor)
  {
    this.major = major;
    this.minor = minor;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static ProtocolVersion read(InputStream in) throws IOException
  {
    int major = in.read() & 0xFF;
    int minor = in.read() & 0xFF;
    return getInstance(major, minor);
  }

  static ProtocolVersion getInstance(int major, int minor)
  {
    if (major == 3)
      {
        switch (minor)
          {
          case 0: return SSL_3;
          case 1: return TLS_1;
          case 2: return TLS_1_1;
          }
      }
    return new ProtocolVersion(major, minor);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    out.write(major);
    out.write(minor);
  }

  byte[] getEncoded()
  {
    return new byte[] {
      (byte) major, (byte) minor
    };
  }

  int getMajor()
  {
    return major;
  }

  int getMinor()
  {
    return minor;
  }

  public boolean equals(Object o)
  {
    if (o == null || !(o instanceof ProtocolVersion))
      {
        return false;
      }
    return ((ProtocolVersion) o).major == this.major
        && ((ProtocolVersion) o).minor == this.minor;
  }

  public int hashCode()
  {
    return major << 8 | minor;
  }

  public int compareTo(Object o)
  {
    if (o == null || !(o instanceof ProtocolVersion))
      {
        return 1;
      }
    if (this.equals(o))
      {
        return 0;
      }
    if (major > ((ProtocolVersion) o).major)
      {
        return 1;
      }
    else if (major < ((ProtocolVersion) o).major)
      {
        return -1;
      }
    if (minor > ((ProtocolVersion) o).minor)
      {
        return 1;
      }
    else if (minor < ((ProtocolVersion) o).minor)
      {
        return -1;
      }
    return 0;
  }

  public String toString()
  {
    if (this == SSL_3)
      {
        return "SSLv3";
      }
    else if (this == TLS_1)
      {
        return "TLSv1";
      }
    else if (this == TLS_1_1)
      {
        return "TLSv1.1";
      }
    else
      {
        return "Unsupported; major=" + major + " minor=" + minor;
      }
  }
}
