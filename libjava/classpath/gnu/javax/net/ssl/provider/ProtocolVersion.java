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

public final class ProtocolVersion
  implements Comparable<ProtocolVersion>, Constructed
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  public static final ProtocolVersion SSL_3 = new ProtocolVersion(3, 0);
  public static final ProtocolVersion TLS_1 = new ProtocolVersion(3, 1);
  public static final ProtocolVersion TLS_1_1 = new ProtocolVersion(3, 2);

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

  public static ProtocolVersion read(InputStream in) throws IOException
  {
    int major = in.read() & 0xFF;
    int minor = in.read() & 0xFF;
    return getInstance(major, minor);
  }

  public static ProtocolVersion forName (final String name)
  {
    if (name.equalsIgnoreCase ("SSLv3"))
      return SSL_3;
    if (name.equalsIgnoreCase ("TLSv1"))
      return TLS_1;
    if (name.equalsIgnoreCase("TLSv1.1"))
      return TLS_1_1;
    throw new IllegalArgumentException ("unknown protocol name: " + name);
  }

  public static ProtocolVersion getInstance(final int major, final int minor)
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

  public static ProtocolVersion getInstance (final short raw_value)
  {
    int major = raw_value >>> 8 & 0xFF;
    int minor = raw_value & 0xFF;
    return getInstance (major, minor);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length ()
  {
    return 2;
  }

  public byte[] getEncoded()
  {
    return new byte[] {
      (byte) major, (byte) minor
    };
  }

  public int major()
  {
    return major;
  }

  public int minor()
  {
    return minor;
  }

  public int rawValue ()
  {
    return (major << 8) | minor;
  }

  public boolean equals(Object o)
  {
    if (!(o instanceof ProtocolVersion))
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

  public int compareTo(ProtocolVersion that)
  {
    if (major > that.major)
      {
        return 1;
      }
    else if (major < that.major)
      {
        return -1;
      }

    if (minor > that.minor)
      {
        return 1;
      }
    else if (minor < that.minor)
      {
        return -1;
      }
    return 0;
  }

  public String toString (String prefix)
  {
    return toString ();
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
