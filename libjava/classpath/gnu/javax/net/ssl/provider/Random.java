/* Random.java -- SSL Random structure.
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

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

class Random implements Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  private final int gmtUnixTime;
  private final byte[] randomBytes;

  // Constructors.
  // -------------------------------------------------------------------------

  Random(int gmtUnixTime, byte[] randomBytes)
  {
    this.gmtUnixTime = gmtUnixTime;
    this.randomBytes = (byte[]) randomBytes.clone();
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static Random read(InputStream in) throws IOException
  {
    int time = (in.read() & 0xFF) << 24 | (in.read() & 0xFF) << 16
             | (in.read() & 0xFF) <<  8 | (in.read() & 0xFF);
    byte[] buf = new byte[28];
    in.read(buf);
    return new Random(time, buf);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    out.write((gmtUnixTime >>> 24) & 0xFF);
    out.write((gmtUnixTime >>> 16) & 0xFF);
    out.write((gmtUnixTime >>>  8) & 0xFF);
    out.write(gmtUnixTime & 0xFF);
    out.write(randomBytes);
  }

  byte[] getEncoded()
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream(32);
    try
      {
        write(bout);
      }
    catch (IOException cantHappen)
      {
        throw new Error(cantHappen.toString());
      }
    return bout.toByteArray();
  }

  int getTime()
  {
    return gmtUnixTime;
  }

  byte[] getRandomBytes()
  {
    return randomBytes;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    out.println("  gmt_unix_time = " + gmtUnixTime + ";");
    out.println("  random_bytes = " + Util.toHexString(randomBytes, ':') + ";");
    out.println("} Random;");
    return str.toString();
  }
}
