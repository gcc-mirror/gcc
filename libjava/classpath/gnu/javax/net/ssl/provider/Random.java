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

import java.io.PrintWriter;
import java.io.StringWriter;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * An SSL nonce.
 *
 * <pre>
struct
{
  uint32 gmt_unix_time;
  opaque random_bytes[28];
} Random;
 */
public class Random implements Builder, Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  static final int RANDOM_LENGTH = 28;

  private final ByteBuffer buffer;

  // Constructors.
  // -------------------------------------------------------------------------

  public Random (final ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
  }

  public Random copy()
  {
    ByteBuffer buffer = ByteBuffer.allocate(32);
    buffer.put((ByteBuffer) this.buffer.duplicate().position(0));
    return new Random(buffer);
  }

  public int length()
  {
    return RANDOM_LENGTH + 4;
  }

  public ByteBuffer buffer()
  {
    return ((ByteBuffer) buffer.duplicate().position(0).limit(length())).slice();
  }

  public int gmtUnixTime ()
  {
    return buffer.getInt(0);
  }

  public byte[] randomBytes()
  {
    byte[] buf = new byte[28];
    buffer.position (4);
    buffer.get (buf);
    return buf;
  }

  public void setGmtUnixTime (final int gmtUnixTime)
  {
    buffer.putInt (0, gmtUnixTime);
  }

  public void setRandomBytes (final byte[] randomBytes)
  {
    setRandomBytes (randomBytes, 0);
  }

  public void setRandomBytes (final byte[] randomBytes, final int offset)
  {
    if (randomBytes.length - offset < RANDOM_LENGTH)
      throw new IllegalArgumentException ("random value too short");
    buffer.position (4);
    buffer.put (randomBytes, offset, RANDOM_LENGTH);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null)
      out.print (prefix);
    out.println("struct {");
    if (prefix != null)
      out.print (prefix);
    out.print ("  gmt_unix_time: ");
    out.print (gmtUnixTime ());
    out.println (";");
    if (prefix != null)
      out.print (prefix);
    out.print ("  random_bytes:  ");
    out.print (Util.toHexString (randomBytes (), ':'));
    out.println (";");
    if (prefix != null)
      out.print (prefix);
    out.print ("} Random;");
    return str.toString();
  }

  public String toString ()
  {
    return toString (null);
  }
}
