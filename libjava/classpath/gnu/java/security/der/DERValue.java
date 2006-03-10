/* DERValue.java -- a value read or written to a DER encoding.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.java.security.der;

import gnu.java.security.x509.Util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class DERValue implements DER
{

  // Fields.
  // ------------------------------------------------------------------------

  private final int tagClass;
  private final boolean constructed;
  private final int tag;
  private int length;
  private final Object value;
  private byte[] encoded;

  // Constructor.
  // ------------------------------------------------------------------------

  public DERValue(int tag, int length, Object value, byte[] encoded)
  {
    tagClass = tag & 0xC0;
    this.tag = tag & 0x1F;
    constructed = (tag & CONSTRUCTED) == CONSTRUCTED;
    this.length = length;
    this.value = value;
    if (encoded != null)
      this.encoded = (byte[]) encoded.clone();
  }

  public DERValue(int tag, Object value)
  {
    this(tag, 0, value, null);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public int getExternalTag()
  {
    return tagClass | tag | (constructed ? 0x20 : 0x00);
  }

  public int getTag()
  {
    return tag;
  }

  public int getTagClass()
  {
    return tagClass;
  }

  public boolean isConstructed()
  {
    return constructed;
  }

  public int getLength()
  {
    if (encoded == null)
      {
        try
          {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            length = DERWriter.write(out, this);
            encoded = out.toByteArray();
          }
        catch (IOException ioe)
          {
            IllegalArgumentException iae = new IllegalArgumentException ();
            iae.initCause (ioe);
            throw iae;
          }
      }
    return length;
  }

  public Object getValue()
  {
    return value;
  }

  public Object getValueAs (final int derType) throws IOException
  {
    byte[] encoded = getEncoded ();
    encoded[0] = (byte) derType;
    return DERReader.read (encoded).getValue ();
  }

  public byte[] getEncoded()
  {
    if (encoded == null)
      {
        try
          {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            length = DERWriter.write(out, this);
            encoded = out.toByteArray();
          }
        catch (IOException ioe)
          {
            IllegalArgumentException iae = new IllegalArgumentException ();
            iae.initCause (ioe);
            throw iae;
          }
      }
    return (byte[]) encoded.clone();
  }

  public int getEncodedLength()
  {
    if (encoded == null)
      {
        try
          {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            length = DERWriter.write(out, this);
            encoded = out.toByteArray();
          }
        catch (IOException ioe)
          {
            IllegalArgumentException iae = new IllegalArgumentException ();
            iae.initCause (ioe);
            throw iae;
          }
      }
    return encoded.length;
  }

  public String toString()
  {
    String start = "DERValue ( [";
    if (tagClass == DER.UNIVERSAL) 
      start = start + "UNIVERSAL ";
    else if (tagClass == DER.PRIVATE) 
      start = start + "PRIVATE ";
    else if (tagClass == DER.APPLICATION) 
      start = start + "APPLICATION ";
    start = start + tag + "] constructed=" + constructed + ", value=";
    if (constructed)
     start = start + "\n" + Util.hexDump(getEncoded(), "\t");
    else
     start = start + value;
    return start + " )";
  }
}
