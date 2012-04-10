/* Entry.java --
   Copyright (C) 2003, 2006, 2010 Free Software Foundation, Inc.

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


package gnu.javax.crypto.keyring;

import gnu.java.security.Configuration;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * An immutable class representing a single entry in a keyring.
 */
public abstract class Entry
{
  private static final Logger log = Configuration.DEBUG ?
                        Logger.getLogger(Entry.class.getName()) : null;
  private static final String[] TYPES = new String[] {
      "Encrypted",
      "PasswordEncrypted",
      "Authenticated",
      "PasswordAuthenticated",
      "Compressed",
      "Certificate",
      "PublicKey",
      "PrivateKey",
      "CertPath",
      "BinaryData" };
  /** This entry's type identifier. */
  protected int type;
  /** This entry's property set. */
  protected Properties properties;
  /** This entry's payload. */
  protected byte[] payload;

  /**
   * Creates a new Entry.
   *
   * @param type This entry's type.
   * @param properties This entry's properties.
   * @throws IllegalArgumentException If the properties argument is null, or if
   *           the type is out of range.
   */
  protected Entry(int type, Properties properties)
  {
    if (type < 0 || type > 255)
      throw new IllegalArgumentException("invalid packet type");
    if (properties == null)
      throw new IllegalArgumentException("no properties");
    this.type = type;
    this.properties = (Properties) properties.clone();
  }

  /**
   * Constructor for use by subclasses.
   */
  protected Entry(final int type)
  {
    if (type < 0 || type > 255)
      throw new IllegalArgumentException("invalid packet type");
    this.type = type;
    properties = new Properties();
  }

  /**
   * Returns this entry's properties object. The properties are cloned before
   * being returned.
   *
   * @return The properties.
   */
  public Properties getProperties()
  {
    return (Properties) properties.clone();
  }

  /**
   * Returns this entry's payload data, or null if
   */
  public byte[] getPayload()
  {
    if (payload == null)
      return null;
    return (byte[]) payload.clone();
  }

  /**
   * This method is called when this entry needs to be written to an output
   * stream.
   *
   * @param out The stream to write to.
   * @throws IOException If an I/O exception occurs.
   */
  public void encode(DataOutputStream out) throws IOException
  {
    if (payload == null)
      encodePayload();
    if (out == null)
      return;
    out.write(type);
    properties.encode(out);
    out.writeInt(payload.length);
    out.write(payload);
  }

  public String toString()
  {
    return new StringBuilder("Entry{")
        .append("type=").append(TYPES[type])
        .append(", properties=").append(properties)
        .append(", payload=")
        .append(payload == null ? "-" : "byte[" + payload.length + "]")
        .append( "}")
        .toString();
  }

  /**
   * Generic decoding method, which simply decodes the properties field
   * and reads the payload field.
   *
   * @param in The input data stream.
   * @throws IOException If an I/O error occurs.
   */
  protected void defaultDecode(DataInputStream in) throws IOException
  {
    properties = new Properties();
    properties.decode(in);
    int len = in.readInt();
    if (len < 0)
      throw new IOException("corrupt length");
    if (Configuration.DEBUG)
      log.fine("About to instantiate new payload byte array for " + this);
    payload = new byte[len];
    in.readFully(payload);
  }

  /**
   * This method is called of subclasses when the payload data needs to be
   * created.
   *
   * @throws IOException If an encoding error occurs.
   */
  protected abstract void encodePayload() throws IOException;
}
