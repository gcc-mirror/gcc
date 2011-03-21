/* PrivateKeyEntry.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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

import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.key.KeyPairCodecFactory;
import gnu.java.security.key.dss.DSSPrivateKey;
import gnu.java.security.key.rsa.GnuRSAPrivateKey;
import gnu.javax.crypto.key.GnuSecretKey;
import gnu.javax.crypto.key.dh.GnuDHPrivateKey;

import java.io.DataInputStream;
import java.io.IOException;
import java.security.Key;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Date;

/**
 * An immutable class representing a private or secret key entry.
 */
public final class PrivateKeyEntry
    extends PrimitiveEntry
{
  public static final int TYPE = 7;
  /** The key. */
  private Key key;

  /**
   * Creates a new key entry.
   *
   * @param key The key.
   * @param creationDate The entry creation date.
   * @param properties The entry properties.
   * @throws IllegalArgumentException If any parameter is null.
   */
  public PrivateKeyEntry(Key key, Date creationDate, Properties properties)
  {
    super(TYPE, creationDate, properties);
    if (key == null)
      throw new IllegalArgumentException("no private key");
    if (! (key instanceof PrivateKey) && ! (key instanceof GnuSecretKey))
      throw new IllegalArgumentException("not a private or secret key");
    this.key = key;
  }

  private PrivateKeyEntry()
  {
    super(TYPE);
  }

  public static PrivateKeyEntry decode(DataInputStream in) throws IOException
  {
    PrivateKeyEntry entry = new PrivateKeyEntry();
    entry.defaultDecode(in);
    String type = entry.properties.get("type");
    if (type == null)
      throw new MalformedKeyringException("no key type");
    if (type.equalsIgnoreCase("RAW-DSS"))
      {
        IKeyPairCodec coder = KeyPairCodecFactory.getInstance("dss");
        entry.key = coder.decodePrivateKey(entry.payload);
      }
    else if (type.equalsIgnoreCase("RAW-RSA"))
      {
        IKeyPairCodec coder = KeyPairCodecFactory.getInstance("rsa");
        entry.key = coder.decodePrivateKey(entry.payload);
      }
    else if (type.equalsIgnoreCase("RAW-DH"))
      {
        IKeyPairCodec coder = KeyPairCodecFactory.getInstance("dh");
        entry.key = coder.decodePrivateKey(entry.payload);
      }
    else if (type.equalsIgnoreCase("RAW"))
      entry.key = new GnuSecretKey(entry.payload, null);
    else if (type.equalsIgnoreCase("PKCS8"))
      {
        try
          {
            KeyFactory kf = KeyFactory.getInstance("RSA");
            PKCS8EncodedKeySpec ks = new PKCS8EncodedKeySpec(entry.payload);
            entry.key = kf.generatePrivate(ks);
          }
        catch (Exception ignored)
          {
          }
        if (entry.key == null)
          {
            try
              {
                KeyFactory kf = KeyFactory.getInstance("DSA");
                PKCS8EncodedKeySpec ks = new PKCS8EncodedKeySpec(entry.payload);
                entry.key = kf.generatePrivate(ks);
              }
            catch (Exception ignored)
              {
              }
            if (entry.key == null)
              throw new MalformedKeyringException("could not decode PKCS#8 key");
          }
      }
    else
      throw new MalformedKeyringException("unsupported key type " + type);
    return entry;
  }

  /**
   * Returns this entry's key.
   *
   * @return The key.
   */
  public Key getKey()
  {
    return key;
  }

  protected void encodePayload() throws IOException
  {
    String format = key.getFormat();
    if (key instanceof DSSPrivateKey)
      {
        properties.put("type", "RAW-DSS");
        IKeyPairCodec coder = KeyPairCodecFactory.getInstance("dss");
        payload = coder.encodePrivateKey((PrivateKey) key);
      }
    else if (key instanceof GnuRSAPrivateKey)
      {
        properties.put("type", "RAW-RSA");
        IKeyPairCodec coder = KeyPairCodecFactory.getInstance("rsa");
        payload = coder.encodePrivateKey((PrivateKey) key);
      }
    else if (key instanceof GnuDHPrivateKey)
      {
        properties.put("type", "RAW-DH");
        IKeyPairCodec coder = KeyPairCodecFactory.getInstance("dh");
        payload = coder.encodePrivateKey((PrivateKey) key);
      }
    else if (key instanceof GnuSecretKey)
      {
        properties.put("type", "RAW");
        payload = key.getEncoded();
      }
    else if (format != null && format.equals("PKCS#8"))
      {
        properties.put("type", "PKCS8");
        payload = key.getEncoded();
      }
    else
      throw new IllegalArgumentException("unsupported private key");
  }

  public String toString()
  {
    return "PrivateKeyEntry{key="
           + (key == null ? "-" : key.getClass().getName()) + "}";
  }
}
