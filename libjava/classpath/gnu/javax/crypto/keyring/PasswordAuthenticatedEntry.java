/* PasswordAuthenticatedEntry.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.util.Util;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;
import gnu.javax.crypto.mac.MacInputStream;
import gnu.javax.crypto.mac.MacOutputStream;
import gnu.javax.crypto.prng.IPBE;
import gnu.javax.crypto.prng.PRNGFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Iterator;
import java.util.HashMap;

/**
 * <p>An entry authenticated with a password-based MAC.</p>
 */
public final class PasswordAuthenticatedEntry extends MaskableEnvelopeEntry
    implements PasswordProtectedEntry, Registry
{

  // Constants and variables
  // -------------------------------------------------------------------------

  public static final int TYPE = 3;

  // Constructor(s)
  // -------------------------------------------------------------------------

  public PasswordAuthenticatedEntry(String mac, int maclen,
                                    Properties properties)
  {
    super(TYPE, properties);

    if (mac == null || mac.length() == 0)
      {
        throw new IllegalArgumentException("no MAC specified");
      }
    this.properties.put("mac", mac);
    this.properties.put("maclen", String.valueOf(maclen));
    setMasked(false);
  }

  private PasswordAuthenticatedEntry()
  {
    super(TYPE);
    setMasked(true);
  }

  // Class methods
  // -------------------------------------------------------------------------

  public static PasswordAuthenticatedEntry decode(DataInputStream in,
                                                  char[] password)
      throws IOException
  {
    PasswordAuthenticatedEntry entry = new PasswordAuthenticatedEntry();
    entry.properties.decode(in);
    IMac mac = entry.getMac(password);
    int len = in.readInt() - mac.macSize();
    MeteredInputStream min = new MeteredInputStream(in, len);
    MacInputStream macin = new MacInputStream(min, mac);
    DataInputStream in2 = new DataInputStream(macin);
    entry.setMasked(false);
    entry.decodeEnvelope(in2);
    byte[] macValue = new byte[mac.macSize()];
    in.readFully(macValue);
    if (!Arrays.equals(macValue, mac.digest()))
      {
        throw new MalformedKeyringException("MAC verification failed");
      }
    return entry;
  }

  public static PasswordAuthenticatedEntry decode(DataInputStream in)
      throws IOException
  {
    PasswordAuthenticatedEntry entry = new PasswordAuthenticatedEntry();
    entry.defaultDecode(in);
    if (!entry.properties.containsKey("mac"))
      {
        throw new MalformedKeyringException("no MAC");
      }
    if (!entry.properties.containsKey("maclen"))
      {
        throw new MalformedKeyringException("no MAC length");
      }
    if (!entry.properties.containsKey("salt"))
      {
        throw new MalformedKeyringException("no salt");
      }
    return entry;
  }

  // Instance methods
  // -------------------------------------------------------------------------

  public void verify(char[] password)
  {
    if (!isMasked() || payload == null)
      {
        return;
      }
    IMac m = null;
    try
      {
        m = getMac(password);
      }
    catch (Exception x)
      {
        throw new IllegalArgumentException(x.toString());
      }

    m.update(payload, 0, payload.length - m.macSize());
    byte[] macValue = new byte[m.macSize()];
    System.arraycopy(payload, payload.length - macValue.length, macValue, 0,
                     macValue.length);
    if (!Arrays.equals(macValue, m.digest()))
      {
        throw new IllegalArgumentException("MAC verification failed");
      }
    try
      {
        DataInputStream in = new DataInputStream(
                                                 new ByteArrayInputStream(
                                                                          payload,
                                                                          0,
                                                                          payload.length
                                                                              - m.macSize()));
        decodeEnvelope(in);
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException("malformed keyring fragment");
      }
    setMasked(false);
    payload = null;
  }

  public void authenticate(char[] password) throws IOException
  {
    if (isMasked())
      {
        throw new IllegalStateException("entry is masked");
      }
    byte[] salt = new byte[8];
    new SecureRandom ().nextBytes (salt);
    properties.put("salt", Util.toString(salt));
    IMac m = getMac(password);
    ByteArrayOutputStream bout = new ByteArrayOutputStream(1024);
    MacOutputStream macout = new MacOutputStream(bout, m);
    DataOutputStream out2 = new DataOutputStream(macout);
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Entry entry = (Entry) it.next();
        entry.encode(out2);
      }
    bout.write(m.digest());
    payload = bout.toByteArray();
  }

  public void encode(DataOutputStream out, char[] password) throws IOException
  {
    authenticate(password);
    encode(out);
  }

  protected void encodePayload(DataOutputStream out) throws IOException
  {
    if (payload == null)
      {
        throw new IllegalStateException("mac not computed");
      }
  }

  // Own methods.
  // ------------------------------------------------------------------------

  private IMac getMac(char[] password) throws MalformedKeyringException
  {
    if (!properties.containsKey("salt"))
      {
        throw new MalformedKeyringException("no salt");
      }
    byte[] salt = Util.toBytesFromString(properties.get("salt"));
    IMac mac = MacFactory.getInstance(properties.get("mac"));
    if (mac == null)
      {
        throw new MalformedKeyringException("no such mac: "
                                            + properties.get("mac"));
      }
    int keylen = mac.macSize();
    int maclen = 0;
    if (!properties.containsKey("maclen"))
      {
        throw new MalformedKeyringException("no MAC length");
      }
    try
      {
        maclen = Integer.parseInt(properties.get("maclen"));
      }
    catch (NumberFormatException nfe)
      {
        throw new MalformedKeyringException("bad MAC length");
      }

    HashMap pbAttr = new HashMap();
    pbAttr.put(IPBE.PASSWORD, password);
    pbAttr.put(IPBE.SALT, salt);
    pbAttr.put(IPBE.ITERATION_COUNT, ITERATION_COUNT);
    IRandom kdf = PRNGFactory.getInstance("PBKDF2-HMAC-SHA");
    kdf.init(pbAttr);

    byte[] dk = new byte[keylen];
    try
      {
        kdf.nextBytes(dk, 0, keylen);
      }
    catch (LimitReachedException shouldNotHappen)
      {
        throw new Error(shouldNotHappen.toString());
      }

    HashMap macAttr = new HashMap();
    macAttr.put(IMac.MAC_KEY_MATERIAL, dk);
    macAttr.put(IMac.TRUNCATED_SIZE, new Integer(maclen));
    try
      {
        mac.init(macAttr);
      }
    catch (InvalidKeyException shouldNotHappen)
      {
        throw new Error(shouldNotHappen.toString());
      }
    return mac;
  }
}
