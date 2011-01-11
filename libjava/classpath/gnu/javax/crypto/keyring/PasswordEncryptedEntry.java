/* PasswordEncryptedEntry.java --
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

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.util.PRNG;
import gnu.java.security.util.Util;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.PadFactory;
import gnu.javax.crypto.pad.WrongPaddingException;
import gnu.javax.crypto.prng.IPBE;
import gnu.javax.crypto.prng.PRNGFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;

/**
 * An envelope that is encrypted with a password-derived key.
 */
public class PasswordEncryptedEntry
    extends MaskableEnvelopeEntry
    implements PasswordProtectedEntry, Registry
{
  private static final Logger log = Logger.getLogger(PasswordEncryptedEntry.class.getName());
  public static final int TYPE = 1;

  public PasswordEncryptedEntry(String cipher, String mode, int keylen,
                                Properties properties)
  {
    super(TYPE, properties);
    if ((cipher == null || cipher.length() == 0)
        || (mode == null || mode.length() == 0))
      throw new IllegalArgumentException("cipher nor mode can be empty");
    this.properties.put("cipher", cipher);
    this.properties.put("mode", mode);
    this.properties.put("keylen", String.valueOf(keylen));
    setMasked(false);
  }

  private PasswordEncryptedEntry()
  {
    super(TYPE);
    setMasked(true);
  }

  public static PasswordEncryptedEntry decode(DataInputStream in,
                                              char[] password)
      throws IOException
  {
    PasswordEncryptedEntry entry = decode(in);
    try
      {
        entry.decrypt(password);
      }
    catch (WrongPaddingException wpe)
      {
        throw new MalformedKeyringException("wrong padding in decrypted data");
      }
    return entry;
  }

  public static PasswordEncryptedEntry decode(DataInputStream in)
      throws IOException
  {
    PasswordEncryptedEntry entry = new PasswordEncryptedEntry();
    entry.defaultDecode(in);
    return entry;
  }

  public void decrypt(char[] password) throws IllegalArgumentException,
      WrongPaddingException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "decrypt");
    if (isMasked() && payload != null)
      {
        long tt = -System.currentTimeMillis();
        IMode mode = getMode(password, IMode.DECRYPTION);
        IPad padding = PadFactory.getInstance("PKCS7");
        padding.init(mode.currentBlockSize());
        byte[] buf = new byte[payload.length];
        int count = 0;
        while (count + mode.currentBlockSize() <= payload.length)
          {
            mode.update(payload, count, buf, count);
            count += mode.currentBlockSize();
          }
        int padlen = padding.unpad(buf, 0, buf.length);
        setMasked(false);
        int len = buf.length - padlen;
        ByteArrayInputStream baos = new ByteArrayInputStream(buf, 0, len);
        DataInputStream in = new DataInputStream(baos);
        try
          {
            decodeEnvelope(in);
          }
        catch (IOException ioe)
          {
            throw new IllegalArgumentException("decryption failed");
          }
        tt += System.currentTimeMillis();
        log.fine("Decrypted in " + tt + "ms.");
      }
    else if (Configuration.DEBUG)
      log.fine("Skip decryption; " + (isMasked() ? "null payload" : "unmasked"));
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "decrypt");
  }

  public void encrypt(char[] password) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "encrypt", String.valueOf(password));
    long tt = -System.currentTimeMillis();
    long t1 = -System.currentTimeMillis();
    byte[] salt = new byte[8];
    PRNG.getInstance().nextBytes(salt);
    t1 += System.currentTimeMillis();
    if (Configuration.DEBUG)
      log.fine("-- Generated salt in " + t1 + "ms.");
    properties.put("salt", Util.toString(salt));
    IMode mode = getMode(password, IMode.ENCRYPTION);
    IPad pad = PadFactory.getInstance("PKCS7");
    pad.init(mode.currentBlockSize());
    ByteArrayOutputStream bout = new ByteArrayOutputStream(1024);
    DataOutputStream out2 = new DataOutputStream(bout);
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Entry entry = (Entry) it.next();
        if (Configuration.DEBUG)
          log.fine("-- About to encode one " + entry);
        t1 = -System.currentTimeMillis();
        entry.encode(out2);
        t1 += System.currentTimeMillis();
        if (Configuration.DEBUG)
          log.fine("-- Encoded an Entry in " + t1 + "ms.");
      }
    byte[] plaintext = bout.toByteArray();
    byte[] padding = pad.pad(plaintext, 0, plaintext.length);
    payload = new byte[plaintext.length + padding.length];
    byte[] lastBlock = new byte[mode.currentBlockSize()];
    int l = mode.currentBlockSize() - padding.length;
    System.arraycopy(plaintext, plaintext.length - l, lastBlock, 0, l);
    System.arraycopy(padding, 0, lastBlock, l, padding.length);
    int count = 0;
    while (count + mode.currentBlockSize() < plaintext.length)
      {
        mode.update(plaintext, count, payload, count);
        count += mode.currentBlockSize();
      }
    mode.update(lastBlock, 0, payload, count);
    setMasked(true);
    tt += System.currentTimeMillis();
    if (Configuration.DEBUG)
      {
        log.fine("Encrypted in " + tt + "ms.");
        log.exiting(this.getClass().getName(), "encrypt");
      }
  }

  public void encode(DataOutputStream out, char[] password) throws IOException
  {
    encrypt(password);
    encode(out);
  }

  protected void encodePayload() throws IOException
  {
    if (payload == null)
      {
        if (Configuration.DEBUG)
          log.fine("Null payload: " + this);
        throw new IllegalStateException("not encrypted");
      }
  }

  private IMode getMode(char[] password, int state)
  {
    String s = properties.get("salt");
    if (s == null)
      throw new IllegalArgumentException("no salt");
    byte[] salt = Util.toBytesFromString(s);
    IBlockCipher cipher = CipherFactory.getInstance(properties.get("cipher"));
    if (cipher == null)
      throw new IllegalArgumentException("no such cipher: "
                                         + properties.get("cipher"));
    int blockSize = cipher.defaultBlockSize();
    if (properties.containsKey("block-size"))
      try
        {
          blockSize = Integer.parseInt(properties.get("block-size"));
        }
      catch (NumberFormatException nfe)
        {
          throw new IllegalArgumentException("bad block size: "
                                             + nfe.getMessage());
        }
    String modeName = properties.get("mode");
    IMode mode = ModeFactory.getInstance(modeName, cipher, blockSize);
    if (mode == null)
      throw new IllegalArgumentException("no such mode: " + modeName);
    HashMap pbAttr = new HashMap();
    pbAttr.put(IPBE.PASSWORD, password);
    pbAttr.put(IPBE.SALT, salt);
    pbAttr.put(IPBE.ITERATION_COUNT, ITERATION_COUNT);
    IRandom kdf = PRNGFactory.getInstance("PBKDF2-HMAC-SHA");
    kdf.init(pbAttr);
    int keylen = 0;
    if (! properties.containsKey("keylen"))
      throw new IllegalArgumentException("no key length");
    try
      {
        keylen = Integer.parseInt(properties.get("keylen"));
      }
    catch (NumberFormatException nfe)
      {
      }
    byte[] dk = new byte[keylen];
    byte[] iv = new byte[blockSize];
    try
      {
        kdf.nextBytes(dk, 0, keylen);
        kdf.nextBytes(iv, 0, blockSize);
      }
    catch (LimitReachedException shouldNotHappen)
      {
        throw new Error(shouldNotHappen.toString());
      }
    HashMap modeAttr = new HashMap();
    modeAttr.put(IMode.KEY_MATERIAL, dk);
    modeAttr.put(IMode.STATE, Integer.valueOf(state));
    modeAttr.put(IMode.IV, iv);
    try
      {
        mode.init(modeAttr);
      }
    catch (InvalidKeyException ike)
      {
        throw new IllegalArgumentException(ike.toString());
      }
    return mode;
  }
}
