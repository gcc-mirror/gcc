/* EncryptedEntry.java --
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
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.PadFactory;
import gnu.javax.crypto.pad.WrongPaddingException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.util.HashMap;
import java.util.Iterator;

public class EncryptedEntry extends MaskableEnvelopeEntry implements Registry
{
  public static final int TYPE = 0;

  public EncryptedEntry(String cipher, String mode, Properties properties)
  {
    super(TYPE, properties);
    if (cipher == null || mode == null)
      throw new IllegalArgumentException("neither cipher nor mode can be null");
    properties.put("cipher", cipher);
    properties.put("mode", mode);
    setMasked(false);
  }

  private EncryptedEntry()
  {
    super(TYPE, new Properties());
    setMasked(true);
  }

  public static EncryptedEntry decode(DataInputStream in) throws IOException
  {
    EncryptedEntry entry = new EncryptedEntry();
    entry.defaultDecode(in);
    if (! entry.properties.containsKey("cipher"))
      throw new MalformedKeyringException("no cipher");
    if (! entry.properties.containsKey("cipher"))
      throw new MalformedKeyringException("no cipher");
    return entry;
  }

  public void decrypt(byte[] key, byte[] iv) throws IllegalArgumentException,
      WrongPaddingException
  {
    if (! isMasked() || payload == null)
      return;
    IMode mode = getMode(key, iv, IMode.DECRYPTION);
    IPad padding = null;
    padding = PadFactory.getInstance("PKCS7");
    padding.init(mode.currentBlockSize());
    byte[] buf = new byte[payload.length];
    int count = 0;
    for (int i = 0; i < payload.length; i++)
      {
        mode.update(payload, count, buf, count);
        count += mode.currentBlockSize();
      }
    int padlen = padding.unpad(buf, 0, buf.length);
    int len = buf.length - padlen;
    DataInputStream in = new DataInputStream(new ByteArrayInputStream(buf, 0, len));
    try
      {
        decodeEnvelope(in);
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException("decryption failed");
      }
    setMasked(false);
    payload = null;
  }

  public void encrypt(byte[] key, byte[] iv) throws IOException
  {
    IMode mode = getMode(key, iv, IMode.ENCRYPTION);
    IPad pad = PadFactory.getInstance("PKCS7");
    pad.init(mode.currentBlockSize());
    ByteArrayOutputStream bout = new ByteArrayOutputStream(1024);
    DataOutputStream out2 = new DataOutputStream(bout);
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Entry entry = (Entry) it.next();
        entry.encode(out2);
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
  }

  public void encodePayload() throws IOException
  {
    if (payload == null)
      throw new IOException("not encrypted");
  }

  private IMode getMode(byte[] key, byte[] iv, int state)
  {
    IBlockCipher cipher = CipherFactory.getInstance(properties.get("cipher"));
    if (cipher == null)
      throw new IllegalArgumentException("no such cipher: " + properties.get("cipher"));
    int blockSize = cipher.defaultBlockSize();
    if (properties.containsKey("block-size"))
      {
        try
          {
            blockSize = Integer.parseInt(properties.get("block-size"));
          }
        catch (NumberFormatException nfe)
          {
            throw new IllegalArgumentException("bad block size: "
                                               + nfe.getMessage());
          }
      }
    IMode mode = ModeFactory.getInstance(properties.get("mode"), cipher, blockSize);
    if (mode == null)
      throw new IllegalArgumentException("no such mode: " + properties.get("mode"));

    HashMap modeAttr = new HashMap();
    modeAttr.put(IMode.KEY_MATERIAL, key);
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
