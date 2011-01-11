/* EAX.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.mode;

import gnu.java.security.Registry;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;

import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * A conventional two-pass authenticated-encrypted mode, EAX. EAX is a
 * <i>Authenticated Encryption with Additional Data</i> (<b>AEAD</b>) scheme,
 * which provides protection and authentication for the message, and provides
 * authentication of an (optional) header. EAX is composed of the counter mode
 * (CTR) and the one-key CBC MAC (OMAC).
 * <p>
 * This class makes full use of the {@link IAuthenticatedMode} interface, that
 * is, all methods of both {@link IMode} and {@link IMac} can be used as
 * specified in the {@link IAuthenticatedMode} interface.
 * <p>
 * References:
 * <ol>
 * <li>M. Bellare, P. Rogaway, and D. Wagner; <a
 * href="http://www.cs.berkeley.edu/~daw/papers/eprint-short-ae.pdf">A
 * Conventional Authenticated-Encryption Mode</a>.</li>
 * </ol>
 */
public class EAX
    implements IAuthenticatedMode
{
  /** The tag size, in bytes. */
  private int tagSize;
  /** The nonce OMAC instance. */
  private IMac nonceOmac;
  /** The header OMAC instance. */
  private IMac headerOmac;
  /** The message OMAC instance. */
  private IMac msgOmac;
  /** The CTR instance. */
  private IMode ctr;
  /** The direction state (encrypting or decrypting). */
  private int state;
  /** Whether we're initialized or not. */
  private boolean init;
  /** The cipher block size. */
  private int cipherBlockSize;
  /** The cipher. */
  private IBlockCipher cipher;
  /** The [t]_n array. */
  private byte[] t_n;
  private static boolean valid = false;

  public EAX(IBlockCipher cipher, int cipherBlockSize)
  {
    this.cipher = cipher;
    this.cipherBlockSize = cipherBlockSize;
    String name = cipher.name();
    int i = name.indexOf('-');
    if (i >= 0)
      name = name.substring(0, i);
    String omacname = Registry.OMAC_PREFIX + name;
    nonceOmac = MacFactory.getInstance(omacname);
    headerOmac = MacFactory.getInstance(omacname);
    msgOmac = MacFactory.getInstance(omacname);
    ctr = ModeFactory.getInstance(Registry.CTR_MODE, cipher, cipherBlockSize);
    t_n = new byte[cipherBlockSize];
    init = false;
  }

  public Object clone()
  {
    return new EAX((IBlockCipher) cipher.clone(), cipherBlockSize);
  }

  public String name()
  {
    return Registry.EAX_MODE + "(" + cipher.name() + ")";
  }

  public int defaultBlockSize()
  {
    return ctr.defaultBlockSize();
  }

  public int defaultKeySize()
  {
    return ctr.defaultKeySize();
  }

  public Iterator blockSizes()
  {
    return ctr.blockSizes();
  }

  public Iterator keySizes()
  {
    return ctr.keySizes();
  }

  public void init(Map attrib) throws InvalidKeyException
  {
    byte[] nonce = (byte[]) attrib.get(IV);
    if (nonce == null)
      throw new IllegalArgumentException("no nonce provided");
    byte[] key = (byte[]) attrib.get(KEY_MATERIAL);
    if (key == null)
      throw new IllegalArgumentException("no key provided");

    Arrays.fill(t_n, (byte) 0);
    nonceOmac.reset();
    nonceOmac.init(Collections.singletonMap(MAC_KEY_MATERIAL, key));
    nonceOmac.update(t_n, 0, t_n.length);
    nonceOmac.update(nonce, 0, nonce.length);
    byte[] N = nonceOmac.digest();
    nonceOmac.reset();
    nonceOmac.update(t_n, 0, t_n.length);
    nonceOmac.update(nonce, 0, nonce.length);
    t_n[t_n.length - 1] = 1;
    headerOmac.reset();
    headerOmac.init(Collections.singletonMap(MAC_KEY_MATERIAL, key));
    headerOmac.update(t_n, 0, t_n.length);
    t_n[t_n.length - 1] = 2;
    msgOmac.reset();
    msgOmac.init(Collections.singletonMap(MAC_KEY_MATERIAL, key));
    msgOmac.update(t_n, 0, t_n.length);
    Integer modeSize = (Integer) attrib.get(MODE_BLOCK_SIZE);
    if (modeSize == null)
      modeSize = Integer.valueOf(cipherBlockSize);
    HashMap ctrAttr = new HashMap();
    ctrAttr.put(KEY_MATERIAL, key);
    ctrAttr.put(IV, N);
    ctrAttr.put(STATE, Integer.valueOf(ENCRYPTION));
    ctrAttr.put(MODE_BLOCK_SIZE, modeSize);
    ctr.reset();
    ctr.init(ctrAttr);
    Integer st = (Integer) attrib.get(STATE);
    if (st != null)
      {
        state = st.intValue();
        if (state != ENCRYPTION && state != DECRYPTION)
          throw new IllegalArgumentException("invalid state");
      }
    else
      state = ENCRYPTION;

    Integer ts = (Integer) attrib.get(TRUNCATED_SIZE);
    if (ts != null)
      tagSize = ts.intValue();
    else
      tagSize = cipherBlockSize;
    if (tagSize < 0 || tagSize > cipherBlockSize)
      throw new IllegalArgumentException("tag size out of range");
    init = true;
  }

  public int currentBlockSize()
  {
    return ctr.currentBlockSize();
  }

  public void encryptBlock(byte[] in, int inOff, byte[] out, int outOff)
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    if (state != ENCRYPTION)
      throw new IllegalStateException("not encrypting");
    ctr.update(in, inOff, out, outOff);
    msgOmac.update(out, outOff, ctr.currentBlockSize());
  }

  public void decryptBlock(byte[] in, int inOff, byte[] out, int outOff)
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    if (state != DECRYPTION)
      throw new IllegalStateException("not decrypting");
    msgOmac.update(in, inOff, ctr.currentBlockSize());
    ctr.update(in, inOff, out, outOff);
  }

  public void update(byte[] in, int inOff, byte[] out, int outOff)
  {
    switch (state)
      {
      case ENCRYPTION:
        encryptBlock(in, inOff, out, outOff);
        break;
      case DECRYPTION:
        decryptBlock(in, inOff, out, outOff);
        break;
      default:
        throw new IllegalStateException("impossible state " + state);
      }
  }

  public void reset()
  {
    nonceOmac.reset();
    headerOmac.reset();
    msgOmac.reset();
    ctr.reset();
  }

  public boolean selfTest()
  {
    return true; // XXX
  }

  public int macSize()
  {
    return tagSize;
  }

  public byte[] digest()
  {
    byte[] tag = new byte[tagSize];
    digest(tag, 0);
    return tag;
  }

  public void digest(byte[] out, int outOffset)
  {
    if (outOffset < 0 || outOffset + tagSize > out.length)
      throw new IndexOutOfBoundsException();
    byte[] N = nonceOmac.digest();
    byte[] H = headerOmac.digest();
    byte[] M = msgOmac.digest();
    for (int i = 0; i < tagSize; i++)
      out[outOffset + i] = (byte)(N[i] ^ H[i] ^ M[i]);
    reset();
  }

  public void update(byte b)
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    headerOmac.update(b);
  }

  public void update(byte[] buf, int off, int len)
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    headerOmac.update(buf, off, len);
  }
}
