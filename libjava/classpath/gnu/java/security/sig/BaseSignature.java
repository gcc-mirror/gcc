/* BaseSignature.java --
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.sig;

import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.util.PRNG;

import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.Map;
import java.util.Random;

/**
 * A base abstract class to facilitate implementations of concrete Signatures.
 */
public abstract class BaseSignature
    implements ISignature
{
  /** The canonical name of this signature scheme. */
  protected String schemeName;

  /** The underlying message digest instance for this signature scheme. */
  protected IMessageDigest md;

  /** The public key to use when verifying signatures. */
  protected PublicKey publicKey;

  /** The private key to use when generating signatures (signing). */
  protected PrivateKey privateKey;

  /** The optional {@link Random} instance to use. */
  private Random rnd;

  /** The optional {@link IRandom} instance to use. */
  private IRandom irnd;

  /** Our default source of randomness. */
  private PRNG prng = null;

  /**
   * Trivial constructor.
   *
   * @param schemeName the name of this signature scheme.
   * @param md the underlying instance of the message digest algorithm.
   * @throws IllegalArgumentException if the designated hash instance is
   *           <code>null</code>.
   */
  protected BaseSignature(String schemeName, IMessageDigest md)
  {
    super();

    this.schemeName = schemeName;
    if (md == null)
      throw new IllegalArgumentException("Message digest MUST NOT be null");

    this.md = md;
  }

  public String name()
  {
    return schemeName + "-" + md.name();
  }

  public void setupVerify(Map attributes) throws IllegalArgumentException
  {
    setup(attributes);
    // do we have a public key?
    PublicKey key = (PublicKey) attributes.get(VERIFIER_KEY);
    if (key != null)
      setupForVerification(key);
  }

  public void setupSign(Map attributes) throws IllegalArgumentException
  {
    setup(attributes);
    // do we have a private key?
    PrivateKey key = (PrivateKey) attributes.get(SIGNER_KEY);
    if (key != null)
      setupForSigning(key);
  }

  public void update(byte b)
  {
    if (md == null)
      throw new IllegalStateException();

    md.update(b);
  }

  public void update(byte[] b, int off, int len)
  {
    if (md == null)
      throw new IllegalStateException();

    md.update(b, off, len);
  }

  public Object sign()
  {
    if (md == null || privateKey == null)
      throw new IllegalStateException();

    return generateSignature();
  }

  public boolean verify(Object sig)
  {
    if (md == null || publicKey == null)
      throw new IllegalStateException();

    return verifySignature(sig);
  }

  public abstract Object clone();

  protected abstract void setupForVerification(PublicKey key)
      throws IllegalArgumentException;

  protected abstract void setupForSigning(PrivateKey key)
      throws IllegalArgumentException;

  protected abstract Object generateSignature() throws IllegalStateException;

  protected abstract boolean verifySignature(Object signature)
      throws IllegalStateException;

  /** Initialises the internal fields of this instance. */
  protected void init()
  {
    md.reset();
    rnd = null;
    irnd = null;
    publicKey = null;
    privateKey = null;
  }

  /**
   * Fills the designated byte array with random data.
   *
   * @param buffer the byte array to fill with random data.
   */
  protected void nextRandomBytes(byte[] buffer)
  {
    if (rnd != null)
      rnd.nextBytes(buffer);
    else if (irnd != null)
      try
        {
          irnd.nextBytes(buffer, 0, buffer.length);
        }
      catch (IllegalStateException x)
        {
          throw new RuntimeException("nextRandomBytes(): " + x);
        }
      catch (LimitReachedException x)
        {
          throw new RuntimeException("nextRandomBytes(): " + x);
        }
    else
      getDefaultPRNG().nextBytes(buffer);
  }

  private void setup(Map attributes)
  {
    init();
    // do we have a Random or SecureRandom, or should we use our own?
    Object obj = attributes.get(SOURCE_OF_RANDOMNESS);
    if (obj instanceof Random)
      rnd = (Random) obj;
    else if (obj instanceof IRandom)
      irnd = (IRandom) obj;
  }

  private PRNG getDefaultPRNG()
  {
    if (prng == null)
      prng = PRNG.getInstance();

    return prng;
  }
}
