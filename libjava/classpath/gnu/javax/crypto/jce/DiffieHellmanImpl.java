/* DiffieHellmanImpl.java -- implementation of the Diffie-Hellman key agreement.
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

import javax.crypto.KeyAgreementSpi;
import javax.crypto.SecretKey;
import javax.crypto.ShortBufferException;
import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.DHParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * The JCE implementation of a 2-party Diffie-Hellman key agreement.
 * 
 * @author Casey Marshall (csm@gnu.org)
 */
public final class DiffieHellmanImpl
    extends KeyAgreementSpi
{
  /** The private key being used for this agreement. */
  private DHPrivateKey key;

  /** The current result. */
  private byte[] result;

  /** True if the caller told us we are done. */
  private boolean last_phase_done;

  /** Trivial default constructor. */
  public DiffieHellmanImpl()
  {
    super();

    key = null;
    result = null;
    last_phase_done = false;
  }

  protected Key engineDoPhase(Key incoming, boolean lastPhase)
      throws InvalidKeyException
  {
    if (key == null)
      throw new IllegalStateException("Not initialized");

    if (last_phase_done)
      throw new IllegalStateException("Last phase already done");

    if (! (incoming instanceof DHPublicKey))
      throw new InvalidKeyException("Key MUST be a DHPublicKey");

    DHPublicKey pub = (DHPublicKey) incoming;
    DHParameterSpec s1 = key.getParams();
    DHParameterSpec s2 = pub.getParams();
    if (! s1.getG().equals(s2.getG()) || ! s1.getP().equals(s2.getP()))
      throw new InvalidKeyException("Incompatible key");
    if (! lastPhase)
      throw new IllegalArgumentException(
          "This key-agreement MUST be concluded in one step only");
    BigInteger resultBI = pub.getY().modPow(key.getX(), s1.getP());
    result = resultBI.toByteArray();
    if (result[0] == 0x00)
      {
        byte[] buf = new byte[result.length - 1];
        System.arraycopy(result, 1, buf, 0, buf.length);
        result = buf;
      }
    last_phase_done = true;
    return null;
  }

  protected byte[] engineGenerateSecret()
  {
    checkState();
    byte[] res = (byte[]) result.clone();
    reset();
    return res;
  }

  protected int engineGenerateSecret(byte[] secret, int offset)
      throws ShortBufferException
  {
    checkState();
    if (result.length > secret.length - offset)
      throw new ShortBufferException();
    System.arraycopy(result, 0, secret, offset, result.length);
    int res = result.length;
    reset();
    return res;
  }

  protected SecretKey engineGenerateSecret(String algorithm)
      throws InvalidKeyException
  {
    checkState();
    byte[] s = (byte[]) result.clone();
    SecretKey res = new SecretKeySpec(s, algorithm);
    reset();
    return res;
  }

  protected void engineInit(Key key, SecureRandom random)
      throws InvalidKeyException
  {
    if (! (key instanceof DHPrivateKey))
      throw new InvalidKeyException("Key MUST be a DHPrivateKey");
    this.key = (DHPrivateKey) key;
    reset();
  }

  protected void engineInit(Key key, AlgorithmParameterSpec params,
                            SecureRandom random)
      throws InvalidKeyException
  {
    engineInit(key, random);
  }

  private void reset()
  {
    result = null;
    last_phase_done = false;
  }

  private void checkState()
  {
    if (result == null || ! last_phase_done)
      throw new IllegalStateException("Not finished");
  }
}
