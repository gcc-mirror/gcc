/* DiffieHellmanImpl.java -- implementation of the Diffie-Hellman key agreement.
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package gnu.javax.crypto;

import gnu.java.security.provider.GnuDHPublicKey;

import java.math.BigInteger;

import java.security.Key;
import java.security.InvalidKeyException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

import javax.crypto.KeyAgreementSpi;
import javax.crypto.SecretKey;
import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.DHParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * The Diffie-Hellman key agreement.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public final class DiffieHellmanImpl extends KeyAgreementSpi
{

  /** The private key being used for this agreement. */
  private DHPrivateKey key;

  /** The current result. */
  private BigInteger result;

  /** True if the caller told us we are done. */
  private boolean last_phase_done;

  /** Trivial default constructor. */
  public DiffieHellmanImpl ()
  {
    key = null;
    result = null;
    last_phase_done = false;
  }

  // KeyAgreementSpi methods.

  protected Key engineDoPhase (final Key incoming, final boolean lastPhase)
    throws InvalidKeyException
  {
    if (key == null)
      throw new IllegalStateException ("not initialized");
    if (last_phase_done)
      throw new IllegalStateException ("last phase already done");

    if (!(incoming instanceof DHPublicKey))
      throw new InvalidKeyException ("expecting javax.crypto.interfaces.DHPublicKey");
    DHPublicKey pub = (DHPublicKey) incoming;
    DHParameterSpec s1 = key.getParams();
    DHParameterSpec s2 = pub.getParams();
    if (!s1.getG().equals (s2.getG())
        || !s1.getP().equals (s2.getP())
        || s1.getL() != s2.getL())
      throw new InvalidKeyException ("supplied key is not compatible");

    result = pub.getY().modPow (key.getX(), s1.getP());
    if (lastPhase)
      {
	last_phase_done = true;
	return null;
      }

    throw new IllegalArgumentException ("only supports two-party Diffie Hellman");
  }

  protected byte[] engineGenerateSecret ()
  {
    if (result == null || !last_phase_done)
      throw new IllegalStateException ("not finished");

    byte[] buf = result.toByteArray ();
    if (buf[0] == 0x00)
      {
	byte[] buf2 = new byte[buf.length - 1];
	System.arraycopy (buf, 1, buf2, 0, buf2.length);
	buf = buf2;
      }
    return buf;
  }

  protected int engineGenerateSecret (final byte[] secret, final int offset)
  {
    byte[] s = engineGenerateSecret();
    System.arraycopy (s, 0, secret, offset, s.length);
    return s.length;
  }

  protected SecretKey engineGenerateSecret (final String algorithm)
    throws InvalidKeyException
  {
    byte[] s = engineGenerateSecret();
    return new SecretKeySpec (s, algorithm);
  }

  protected void engineInit (final Key key, final SecureRandom random)
    throws InvalidKeyException
  {
    if (!(key instanceof DHPrivateKey))
      throw new InvalidKeyException ("not a javax.crypto.interfaces.DHPrivateKey");
    this.key = (DHPrivateKey) key;
    result = null;
    last_phase_done = false;
  }

  protected void engineInit (final Key key, final AlgorithmParameterSpec params,
                             final SecureRandom random)
    throws InvalidKeyException
  {
    engineInit (key, random);
  }
}
