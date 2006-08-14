/* DiffieHellmanSender.java -- 
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


package gnu.javax.crypto.key.dh;

import gnu.java.security.prng.IRandom;

import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.key.IncomingMessage;
import gnu.javax.crypto.key.OutgoingMessage;

import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Map;

import javax.crypto.interfaces.DHPrivateKey;

/**
 * This implementation is the sender's part of the basic version of the
 * Diffie-Hellman key agreement exchange (A in [HAC]).
 * 
 * @see DiffieHellmanKeyAgreement
 */
public class DiffieHellmanSender
    extends DiffieHellmanKeyAgreement
{
  private BigInteger x; // the sender's random secret

  // default 0-arguments constructor

  protected void engineInit(Map attributes) throws KeyAgreementException
  {
    Object random = attributes.get(SOURCE_OF_RANDOMNESS);
    rnd = null;
    irnd = null;
    if (random instanceof SecureRandom)
      rnd = (SecureRandom) random;
    else if (random instanceof IRandom)
      irnd = (IRandom) random;
    ownerKey = (DHPrivateKey) attributes.get(KA_DIFFIE_HELLMAN_OWNER_PRIVATE_KEY);
    if (ownerKey == null)
      throw new KeyAgreementException("missing owner's private key");
  }

  protected OutgoingMessage engineProcessMessage(IncomingMessage in)
      throws KeyAgreementException
  {
    switch (step)
      {
      case 0:
        return sendRandomSecret(in);
      case 1:
        return computeSharedSecret(in);
      default:
        throw new IllegalStateException("unexpected state");
      }
  }

  private OutgoingMessage sendRandomSecret(IncomingMessage in)
      throws KeyAgreementException
  {
    BigInteger p = ownerKey.getParams().getP();
    BigInteger g = ownerKey.getParams().getG();
    // A chooses a random integer x, 1 <= x <= p-2
    // rfc-2631 restricts x to only be in [2, p-1]
    BigInteger p_minus_2 = p.subtract(TWO);
    byte[] xBytes = new byte[(p_minus_2.bitLength() + 7) / 8];
    do
      {
        nextRandomBytes(xBytes);
        x = new BigInteger(1, xBytes);
      }
    while (! (x.compareTo(TWO) >= 0 && x.compareTo(p_minus_2) <= 0));
    // A sends B the message: g^x mod p
    OutgoingMessage result = new OutgoingMessage();
    result.writeMPI(g.modPow(x, p));
    return result;
  }

  private OutgoingMessage computeSharedSecret(IncomingMessage in)
      throws KeyAgreementException
  {
    BigInteger m1 = in.readMPI();
    if (m1 == null)
      throw new KeyAgreementException("missing message (2)");
    BigInteger p = ownerKey.getParams().getP();
    ZZ = m1.modPow(x, p); // ZZ = (yb ^ xa) mod p
    complete = true;
    return null;
  }
}
