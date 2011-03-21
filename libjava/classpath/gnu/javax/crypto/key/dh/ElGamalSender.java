/* ElGamalSender.java --
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

import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.key.IncomingMessage;
import gnu.javax.crypto.key.OutgoingMessage;

import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Map;

import javax.crypto.interfaces.DHPublicKey;

/**
 * This implementation is the sender's part of the ElGamal key agreement
 * exchange (A in [HAC]).
 *
 * @see ElGamalKeyAgreement
 */
public class ElGamalSender
    extends ElGamalKeyAgreement
{
  /** The recipient's public key. */
  private DHPublicKey B;

  // default 0-arguments constructor

  protected void engineInit(Map attributes) throws KeyAgreementException
  {
    rnd = (SecureRandom) attributes.get(SOURCE_OF_RANDOMNESS);
    // One-time setup (key generation and publication). Each user B generates
    // a keypair and publishes its public key
    B = (DHPublicKey) attributes.get(KA_ELGAMAL_RECIPIENT_PUBLIC_KEY);
    if (B == null)
      throw new KeyAgreementException("missing recipient public key");
  }

  protected OutgoingMessage engineProcessMessage(IncomingMessage in)
      throws KeyAgreementException
  {
    switch (step)
      {
      case 0:
        return computeSharedSecret(in);
      default:
        throw new IllegalStateException("unexpected state");
      }
  }

  private OutgoingMessage computeSharedSecret(IncomingMessage in)
      throws KeyAgreementException
  {
    BigInteger p = B.getParams().getP();
    BigInteger g = B.getParams().getG();
    BigInteger yb = B.getY();
    // A chooses a random integer x, 1 <= x <= p-2
    // rfc-2631 restricts x to only be in [2, p-1]
    BigInteger p_minus_2 = p.subtract(TWO);
    byte[] xBytes = new byte[(p_minus_2.bitLength() + 7) / 8];
    BigInteger x;
    do
      {
        nextRandomBytes(xBytes);
        x = new BigInteger(1, xBytes);
      }
    while (x.compareTo(TWO) >= 0 && x.compareTo(p_minus_2) <= 0);
    // A sends B the message: g^x mod p
    OutgoingMessage result = new OutgoingMessage();
    result.writeMPI(g.modPow(x, p));
    // A computes the key as K = (yb)^x mod p
    ZZ = yb.modPow(x, p); // ZZ = (yb ^ xa) mod p
    complete = true;
    return result;
  }
}
