/* DiffieHellmanKeyAgreement.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.util.Util;

import gnu.javax.crypto.key.BaseKeyAgreementParty;
import gnu.javax.crypto.key.KeyAgreementException;

import java.math.BigInteger;

import javax.crypto.interfaces.DHPrivateKey;

/**
 * The basic version of the Diffie-Hellman key agreement is described in the
 * Handbook of Applied Cryptography [HAC] as follows:
 * <ul>
 * <li>An appropriate prime p and generator g of Z<sub>p</sub><sup>*</sup>
 * (2 &lt;= g &lt;= p-2) are selected and published.</li>
 * <li>A and B each send the other one message over an open channel; as a
 * result, they both can then compute a shared secret key K which they can use
 * to protect their future communication.</li>
 * <li>A chooses a random secret x, 1 &lt;= x &lt;= p-2, and sends B message
 * (1) which is g^x mod p.</li>
 * <li>B chooses a random secret y, 1 &lt;= y &lt;= p-2, and sends A message
 * (2) which is g^y mod p.</li>
 * <li>B receives message (1) and computes the shared key as K = (g^x)^y mod p.
 * </li>
 * <li>A receives message (2) and computes the shared key as K = (g^y)^x mod p.
 * </li>
 * </ul>
 * <p>
 * RFC-2631 describes a <i>Static-Static Mode</i> of operations with
 * Diffie-Hellman keypairs as follows:
 * <pre>
 *  &quot;In Static-Static mode, both the sender and the recipient have a
 *  static (and certified) key pair. Since the sender's and recipient's
 *  keys are therefore the same for each message, ZZ will be the same for
 *  each message. Thus, partyAInfo MUST be used (and different for each
 *  message) in order to ensure that different messages use different
 *  KEKs. Implementations MAY implement Static-Static mode.&quot;
 * </pre>
 * 
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://www.ietf.org/rfc/rfc2631.txt">Diffie-Hellman Key
 * Agreement Method</a><br>
 * Eric Rescorla.</li>
 * <li><a href="http://www.cacr.math.uwaterloo.ca/hac">[HAC]</a>: Handbook of
 * Applied Cryptography.<br>
 * CRC Press, Inc. ISBN 0-8493-8523-7, 1997<br>
 * Menezes, A., van Oorschot, P. and S. Vanstone.</li>
 * </ol>
 */
public abstract class DiffieHellmanKeyAgreement
    extends BaseKeyAgreementParty
{
  public static final String SOURCE_OF_RANDOMNESS = "gnu.crypto.dh.ka.prng";
  public static final String KA_DIFFIE_HELLMAN_OWNER_PRIVATE_KEY =
      "gnu.crypto.dh.ka.owner.private.key";
  /** The key agreement party's private key. */
  protected DHPrivateKey ownerKey;
  /** The shared secret key. */
  protected BigInteger ZZ;

  protected DiffieHellmanKeyAgreement()
  {
    super(Registry.DH_KA);
  }

  protected byte[] engineSharedSecret() throws KeyAgreementException
  {
    return Util.trim(ZZ);
  }

  protected void engineReset()
  {
    ownerKey = null;
    ZZ = null;
  }
}
