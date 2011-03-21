/* SRP6TLSServer.java --
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


package gnu.javax.crypto.key.srp6;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.key.OutgoingMessage;
import gnu.javax.crypto.key.IncomingMessage;
import gnu.javax.crypto.sasl.srp.SRP;
import gnu.javax.crypto.sasl.srp.SRPAuthInfoProvider;
import gnu.javax.crypto.sasl.srp.SRPRegistry;

import java.io.IOException;
import java.math.BigInteger;
import java.security.KeyPair;
import java.security.SecureRandom;
import java.util.HashMap;
import java.util.Map;

/**
 * A variation of the SRP6 key agreement protocol, for the server-side as
 * proposed in <a
 * href="http://www.ietf.org/internet-drafts/draft-ietf-tls-srp-05.txt">Using
 * SRP for TLS Authentication</a>. The only difference between it and the SASL
 * variant is that the shared secret is the entity <code>S</code> and not
 * <code>H(S)</code>.
 */
public class SRP6TLSServer
    extends SRP6KeyAgreement
{
  /** The user's ephemeral key pair. */
  private KeyPair hostKeyPair;
  /** The SRP password database. */
  private SRPAuthInfoProvider passwordDB;

  // default 0-arguments constructor

  protected void engineInit(final Map attributes) throws KeyAgreementException
  {
    rnd = (SecureRandom) attributes.get(SOURCE_OF_RANDOMNESS);
    final String md = (String) attributes.get(HASH_FUNCTION);
    if (md == null || md.trim().length() == 0)
      throw new KeyAgreementException("missing hash function");
    srp = SRP.instance(md);
    passwordDB = (SRPAuthInfoProvider) attributes.get(HOST_PASSWORD_DB);
    if (passwordDB == null)
      throw new KeyAgreementException("missing SRP password database");
  }

  protected OutgoingMessage engineProcessMessage(final IncomingMessage in)
      throws KeyAgreementException
  {
    switch (step)
      {
      case 0:
        return sendParameters(in);
      case 1:
        return computeSharedSecret(in);
      default:
        throw new IllegalStateException("unexpected state");
      }
  }

  protected void engineReset()
  {
    hostKeyPair = null;
    super.engineReset();
  }

  private OutgoingMessage sendParameters(final IncomingMessage in)
      throws KeyAgreementException
  {
    final String I = in.readString();
    // get s and v for user identified by I
    // ----------------------------------------------------------------------
    final Map credentials;
    try
      {
        final Map userID = new HashMap();
        userID.put(Registry.SASL_USERNAME, I);
        userID.put(SRPRegistry.MD_NAME_FIELD, srp.getAlgorithm());
        credentials = passwordDB.lookup(userID);
      }
    catch (IOException x)
      {
        throw new KeyAgreementException("computeSharedSecret()", x);
      }

    final BigInteger s = new BigInteger(
        1, Util.fromBase64((String) credentials.get(SRPRegistry.SALT_FIELD)));
    final BigInteger v = new BigInteger(
        1, Util.fromBase64((String) credentials.get(SRPRegistry.USER_VERIFIER_FIELD)));
    final Map configuration;
    try
      {
        final String mode = (String) credentials.get(SRPRegistry.CONFIG_NDX_FIELD);
        configuration = passwordDB.getConfiguration(mode);
      }
    catch (IOException x)
      {
        throw new KeyAgreementException("computeSharedSecret()", x);
      }
    N = new BigInteger(
        1, Util.fromBase64((String) configuration.get(SRPRegistry.SHARED_MODULUS)));
    g = new BigInteger(
        1, Util.fromBase64((String) configuration.get(SRPRegistry.FIELD_GENERATOR)));
    // generate an ephemeral keypair
    final SRPKeyPairGenerator kpg = new SRPKeyPairGenerator();
    final Map attributes = new HashMap();
    if (rnd != null)
      attributes.put(SRPKeyPairGenerator.SOURCE_OF_RANDOMNESS, rnd);
    attributes.put(SRPKeyPairGenerator.SHARED_MODULUS, N);
    attributes.put(SRPKeyPairGenerator.GENERATOR, g);
    attributes.put(SRPKeyPairGenerator.USER_VERIFIER, v);
    kpg.setup(attributes);
    hostKeyPair = kpg.generate();
    final BigInteger B = ((SRPPublicKey) hostKeyPair.getPublic()).getY();
    final OutgoingMessage result = new OutgoingMessage();
    result.writeMPI(N);
    result.writeMPI(g);
    result.writeMPI(s);
    result.writeMPI(B);
    return result;
  }

  protected OutgoingMessage computeSharedSecret(final IncomingMessage in)
      throws KeyAgreementException
  {
    final BigInteger A = in.readMPI();
    final BigInteger B = ((SRPPublicKey) hostKeyPair.getPublic()).getY();
    final BigInteger u = uValue(A, B); // u = H(A | B)
    // compute S = (Av^u) ^ b
    final BigInteger b = ((SRPPrivateKey) hostKeyPair.getPrivate()).getX();
    final BigInteger v = ((SRPPrivateKey) hostKeyPair.getPrivate()).getV();
    final BigInteger S = A.multiply(v.modPow(u, N)).modPow(b, N);
    K = S;
    complete = true;
    return null;
  }
}
