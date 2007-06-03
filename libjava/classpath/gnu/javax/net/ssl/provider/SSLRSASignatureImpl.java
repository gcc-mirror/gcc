/* SSLRSASignatureImpl.java -- SSL/TLS RSA implementation.
   Copyright (C) 2006  Free Software Foundation, Inc.

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
exception statement from your version. */


package gnu.javax.net.ssl.provider;

import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;
import gnu.java.security.sig.rsa.RSA;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.InvalidParameterException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SignatureException;
import java.security.SignatureSpi;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Arrays;

/**
 * An implementation of of the RSA signature algorithm; this is an RSA
 * encrypted MD5 hash followed by a SHA-1 hash.
 * 
 * @author Casey Marshall (csm@gnu.org)
 */
public class SSLRSASignatureImpl extends SignatureSpi
{
  private static final SystemLogger logger = SystemLogger.SYSTEM;
  private RSAPublicKey pubkey;
  private RSAPrivateKey privkey;
  private final MessageDigest md5, sha;
  private boolean initSign = false;
  private boolean initVerify = false;
  
  public SSLRSASignatureImpl() throws NoSuchAlgorithmException
  {
    md5 = MessageDigest.getInstance("MD5");
    sha = MessageDigest.getInstance("SHA-1");
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineInitVerify(java.security.PublicKey)
   */
  @Override protected void engineInitVerify(PublicKey publicKey)
      throws InvalidKeyException
  {
    try
      {
        pubkey = (RSAPublicKey) publicKey;
        initVerify = true;
        initSign = false;
        privkey = null;
      }
    catch (ClassCastException cce)
      {
        throw new InvalidKeyException(cce);
      }
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineInitSign(java.security.PrivateKey)
   */
  @Override protected void engineInitSign(PrivateKey privateKey)
      throws InvalidKeyException
  {
    try
      {
        privkey = (RSAPrivateKey) privateKey;
        initSign = true;
        initVerify = false;
        pubkey = null;
      }
    catch (ClassCastException cce)
      {
        throw new InvalidKeyException(cce);
      }
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineUpdate(byte)
   */
  @Override protected void engineUpdate(byte b) throws SignatureException
  {
    if (!initSign && !initVerify)
      throw new IllegalStateException("not initialized");
    if (Debug.DEBUG)
      logger.log(Component.SSL_HANDSHAKE, "SSL/RSA update 0x{0}",
                 Util.formatInt(b & 0xFF, 16, 2));
    md5.update(b);
    sha.update(b);
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineUpdate(byte[], int, int)
   */
  @Override protected void engineUpdate(byte[] b, int off, int len)
      throws SignatureException
  {
    if (!initSign && !initVerify)
      throw new IllegalStateException("not initialized");
    if (Debug.DEBUG)
      logger.log(Component.SSL_HANDSHAKE, "SSL/RSA update\n{0}",
                 Util.hexDump(b, off, len, ">> "));
    md5.update(b, off, len);
    sha.update(b, off, len);
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineSign()
   */
  @Override protected byte[] engineSign() throws SignatureException
  {
    // FIXME we need to add RSA blinding to this, somehow.
    
    if (!initSign)
      throw new SignatureException("not initialized for signing");
    // Pad the hash results with RSA block type 1.
    final int k = (privkey.getModulus().bitLength() + 7) >>> 3;
    final byte[] d = Util.concat(md5.digest(), sha.digest());
    if (k - 11 < d.length)
      throw new SignatureException("message too long");
    final byte[] eb = new byte[k];
    eb[0] = 0x00;
    eb[1] = 0x01;
    for (int i = 2; i < k - d.length - 1; i++)
      eb[i] = (byte) 0xFF;
    System.arraycopy(d, 0, eb, k - d.length, d.length);
    BigInteger EB = new BigInteger(eb);

    // Private-key encrypt the padded hashes.
    BigInteger EM = RSA.sign(privkey, EB);
    return Util.trim(EM);
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineVerify(byte[])
   */
  @Override protected boolean engineVerify(byte[] sigBytes)
      throws SignatureException
  {
    if (!initVerify)
      throw new SignatureException("not initialized for verifying");

    // Public-key decrypt the signature representative.
    BigInteger EM = new BigInteger(1, (byte[]) sigBytes);
    BigInteger EB = RSA.verify(pubkey, EM);

    // Unpad the decrypted message.
    int i = 0;
    final byte[] eb = EB.toByteArray();
    if (eb[0] == 0x00)
      {
        for (i = 0; i < eb.length && eb[i] == 0x00; i++)
          ;
      }
    else if (eb[0] == 0x01)
      {
        for (i = 1; i < eb.length && eb[i] != 0x00; i++)
          {
            if (eb[i] != (byte) 0xFF)
              {
                throw new SignatureException("bad padding");
              }
          }
        i++;
      }
    else
      {
        throw new SignatureException("decryption failed");
      }
    byte[] d1 = Util.trim(eb, i, eb.length - i);
    byte[] d2 = Util.concat(md5.digest(), sha.digest());
    if (Debug.DEBUG)
      logger.logv(Component.SSL_HANDSHAKE, "SSL/RSA d1:{0} d2:{1}",
                  Util.toHexString(d1, ':'), Util.toHexString(d2, ':'));
    return Arrays.equals(d1, d2);
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineSetParameter(java.lang.String, java.lang.Object)
   */
  @Override protected void engineSetParameter(String param, Object value)
      throws InvalidParameterException
  {
    throw new InvalidParameterException("parameters not supported");
  }

  /* (non-Javadoc)
   * @see java.security.SignatureSpi#engineGetParameter(java.lang.String)
   */
  @Override protected Object engineGetParameter(String param)
      throws InvalidParameterException
  {
    throw new InvalidParameterException("parameters not supported");
  }
}
