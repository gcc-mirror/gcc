/* Signature.java --- Signature Class
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package java.security;
import java.security.spec.AlgorithmParameterSpec;

/**
   Signature is used to provide an interface to digital signature 
   algorithms. Digital signatures provide authentication and data 
   integrity of digital data. 

   The GNU provider provides the NIST standard DSA which uses DSA 
   and SHA-1. It can be specified by SHA/DSA, SHA-1/DSA or its 
   OID. If the RSA signature algorithm is provided then
   it could be MD2/RSA. MD5/RSA, or SHA-1/RSA. The algorithm must
   be specified because there is no default.

   Signature provides implementation-independent algorithms which 
   are requested by the user through getInstance. It can be 
   requested by specifying just the algorithm name or by 
   specifying both the algorithm name and provider name. 

   The three phases of using Signature are:

   1. Initialing

   * It must be initialized with a private key for 
   signing. 
   * It must be initialized with a public key for 
   verifying.

   2. Updating

   Update the bytes for signing or verifying with calls 
   to update.

   3. Signing or Verify the signature on the currently stored
   bytes by calling sign or verify.

   @author Mark Benvenuto <ivymccough@worldnet.att.net>
   @since JDK 1.1
 */
public abstract class Signature extends SignatureSpi
{
  /**
     Possible state variable which signifies if it has not been 
     initialized.
   */
  protected static final int UNINITIALIZED = 1;

  /**
     Possible state variable which signifies if it has been 
     initialized for signing.
   */
  protected static final int SIGN = 2;

  /**
     Possible state variable which signifies if it has been 
     initialized for verifying.
   */
  protected static final int VERIFY = 3;

  /**
     State of this Signature class.
   */
  protected int state = UNINITIALIZED;

  private String algorithm;
  private Provider provider;

  /**
     Creates a new signature for this algorithm.

     @param algorithm the algorithm to use
   */
  protected Signature(String algorithm)
  {
    this.algorithm = algorithm;
    state = UNINITIALIZED;
  }

  /** 
     Gets an instance of the Signature class representing
     the specified signature. If the algorithm is not found then, 
     it throws NoSuchAlgorithmException.

     @param algorithm the name of signature algorithm to choose
     @return a Signature repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by providers
   */
  public static Signature getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    String name = "Signature." + algorithm;
    Provider[] p = Security.getProviders();

    for (int i = 0; i < p.length; i++)
      {
	String classname = p[i].getProperty(name);
	if (classname != null)
	  return getInstance(classname, algorithm, p[i]);
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /** 
     Gets an instance of the Signature class representing
     the specified signature from the specified provider. If the 
     algorithm is not found then, it throws NoSuchAlgorithmException.
     If the provider is not found, then it throws
     NoSuchProviderException.

     @param algorithm the name of signature algorithm to choose
     @param provider the name of the provider to find the algorithm in
     @return a Signature repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by the provider
     @throws NoSuchProviderException if the provider is not found
   */
  public static Signature getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException();

    return getInstance(p.getProperty("Signature." + algorithm), algorithm, p);
  }

  private static Signature getInstance(String classname,
				       String algorithm,
				       Provider provider)
    throws NoSuchAlgorithmException
  {
    try
      {
	Object o = Class.forName(classname).newInstance();
	Signature sig;
	if (o instanceof SignatureSpi)
	  sig = (Signature) (new DummySignature((SignatureSpi) o, algorithm));
	else
	  {
	    sig = (Signature) o;
	    sig.algorithm = algorithm;
	  }

	sig.provider = provider;
	return sig;
      }
    catch (ClassNotFoundException cnfe)
      {
	throw new NoSuchAlgorithmException("Class not found");
      }
    catch (InstantiationException ie)
      {
	throw new NoSuchAlgorithmException("Class instantiation failed");
      }
    catch (IllegalAccessException iae)
      {
	throw new NoSuchAlgorithmException("Illegal Access");
      }
  }

  /**
     Gets the provider that the Signature is from.

     @return the provider the this Signature 
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
     Initializes this class with the public key for 
     verification purposes.

     @param publicKey the public key to verify with

     @throws InvalidKeyException invalid key
   */
  public final void initVerify(PublicKey publicKey) throws InvalidKeyException
  {
    state = VERIFY;
    engineInitVerify(publicKey);
  }

  /**
     Verify Signature with a certificate. This is a FIPS 140-1 compatible method
     since it verifies a signature with a certificate.

     If the certificate is an X.509 certificate, has a KeyUsage parameter and
     the parameter indicates this key is not to be used for signing then an 
     error is returned.

     @param certificate a certificate containing a public key to verify with
   */
  public final void initVerify(java.security.cert.Certificate certificate)
    throws InvalidKeyException
  {
    state = VERIFY;
    if (certificate.getType().equals("X509"))
      {
	java.security.cert.X509Certificate cert =
	  (java.security.cert.X509Certificate) certificate;

	boolean[]array = cert.getKeyUsage();
	if (array != null && array[0] == false)
	  throw new InvalidKeyException
	    ("KeyUsage of this Certificate indicates it cannot be used for digital signing");
      }
    this.initVerify(certificate.getPublicKey());
  }

  /**
     Initializes this class with the private key for 
     signing purposes.

     @param privateKey the private key to sign with

     @throws InvalidKeyException invalid key
   */
  public final void initSign(PrivateKey privateKey) throws InvalidKeyException
  {
    state = SIGN;
    engineInitSign(privateKey);
  }

  /**
     Initializes this class with the private key and source 
     of randomness for signing purposes.

     @param privateKey the private key to sign with
     @param random Source of randomness

     @throws InvalidKeyException invalid key

     @since JDK 1.2
   */
  public final void initSign(PrivateKey privateKey, SecureRandom random)
    throws InvalidKeyException
  {
    state = SIGN;
    engineInitSign(privateKey, random);
  }


  /**
     Returns the signature bytes of all the data fed to this class.
     The format of the output depends on the underlying signature
     algorithm.

     @return the signature

     @throws SignatureException engine not properly initialized
   */
  public final byte[] sign() throws SignatureException
  {
    if (state == SIGN)
      {
	state = UNINITIALIZED;
	return engineSign();
      }
    else
      throw new SignatureException();
  }

  /**
     Generates signature bytes of all the data fed to this class 
     and outputs it to the passed array. The format of the 
     output depends on the underlying signature algorithm.

     After calling this method, the signature is reset to its
     initial state and can be used to generate additional
     signatures.

     @param outbuff array of bytes
     @param offset the offset to start at in the array
     @param len the length of the bytes to put into the array. 
     Neither this method or the GNU provider will 
     return partial digests. If len is less than the 
     signature length, this method will throw 
     SignatureException. If it is greater than or equal
     then it is ignored.

     @return number of bytes in outbuf

     @throws SignatureException engine not properly initialized

     @since JDK 1.2
   */
  public final int sign(byte[]outbuf, int offset, int len)
    throws SignatureException
  {
    if (state == SIGN)
      {
	state = UNINITIALIZED;
	return engineSign(outbuf, offset, len);
      }
    else
      throw new SignatureException();
  }

  /**
     Verifies the passed signature.

     @param signature the signature bytes to verify

     @return true if verified, false otherwise

     @throws SignatureException engine not properly initialized
     or wrong signature
   */
  public final boolean verify(byte[]signature) throws SignatureException
  {
    if (state == VERIFY)
      {
	state = UNINITIALIZED;
	return engineVerify(signature);
      }
    else
      throw new SignatureException();
  }

  /**
     Updates the data to be signed or verified with the specified 
     byte.

     @param b byte to update with

     @throws SignatureException Engine not properly initialized
   */
  public final void update(byte b) throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(b);
    else
      throw new SignatureException();
  }

  /**
     Updates the data to be signed or verified with the specified 
     bytes.

     @param data array of bytes

     @throws SignatureException engine not properly initialized
   */
  public final void update(byte[]data) throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(data, 0, data.length);
    else
      throw new SignatureException();
  }

  /**
     Updates the data to be signed or verified with the specified 
     bytes.

     @param data array of bytes
     @param off the offset to start at in the array
     @param len the length of the bytes to use in the array

     @throws SignatureException engine not properly initialized
   */
  public final void update(byte[]data, int off, int len)
    throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(data, off, len);
    else
      throw new SignatureException();
  }

  /** 
     Gets the name of the algorithm currently used.
     The names of algorithms are usually SHA/DSA or SHA/RSA.

     @return name of algorithm.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
     Returns a representation of the Signature as a String

     @return a string representing the signature
   */
  public String toString()
  {
    return (algorithm + " Signature");
  }

  /**
     Sets the specified algorithm parameter to the specified value.

     @param param parameter name
     @param value parameter value

     @throws InvalidParameterException invalid parameter, parameter 
     already set and cannot set again, a security exception, 
     etc.

     @deprecated use the other setParameter
   */
  public final void setParameter(String param, Object value)
    throws InvalidParameterException
  {
    engineSetParameter(param, value);
  }

  /**
     Sets the signature engine with the specified 
     AlgorithmParameterSpec;

     By default this always throws UnsupportedOperationException 
     if not overridden;

     @param params the parameters

     @throws InvalidParameterException invalid parameter, parameter 
     already set and cannot set again, a security exception, 
     etc.
   */
  public final void setParameter(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    engineSetParameter(params);
  }

  /**
     Gets the value for the specified algorithm parameter.

     @param param parameter name

     @return parameter value

     @throws InvalidParameterException invalid parameter

     @deprecated use the other getParameter
   */
  public final Object getParameter(String param)
    throws InvalidParameterException
  {
    return engineGetParameter(param);
  }

  /**
     Returns a clone if cloneable.

     @return a clone if cloneable.

     @throws CloneNotSupportedException if the implementation does 
     not support cloning
   */
  public Object clone() throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException();
  }
}
