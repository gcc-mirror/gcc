/* KeyFactory.java --- Key Factory Class
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
import java.security.spec.KeySpec;
import java.security.spec.InvalidKeySpecException;

/**
   Key factories are used to convert keys (opaque cryptographic 
   keys of type Key) into key specifications (transparent 
   representations of the underlying key material).

   Key factories are bi-directional. They allow a key class 
   to be converted into a key specification (key material) and
   back again.

   For example DSA public keys can be specified as 
   DSAPublicKeySpec or X509EncodedKeySpec. The key factory
   translate these key specifications. 

   @since JDK 1.2
   @author Mark Benvenuto
 */
public class KeyFactory
{
  private KeyFactorySpi keyFacSpi;
  private Provider provider;
  private String algorithm;

  /**
     Constructs a new keyFactory with the specified parameters.

     @param keyFacSpi Key Factory SPI to use
     @param provider the provider of the Key Factory SPI
     @param algorithm the name of the key algorithm for this key factory
   */
  protected KeyFactory(KeyFactorySpi keyFacSpi, Provider provider,
		       String algorithm)
  {
    this.keyFacSpi = keyFacSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /** 
     Gets an instance of the KeyFactory class representing
     the specified key factory. If the algorithm is not 
     found then, it throws NoSuchAlgorithmException.

     @param algorithm the name of algorithm to choose
     @return a KeyFactory repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by providers
   */
  public static KeyFactory getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();

    for (int i = 0; i < p.length; i++)
      {
	String classname = p[i].getProperty("KeyFactory." + algorithm);
	if (classname != null)
	  return getInstance(classname, algorithm, p[i]);
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /** 
     Gets an instance of the KeyFactory class representing
     the specified key factory from the specified provider. 
     If the algorithm is not found then, it throws 
     NoSuchAlgorithmException. If the provider is not found, then 
     it throws NoSuchProviderException.

     @param algorithm the name of algorithm to choose
     @param provider the name of the provider to find the algorithm in
     @return a KeyFactory repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by the provider
     @throws NoSuchProviderException if the provider is not found
   */
  public static KeyFactory getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException();

    return getInstance(p.getProperty("KeyFactory." + algorithm),
		       algorithm, p);
  }

  private static KeyFactory getInstance(String classname,
					String algorithm,
					Provider provider)
    throws NoSuchAlgorithmException
  {

    try
      {
	return new KeyFactory((KeyFactorySpi) Class.forName(classname).
			      newInstance(), provider, algorithm);
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
     Gets the provider that the class is from.

     @return the provider of this class
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
     Returns the name of the algorithm used

     @return A string with the name of the algorithm
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
     Generates a public key from the provided key specification.

     @param keySpec key specification

     @return the public key

     @throws InvalidKeySpecException invalid key specification for
     this key factory to produce a public key
   */
  public final PublicKey generatePublic(KeySpec keySpec) throws
    InvalidKeySpecException
  {
    return keyFacSpi.engineGeneratePublic(keySpec);
  }

  /**
     Generates a private key from the provided key specification.

     @param keySpec key specification

     @return the private key

     @throws InvalidKeySpecException invalid key specification for
     this key factory to produce a private key
   */
  public final PrivateKey generatePrivate(KeySpec keySpec) throws
    InvalidKeySpecException
  {
    return keyFacSpi.engineGeneratePrivate(keySpec);
  }

  /**
     Returns a key specification for the given key. keySpec 
     identifies the specification class to return the key 
     material in.

     @param key the key
     @param keySpec the specification class to return the 
     key material in.

     @return the key specification in an instance of the requested
     specification class

     @throws InvalidKeySpecException the requested key specification
     is inappropriate for this key or the key is 
     unrecognized.
   */
  public final KeySpec getKeySpec(Key key, Class keySpec)
    throws InvalidKeySpecException
  {
    return keyFacSpi.engineGetKeySpec(key, keySpec);
  }

  /**
     Translates the key from an unknown or untrusted provider
     into a key for this key factory.

     @param the key from an unknown or untrusted provider

     @return the translated key

     @throws InvalidKeySpecException if the key cannot be 
     processed by this key factory
   */
  public final Key translateKey(Key key) throws InvalidKeyException
  {
    return keyFacSpi.engineTranslateKey(key);
  }
}
