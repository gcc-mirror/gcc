/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.security;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date February 10, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification.
 * Status:  Partially implemented to the 1.1 spec.
 * It is known not to comply with the 1.2 spec.
 */

public abstract class KeyPairGenerator extends KeyPairGeneratorSpi
{
  protected KeyPairGenerator(String algorithm)
  {
    name = algorithm;
  }

  public static KeyPairGenerator getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    String name = "KeyPairGenerator." + algorithm;
    Provider[] provs = Security.getProviders();
    for (int i = 0; i < provs.length; ++i)
      {
	String val = provs[i].getProperty(name);
	if (val != null)
	  {
	    try
	      {
		return (KeyPairGenerator) Class.forName(val).newInstance();
	      }
	    catch (Throwable _)
	      {
		// We just ignore failures.
	      }
	  }
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  public static KeyPairGenerator getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    String name = "KeyPairGenerator." + algorithm;
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    String val = p.getProperty(name);
    if (val != null)
      {
	try
	  {
	    return (KeyPairGenerator) Class.forName(val).newInstance();
	  }
	catch (Throwable _)
	  {
	    // Nothing.
	  }
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  public String getAlgorithm()
  {
    return name;
  }

  public abstract void initialize(int strength, SecureRandom random);
  public abstract KeyPair generateKeyPair();

  // Algorithm name.
  private String name;
}
