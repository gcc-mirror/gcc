/* SecureRandom.java --- Secure Random class implmentation
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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
import java.io.Serializable;
import java.util.Random;
import java.util.Enumeration;

/**
   SecureRandom is the class interface for using SecureRandom
   providers. It provides an interface to the SecureRandomSpi
   engine so that programmers can generate pseudo-random numbers.

   @author Mark Benvenuto <ivymccough@worldnet.att.net>
 */
public class SecureRandom extends Random
{
  //Serialized Field
  long counter = 0;		//Serialized
  MessageDigest digest = null;
  Provider provider = null;
  byte[] randomBytes = null;	//Always null
  int randomBytesUsed = 0;
  SecureRandomSpi secureRandomSpi = null;
  byte[] state = null;

  /**
     Default constructor for SecureRandom. It constructs a 
     new SecureRandom by instantating the first SecureRandom 
     algorithm in the default security provier. 

     It is not seeded and should be seeded using setSeed or else
     on the first call to getnextBytes it will force a seed.

     It is maintained for backwards compatibility and programs
     should use getInstance.
   */
  public SecureRandom()
  {
    Provider p[] = Security.getProviders();

    //Format of Key: SecureRandom.algname
    String key;

    String classname = null;
    int i, flag = 0;
    Enumeration e;
    for (i = 0; i < p.length; i++)
      {
	e = p[i].propertyNames();
	while (e.hasMoreElements())
	  {
	    key = (String) e.nextElement();
	    if (key.startsWith("SecureRandom."))
	      if ((classname = p[i].getProperty(key)) != null)
		break;
	  }
	if (classname != null)
	    break;
      }

    //if( classname == null)
    //  throw new NoSuchAlgorithmException();

    try
      {
	this.secureRandomSpi =
	  (SecureRandomSpi) Class.forName(classname).newInstance();

	//s.algorithm = algorithm;
	this.provider = p[i];
      }
    catch (ClassNotFoundException cnfe)
      {
	//throw new NoSuchAlgorithmException("Class not found");
      }
    catch (InstantiationException ie)
      {
	//throw new NoSuchAlgorithmException("Class instantiation failed");
      }
    catch (IllegalAccessException iae)
      {
	//throw new NoSuchAlgorithmException("Illegal Access");
      }
  }

  /**
     A constructor for SecureRandom. It constructs a new 
     SecureRandom by instantating the first SecureRandom algorithm 
     in the default security provier. 

     It is seeded with the passed function and is useful if the user
     has access to hardware random device (like a radiation detector).

     It is maintained for backwards compatibility and programs
     should use getInstance.

     @param seed Seed bytes for class
   */
  public SecureRandom(byte[] seed)
  {
    this();
    setSeed(seed);
  }

  /**
     A constructor for SecureRandom. It constructs a new 
     SecureRandom using the specified SecureRandomSpi from
     the specified security provier. 

     @param secureRandomSpi A SecureRandomSpi class
     @param provider A Provider class
   */
  protected SecureRandom(SecureRandomSpi secureRandomSpi, Provider provider)
  {
    this.secureRandomSpi = secureRandomSpi;
    this.provider = provider;
  }

  /**
     Returns an instance of a SecureRandom. It creates the class
     for the specified algorithm if it exists from a provider.

     @param algorithm A SecureRandom algorithm to use

     @return Returns a new SecureRandom implmenting the chosen algorithm

     @throws NoSuchAlgorithmException if the algorithm cannot be found
   */
  public static SecureRandom getInstance(String algorithm) throws
    NoSuchAlgorithmException
  {
    Provider p[] = Security.getProviders();

    //Format of Key: SecureRandom.algname
    StringBuffer key = new StringBuffer("SecureRandom.");
    key.append(algorithm);

    String classname = null;
    int i;
    for (i = 0; i < p.length; i++)
      {
	if ((classname = p[i].getProperty(key.toString())) != null)
	  break;
      }

    if (classname == null)
        throw new NoSuchAlgorithmException();

    try
      {
	return new SecureRandom((SecureRandomSpi) Class.forName(classname).
				newInstance(), p[i]);
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
     Returns an instance of a SecureRandom. It creates the class
     for the specified algorithm from the specified provider.

     @param algorithm A SecureRandom algorithm to use
     @param provider A security provider to use

     @return Returns a new SecureRandom implmenting the chosen algorithm

     @throws NoSuchAlgorithmException if the algorithm cannot be found
     @throws NoSuchProviderException if the provider cannot be found
   */
  public static SecureRandom getInstance(String algorithm,
					 String provider) throws
    NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException();

    //Format of Key: SecureRandom.algName
    StringBuffer key = new StringBuffer("SecureRandom.");
    key.append(algorithm);

    String classname = p.getProperty(key.toString());
    if (classname == null)
      throw new NoSuchAlgorithmException();

    try
      {
	return new SecureRandom((SecureRandomSpi) Class.forName(classname).
				newInstance(), p);
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
     Returns the provider being used by the current SecureRandom class.

     @return The provider from which this SecureRandom was attained
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
     Seeds the SecureRandom. The class is re-seeded for each call and 
     each seed builds on the previous seed so as not to weaken security.

     @param seed seed bytes to seed with
   */
  public void setSeed(byte[] seed)
  {
    secureRandomSpi.engineSetSeed(seed);
  }

  /**
     Seeds the SecureRandom. The class is re-seeded for each call and 
     each seed builds on the previous seed so as not to weaken security.

     @param seed 8 seed bytes to seed with
   */
  public void setSeed(long seed)
  {
    // This particular setSeed will be called by Random.Random(), via
    // our own constructor, before secureRandomSpi is initialized.  In
    // this case we can't call a method on secureRandomSpi, and we
    // definitely don't want to throw a NullPointerException.
    // Therefore we test.
    if (secureRandomSpi != null)
      {
        byte tmp[] = { (byte) (0xff & (seed >> 56)),
		       (byte) (0xff & (seed >> 48)),
		       (byte) (0xff & (seed >> 40)),
		       (byte) (0xff & (seed >> 32)),
		       (byte) (0xff & (seed >> 24)),
		       (byte) (0xff & (seed >> 16)),
		       (byte) (0xff & (seed >> 8)),
		       (byte) (0xff & seed)
	};
	secureRandomSpi.engineSetSeed(tmp);
      }
  }

  /**
     Generates a user specified number of bytes. This function
     is the basis for all the random functions.

     @param bytes array to store generated bytes in
   */
  public void nextBytes(byte[] bytes)
  {
    randomBytesUsed += bytes.length;
    counter++;
    secureRandomSpi.engineNextBytes(bytes);
  }

  /**
     Generates an integer containing the user specified
     number of random bits. It is right justified and padded
     with zeros.

     @param numBits number of random bits to get, 0 <= numBits <= 32;

     @return the random bits
   */
  protected final int next(int numBits)
  {
    if (numBits == 0)
      return 0;

    byte tmp[] = new byte[numBits / 8 + (1 * (numBits % 8))];

    secureRandomSpi.engineNextBytes(tmp);
    randomBytesUsed += tmp.length;
    counter++;

    int ret = 0;

    for (int i = 0; i < tmp.length; i++)
      ret |= tmp[i] << (8 * i);

    return ret;
  }

  /**
     Returns the given number of seed bytes. This method is
     maintained only for backwards capability. 

     @param numBytes number of seed bytes to get

     @return an array containing the seed bytes
   */
  public static byte[] getSeed(int numBytes)
  {
    byte tmp[] = new byte[numBytes];

    new Random().nextBytes(tmp);
    return tmp;
    //return secureRandomSpi.engineGenerateSeed( numBytes );
  }

  /**
     Returns the specified number of seed bytes.

     @param numBytes number of seed bytes to get

     @return an array containing the seed bytes
   */
  public byte[] generateSeed(int numBytes)
  {
    return secureRandomSpi.engineGenerateSeed(numBytes);
  }

}
