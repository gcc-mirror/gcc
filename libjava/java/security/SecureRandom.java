/* SecureRandom.java --- Secure Random class implementation
   Copyright (C) 1999, 2001, 2002, 2003 Free Software Foundation, Inc.

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

import gnu.java.security.Engine;

/**
 * An interface to a cryptographically secure pseudo-random number
 * generator (PRNG). Random (or at least unguessable) numbers are used
 * in all areas of security and cryptography, from the generation of
 * keys and initialization vectors to the generation of random padding
 * bytes.
 *
 * @author Mark Benvenuto <ivymccough@worldnet.att.net>
 * @author Casey Marshall
 */
public class SecureRandom extends Random
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** Service name for PRNGs. */
  private static final String SECURE_RANDOM = "SecureRandom";

  private static final long serialVersionUID = 4940670005562187L;

  //Serialized Field
  long counter = 0;		//Serialized
  Provider provider = null;
  byte[] randomBytes = null;	//Always null
  int randomBytesUsed = 0;
  SecureRandomSpi secureRandomSpi = null;
  byte[] state = null;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
     Default constructor for SecureRandom. It constructs a 
     new SecureRandom by instantating the first SecureRandom 
     algorithm in the default security provier. 

     It is not seeded and should be seeded using setSeed or else
     on the first call to getnextBytes it will force a seed.

     It is maintained for backwards compatibility and programs
     should use {@link #getInstance(java.lang.String)}.
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
            if (key.startsWith("SECURERANDOM."))
              {
                if ((classname = p[i].getProperty(key)) != null)
                  {
                    try
                      {
                        secureRandomSpi = (SecureRandomSpi) Class.
                          forName(classname).newInstance();
                        provider = p[i];
                        return;
                      }
                    catch (Throwable ignore) { }
                  }
              }
          }
      }

    // Nothing found. Fall back to SHA1PRNG
    secureRandomSpi = new gnu.java.security.provider.SHA1PRNG();
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

  // Class methods.
  // ------------------------------------------------------------------------

  /**
   * Returns an instance of a SecureRandom. It creates the class from
   * the first provider that implements it.
   *
   * @param algorithm The algorithm name.
   * @return A new SecureRandom implementing the given algorithm.
   * @throws NoSuchAlgorithmException If no installed provider implements
   *         the given algorithm.
   */
  public static SecureRandom getInstance(String algorithm) throws
    NoSuchAlgorithmException
  {
    Provider p[] = Security.getProviders();
    for (int i = 0; i < p.length; i++)
      {
        try
          {
            return getInstance(algorithm, p[i]);
          }
        catch (NoSuchAlgorithmException ignored)
          {
          }
      }

    // None found.
    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Returns an instance of a SecureRandom. It creates the class
   * for the specified algorithm from the named provider.
   *
   * @param algorithm The algorithm name.
   * @param provider  The provider name.
   * @return A new SecureRandom implementing the chosen algorithm.
   * @throws NoSuchAlgorithmException If the named provider does not implement
   *         the algorithm, or if the implementation cannot be
   *         instantiated.
   * @throws NoSuchProviderException If no provider named
   *         <code>provider</code> is currently installed.
   * @throws IllegalArgumentException If <code>provider</code> is null
   *         or is empty.
   */
  public static SecureRandom getInstance(String algorithm, String provider)
  throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null || provider.length() == 0)
      throw new IllegalArgumentException("Illegal provider");

    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException();
    
    return getInstance(algorithm, p);
  }

  /**
   * Returns an instance of a SecureRandom. It creates the class for
   * the specified algorithm from the given provider.
   *
   * @param algorithm The SecureRandom algorithm to create.
   * @param provider  The provider to get the instance from.
   * @throws NoSuchAlgorithmException If the algorithm cannot be found, or
   *         if the class cannot be instantiated.
   * @throws IllegalArgumentException If <code>provider</code> is null.
   */
  public static SecureRandom getInstance(String algorithm, Provider provider)
  throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("Illegal provider");
    try
      {
        return new SecureRandom((SecureRandomSpi)
          Engine.getInstance(SECURE_RANDOM, algorithm, provider),
          provider);
      }
    catch (java.lang.reflect.InvocationTargetException ite)
      {
	throw new NoSuchAlgorithmException(algorithm);
      }
    catch (ClassCastException cce)
      {
        throw new NoSuchAlgorithmException(algorithm);
      }
  }

  // Instance methods.
  // ------------------------------------------------------------------------

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
      ret |= (tmp[i] & 0xFF) << (8 * i);

    long mask = (1L << numBits) - 1;
    return (int) (ret & mask);
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
