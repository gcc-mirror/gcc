/* AlgorithmParameterGenerator.java --- Algorithm Parameter Generator
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
   AlgorithmParameterGenerator is used to generate 
   algorithm parameters for specified algorithms.
   This class is used to generate the algorithm parameters
   for a specific algorithm.

   @since JDK 1.2
   @author Mark Benvenuto
 */
public class AlgorithmParameterGenerator
{
  private AlgorithmParameterGeneratorSpi paramGenSpi;
  private Provider provider;
  private String algorithm;

  /**
     Creates an instance of AlgorithmParameters

     @param paramSpi A parameters engine to use
     @param provider A provider to use
     @param algorithm The algorithm 
   */
  protected AlgorithmParameterGenerator(AlgorithmParameterGeneratorSpi
					paramGenSpi, Provider provider,
					String algorithm)
  {
    this.paramGenSpi = paramGenSpi;
    this.provider = provider;
    this.algorithm = algorithm;
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
     Gets an instance of the AlgorithmParameterGenerator class 
     which generates algorithm parameters for the specified algorithm. 
     If the algorithm is not found then, it throws NoSuchAlgorithmException.

     @param algorithm the name of algorithm to choose
     @return a AlgorithmParameterGenerator repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by providers
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();

    for (int i = 0; i < p.length; i++)
      {
	String classname =
	  p[i].getProperty("AlgorithmParameterGenerator." + algorithm);
	if (classname != null)
	  return getInstance(classname, algorithm, p[i]);
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /** 
     Gets an instance of the AlgorithmParameterGenerator class 
     which generates algorithm parameters for the specified algorithm. 
     If the algorithm is not found then, it throws NoSuchAlgorithmException.

     @param algorithm the name of algorithm to choose
     @param provider the name of the provider to find the algorithm in
     @return a AlgorithmParameterGenerator repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by the provider
     @throws NoSuchProviderException if the provider is not found
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm,
							String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException();

    return getInstance(p.
		       getProperty("AlgorithmParameterGenerator." +
				   algorithm), algorithm, p);
  }

  private static AlgorithmParameterGenerator getInstance(String classname,
							 String algorithm,
							 Provider provider)
    throws NoSuchAlgorithmException
  {

    try
      {
	return new
	  AlgorithmParameterGenerator((AlgorithmParameterGeneratorSpi) Class.
				      forName(classname).newInstance(),
				      provider, algorithm);
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
     Initializes the Algorithm Parameter Generator with the specified
     size. (Since no source of randomness is supplied, a default
     one is supplied).

     @param size size (in bits) to use
   */
  public final void init(int size)
  {
    init(size, new SecureRandom());
  }

  /**
     Initializes the Algorithm Parameter Generator with the specified
     size and source of randomness.

     @param size size (in bits) to use
     @param random source of randomness to use
   */
  public final void init(int size, SecureRandom random)
  {
    paramGenSpi.engineInit(size, random);
  }

  /**
     Initializes the Algorithm Parameter Generator with the specified
     AlgorithmParameterSpec. (Since no source of randomness is supplied, 
     a default one is supplied).

     @param genParamSpec the AlgorithmParameterSpec class to use
   */
  public final void init(AlgorithmParameterSpec genParamSpec) throws
    InvalidAlgorithmParameterException
  {
    init(genParamSpec, new SecureRandom());
  }

  /**
     Initializes the Algorithm Parameter Generator with the specified
     AlgorithmParameterSpec and source of randomness.

     @param genParamSpec the AlgorithmParameterSpec class to use
     @param random source of randomness to use
   */
  public final void init(AlgorithmParameterSpec genParamSpec,
			 SecureRandom random) throws
    InvalidAlgorithmParameterException
  {
    paramGenSpi.engineInit(genParamSpec, random);
  }

  /**
     Generate a new set of AlgorithmParameters.

     @returns a new set of algorithm parameters
   */
  public final AlgorithmParameters generateParameters()
  {
    return paramGenSpi.engineGenerateParameters();
  }
}
