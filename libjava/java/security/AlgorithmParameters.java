/* AlgorithmParameters.java --- Algorithm Parameters Implementation Class
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
import java.security.spec.InvalidParameterSpecException;
import java.security.spec.AlgorithmParameterSpec;
import java.io.IOException;

/**
   AlgorithmParameters is the Algorithm Parameters class which 
   provides an interface through which to modify parameters for 
   classes. This class is used to manage the algorithm parameters.

   @since JDK 1.2
   @author Mark Benvenuto
 */
public class AlgorithmParameters
{
  private AlgorithmParametersSpi paramSpi;
  private Provider provider;
  private String algorithm;

  /**
     Creates an instance of AlgorithmParameters

     @param paramSpi A parameters engine to use
     @param provider A provider to use
     @param algorithm The algorithm 
   */
  protected AlgorithmParameters(AlgorithmParametersSpi paramSpi,
				Provider provider, String algorithm)
  {
    this.paramSpi = paramSpi;
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
     Gets an instance of the AlgorithmParameters class representing
     the specified algorithm parameters. If the algorithm is not 
     found then, it throws NoSuchAlgorithmException.

     The returned AlgorithmParameters must still be intialized with
     init().

     @param algorithm the name of algorithm to choose
     @return a AlgorithmParameters repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by providers
   */
  public static AlgorithmParameters getInstance(String algorithm) throws
    NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();

    for (int i = 0; i < p.length; i++)
      {
	String classname =
	  p[i].getProperty("AlgorithmParameters." + algorithm);
	if (classname != null)
	  return getInstance(classname, algorithm, p[i]);
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /** 
     Gets an instance of the AlgorithmParameters class representing
     the specified algorithm parameters from the specified provider. 
     If the algorithm is not found then, it throws 
     NoSuchAlgorithmException. If the provider is not found, then 
     it throws NoSuchProviderException.

     The returned AlgorithmParameters must still be intialized with
     init().

     @param algorithm the name of algorithm to choose
     @param provider the name of the provider to find the algorithm in
     @return a AlgorithmParameters repesenting the desired algorithm

     @throws NoSuchAlgorithmException if the algorithm is not implemented by the provider
     @throws NoSuchProviderException if the provider is not found
   */
  public static AlgorithmParameters getInstance(String algorithm,
						String provider) throws
    NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException();

    return getInstance(p.getProperty("AlgorithmParameters." + algorithm),
		       algorithm, p);
  }

  private static AlgorithmParameters getInstance(String classname,
						 String algorithm,
						 Provider provider)
    throws NoSuchAlgorithmException
  {

    try
      {
	return new AlgorithmParameters((AlgorithmParametersSpi) Class.
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
     Initializes the engine with the specified 
     AlgorithmParameterSpec class.

     @param paramSpec A AlgorithmParameterSpec to initialize with

     @throws InvalidParameterSpecException For an inapporiate ParameterSpec class
   */
  public final void init(AlgorithmParameterSpec paramSpec) throws
    InvalidParameterSpecException
  {
    paramSpi.engineInit(paramSpec);
  }

  /**
     Initializes the engine with the specified 
     parameters stored in the byte array and decodes them
     according to the ASN.1 specification. If the ASN.1
     specification exists then it succeeds or else it throws
     IOException.

     @param params Parameters to initialize with

     @throws IOException Decoding Error
   */
  public final void init(byte[]params) throws IOException
  {
    paramSpi.engineInit(params);
  }

  /**
     Initializes the engine with the specified 
     parameters stored in the byte array and decodes them
     according to the specified decoding specification. 
     If format is null, then it is decoded using the ASN.1 
     specification if it exists or else it throws
     IOException.

     @param params Parameters to initialize with
     @param format Name of decoding format to use

     @throws IOException Decoding Error
   */
  public final void init(byte[]params, String format) throws IOException
  {
    paramSpi.engineInit(params, format);
  }

  /**
     Returns a specification of this AlgorithmParameters object.
     paramSpec identifies the class to return the AlgortihmParameters
     in. 

     @param paramSpec Class to return AlgorithmParameters in

     @return the parameter specification

     @throws InvalidParameterSpecException if the paramSpec is an invalid parameter class
   */
  public final AlgorithmParameterSpec getParameterSpec(Class paramSpec) throws
    InvalidParameterSpecException
  {
    return paramSpi.engineGetParameterSpec(paramSpec);
  }

  /**
     Returns the parameters in the default encoding format. 
     The primary encoding format is ASN.1 format if it exists
     for the specified type.

     @return byte array representing the parameters
   */
  public final byte[] getEncoded() throws IOException
  {
    return paramSpi.engineGetEncoded();
  }

  /**
     Returns the parameters in the specified encoding format. 
     If <code>format</code> is <code>null</code> then the 
     primary encoding format is used, the ASN.1 format, 
     if it exists for the specified type.

     @return byte array representing the parameters
   */
  public final byte[] getEncoded(String format) throws IOException
  {
    return paramSpi.engineGetEncoded(format);
  }

  /**
     Returns a string representation of the encoding format

     @return a string containing the string representation
   */
  public final String toString()
  {
    return paramSpi.engineToString();
  }
}
