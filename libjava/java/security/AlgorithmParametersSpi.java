/* AlgorithmParametersSpi.java --- Algorithm Parameters SPI
   Copyright (C) 1999, 2004  Free Software Foundation, Inc.

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

import java.io.IOException;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;

/**
 * AlgorithmParametersSpi is the Service Provider Interface
 * for the Algorithm Parameters class. This class is used
 * to manage the algorithm parameters.
 *
 * @since 1.2
 * @author Mark Benvenuto
 */
public abstract class AlgorithmParametersSpi
{
  /**
   * Creates a new instance of AlgorithmParametersSpi
   */
  public AlgorithmParametersSpi()
  {
  }

  /**
   * Initializes the engine with the specified 
   * AlgorithmParameterSpec class.
   *
   * @param paramSpec A AlgorithmParameterSpec to initialize with
   *
   * @throws InvalidParameterSpecException For an inapporiate
   * ParameterSpec class
   */
  protected abstract void engineInit(AlgorithmParameterSpec paramSpec)
    throws InvalidParameterSpecException;

  /**
   * Initializes the engine with the specified 
   * parameters stored in the byte array and decodes them
   * according to the ASN.1 specification. If the ASN.1
   * specification exists then it succeeds or else it throws
   * IOException.
   *
   * @param params Parameters to initialize with
   *
   * @throws IOException Decoding Error
   */
  protected abstract void engineInit(byte[]params) throws IOException;

  /**
   * Initializes the engine with the specified 
   * parameters stored in the byte array and decodes them
   * according to the specified decoding specification. 
   * If format is null, then it is decoded using the ASN.1 
   * specification if it exists or else it throws
   * IOException.
   *
   * @param params Parameters to initialize with
   * @param format Name of decoding format to use
   *
   * @throws IOException Decoding Error
   */
  protected abstract void engineInit(byte[]params, String format)
    throws IOException;


  /**
   * Returns a specification of this AlgorithmParameters object.
   * paramSpec identifies the class to return the AlgortihmParameters
   * in.
   *
   * @param paramSpec Class to return AlgorithmParameters in
   *
   * @return the parameter specification
   *
   * @throws InvalidParameterSpecException if the paramSpec is an
   * invalid parameter class
   */
  protected abstract AlgorithmParameterSpec engineGetParameterSpec(Class
								   paramSpec)
    throws InvalidParameterSpecException;


  /**
   * Returns the parameters in the default encoding format. 
   * The primary encoding format is ASN.1 format if it exists
   * for the specified type.
   *
   * @return byte array representing the parameters
   */
  protected abstract byte[] engineGetEncoded() throws IOException;


  /**
   * Returns the parameters in the specified encoding format. 
   * If <code>format</code> is <code>null</code> then the 
   * primary encoding format is used, the ASN.1 format, 
   * if it exists for the specified type.
   *
   * @return byte array representing the parameters
   */
  protected abstract byte[] engineGetEncoded(String format)
    throws IOException;

  /**
   * Returns a string describing the parameters in the 
   * AlgorithmParametersSpi class.
   *
   * @return A string representing the format of the parameters.
   */
  protected abstract String engineToString();
}
