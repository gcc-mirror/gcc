/* SignatureSpi.java --- Signature Service Provider Interface
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
   SignatureSpi defines the Service Provider Interface (SPI)
   for the Signature class. The signature class provides an 
   interface to a digital signature algorithm. Digital signatures
   are used for authentication and integrity of data.

   @author Mark Benvenuto <ivymccough@worldnet.att.net>

   @since JDK 1.2
 */
public abstract class SignatureSpi
{
  /**
     Source of randomness
   */
  protected SecureRandom appRandom;

  /**
     Creates a new instance of SignatureSpi.
   */
  public SignatureSpi()
  {
    appRandom = null;
  }

  /**
     Initializes this class with the public key for 
     verification purposes.

     @param publicKey the public key to verify with

     @throws InvalidKeyException invalid key
   */
  protected abstract void engineInitVerify(PublicKey publicKey)
    throws InvalidKeyException;

  /**
     Initializes this class with the private key for 
     signing purposes.

     @param privateKey the private key to sign with

     @throws InvalidKeyException invalid key
   */
  protected abstract void engineInitSign(PrivateKey privateKey)
    throws InvalidKeyException;

  /**
     Initializes this class with the private key and source 
     of randomness for signing purposes.

     This cannot be abstract backward compatibility reasons

     @param privateKey the private key to sign with
     @param random Source of randomness

     @throws InvalidKeyException invalid key

     @since JDK 1.2
   */
  protected void engineInitSign(PrivateKey privateKey, SecureRandom random)
    throws InvalidKeyException
  {
    appRandom = random;
    engineInitSign(privateKey);
  }

  /**
     Updates the data to be signed or verified with the specified 
     byte.

     @param b byte to update with

     @throws SignatureException Engine not properly initialized
   */
  protected abstract void engineUpdate(byte b) throws SignatureException;

  /**
     Updates the data to be signed or verified with the specified 
     bytes.

     @param b array of bytes
     @param off the offset to start at in the array
     @param len the length of the bytes to use in the array

     @throws SignatureException engine not properly initialized
   */
  protected abstract void engineUpdate(byte[] b, int off, int len)
    throws SignatureException;

  /**
     Returns the signature bytes of all the data fed to this class.
     The format of the output depends on the underlying signature
     algorithm.

     @return the signature

     @throws SignatureException engine not properly initialized
   */
  protected abstract byte[] engineSign() throws SignatureException;

  /**
     Generates signature bytes of all the data fed to this class 
     and outputs it to the passed array. The format of the 
     output depends on the underlying signature algorithm.

     This cannot be abstract backward compatibility reasons.
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
  protected int engineSign(byte[] outbuf, int offset, int len)
    throws SignatureException
  {
    byte tmp[] = engineSign();

    if (tmp.length > len)
      throw new SignatureException("Invalid Length");

    System.arraycopy(outbuf, offset, tmp, 0, tmp.length);

    return tmp.length;
  }

  /**
     Verifies the passed signature.

     @param sigBytes the signature bytes to verify

     @return true if verified, false otherwise

     @throws SignatureException engine not properly initialized
     or wrong signature
   */
  protected abstract boolean engineVerify(byte[] sigBytes)
    throws SignatureException;

  /**
     Sets the specified algorithm parameter to the specified value.

     @param param parameter name
     @param value parameter value

     @throws InvalidParameterException invalid parameter, parameter 
     already set and cannot set again, a security exception, 
     etc.

     @deprecated use the other setParameter
   */
  protected abstract void engineSetParameter(String param, Object value)
    throws InvalidParameterException;

  /**
     Sets the signature engine with the specified 
     AlgorithmParameterSpec;

     This cannot be abstract backward compatibility reasons
     By default this always throws UnsupportedOperationException 
     if not overridden;

     @param params the parameters

     @throws InvalidParameterException invalid parameter, parameter 
     already set and cannot set again, a security exception, 
     etc.
   */
  protected void engineSetParameter(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    throw new UnsupportedOperationException();
  }

  /**
     Gets the value for the specified algorithm parameter.

     @param param parameter name

     @return parameter value

     @throws InvalidParameterException invalid parameter

     @deprecated use the other getParameter
   */
  protected abstract Object engineGetParameter(String param)
    throws InvalidParameterException;

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
