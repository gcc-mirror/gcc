
/* MessageDigest.java --- The message digest interface.
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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

public abstract class MessageDigest extends MessageDigestSpi
{
  private String algorithm;
  Provider provider;
  private byte[] lastDigest;

  /**
     Creates a MessageDigest representing the specified
     algorithm.

     @param algorithm the name of digest algorithm to choose
   */
  protected MessageDigest(String algorithm)
  {
    this.algorithm = algorithm;
    provider = null;
  }

  /** 
     Gets an instance of the MessageDigest class representing
     the specified digest. If the algorithm is not found then, 
     it throws NoSuchAlgorithmException.

     @param algorithm the name of digest algorithm to choose
     @return a MessageDigest representing the desired algorithm

     @exception NoSuchAlgorithmException if the algorithm is not implemented by
    					 providers
   */
  public static MessageDigest getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    for (int i = 0; i < p.length; i++)
      {
	try
	  {
	    return getInstance(algorithm, p[i]);
	  }
	catch (NoSuchAlgorithmException ignored) {}
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /** 
     Gets an instance of the MessageDigest class representing
     the specified digest from the specified provider. If the 
     algorithm is not found then, it throws NoSuchAlgorithmException.
     If the provider is not found, then it throws
     NoSuchProviderException.

     @param algorithm the name of digest algorithm to choose
     @param provider the name of the provider to find the algorithm in
     @return a MessageDigest representing the desired algorithm

     @exception NoSuchAlgorithmException if the algorithm is not implemented by
     					 the provider
     @exception NoSuchProviderException if the provider is not found
   */

  public static MessageDigest getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);

    if (p == null)
      throw new NoSuchProviderException(provider);

    return getInstance(algorithm, p);
  }

  private static MessageDigest getInstance(String algorithm, Provider p)
    throws NoSuchAlgorithmException
  {
    // try the name as is
    String className = p.getProperty("MessageDigest." + algorithm);
    if (className == null) { // try all uppercase
      String upper = algorithm.toUpperCase();
      className = p.getProperty("MessageDigest." + upper);
      if (className == null) { // try if it's an alias
        String alias = p.getProperty("Alg.Alias.MessageDigest." +algorithm);
        if (alias == null) { // try all-uppercase alias name
          alias = p.getProperty("Alg.Alias.MessageDigest." +upper);
          if (alias == null) { // spit the dummy
            throw new NoSuchAlgorithmException(algorithm);
          }
        }
        className = p.getProperty("MessageDigest." + alias);
        if (className == null) {
          throw new NoSuchAlgorithmException(algorithm);
        }
      }
    }
    return getInstance(className, algorithm, p);
  }

  private static MessageDigest getInstance(String classname,
					   String algorithm,
					   Provider provider)
    throws NoSuchAlgorithmException
  {
    if (classname == null)
      throw new NoSuchAlgorithmException(algorithm);

    MessageDigest result = null;
    try
      {
        Object obj = Class.forName(classname).newInstance();
        if (obj instanceof MessageDigest) {
          result = (MessageDigest) obj;
          result.algorithm = algorithm;
        } else if (obj instanceof MessageDigestSpi) {
          result = new DummyMessageDigest((MessageDigestSpi) obj, algorithm);
        } else {
          throw new ClassCastException("Class "+classname+" from Provider "
              +provider.getName()
              +" does not extend java.security.MessageDigestSpi");
        }
        result.provider = provider;
        return result;
      }
    catch (ClassNotFoundException cnfe)
      {
	throw new NoSuchAlgorithmException(algorithm + ": Class not found.");
      }
    catch (InstantiationException ie)
      {
	throw new NoSuchAlgorithmException(algorithm
					   + ": Class instantiation failed.");
      }
    catch (IllegalAccessException iae)
      {
	throw new NoSuchAlgorithmException(algorithm + ": Illegal Access");
      }
  }


  /**
     Gets the provider that the MessageDigest is from.

     @return the provider the this MessageDigest
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
     Updates the digest with the byte.

     @param input byte to update the digest with
   */
  public void update(byte input)
  {
    engineUpdate(input);
  }

  /**
     Updates the digest with the bytes from the array from the
     specified offset to the specified length.

     @param input bytes to update the digest with
     @param offset the offset to start at
     @param len length of the data to update with
   */
  public void update(byte[]input, int offset, int len)
  {
    engineUpdate(input, offset, len);
  }

  /**
     Updates the digest with the bytes from the array.

     @param input bytes to update the digest with
   */
  public void update(byte[]input)
  {
    engineUpdate(input, 0, input.length);
  }

  /**
     Computes the digest of the stored data.

     @return a byte array representing the message digest
   */
  public byte[] digest()
  {
    return lastDigest = engineDigest();
  }

  /**
     Computes the final digest of the stored bytes and returns
     them. 

     @param buf An array of bytes to store the digest
     @param offset An offset to start storing the digest at
     @param len The length of the buffer
     @return Returns the length of the buffer
   */
  public int digest(byte[]buf, int offset, int len) throws DigestException
  {
    return engineDigest(buf, offset, len);
  }

  /**
     Computes a final update using the input array of bytes,
     then computes a final digest and returns it. It calls 
     update(input) and then digest();

     @param input An array of bytes to perform final update with
     @return a byte array representing the message digest
   */
  public byte[] digest(byte[]input)
  {
    update(input);
    return digest();
  }

  /**
     Returns a representation of the MessageDigest as a String.

     @return a string representing the message digest
   */
  public String toString()
  {
    return (getClass()).getName()
      + " Message Digest <" + digestToString() + ">";
  }

  /**
     Does a simple byte comparison of the two digests.

     @param digesta first digest to compare
     @param digestb second digest to compare
     @return true if they are equal, false otherwise
   */
  public static boolean isEqual(byte[]digesta, byte[]digestb)
  {
    if (digesta.length != digestb.length)
      return false;

    for (int i = digesta.length - 1; i >= 0; --i)
      if (digesta[i] != digestb[i])
	return false;

    return true;
  }


  /**
     Resets the message digest.
   */
  public void reset()
  {
    engineReset();
  }

  /** 
     Gets the name of the algorithm currently used.
     The names of algorithms are usually SHA-1 or MD5.

     @return name of algorithm.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
     Gets the length of the message digest.
     The default is zero which means that this message digest
     does not implement this function.

     @return length of the message digest
   */
  public final int getDigestLength()
  {
    return engineGetDigestLength();
  }

  /**
     Returns a clone of this class if supported.
     If it does not then it throws CloneNotSupportedException.
     The cloning of this class depends on whether the subclass
     MessageDigestSpi implements Cloneable which contains the
     actual implementation of the appropriate algorithm.

     @return clone of this class

     @exception CloneNotSupportedException this class does not support cloning
   */
  public Object clone() throws CloneNotSupportedException
  {
    if (this instanceof Cloneable)
      return super.clone();
    else
      throw new CloneNotSupportedException();
  }

  private String digestToString()
  {
    byte[] digest = lastDigest;

    if (digest == null)
      return "incomplete";

    StringBuffer buf = new StringBuffer();
    int len = digest.length;
    for (int i = 0; i < len; ++i)
      {
	byte b = digest[i];
	byte high = (byte) ((b & 0xff) >>> 4);
	byte low = (byte) (b & 0xf);

	buf.append(high > 9 ? ('a' - 10) + high : '0' + high);
	buf.append(low > 9 ? ('a' - 10) + low : '0' + low);
      }

    return buf.toString();
  }

}
