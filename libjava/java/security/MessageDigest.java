// MessageDigest.java

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.security;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date February 11, 2000.
 */

/**
 * Written using on-line Java Platform 1.1 API Specification.
 * Status:  Believed complete and correct to 1.1 spec.
 * It is known not to comply with the 1.2 spec.
 */

public abstract class MessageDigest
{
  protected MessageDigest (String algorithm)
  {
    name = algorithm;
  }

  public static MessageDigest getInstance (String algorithm)
    throws NoSuchAlgorithmException
  {
    String name = "MessageDigest." + algorithm;
    Provider[] provs = Security.getProviders ();
    for (int i = 0; i < provs.length; ++i)
      {
	String val = provs[i].getProperty (name);
	if (val != null)
	  {
	    try
	      {
		return (MessageDigest) Class.forName(val).newInstance ();
	      }
	    catch (Throwable _)
	      {
		// We just ignore failures.
	      }
	  }
      }

    throw new NoSuchAlgorithmException (algorithm);
  }

  public static MessageDigest getInstance (String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    String name = "MessageDigest." + algorithm;
    Provider p = Security.getProvider (provider);
    if (p == null)
      throw new NoSuchProviderException (provider);
    String val = p.getProperty (name);
    if (val != null)
      {
	try
	  {
	    return (MessageDigest) Class.forName(val).newInstance ();
	  }
	catch (Throwable _)
	  {
	    // Nothing.
	  }
      }

    throw new NoSuchAlgorithmException (algorithm);
  }

  public void update (byte input)
  {
    engineUpdate (input);
  }

  public void update (byte[] input, int offset, int len)
  {
    engineUpdate (input, offset, len);
  }

  public void update (byte[] input)
  {
    engineUpdate (input, 0, input.length);
  }

  public byte[] digest ()
  {
    return engineDigest ();
  }

  public byte[] digest (byte[] input)
  {
    update (input);
    return engineDigest ();
  }

  public String toString ()
  {
    // There is no spec for this.
    return "[MessageDigest: " + name + "]";
  }

  public static boolean isEqual (byte[] digesta, byte[] digestb)
  {
    if (digesta == digestb)
      return true;
    if (digesta.length != digestb.length)
      return false;
    for (int i = digesta.length - 1; i >= 0; --i)
      if (digesta[i] != digestb[i])
	return false;
    return true;
  }

  public void reset ()
  {
    engineReset ();
  }

  public final String getAlgorithm ()
  {
    return name;
  }

  protected abstract void engineUpdate (byte input);
  protected abstract void engineUpdate (byte input[], int offset, int len);
  protected abstract byte[] engineDigest ();
  protected abstract void engineReset ();

  public Object clone() throws CloneNotSupportedException
  {
    return super.clone ();
  }

  // Algorithm name.
  private String name;
}
