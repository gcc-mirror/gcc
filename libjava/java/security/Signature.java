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

public abstract class Signature
{
  protected Signature (String name)
  {
    state = UNINITIALIZED;
    this.name = name;
  }

  public static Signature getInstance (String algorithm)
    throws NoSuchAlgorithmException
  {
    String name = "Signature." + algorithm;
    Provider[] provs = Security.getProviders ();
    for (int i = 0; i < provs.length; ++i)
      {
	String val = provs[i].getProperty (name);
	if (val != null)
	  {
	    try
	      {
		return (Signature) Class.forName(val).newInstance ();
	      }
	    catch (Throwable _)
	      {
		// We just ignore failures.
	      }
	  }
      }

    throw new NoSuchAlgorithmException (algorithm);
  }

  public static Signature getInstance (String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    String name = "Signature." + algorithm;
    Provider p = Security.getProvider (provider);
    if (p == null)
      throw new NoSuchProviderException (provider);
    String val = p.getProperty (name);
    if (val != null)
      {
	try
	  {
	    return (Signature) Class.forName(val).newInstance ();
	  }
	catch (Throwable _)
	  {
	    // Nothing.
	  }
      }

    throw new NoSuchAlgorithmException (algorithm);
  }

  public final void initVerify (PublicKey publicKey)
    throws InvalidKeyException
  {
    engineInitVerify (publicKey);
  }

  public final void initSign (PrivateKey privateKey)
    throws InvalidKeyException
  {
    engineInitSign (privateKey);
  }

  public final byte[] sign ()
    throws SignatureException
  {
    return engineSign ();
  }

  public final boolean verify (byte[] signature)
    throws SignatureException
  {
    return engineVerify (signature);
  }

  public final void update (byte b)
    throws SignatureException
  {
    engineUpdate (b);
  }

  public final void update (byte[] data)
    throws SignatureException
  {
    engineUpdate (data, 0, data.length);
  }

  public final void update (byte[] data, int off, int len)
    throws SignatureException
  {
    engineUpdate (data, off, len);
  }

  public final String getAlgorithm ()
  {
    return name;
  }

  public String toString ()
  {
    // There is no spec for this.  FIXME: this is a bad choice.
    return name + "; state = " + state;
  }

  public final void setParameter (String param, Object value)
    throws InvalidParameterException
  {
    engineSetParameter (param, value);
  }

  public final Object getParameter (String param)
    throws InvalidParameterException
  {
    return engineGetParameter (param);
  }

  protected abstract void engineInitVerify (PublicKey publicKey)
    throws InvalidKeyException;
  protected abstract void engineInitSign (PrivateKey privateKey)
    throws InvalidKeyException;
  protected abstract void engineUpdate (byte b)
    throws SignatureException;
  protected abstract void engineUpdate (byte[] b, int off, int len)
    throws SignatureException;
  protected abstract byte[] engineSign ()
    throws SignatureException;
  protected abstract boolean engineVerify (byte[] sigBytes)
    throws SignatureException;
  protected abstract void engineSetParameter (String param, Object value)
    throws InvalidParameterException;
  protected abstract Object engineGetParameter (String param)
    throws InvalidParameterException;

  public Object clone() throws CloneNotSupportedException
  {
    return super.clone ();
  }

  protected static final int UNINITIALIZED = 0;
  protected static final int SIGN          = 2;
  protected static final int VERIFY        = 3;

  // Current state.
  protected int state;

  // Name of this object.
  private String name;
}
