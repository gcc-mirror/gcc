/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.security;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date February 9, 2000.
 */

/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */
 
public class KeyPair /* FIXME: implements serializable */
{
  public KeyPair (PublicKey publicKey, PrivateKey privateKey)
  {
    this.publicKey = publicKey;
    this.privateKey = privateKey;
  }

  public PublicKey getPublic ()
  {
    return publicKey;
  }

  public PrivateKey getPrivate ()
  {
    return privateKey;
  }

  // The keys.
  private PublicKey publicKey;
  private PrivateKey privateKey;
}
