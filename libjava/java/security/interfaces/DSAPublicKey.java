/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.security.interfaces;
import java.security.PublicKey;
import java.math.BigInteger;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date February 1, 2000.
 */

/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */
 
public interface DSAPublicKey extends DSAKey, PublicKey
{
  // FIXME: need to set this at some point when serialization is implemented.
  public static final long serialVersionUID = 0;

  public BigInteger getY();
}
