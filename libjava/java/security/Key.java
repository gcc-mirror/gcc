/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.security;
import java.io.Serializable;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date February 1, 2000.
 */

/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */
 
public interface Key extends Serializable
{
  public static final long serialVersionUID = 6603384152749567654L;

  public String getAlgorithm();
  public String getFormat();
  public byte[] getEncoded();
}
