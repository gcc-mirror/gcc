/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.security.spec;
import java.security.GeneralSecurityException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date February 1, 2000.
 */

/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */

// JDK1.2
public class InvalidParameterSpecException extends GeneralSecurityException
{
  public InvalidParameterSpecException()
  {
    super();
  }

  public InvalidParameterSpecException(String msg)
  {
    super(msg);
  }
}
