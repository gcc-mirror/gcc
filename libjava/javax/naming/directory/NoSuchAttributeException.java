/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;

import javax.naming.NamingException;

public class NoSuchAttributeException extends NamingException
{
  public NoSuchAttributeException ()
  {
    super ();
  }

  public NoSuchAttributeException (String msg)
  {
    super (msg);
  }
}
