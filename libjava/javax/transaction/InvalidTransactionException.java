/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.transaction;
 
import java.rmi.RemoteException;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date April 18, 2001
 */

public class InvalidTransactionException extends RemoteException
{
  public InvalidTransactionException ()
  {
    super();
  }

  public InvalidTransactionException (String msg)
  {
    super(msg);
  }
}
