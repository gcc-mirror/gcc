/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.transaction;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date May 25, 2001
 */

public abstract interface UserTransaction
{
  public void begin() throws NotSupportedException, SystemException;
  public void commit()
    throws RollbackException, HeuristicMixedException,
      HeuristicRollbackException, java.lang.SecurityException,
      java.lang.IllegalStateException, SystemException;
  public void rollback()
    throws java.lang.IllegalStateException, java.lang.SecurityException,
      SystemException;
  public void setRollbackOnly()
    throws java.lang.IllegalStateException, SystemException;
  public int getStatus() throws SystemException;
  public void setTransactionTimeout(int seconds) throws SystemException;
}
