/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.transaction.xa;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date May 25, 2001
 */

public abstract interface Xid
{
  public static final int MAXGTRIDSIZE = 64;
  public static final int MAXBQUALSIZE = 64;

  public int getFormatId();
  public byte[] getGlobalTransactionId();
  public byte[] getBranchQualifier();
}
