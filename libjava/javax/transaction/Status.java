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

public abstract interface Status
{
  public static final int STATUS_ACTIVE = 0;
  public static final int STATUS_MARKED_ROLLBACK = 1;
  public static final int STATUS_PREPARED = 2;
  public static final int STATUS_COMMITTED = 3;
  public static final int STATUS_ROLLEDBACK = 4;
  public static final int STATUS_UNKNOWN = 5;
  public static final int STATUS_NO_TRANSACTION = 6;
  public static final int STATUS_PREPARING = 7;
  public static final int STATUS_COMMITTING = 8;
  public static final int STATUS_ROLLING_BACK = 9;
}
