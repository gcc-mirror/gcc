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

public abstract interface XAResource
{
  public static final int TMENDRSCAN = 8388608;
  public static final int TMFAIL = 536870912;
  public static final int TMJOIN = 2097152;
  public static final int TMNOFLAGS = 0;
  public static final int TMONEPHASE = 1073741824;
  public static final int TMRESUME = 134217728;
  public static final int TMSTARTRSCAN = 16777216;
  public static final int TMSUCCESS = 67108864;
  public static final int TMSUSPEND = 33554432;
  public static final int XA_RDONLY = 3;
  public static final int XA_OK = 0;

  public void commit(Xid xid, boolean onePhase) throws XAException;
  public void end(Xid xid, int flags) throws XAException;
  public void forget(Xid xid) throws XAException;
  public int getTransactionTimeout() throws XAException;
  public boolean isSameRM(XAResource xares) throws XAException;
  public int prepare(Xid xid) throws XAException;
  public Xid[] recover(int flag) throws XAException;
  public void rollback(Xid xid) throws XAException;
  public boolean setTransactionTimeout(int seconds) throws XAException;
  public void start(Xid xid, int flags) throws XAException;
}
