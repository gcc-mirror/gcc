/* XAResource.java -- 
   Copyright (C) 2001, 2002, 2005  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package javax.transaction.xa;
 
/**
 * @author Warren Levy (warrenl@redhat.com)
 * @date May 25, 2001
 */

public interface XAResource
{
  int TMENDRSCAN = 8388608;
  int TMFAIL = 536870912;
  int TMJOIN = 2097152;
  int TMNOFLAGS = 0;
  int TMONEPHASE = 1073741824;
  int TMRESUME = 134217728;
  int TMSTARTRSCAN = 16777216;
  int TMSUCCESS = 67108864;
  int TMSUSPEND = 33554432;
  int XA_RDONLY = 3;
  int XA_OK = 0;

  void commit(Xid xid, boolean onePhase) throws XAException;
  void end(Xid xid, int flags) throws XAException;
  void forget(Xid xid) throws XAException;
  int getTransactionTimeout() throws XAException;
  boolean isSameRM(XAResource xares) throws XAException;
  int prepare(Xid xid) throws XAException;
  Xid[] recover(int flag) throws XAException;
  void rollback(Xid xid) throws XAException;
  boolean setTransactionTimeout(int seconds) throws XAException;
  void start(Xid xid, int flags) throws XAException;
}
