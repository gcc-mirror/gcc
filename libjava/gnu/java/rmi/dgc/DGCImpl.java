/*
  Copyright (c) 1996, 1997, 1998, 1999, 2002 Free Software Foundation, Inc.

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

package gnu.java.rmi.dgc;

import java.rmi.dgc.DGC;
import java.rmi.dgc.Lease;
import java.rmi.dgc.VMID;
import java.rmi.server.ObjID;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.RMISocketFactory;
import gnu.java.rmi.server.UnicastServerRef;

import java.util.Hashtable;

/**
  * I let DGCImpl to extend UnicastServerRef, but not 
  * UnicastRemoteObject, because UnicastRemoteObject must
  * exportObject automatically.
  */
public class DGCImpl
    extends UnicastServerRef implements DGC {

    private static final long LEASE_VALUE = 600000L;
    // leaseCache caches a LeaseRecord associated with a vmid
    private Hashtable leaseCache = new Hashtable();

public DGCImpl() throws RemoteException {
    	super(new ObjID(ObjID.DGC_ID), 0, RMISocketFactory.getSocketFactory());
}

public Lease dirty(ObjID[] ids, long sequenceNum, Lease lease) throws RemoteException {
	VMID vmid = lease.getVMID();
    	if (vmid == null)
    	    vmid = new VMID();
    	long leaseValue = LEASE_VALUE;
    	//long leaseValue = lease.getValue();
    lease = new Lease(vmid, leaseValue);
        synchronized(leaseCache){
            LeaseRecord lr = (LeaseRecord)leaseCache.get(vmid);
            if (lr != null)
                lr.reset(leaseValue);
            else{
                lr = new LeaseRecord(vmid, leaseValue);
                leaseCache.put(vmid, lr);
            }
        }
        
	return (lease);
}

public void clean(ObjID[] ids, long sequenceNum, VMID vmid, boolean strong) throws RemoteException {
  // Not implemented
}
    
  /**
   * LeaseRecord associates a vmid to expireTime.
   */
  private static class LeaseRecord{
    private VMID vmid;
    private long expireTime;
    
    LeaseRecord(VMID vmid, long leaseValue){
      this.vmid = vmid;
      reset(leaseValue);
    }
    
    // reset expireTime
    void reset(long leaseValue){
      long l = System.currentTimeMillis();
      expireTime = l + leaseValue;
    }

    boolean isExpired(){
      long l = System.currentTimeMillis();
      if ( l > expireTime)
	return true;
      return false;
    }
        
  } //End of LeaseRecord

} //End of DGCImpl
