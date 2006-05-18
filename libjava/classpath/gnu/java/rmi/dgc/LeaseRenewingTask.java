/* LeaseRenewingTask.java -- The task to renew the lease.
   Copyright (C) 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.java.rmi.server.UnicastRef;

import java.lang.ref.WeakReference;
import java.rmi.dgc.Lease;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Timer;
import java.util.TimerTask;
import java.util.WeakHashMap;

/**
 * The task to renew the lease to some object reference. The UnicastRef
 * being renewed is stored as a weak reference. So the presence of the
 * sheduled task does not prevent it from being garbage collected. If the
 * reference has not been garbage collected, the task is resheduled after
 * the lease is renewed.
 * 
 *  @author Audrius Meskauskas (Audriusa@Bioinformatics.org)
 */
public class LeaseRenewingTask
{ 
  /**
   * The sheduled timer task to call the renew() method.
   */
  class LeaseTimerTask extends TimerTask
  {
    public void run()
    {
      renew();
    }
  }
  
  /**
   * The default requested lease duration time (one minute by default).
   */
  public static long REQUEST_LEASE_DURATION =  60000;
  
  /**
   * The reference to the UnicastRef that must renew its lease until not
   * garbage collected. The different members of this list may point to the
   * different instances of the UnicastRef's, but these references are
   * pointing to the same remote object (they .equals() returns
   * true when comparing with each other). Using LinkedList to make
   * frequent deletions from the middle easy.
   */
  LinkedList ref = new LinkedList();
  
  /**
   * The granted (or supposed) lease.
   */
  Lease lease = new Lease(null, REQUEST_LEASE_DURATION);
  
  /**
   * The timer, shared by all lease renewing tasks. The same instance is also
   * used for the reference protector discarding in DGCImpl.
   */
  static Timer timer = new Timer(true);
  
  /**
   * Maps the UnicastRef to its renewing task.
   */
  static WeakHashMap existingTasks = new WeakHashMap();  
  
  /**
   * Creates the lease renewing task that renews the lease of the given
   * UnicastRef until it is not collected. This constructor requests the lease
   * value from the server and schedules the lease renewal action.
   * 
   * @param renewIt the reference that must be renewed.
   */
  public LeaseRenewingTask(UnicastRef renewIt)
  {
    lease = notifyDGC(renewIt);
    if (lease != null)
      {
        schedule(lease);
        ref.add(new WeakReference(renewIt));
      }
  }
  
  /**
   * Schedule periodic leases for the given UnicastRef reference.
   * 
   * @param renewIt the reference, for that the leases must be scheduled.
   */
  public static void scheduleLeases(UnicastRef renewIt)
  {
    // No need to schedule leases for null.
    if (renewIt == null)
      return;
    try {
    synchronized (existingTasks)
      {
        // Check maybe the task for refreshing this remote object already
        // exists.
        LeaseRenewingTask task = (LeaseRenewingTask) existingTasks.get(renewIt);

        if (task != null)
          {
            // Extend the reference list only. The scheduling must be
            // alredy done with the previous lease.
            synchronized (task.ref)
              {
                task.ref.add(new WeakReference(renewIt));
              }
          }
        else
          existingTasks.put(renewIt, new LeaseRenewingTask(renewIt));
      }
    }
    catch (Exception ex)
    {
      InternalError ierr = new InternalError("Lease for "+renewIt);
      ierr.initCause(ex);
      throw ierr;
    }
  }
  
  /**
   * Shedule the renewing call, taking into consideration that the following
   * lease was granted.
   * 
   * @param lease the lease that was granted.
   */
  public void schedule(Lease lease)
  {
    long value = lease.getValue();
    
    // Shedule a 10 % earlier because some time is needed for the message
    // to reach the server.
    long reduced = (value * 90)/100;
    if (reduced == 0)
      reduced = value;
    
    timer.schedule(new LeaseTimerTask(), reduced);
  }

  /**
   * Renew the lease.
   */
  public void renew()
  {
    Object renewIt = null;
    // Iterate throw the list of associated references. If all are
    // discarded, there is no need to renew.
    synchronized (ref)
    {
      Iterator iter = ref.iterator();
      WeakReference w;
      while (iter.hasNext() && renewIt == null)
        {
          w = (WeakReference) iter.next();
          renewIt = w.get();
          if (renewIt == null)
            // Discard the weak reference if its target has been garbage
            // collected.
            iter.remove();
        }
    }
    
    if (renewIt!=null)
      {
        Lease lease = notifyDGC( (UnicastRef) renewIt);
        
        // Schedule the next renewing session.
        if (lease!=null)
          schedule(lease);
      }
      {
        // All references collected - discard this entry.
      }
  }
  
  /**
   * Notify DGC that we still hold this reference.
   * 
   * @param renewIt the reference we still have (must not be null).
   */
  public Lease notifyDGC(UnicastRef renewIt)
  {
    try
      {
        return renewIt.notifyDGC(lease);
      }
    catch (Exception e)
      {
        // Failed to notify.
        // TODO Take some relevant action in the case if we failed
        // to notify the remote object owner.
        return null;
      }
  }

}
