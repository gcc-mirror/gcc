/* GThreadMutex.java -- Implements a mutex object for glib's gthread
   abstraction, for use with GNU Classpath's --portable-native-sync option.
   This is used in gthread-jni.c
   
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.java.awt.peer.gtk;

/** Implements a mutex object for glib's gthread
    abstraction, for use with GNU Classpath's --portable-native-sync option.
    This is used in gthread-jni.c.

    We use this object to implement the POSIX semantics for Mutexes.  They are
    needed are needed for the function vector that is passed to glib's
    g_thread subpackage's initialization function.

    The GThreadMutex object itself serves as the Real Lock; if code has
    entered the monitor for this GThreadMutex object (in Java language, if
    it's synchronized on this object) then it holds the lock that this object
    represents.

    @author Steven Augart
    May, 2004

    
*/
   
class GThreadMutex 
{
  /** Might "lock" be locked?  Is anyone waiting
      to get that lock?  How long is the queue?

      If zero, nobody holds a lock on this GThreadMutex object, and nobody is
      trying to get one.   Before someone attempts to acquire a lock on this
      object, they must increment potentialLockers.  After they release their
      lock on this object, they must decrement potentialLockers.

      Access to this field is guarded by synchronizing on the object
      <code>lockForPotentialLockers</code>.

      After construction, we only access this field via JNI.
  */
  volatile int potentialLockers;

  /** An object to synchronize to if you want to examine or modify the
      <code>potentialLockers</code> field.  Only hold this lock for brief
      moments, just long enough to check or set the value of
      <code>lockForPotentialLockers</code>.  
      
      We use this representation so that g_thread_mutex_trylock() will work
      with the POSIX semantics.  This is the only case in which you ever hold a
      lock on <code>lockForPotentialLockers</code> while trying to get another
      lock -- if you are the mutex_trylock() implementation, and you have just
      checked that <code>potentialLockers</code> has the value zero.  In that
      case, mutex_trylock() holds the lock on lockForPotentialLockers so that
      another thread calling mutex_trylock() or mutex_lock() won't increment
      potentialLockers after we've checked it and before we've gained the lock
      on the POSIX mutex.   Of course, in that case the operation of gaining
      the POSIX lock itself will succeed immediately, and once it has
      succeeded, trylock releases lockForPotentialLockers right away,
      incremented to 1 (one).

      After construction, we only access this field via JNI.
  */     
  Object lockForPotentialLockers;

  GThreadMutex() 
  {
    potentialLockers = 0;
    lockForPotentialLockers = new Object();
  }
}
// Local Variables:
// c-file-style: "gnu"
// End:
