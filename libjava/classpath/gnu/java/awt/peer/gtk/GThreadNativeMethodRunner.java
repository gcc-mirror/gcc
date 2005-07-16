/* GThreadNativeMethodRunner.java -- Implements pthread_create(), under
   glib's gthread abstraction, for use with GNU Classpath's
   --portable-native-sync option. 
   This is used by gthread-jni.c
   
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.gtk;

import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/** Implements pthread_create(), under glib's gthread abstraction, for use
    with GNU Classpath's --portable-native-sync option.  This is used in
    gthread-jni.c

    Also implements a registry for threads, mapping Thread objects to small
    integers.  The registry uses weak references for threads that aren't
    joinable, so that they will be garbage collected.

    There are a number of possible alternative implementations.
    
    
    The rest of this comment consists of an answer to a question that was
    raised on the commit-classpath mailing list:

    Mark Wielaard wrote:

    > Can't we assume that jobject and gpointer are both (void *) so we don't
    > need the int <-> Thread (global jobject ref) mapping?
    > Maybe there are platforms where jobject and gpointer aren't the same,
    > but I guess that is pretty unlikely.


    I agree with you on the pointer size issues.  A gpointer is a void *, so
    it's certainly guaranteed to be at least as large as any other
    pointer. And a jobject is implicitly an opaque pointer (in Jikes RVM, we
    use small integers, but we coerce them into the representation of a
    pointer).

    The int <==> Thread mapping addresses a different issue.  I realize that I
    did not document this properly (two and a half lines in thread_create),
    and the point is subtle (at least to me; took me a while to figure out).

    The int => Thread mapping always returns jobjects that are local
    references, not global ones.  This is because Thread objects need to be
    able to go away and be garbage collected after the thread they refer to
    has died.

    If we keep a global object reference to a thread, then when do we delete
    that global object reference?  We have an answer in the case of GThread
    objects that were explicitly created with the joinable attribute.  It is
    safe for us to maintain a global reference to any joinable thread, since
    the joinable thread must linger (even if only in a zombie state)
    until it's explicitly joined via a g_thread_join() call.  The global ref
    could be cleaned up at that point too.

    However, in the case of GThreads that were created non-joinable by
    g_thread_create(), and in the case of Java threads that were created
    within pure Java code (not via g_thread_create()), we don't want them to
    linger forever, and there is no way to tell when the last reference
    to such threads needs to expire.  In the case of this application -- AWT
    with GTK peers -- it would probably be safe anyway, since there are not
    very many threads we create, but I was going for correctness even in the
    case of long-running programs that might set up and tear down AWT
    interfaces many times.

    So, I duplicated the POSIX thread-ID semantics.  The thread ID of a
    non-joinable thread remains valid as long as that thread is still alive.
    Once that thread dies, the old thread ID may be reused at any moment.  And
    that's why the array indexed by thread ID numbers is an array of weak
    references.

    That's also why the int => Thread jobject mapping function always returns
    local references, since global references would lock the Thread in memory
    forever.

    I would dearly love there to be a cleaner solution.  I dislike the
    repeated dips from C code into Java that are necessary to look up thread
    ID numbers.  If anyone can think of one, I'm all ears.
*/

class GThreadNativeMethodRunner 
  extends Thread 
{
  /** The C function pointer that was passed to g_thread_create().
      Specifically, this the numeric address of an object of 
      C type "void *(*funcPtr)(void *funcArg)".   
  */
  private final long funcPtr;

  /** The argument for the function "funcPtr(funcArg)". */
  private final long funcArg;
  
  GThreadNativeMethodRunner(long funcPtr, long funcArg, boolean joinable) 
  {
    this.funcPtr = funcPtr;
    this.funcArg = funcArg;

    if (joinable)
      registerSelfJoinable();
  }

  public void run() 
  {
    nativeRun(funcPtr, funcArg);
  }

  private native void nativeRun(long funcPtr, long funcArg);

  /** THREADS is an array of threads, indexed by thread ID codes.  Not sure
      whether this is the "best" approach but it does make it O(1) to look up a
      thread by its ID. 

      Zero is a valid thread ID code.  Any negative number is invalid.

      Possible future fixes (TODO?)

     - The THREADS array will only grow. probably not a problem.
        But we could keep count when nulling entries and shrink when we have
        lots of nulls at the end. Probably not worth it. --mjw

     - Could make this a set of Object; see the comment on "joinable" below.

     The initial size of 17 is just a starting point.  Any number will do,
     including zero.
  */ 
  private static WeakReference[] threads = new WeakReference[17]; 

  /**  Used by threadToThreadID, below.  Returns the registration number of
       the newly-registered thread.  
  */
  private static synchronized int registerThread(Thread t) 
  {
    int i;

    for (i = 0; i < threads.length; ++i) 
      {
	WeakReference ref = threads[i];
	if (ref == null)
	  break;                  // found an empty spot.
      }

    if (i == threads.length) 
      {
	/* expand the array */
	WeakReference[] bigger = new WeakReference[threads.length * 2];
        System.arraycopy(threads, 0, bigger, 0, threads.length);
	threads = bigger;
      }

    threads[i] = new WeakReference(t);

    return i;
  }
  
  /**  Look up the Thread ID # for a Thread.  Assign a Thread ID # if none
       exists.  This is a general routine for handling all threads, including
       the VM's main thread, if appropriate.


       Runs in O(n/2) time.

       We can't just issue a threadID upon thread creation.  If we were to do
       that, not all threads would have a threadID, because not all threads
       are launched by GThreadNativeMethodRunner.
  */ 
  static synchronized int threadToThreadID(Thread t) 
  {
    for (int i = 0; i < threads.length; ++i ) 
      {
	if (threads[i] == null)
	  continue;
	Thread referent = (Thread) threads[i].get();
	if (referent == null) 
	  {
	    threads[i] = null;      // Purge the dead WeakReference.
	    continue;
	  }
	if (referent.equals(t))
	  return i;
      } // for()

    /* No match found. */
    return registerThread(t);
  }

  /** @param threadID Must be a non-negative integer.

      Used to return null if the thread number was out of range or if
      the thread was unregistered.   Now we throw an exception.

      Possible Alternative Interface:  We could go back to returning null in
           some sort of check-free mode, so code that calls this function must
           be prepared to get null. 
  */ 
  static Thread threadIDToThread(int threadID) 
    throws IllegalArgumentException
  {
    if (threadID < 0)
      throw new IllegalArgumentException("Received a negative threadID, " 
					 + threadID); 
    if (threadID >= threads.length)
      throw new IllegalArgumentException("Received a threadID (" + threadID 
					 + ") higher than was" 
					 + " ever issued"); 
    
    /* Note: if the user is using a stale reference, things will just
       break.    We might end up getting a different thread than the one
       expected. 
       
       TODO: Add an error-checking mode where the user's problems with threads
          are announced.  For instance, if the user asks for the thread
          associated with a threadID that was never issued, we could print a
          warning or even abort.
       
       TODO: Consider optionally disabling all of the error-checking we
          already have; it probably slows down the implementation.  We could
          just return NULL.  This is just the reverse of the above TODO item.
    */ 

    WeakReference threadRef = threads[threadID];

    if (threadRef == null)
      throw new IllegalArgumentException("Asked to look up a stale or unissued"
					 + "threadID (" + threadID + ")" );
    
      
    Thread referent = (Thread) threadRef.get();
    if (referent == null)
      throw new IllegalArgumentException ("Asked to look up a stale threadID ("
					  + threadID + ")");
    return referent;
  }
  
  /** Joinable threads need a hard reference, so that they won't go away when
      they die.  That is because their thread IDs need to stay valid until the
      thread is joined via thread_join(threadID).  Joinable threads have to be
      explicitly joined before they are allowed to go away completely.

      Possible Alternative Implementation: Eliminate the Joinable set.  When
          calling getThreadIDFromThread() you know whether or not the thread
          is joinable.  So just store the Thread itself in the threads array?
          Make that array an Object array and check with instanceof.  This
          looks cleaner and more robust to me and it saves a native -> Java
          call. But instanceof might be expensive.  --mjw
  */
  private static final Set joinable = 
       Collections.synchronizedSet(new HashSet()); 
  
  /** Only called from the constructor. */
  private void registerSelfJoinable() 
  {
    joinable.add(this);
  }
  
  /** This method is only called from JNI, and only after we have succeeded in
      a thread_join() operation.  */
  static void deRegisterJoinable(Thread thread) 
  {
    joinable.remove(thread);
  }
}

// Local Variables:
// c-file-style: "gnu"
// End:
