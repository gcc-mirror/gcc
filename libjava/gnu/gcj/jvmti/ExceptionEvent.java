// ExceptionEvent - an exception event for JVMTI

/* Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.jvmti;

import java.util.WeakHashMap;

/**
 * Class to create and send JVMTI Exception events
 *
 * @author Kyle Galloway (kgallowa@redhat.com)
 */
public class ExceptionEvent
{
  // Information about where the exception was thrown
  private long _throwMeth, _throwLoc;
  
  // Information about where the exception was or can be caught
  private long _catchMeth, _catchLoc;
  
  // Thread where the exception occurred
  private Thread _thread;
  
  // The exception
  private Throwable _ex;
  
  // A hash map of the exceptions we've already seen in a thread's call stack
  private static WeakHashMap<Thread, Throwable> _exMap = new WeakHashMap<Thread, Throwable>();
  
  /**
   * Constructs a new ExceptionEvent and sends it.  If it is not caught
   * within the frame where it was thrown (catchMeth and catchLoc are null),
   * check_catch will check for a possible catch further up the call stack 
   * before marking it uncaught.
   * 
   * @param thr the thread where the exception occurred
   * @param throwMeth the method of the throw (a jmethodID)
   * @param throwLoc the location of the throw (a jlocation)
   * @param ex the exception
   * @param catchMeth the method of the catch (a jmethodID), null indicates
   * that the exception was not caught in the frame where it was thrown
   * @param catchLoc the location of the catch (a jlocation), null indicates
   * that the exception was not caught in the frame where it was thrown
   */
  private ExceptionEvent(Thread thr, long throwMeth, long throwLoc,
		                 Throwable ex, long catchMeth, long catchLoc)
  {
    this._thread = thr;
    this._ex = ex;
    this._throwMeth = throwMeth;
    this._throwLoc = throwLoc;
    this._catchMeth = catchMeth;
    this._catchLoc = catchLoc;
  }
  
  public static void postExceptionEvent(Thread thr, long throwMeth,
		                                long throwLoc, Throwable ex,
		                                long catchMeth, long catchLoc)
  {
    // Check to see if there is an entry for this Thread thr in the has map.
	// If not, add the thread to the hash map and send an ExceptionEvent.
	if (_exMap.containsKey(thr))
	  {
		// Check to see if we are receiving events for the same exception, or a
		// new one.  If it is not the same exception beign rethrown, send a new
		// event.
	    if (!(_exMap.get(thr) == ex))
	      {
            _exMap.put(thr, ex);
            ExceptionEvent event = new ExceptionEvent(thr, throwMeth,
            		                                  throwLoc, ex, catchMeth,
            		                                  catchLoc);  
            event.sendEvent ();
          }
	  }
	else
	  {
	    _exMap.put(thr, ex);
	    ExceptionEvent event = new ExceptionEvent(thr, throwMeth,
                                                  throwLoc, ex, catchMeth,
                                                  catchLoc);
	    event.sendEvent();
	  }
  }
  
  public native void sendEvent();
  
  public native void checkCatch();
}
