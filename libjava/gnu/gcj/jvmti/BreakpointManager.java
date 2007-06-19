// BreakpointManager.java - A convenience class for dealing with breakpoints

/* Copyright (C) 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.jvmti;

import java.util.Hashtable;

/**
 * A class which manages breakpoints in the VM interpreter engine.
 *
 * BreakpointManager is a location manager that the interpreter
 * uses to lookup the original instruction for any given installed
 * breakpoint. JVMTI does not allow multiple breakpoints to be set
 * at any given location.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public class BreakpointManager
{
  private static BreakpointManager _instance = new BreakpointManager ();

  // List of breakpoints indexed by Location
  private Hashtable _breakpoints;

  private BreakpointManager ()
  {
    _breakpoints = new Hashtable ();
  }

  /**
   * Creates a new breakpoint. SetBreakpoint will verify the validity
   * of the arguments.
   *
   * @param method  method in which to set breakpoint (a jmethodID)
   * @param location index where the breakpoint is to be set (a jlocation)
   */
  public static Breakpoint newBreakpoint (long method, long location)
  {
    NormalBreakpoint bp = new NormalBreakpoint (method, location);
    Location loc = new Location (method, location);
    bp.install ();
    _instance._breakpoints.put (loc, bp);
    return bp;
  }

  /**
   * Deletes the breakpoint at the given Location
   *
   * @param method method in which to clear breakpoint
   * @param location index of breakpoint in method
   */
  public static void deleteBreakpoint (long method, long location)
  {
    Location loc = new Location (method, location);
    Breakpoint bp = (Breakpoint) _instance._breakpoints.get (loc);
    if (bp != null)
      {
	bp.remove ();
	_instance._breakpoints.remove (loc);
      }
  }

  /**
   * Returns the breakpoint at the given location or null if none installed
   * at location
   *
   * @param method the jmethodID of the breakpoint location
   * @param location the index in the method
   */
  public static Breakpoint getBreakpoint (long method, long location)
  {
    Location loc = new Location (method, location);
    return (Breakpoint) _instance._breakpoints.get (loc);
  }
}
