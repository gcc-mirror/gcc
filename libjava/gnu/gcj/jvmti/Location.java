// Location.java - a wrapper class for breakpoint locations in JVMTI

/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.jvmti;

import java.lang.Long;

/**
 * This class represents a breakpoint location (pair<jmethodID,jlocation>).
 * BreakpointManager uses this class as a key in the Map of installed
 * breakpoints.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public class Location
{
  // method (a jmethodID in JVMTI)
  private long method;

  // index (a jlocation in JVMTI)
  private long location;

  /**
   * Constructor
   *
   * @param method   the method defined by this location (a jmethodID)
   * @param location the integer index of the insn in the method (a jlocation)
   */
  public Location (long method, long location)
  {
    this.method = method;
    this.location = location;
  }

  public int hashCode ()
  {
    return toString ().hashCode ();
  }

  public boolean equals (Object obj)
  {
    Location loc = (Location) obj;
    return (loc.method == method && loc.location == location);
  }

  /**
   * Converts the Location to a String
   */
  public String toString ()
  {
    return Long.toHexString (method) + "." + Long.toString (location);
  }
}
