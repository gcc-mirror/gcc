// Breakpoint.java -  a breakpoint in the interpreter

/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.jvmti;

import gnu.gcj.RawDataManaged;

/**
 * Class representing a Breakpoint.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public class Breakpoint
{
  // Location of this breakpoint
  private long method;
  private long location;

  // The original instruction that this breakpoint replaced
  private RawDataManaged data;

  /**
   * Constructs a new Breakpoint. SetBreakpoint will verify the
   * validity of the arguments.
   *
   * @param method the method (a jmethodID)
   * @param location the jlocation of the breakpoint (a jlocation)
   */
  public Breakpoint (long method, long location)
  {
    this.method = method;
    this.location = location;
    initialize_native ();
  }

  private native void initialize_native ();

  public native void install ();

  public native void remove ();

  /**
   * Returns the original instruction at the location where
   * this breakpoint was set
   */
  public RawDataManaged getInsn ()
  {
    return data;
  }
}
