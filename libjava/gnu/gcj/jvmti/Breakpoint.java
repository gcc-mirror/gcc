// Breakpoint.java -  a base class for interpreter breakpoints

/* Copyright (C) 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.jvmti;

import gnu.gcj.RawDataManaged;

/**
 * Base class representing a type of breakpoint in the interpreter.
 * This class deals with saving insns and installing and
 * uninstalling insns in the interpreter for all breakpoint classes.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public abstract class Breakpoint
{
  // Location of this breakpoint
  protected long method;
  protected long location;

  // The original instruction that this breakpoint replaced
  private RawDataManaged data;

  /**
   * Constructs a new Breakpoint
   *
   * @param method the method in which to set the breakpoint
   * @param location the location at which to set the breakpoint
   */
  public Breakpoint (long method, long location)
  {
    this.method = method;
    this.location = location;
  }

  public Breakpoint ()
  {
  }

  private native void _save_insn ();

  /**
   * Installs the breakpoint into the interpreter
   */
  public native void install ();

  /**
   * Removes the breakpoint from the interpreter, re-installing
   * the original instruction.
   */
  public native void remove ();

  /**
   * Returns the original instruction at the location where
   * this breakpoint was set
   */
  public RawDataManaged getInsn ()
  {
    return data;
  }

  /**
   * Execute the actions of this breakpoint
   */
  public abstract void execute ();
}
