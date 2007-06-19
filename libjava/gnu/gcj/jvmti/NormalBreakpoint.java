// NormalBreakpoint.java -  a "normal" breakpoint in the interpreter

/* Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.jvmti;

/**
 * This class represents a "normal" breakpoint in the interpreter.
 * When the interpreter hits this breakpoint type, it will send out
 * a JVMTI breakpoint notification.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public class NormalBreakpoint
  extends Breakpoint
{
  public NormalBreakpoint (long method, long id)
  {
    super (method, id);
  }

  public native void execute ();
}
