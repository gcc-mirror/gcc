// VirtualMachineError.java

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang;
 
/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 1, 1998 
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

/* FIXME: We should consider adding some special error message when this
 * exception is thrown, or maybe if it being caught at top-level.  Such
 * a message would direct the user to send a bug report to
 * gcj-bugs@cygnus.com, or something like that. --KKT */

public abstract class VirtualMachineError extends Error
{
  public VirtualMachineError ()
  {
    super ();
  }

  public VirtualMachineError (String msg)
  {
    super (msg);
  }
}
