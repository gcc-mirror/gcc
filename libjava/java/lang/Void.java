/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 18, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Complete.
 */

public final class Void
{
  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public final static Class TYPE = void.class;

  // Don't allow Void objects to be made.
  private Void ()
  {
  }
}
