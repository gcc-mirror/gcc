/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang;
 
/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date September 18, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class ClassNotFoundException extends Exception
{
  public ClassNotFoundException()
  {
    super();
  }

  // TODO12:
  // public ClassNotFoundException(String msg, Throwable ex)
  // {
  // }

  public ClassNotFoundException(String msg)
  {
    super(msg);
  }

  // TODO12:
  // public Throwable getException()
  // {
  // }

  // TBD: if this needs to be implemented
  // public void printStackTrace()
  // {
  // }

  // TBD: if this needs to be implemented
  // public void printStackTrace(PrintStream ps)
  // {
  // }

  // TBD: if this needs to be implemented
  // public void printStackTrace(PrintWriter pw)
  // {
  // }
}
