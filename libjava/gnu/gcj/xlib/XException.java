/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

/**
 * Runtime exception that occurred during an Xlib operation. 
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class XException extends RuntimeException
{  
  public XException() {}
  public XException(String msg) { super(msg); }
  
  public XException(Display display, int status)
  {
    super(toString(display, status));
  }
  
  static native String toString(Display display, int status);
}
