/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import java.io.IOException;

/**
 * Indicates that something went wrong with the connection to an X11
 * display.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class XConnectException extends IOException
{
  public XConnectException()
  {
    super();
  }

  public XConnectException(String message)
  {
    super(message);
  }
}
