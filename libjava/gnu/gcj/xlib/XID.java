/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

/**
 * Common base class for all resources that are stored on the server
 * and refered to on the client side using XIDs.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class XID
{
  public XID(Display display, int xid)
  {
    this.display = display;
    this.xid = xid;
  }

  public final int getXID()
  {
    return xid;
  }

  public final Display getDisplay()
  {
    return display;
  }

  protected Display display;
  protected int xid;

  private Object clientData;
  public final Object getClientData()
  {
    return clientData;
  }
  public final void setClientData(Object clientData)
  {
    this.clientData = clientData;
  }

  protected String params()
  {
    return "display=" + display + ",xid=" + Integer.toHexString(xid);
  }
  
  public String toString()
  {
    return getClass().getName() +
      "[" + params() + "]";
  }
}
