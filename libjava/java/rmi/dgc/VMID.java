/* VMID.java
   Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.rmi.dgc;

import java.io.Serializable;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.rmi.server.UID;

public final class VMID	implements Serializable
{
  static final long serialVersionUID = -538642295484486218L;
  
  static final boolean areWeUnique;
  
  static byte[] localAddr;

  private byte[] addr;
  
  private UID uid;

  static
  {
    byte[] addr;
    boolean awu = true;
    try {
      addr = InetAddress.getLocalHost().getAddress();
      if (addr[0] == 127 && addr[1] == 0 && addr[2] == 0 && addr[3] == 1) {
        awu = false;
      }
    }
    catch (UnknownHostException _) {
      addr = new byte[]{ 127, 0, 0, 1 };
      awu = false;
    }
    localAddr = addr;
    areWeUnique = awu;
  }

  public VMID()
  {
    addr = localAddr;
    uid = new UID();
  }

  /**
   * @deprecated
   */
  public static boolean isUnique ()
  {
    return areWeUnique;
  }

  public int hashCode ()
  {
    return super.hashCode();
  }

  public boolean equals (Object obj)
  {
    if (!(obj instanceof VMID))
      {
        return false;
      }
    
    VMID other = (VMID) obj;
    if (addr.length != other.addr.length)
      {
        return false;
      }
    
    for (int i = addr.length - 1; i >= 0; i--)
      {
        if (addr[i] != other.addr[i])
          {
            return false;
          }
      }
    
    return uid.equals(other.uid);
  }

  public String toString ()
  {
    StringBuffer buf = new StringBuffer ("[VMID: ");
    
    for (int i = 0; i < addr.length; i++)
      {
        if (i > 0)
          {
            buf.append (".");
          }
        
        buf.append (Integer.toString (addr [i]));
      }
    
    buf.append (" ");
    buf.append (uid.toString ());
    buf.append ("]");

    return buf.toString();
  }
}
