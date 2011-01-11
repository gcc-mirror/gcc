/* VMID.java -- The object ID, unique between all virtual machines.
   Copyright (c) 1996, 1997, 1998, 1999, 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.java.lang.CPStringBuilder;

import java.io.Serializable;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.rmi.server.UID;
import java.util.Arrays;

/**
 * An identifier that is unique accross the all virtual machines. This class is
 * used by distributed garbage collector to identify the virtual machine of
 * the client, but may also be used in various other cases, when such identifier
 * is required. This class separately stores and transfers the host IP
 * address, but will try to do its best also for the case if it failed to
 * determine it. The alternative algorithms are used in {@link UID} that is
 * part of this class. The VMID's, created on the same host, but in the two
 * separately (parallely) running virtual machines are different.
 */
public final class VMID implements Serializable
{
  /**
   * Use SVUID for interoperability.
   */
  static final long serialVersionUID = -538642295484486218L;

  /**
   * If true, the IP of this host can ve reliably determined.
   */
  static boolean areWeUnique;

  /**
   * The IP address of the local host.
   */
  static byte[] localAddr;

  /**
   * The IP address of the local host.
   */
  private byte[] addr;

  /**
   * The cached hash code.
   */
  transient int hash;

  /**
   * The UID of this VMID.
   */
  private UID uid;

  static
    {
      // This "local host" value usually indicates that the local
      // IP address cannot be reliably determined.
      byte[] localHost = new byte[] { 127, 0, 0, 1 };

      try
        {
          localAddr = InetAddress.getLocalHost().getAddress();
          areWeUnique = !Arrays.equals(localHost, localAddr);
        }
      catch (UnknownHostException uhex)
        {
          localAddr = localHost;
          areWeUnique = false;
        }
    }

  /**
   * Create the new VMID. All VMID's are unique accross tha all virtual
   * machines.
   */
  public VMID()
  {
    addr = localAddr;
    uid = new UID();
  }

  /**
   * Return true if it is possible to get the accurate address of this host.
   * If false is returned, the created VMID's are less reliable, but the
   * starting time and possibly the memory allocation are also taken into
   * consideration in the incorporated UID. Hence the VMID's, created on the
   * different virtual machines, still should be different.
   *
   * @deprecated VMID's are more or less always reliable.
   *
   * @return false if the local host ip address is 127.0.0.1 or unknown,
   * true otherwise.
   */
  public static boolean isUnique ()
  {
    return areWeUnique;
  }

  /**
   * Get the hash code of this VMID.
   */
  public int hashCode ()
  {
    if (hash==0)
      {
        for (int i = 0; i < localAddr.length; i++)
            hash += addr[i];
        hash = hash ^ uid.hashCode();
      }
    return hash;
  }

  /**
   * Returns true if the passed parameter is also VMID and it is equal to this
   * VMID. The VMID should only be equal to itself (also if the passed value is
   * another instance, cloned by serialization).
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof VMID)
      {
        VMID other = (VMID) obj;

        // The UID's are compared faster than arrays.
        return uid.equals(other.uid) && Arrays.equals(addr, other.addr);
      }
    else
      return false;

  }

  /**
   * Get the string representation of this VMID.
   */
  public String toString ()
  {
    CPStringBuilder buf = new CPStringBuilder ("[VMID: ");

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
