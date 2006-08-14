/* UID.java -- The unique object Id
   Copyright (c) 2006 Free Software Foundation, Inc.

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


package java.rmi.server;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;
import java.net.InetAddress;

/**
 * Represents the unique identifier over time for the host which has generated
 * it. It contains time (when created), counter (the number of the UID
 * creation order) and virtual machine id components. The UID can also be
 * constructed specifying a "well known" identifier in the for of short:
 * this identifier defines the UID uniqueness alone. 
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public final class UID
    implements Serializable
{
  /**
   * Use the serial version uid for interoperability.
   */
  private static final long serialVersionUID = 1086053664494604050L;
 
  /**
   * The UID counter (the ordinary number in the sequence of number of UID's,
   * created during the recent millisecond). In the next millisecond, it 
   * starts from the minimal value again. In the unlikely case of creating
   * more than 65536 uids per millisecond the process pauses till the next
   * ms.
   */
  private static short uidCounter = Short.MIN_VALUE;
  
  /**
   * The time, when the last UID has been created.
   */
  private static long last;

  /**
   * This constant tries to be the unique identifier of the virtual machine.
   */
  private static final int machineId = getMachineId();

  /**
   * The UID number in the UID creation sequence.
   */
  private short count;

  /**
   * Always gets the uniqueNr value.
   */
  private int unique;

  /**
   * The time stamp, when the UID was created.
   */
  private long time;
  
  /**
   * Create the new UID that would have the described features of the
   * uniqueness.
   */
  public UID()
  {
    synchronized (UID.class)
      {
        time = System.currentTimeMillis();
        unique = machineId;
        if (time > last)
          {
            last = time;
            count = uidCounter = Short.MIN_VALUE;
          }
        else
          {
            if (uidCounter == Short.MAX_VALUE)
              {
                // Make a 2 ms pause if the counter has reached the maximal
                // value. This should seldom happen.
                try
                  {
                    Thread.sleep(2);
                  }
                catch (InterruptedException e)
                  {
                  }
                uidCounter = Short.MIN_VALUE;
                time = last = System.currentTimeMillis();
              }
            count = ++uidCounter;
          }
      }
  }
  
  /**
   * Create the new UID with the well known id (number). All UIDs, creates
   * with the this constructor having the same parameter are equal to each
   * other (regardless to the host and time where they were created.
   * 
   * @param wellKnownId the well known UID.
   */
  public UID(short wellKnownId)
  {
    unique = wellKnownId;
  }
  
  /**
   * Get the hashCode of this UID.
   */
  public int hashCode()
  {
    return (int) (unique ^ time ^ count);
  }
  
  /**
   * Compare this UID with another UID for equality (not equal to other types of
   * objects).
   */
  public boolean equals(Object other)
  {
    if (other instanceof UID)
      {
        UID ui = (UID) other;
        return unique == ui.unique && time == ui.time && count == ui.count;
      }
    else
      return false;
  }
  
  public static UID read(DataInput in) throws IOException
  {
    UID uid = new UID();
    uid.unique = in.readInt();
    uid.time = in.readLong();
    uid.count = in.readShort();
    return (uid);
  }

  public void write(DataOutput out) throws IOException
  {
    out.writeInt(unique);
    out.writeLong(time);
    out.writeShort(count);
  }

  /**
   * Do our best to get the Id of this virtual machine.
   */
  static int getMachineId()
  {
    int hostIpHash;

    try
      {
        // Try to get the host IP.
        String host = InetAddress.getLocalHost().toString();
        // This hash is content - based, not the address based.
        hostIpHash = host.hashCode();
      }
    catch (Exception e)
      {
        // Failed due some reason.
        hostIpHash = 0;
      }

    // Should be the unque address if hashcodes are addresses.
    // Additionally, add the time when the RMI system was probably started
    // (this class was first instantiated).
    return new Object().hashCode() ^ (int) System.currentTimeMillis()
           ^ hostIpHash;
  }
  
    /**
   * Get the string representation of this UID.
   * 
   * @return a string, uniquely identifying this id.
   */
  public String toString()
  {
    int max = Character.MAX_RADIX;
    // Translate into object count, counting from 0.
    long lc = (count - Short.MIN_VALUE) & 0xFFFF;
    return Long.toString(unique, max) + ":" + Long.toString(time, max) + "."
           + Long.toString(lc, max);
  }
}
