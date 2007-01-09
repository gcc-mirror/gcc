/* PersistentHasthable.java -- Persistent hash table.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package gnu.classpath.tools.rmiregistry;

import gnu.classpath.tools.common.Persistent;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Hashtable;
import java.util.Map;
import java.util.TimerTask;

/**
 * The persistent hash table. The changes are written to dist after
 * SAVE_AT_MOST_AFTER time from the latest database change or at most after
 * ALWAYS_UPDATE, if the database is updated very frequently. To ensure that no
 * information is lost, the shutdown method must be called before exit.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class PersistentHashTable 
  extends Hashtable 
  implements Serializable, Persistent
{

  /**
   * Use serialVersionUID for interoperability
   */
  private static final long serialVersionUID = 1;

  class WriteToDiskTask extends TimerTask
  {
    /**
     * Save the database.
     */
    public void run()
    {
      writeContent();
      sheduled = null;
    }
  }

  /**
   * The database file.
   */
  File database;

  /**
   * The currently sheduled write to disk task, null if none.
   */
  WriteToDiskTask sheduled = null;

  /**
   * The time, when the disk database was last updated.
   */
  long lastUpdated;
  
  /**
   * Setting to false prevents the automated disk update.
   * The initial value is true to prevent writing while reading and is set
   * to false in createInstance.
   */
  transient boolean ready;
  
  /**
   * Use static method to obtain the instance.
   */
  private PersistentHashTable(File file)
  {
    if (file == null)
      throw new NullPointerException("Null file provided");
    database = file;
  }

  /**
   * Create a new persistent table that stores its information into the given
   * file.
   * 
   * @param file
   *          the file, where the table stores its information.
   * @param coldStart
   *          if true, the existing file with this name will be erased and
   *          ignored. Otherwise, it will be assumed that the file contains the
   *          persistent table information.
   */
  public static Map createInstance(File file, boolean coldStart)
  {
    try
      {
        PersistentHashTable k2v;
        System.out.println ("Here1");
        if (file.exists())
          {
        System.out.println ("Here2");
            if (coldStart)
              {
        System.out.println ("Here2.5");
                file.delete();
                k2v = new PersistentHashTable(file);
              }
            else
              {
        System.out.println ("Here3");
                FileInputStream fi = new FileInputStream(file);
        System.out.println ("Here3.1");
                BufferedInputStream b = new BufferedInputStream(fi);
        System.out.println ("Here3.2");
                ObjectInputStream oin = new ObjectInputStream(b);
        System.out.println ("Here3.3");

        System.out.println ("Here4");
                k2v = (PersistentHashTable) oin.readObject();
                oin.close();
        System.out.println ("Here5");
              }
          }
        else
          {
        System.out.println ("Here6");
          k2v = new PersistentHashTable(file);
        System.out.println ("Here7");
          }
        
        System.out.println ("Here8");
        k2v.ready = true;
        return k2v;
      }
    catch (Exception ioex)
      {
        InternalError ierr = new InternalError("Unable to intialize with file "
                                               + file);
        ierr.initCause(ioex);
        throw ierr;
      }
  }
  
  
  /**
   * Write the database content to the disk.
   */
  public synchronized void writeContent()
  {
    try
      {
        FileOutputStream fou = new FileOutputStream(database);
        BufferedOutputStream b = new BufferedOutputStream(fou);
        ObjectOutputStream oout = new ObjectOutputStream(b);
        oout.writeObject(this);
        oout.close();
      }
    catch (Exception ioex)
      {
        InternalError ierr = new InternalError(
          "Failed to write database to disk: "+ database);
        ierr.initCause(ioex);
        throw ierr;
      }
  }

  /**
   * Mark the modified database as modified. The database will be written after
   * several seconds, unless another modification occurs.
   */
  public void markDirty()
  {
    if (System.currentTimeMillis() - lastUpdated > ALWAYS_UPDATE)
      {
        // Force storing to disk under intensive operation.
        writeContent();
        lastUpdated = System.currentTimeMillis();
        if (sheduled != null)
          {
            sheduled.cancel();
            sheduled = null;
          }
      }
    else
      {
        // Otherwise coalesce the disk database copy update events.
        if (sheduled != null)
          sheduled.cancel();
        sheduled = new WriteToDiskTask();
        timer.schedule(sheduled, SAVE_AT_MOST_AFTER);
      }
  }

  /**
   * Save the current database state to the disk before exit.
   */
  public void shutdown()
  {
    if (sheduled != null)
      {
        writeContent();
        sheduled = null;
      }
  }

  /**
   * Update the memory maps and mark as should be written to the disk.
   */
  public Object put(Object key, Object value)
  {
    super.put(key, value);
    if (ready)
      markDirty();
    return value;
  }

  /**
   * Update the memory maps and mark as should be written to the disk.
   */
  public Object remove(Object key)
  {
    Object removed = super.remove(key);
    if (ready)
      markDirty();
    return removed;
  }

}
