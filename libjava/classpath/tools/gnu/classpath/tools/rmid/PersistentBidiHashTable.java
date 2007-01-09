/* PersistentBidiHasthable.java -- Bidirectional persistent hash table.
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

package gnu.classpath.tools.rmid;

import gnu.classpath.tools.common.Persistent;
import gnu.classpath.tools.rmid.ActivationSystemImpl;
import gnu.java.rmi.activation.BidiTable;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.TimerTask;

/**
 * The persistent bidirectional hash table, maps both a to b and b to a. The
 * changes are written to dist after SAVE_AT_MOST_AFTER time from the latest
 * database change or at most after ALWAYS_UPDATE, if the database is updated
 * very frequently. To ensure that no information is lost, the shutdown method
 * must be called before exit.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class PersistentBidiHashTable extends BidiTable implements
    Persistent
{
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
   * Replaces instances of ActivationSystemImpl into the currently active
   * instance of the ActivationSystemImpl
   */
  class AdaptedReader extends ObjectInputStream
  {
    AdaptedReader(InputStream in) throws IOException
    {
      super(in);
      enableResolveObject(true);
    }

    protected Object resolveObject(Object obj) throws IOException
    {
      if (obj instanceof ActivationSystemImpl)
        return ActivationSystemImpl.singleton2;
      else
        return obj;
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
   * Create the unitialised instance that must be initalised when
   * ActivationSystemImpl.singleton2 is assigned.
   */
  public PersistentBidiHashTable()
  {
    // Do not initalise the table fields - the initalise method must be
    // called later.
    super(0);
  }

  /**
   * Create a new persistent table that stores its information into the given
   * file. The ActivationSystemImpl.singleton2 must be assigned.
   * 
   * @param file
   *          the file, where the table stores its information.
   * @param coldStart
   *          if true, the existing file with this name will be erased and
   *          ignored. Otherwise, it will be assumed that the file contains the
   *          persistent table information.
   */
  public void init(File file, boolean coldStart)
  {
    try
      {
        database = file;
        if (database.exists())
          {
            if (coldStart)
              {
                k2v = new Hashtable();
                v2k = new Hashtable();
                database.delete();
              }
            else
              {
                FileInputStream fi = new FileInputStream(file);
                BufferedInputStream b = new BufferedInputStream(fi);
                ObjectInputStream oin = new AdaptedReader(b);

                k2v = (Map) oin.readObject();
                oin.close();

                v2k = new Hashtable(k2v.size());

                // Reguild v2k from k2v:
                Iterator en = k2v.keySet().iterator();
                Object key;
                while (en.hasNext())
                  {
                    key = en.next();
                    v2k.put(k2v.get(key), key);
                  }
              }
          }
        else
          {
            k2v = new Hashtable();
            v2k = new Hashtable();
          }
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
        oout.writeObject(k2v);
        oout.close();
      }
    catch (Exception ioex)
      {
        InternalError ierr = new InternalError(
                                               "Failed to write database to disk: "
                                                   + database);
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
  public void put(Object key, Object value)
  {
    super.put(key, value);
    markDirty();
  }

  /**
   * Update the memory maps and mark as should be written to the disk.
   */
  public void removeKey(Object key)
  {
    super.removeKey(key);
    markDirty();
  }

}
