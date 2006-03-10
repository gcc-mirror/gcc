/* FileBasedPreferences.java -- File-based preference implementation
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


package gnu.java.util.prefs;

import gnu.classpath.SystemProperties;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.Properties;
import java.util.prefs.AbstractPreferences;
import java.util.prefs.BackingStoreException;

/**
 * This is a simple file-based preference implementation which writes
 * the preferences as properties files.  A node is represented as a directory
 * beneath the user's home directory.  The preferences for the node are
 * stored in a single properties file in that directory.  Sub-nodes are
 * stored in subdirectories.  This implementation uses file locking to
 * mediate access to the properties files. 
 */
public class FileBasedPreferences
    extends AbstractPreferences
{
  /**
   * Name of the property file storing the data in a given directory.
   */
  private static final String DATA_FILE = "data.properties";

  /**
   * The directory corresponding to this preference node.
   */
  private File directory;

  /**
   * The file holding the data for this node.
   */
  private File dataFile;

  /**
   * The data in this node.
   */
  private Properties properties;

  /**
   * Create the root node for the file-based preferences.
   */
  FileBasedPreferences()
  {
    super(null, "");
    String home = SystemProperties.getProperty("user.home");
    this.directory = new File(new File(home, ".classpath"), "userPrefs");
    this.dataFile = new File(this.directory, DATA_FILE);
    load();
  }

  /**
   * Create a new file-based preference object with the given parent
   * and the given name.
   * @param parent the parent
   * @param name the name of this node
   */
  FileBasedPreferences(FileBasedPreferences parent, String name)
  {
    super(parent, name);
    this.directory = new File(parent.directory, name);
    this.dataFile = new File(this.directory, DATA_FILE);
    load();
  }

  private void load()
  {
    this.properties = new Properties();
    FileInputStream fis = null;
    FileLock lock = null;
    try
      {
        fis = new FileInputStream(this.dataFile);
        FileChannel channel = fis.getChannel();
        lock = channel.lock(0, Long.MAX_VALUE, true);
        this.properties.load(fis);
        // We release the lock and close the stream in the 'finally'
        // clause.
      }
    catch (IOException _)
      {
        // We don't mind; this means we're making a new node.
        newNode = true;
      }
    finally
      {
        try
          {
            // Release the lock and close the stream.
            if (lock != null)
              lock.release();
          }
        catch (IOException ignore)
          {
            // Ignore.
          }
        try
          {
            // Close the stream.
            if (fis != null)
              fis.close();
          }
        catch (IOException ignore)
          {
            // Ignore.
          }
      }
  }

  public boolean isUserNode()
  {
    // For now file preferences are always user nodes.
    return true;
  }

  protected String[] childrenNamesSpi() throws BackingStoreException
  {
    // FIXME: security manager.
    String[] result = directory.list(new FilenameFilter()
                          {
                            public boolean accept(File dir, String name)
                            {
                              return new File(dir, name).isDirectory();
                            }
                          });
    if (result == null)
      result = new String[0];
    return result;
  }

  protected AbstractPreferences childSpi(String name)
  {
    return new FileBasedPreferences(this, name);
  }

  protected String[] keysSpi() throws BackingStoreException
  {
    return (String[]) properties.keySet().toArray(new String[0]);
  }

  protected String getSpi(String key)
  {
    return properties.getProperty(key);
  }

  protected void putSpi(String key, String value)
  {
    properties.put(key, value);
  }

  protected void removeSpi(String key)
  {
    properties.remove(key);
  }

  protected void flushSpi() throws BackingStoreException
  {
    // FIXME: security manager.
    try
      {
        if (isRemoved())
          {
            // Delete the underlying file.
            // FIXME: ideally we would also delete the directory
            // if it had no subdirectories.  This doesn't matter
            // much though.
            // FIXME: there's a strange race here if a different VM is
            // simultaneously updating this node.
            dataFile.delete();
          }
        else
          {
            // Write the underlying file.
            directory.mkdirs();
            
            FileOutputStream fos = null;
            FileLock lock = null;
            try
              {
                // Note that we let IOExceptions from the try clause
                // propagate to the outer 'try'.
                fos = new FileOutputStream(dataFile);
                FileChannel channel = fos.getChannel();
                lock = channel.lock();
                properties.store(fos, "created by GNU Classpath FileBasedPreferences");
                // Lock is released and file closed in the finally clause.
              }
            finally
              {
                try
                  {
                    if (lock != null)
                      lock.release();
                  }
                catch (IOException _)
                  {
                    // Ignore.
                  }
                try
                  {
                    if (fos != null)
                      fos.close();
                  }
                catch (IOException _)
                  {
                    // Ignore.
                  }
              }
          }
      }
    catch (IOException ioe)
      {
        throw new BackingStoreException(ioe);
      }
  }

  protected void syncSpi() throws BackingStoreException
  {
    // FIXME: we ought to synchronize but instead we merely flush.
    flushSpi();
  }

  protected void removeNodeSpi() throws BackingStoreException
  {
    // We can simply delegate.
    flushSpi();
  }
}
