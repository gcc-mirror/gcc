/* GConfNativePeer.java -- GConf based preference peer for native methods
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


package gnu.java.util.prefs.gconf;

import java.util.List;
import java.util.prefs.BackingStoreException;

/**
 * Native peer for GConf based preference backend.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public final class GConfNativePeer
{
  /**
   * Object to achieve locks for methods that need to be synchronized.
   */
  private static final Object[] semaphore = new Object[0];

  /**
   * Creates a new instance of GConfNativePeer
   */
  public GConfNativePeer()
  {
    synchronized (semaphore)
      {
        init_class();
      }
  }

  /**
   * Queries whether the node <code>node</code> exists in theGConf database.
   * Returns <code>true</code> or <code>false</code>.
   * 
   * @param node the node to check.
   */
  public boolean nodeExist(String node)
  {
    return gconf_client_dir_exists(node);
  }

  /**
   * Add the node <code>node</code> to the list of nodes the GConf will watch.
   * An event is raised everytime this node is changed. You can add a node
   * multiple times.
   * 
   * @param node the node to track.
   */
  public void startWatchingNode(String node)
  {
    gconf_client_add_dir(node);
  }

  /**
   * Remove the node <code>node</code> to the list of nodes the GConf is
   * watching. Note that if a node has been added multiple times, you must
   * remove it the same number of times before the remove takes effect.
   * 
   * @param node the node you don't want to track anymore.
   */
  public void stopWatchingNode(String node)
  {
    gconf_client_remove_dir(node);
  }

  /**
   * Change the value of key to val. Automatically creates the key if it didn't
   * exist before (ie it was unset or it only had a default value).
   * Key names must be valid GConf key names, that is, there can be more
   * restrictions than for normal Preference Backend.
   * 
   * @param key the key to alter (or add).
   * @param value the new value for this key.
   * @return true if the key was updated, false otherwise.
   */
  public boolean setString(String key, String value)
  {
    return gconf_client_set_string(key, value);
  }

  /**
   * Unsets the value of key; if key is already unset, has no effect. Depending
   * on the GConf daemon, unsetting a key may have the side effect to remove it
   * completely form the database.
   * 
   * @param key the key to unset.
   * @return true on success, false if the key was not updated.
   */
  public boolean unset(String key)
  {
    return gconf_client_unset(key);
  }

  /**
   * Gets the value of a configuration key.
   * 
   * @param key the configuration key.
   * @return the values of this key, null if the key is not valid.
   */
  public String getKey(String key)
  {
    return gconf_client_get_string(key);
  }

  /**
   * Lists the key in the given node. Does not list subnodes. Keys names are the
   * stripped names (name relative to the current node) of the keys stored in
   * this node.
   * 
   * @param node the node where keys are stored.
   * @return a java.util.List of keys. If there are no keys in the given node, a
   *         list of size 0 is returned.
   */
  public List<String> getKeys(String node) throws BackingStoreException
  {
    return gconf_client_all_keys(node);
  }

  /**
   * Lists the subnodes in <code>node</code>. The returned list contains
   * allocated strings. Each string is the name relative tho the given node.
   * 
   * @param node the node to get subnodes from. If there are no subnodes in the
   *          given node, a list of size 0 is returned.
   */
  public List<String> getChildrenNodes(String node) throws BackingStoreException
  {
    return gconf_client_all_nodes(node);
  }

  /**
   * Escape the given string so the it is a valid GConf name.
   */
  public static String escapeString(String plain)
  {
    return gconf_escape_key(plain);
  }
  
  /**
   * Unescape a string escaped with {@link #escapeString}.
   */
  public static String unescapeString(String escaped)
  {
    return gconf_unescape_key(escaped);
  }
  
  /**
   * Suggest to the backend GConf daemon to synch with the database.
   */
  public void suggestSync() throws BackingStoreException
  {
    gconf_client_suggest_sync();
  }
  
  protected void finalize() throws Throwable
  {
    try
      {
        synchronized (semaphore)
          {
            finalize_class();
          }
      }
    finally
      {
        super.finalize();
      }
  }

  /* ***** native methods ***** */

  /*
   * Basicly, these are one to one mappings to GConfClient functions.
   * GConfClient instances are handled by the native layer, and are hidden from
   * the main java class.
   */

  /**
   * Initialize the GConf native peer and enable the object cache.
   * It is meant to be used by the static initializer.
   */
  native static final private void init_id_cache();
  
  /**
   * Initialize the GConf native peer. This is meant to be used by the
   * class constructor.
   */
  native static final private void init_class();

  /**
   * Class finalizer.
   */
  native static final private void finalize_class();

  /**
   * Queries the GConf database to see if the given node exists, returning
   * true if the node exist, false otherwise.
   * 
   * @param node the node to query for existence.
   * @return true if the node exist, false otherwise.
   */
  native static final protected boolean gconf_client_dir_exists(String node);

  /**
   * Adds the given node to the list of nodes that GConf watches for
   * changes.
   * 
   * @param node the node to watch for changes.
   */
  native static final protected void gconf_client_add_dir(String node);

  /**
   * Removes the given node from the list of nodes that GConf watches for
   * changes.
   * 
   * @param node the node to remove from from the list of watched nodes.
   */
  native static final protected void gconf_client_remove_dir(String node);

  /**
   * Sets the given key/value pair into the GConf database.
   * The key must be a valid GConf key.
   * 
   * @param key the key to store in the GConf database
   * @param value the value to associate to the given key.
   * @return true if the change has effect, false otherwise.
   */
  native static final protected boolean gconf_client_set_string(String key,
                                                                String value);

  /**
   * Returns the key associated to the given key. Null is returned if the
   * key is not valid.
   * 
   * @param key the key to return the value of.
   * @return The value associated to the given key, or null.
   */
  native static final protected String gconf_client_get_string(String key);

  /**
   * Usets the given key, removing the key from the database.
   * 
   * @param key the key to remove.
   * @return true if the operation success, false otherwise.
   */
  native static final protected boolean gconf_client_unset(String key);

  /**
   * Suggest to the GConf native peer a sync with the database.
   *
   */
  native static final protected void gconf_client_suggest_sync()
    throws BackingStoreException;
  
  /**
   * Returns a list of all nodes under the given node.
   * 
   * @param node the source node.
   * @return A list of nodes under the given source node.
   */
  native
  static final protected List<String> gconf_client_all_nodes(String node)
    throws BackingStoreException;
  
  /**
   * Returns a list of all keys stored in the given node.
   * 
   * @param node the source node.
   * @return A list of all keys stored in the given node.
   */
  native
  static final protected List<String> gconf_client_all_keys(String node)
    throws BackingStoreException;

  /**
   * Escape the input String so that it's a valid element for GConf.
   * 
   * @param plain the String to escape.
   * @return An escaped String for use with GConf.
   */
  native
  static final protected String gconf_escape_key(String plain);
  
  /**
   * Converts a string escaped with gconf_escape_key back into its
   * original form.
   * 
   * @param escaped key as returned by gconf_escape_key 
   * @return An unescaped key.
   */
  native
  static final protected String gconf_unescape_key(String escaped);
  
  static
    {
      System.loadLibrary("gconfpeer");
      init_id_cache();
    }
}
