/* GConfBasedPreferences.java -- GConf based Preferences implementation
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

import gnu.java.util.prefs.gconf.GConfNativePeer;

import java.security.Permission;

import java.util.List;
import java.util.prefs.AbstractPreferences;
import java.util.prefs.BackingStoreException;

/**
 * This is a GConf based preference implementation which writes the preferences
 * as GConf key-value pairs. System Root is defined to be the
 * <code>"/system"</code> directory of GConf for the current user, while User
 * Root is <code>"/apps/java"</code>. These defaults can be modified by
 * defining two system properties:<br />
 * <br />
 * User Root:<br />
 * <br />
 * 
 * <pre>
 * gnu.java.util.prefs.gconf.user_root
 * </pre>
 * 
 * <br />
 * <br />
 * and System Root:<br />
 * <br />
 * 
 * <pre>
 * gnu.java.util.prefs.gconf.system_root
 * </pre>
 * 
 * <br />
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class GConfBasedPreferences
    extends AbstractPreferences
{
  /** Get access to Runtime permission */
  private static final Permission PERMISSION
    = new RuntimePermission("preferences");

  /** CGonf client backend */
  private static GConfNativePeer backend = new GConfNativePeer();

  /** Default user root path */
  private static final String DEFAULT_USER_ROOT = "/apps/classpath";

  /** Default system root path */
  private static final String DEFAULT_SYSTEM_ROOT = "/system";

  /** current node full path */
  private String node = "";

  /** True if this is a preference node in the user tree, false otherwise. */
  private final boolean isUser;

  /**
   * Creates a preference root user node.
   */
  public GConfBasedPreferences()
  {
    this(true);
  }

  /**
   * Creates a preference root node. When <code>isUser</code> is true it will
   * be user node otherwise it will be a system node.
   */
  public GConfBasedPreferences(boolean isUser)
  {
    this(null, "", isUser);
  }

  /**
   * Creates a new preference node given a parent node and a name, which has to
   * be relative to its parent. When <code>isUser</code> is true it will be user
   * node otherwise it will be a system node.
   * 
   * @param parent The parent node of this newly created node.
   * @param name A name relative to the parent node.
   * @param isUser Set to <code>true</code> initializes this node to be
   * a user node, <code>false</code> initialize it to be a system node.
   */
  public GConfBasedPreferences(AbstractPreferences parent, String name,
                               boolean isUser)
  {
    super(parent, name);
    this.isUser = isUser;

    // stores the fully qualified name of this node
    String absolutePath = this.absolutePath();
    if (absolutePath != null && absolutePath.endsWith("/"))
      {
        absolutePath = absolutePath.substring(0, absolutePath.length() - 1);
      }

    // strip invalid characters
    // please, note that all names are unescaped into the native peer
    int index = absolutePath.lastIndexOf('/');
    if (index > -1)
      {
        absolutePath = absolutePath.substring(0, index + 1);
        absolutePath = absolutePath + GConfNativePeer.escapeString(name);
      }
    
    this.node = this.getRealRoot(isUser) + absolutePath;

    boolean nodeExist = backend.nodeExist(this.node);

    this.newNode = !nodeExist;
  }

  /**
   * Returns a child node with the given name.
   * If the child node does not exists, it will be created.
   * 
   * @param name The name of the requested node.
   * @return A new reference to the node, creating the node if it is necessary.
   */
  protected AbstractPreferences childSpi(String name)
  {
    // we don't check anything here, if the node is a new node this will be
    // detected in the constructor, so we simply return a new reference to
    // the requested node.
    
    GConfBasedPreferences preferenceNode
      = new GConfBasedPreferences(this, name, this.isUser);
    
    // register the node for to GConf so that it can listen
    // events outside the scope of the application
    backend.startWatchingNode(this.node);
    
    return preferenceNode;
  }

  /**
   * Returns an array of names of the children of this preference node.
   * If the current node does not have children, the returned array will be
   * of <code>size</code> 0 (that is, not <code>null</code>).
   * 
   * @return A <code>String</code> array of names of children of the current
   * node.
   * @throws BackingStoreException if this operation cannot be completed.
   */
  protected String[] childrenNamesSpi() throws BackingStoreException
  {
    List<String> nodeList = backend.getChildrenNodes(this.node);
    String[] nodes = new String[nodeList.size()];
    nodeList.toArray(nodes);

    return nodes;
  }

  /**
   * Suggest a flush to the backend. Actually, this is only a suggestion as
   * GConf handles this for us asynchronously. More over, both sync and flush
   * have the same meaning in this class, so calling sync has exactly the same
   * effect.
   * 
   * @see #sync
   * @throws BackingStoreException if this operation cannot be completed.
   */
  public void flush() throws BackingStoreException
  {
    backend.suggestSync();
  }

  /**
   * Request a flush.
   * 
   * @see #flush
   * @throws BackingStoreException if this operation cannot be completed.
   */
  protected void flushSpi() throws BackingStoreException
  {
    this.flush();
  }

  /**
   * Returns all of the key in this preference node.
   * If the current node does not have preferences, the returned array will be
   * of size zero.
   * 
   * @return A <code>String</code> array of keys stored under the current
   * node.
   * @throws BackingStoreException if this operation cannot be completed.
   */
  protected String[] keysSpi() throws BackingStoreException
  {
    List<String> keyList = backend.getKeys(this.node);
    String[] keys = new String[keyList.size()];
    keyList.toArray(keys);

    return keys;
  }

  /**
   * Does a recursive postorder traversal of the preference tree, starting from
   * the given directory invalidating every preference found in the node.
   * 
   * @param directory The name of the starting directory (node)
   */
  private void postorderRemove(String directory)
  {
    try
      {
        // gets the listing of directories in this node
        List<String> dirs = backend.getChildrenNodes(directory);

        if (dirs.size() != 0)
          {
            for (String currentDir : dirs)
              {
                // recursive search inside this directory
                postorderRemove(currentDir);
              }
          }

        // remove all the keys associated to this directory
        List<String> entries = backend.getKeys(directory);

        if (entries.size() != 0)
          {
            for (String key : entries)
              {
                this.removeSpi(key);
              }
          }
      }
    catch (BackingStoreException ex)
      {
        /* ignore */
      }
  }

  /**
   * Stores the given key-value pair into this preference node.
   * 
   * @param key The key of this preference.
   * @param value The value of this preference.
   */
  protected void putSpi(String key, String value)
  {
    backend.setString(this.getGConfKey(key), value);
  }

  /**
   * Removes this preference node, including all its children.
   * Also removes the preferences associated.
   */
  protected void removeNodeSpi() throws BackingStoreException
  {
    this.postorderRemove(this.node);
    this.flush();
  }

  /**
   * Removes the given key from this preference node.
   * If the key does not exist, no operation is performed.
   * 
   * @param key The key to remove.
   */
  protected void removeSpi(String key)
  {
    backend.unset(this.getGConfKey(key));
  }

  /**
   * Suggest a sync to the backend. Actually, this is only a suggestion as GConf
   * handles this for us asynchronously. More over, both sync and flush have the
   * same meaning in this class, so calling flush has exactly the same effect.
   * 
   * @see #flush
   * @throws BackingStoreException if this operation cannot be completed due to
   *           a failure in the backing store, or inability to communicate with
   *           it.
   */
  public void sync() throws BackingStoreException
  {
    this.flush();
  }

  /**
   * Request a sync.
   * 
   * @see #sync
   * @throws BackingStoreException if this operation cannot be completed due to
   *           a failure in the backing store, or inability to communicate with
   *           it.
   */
  protected void syncSpi() throws BackingStoreException
  {
    this.sync();
  }

  /**
   * Returns the value of the given key.
   * If the keys does not have a value, or there is an error in the backing
   * store, <code>null</code> is returned instead.
   * 
   * @param key The key to retrieve.
   * @return The value associated with the given key.
   */
  protected String getSpi(String key)
  {
    return backend.getKey(this.getGConfKey(key));
  }

  /**
   * Returns <code>true</code> if this preference node is a user node,
   * <code>false</code> if is a system preference node.
   * 
   * @return <code>true</code> if this preference node is a user node,
   * <code>false</code> if is a system preference node.
   */
  public boolean isUserNode()
  {
    return this.isUser;
  }

  /*
   * PRIVATE METHODS
   */

  /**
   * Builds a GConf key string suitable for operations on the backend.
   * 
   * @param key The key to convert into a valid GConf key.
   * @return A valid Gconf key.
   */
  private String getGConfKey(String key)
  {
    String nodeName = "";
    
    // strip key
    // please, note that all names are unescaped into the native peer
    key = GConfNativePeer.escapeString(key);
    
    if (this.node.endsWith("/"))
      {
        nodeName = this.node + key;
      }
    else
      {
        nodeName = this.node + "/" + key;
      }
    
    return nodeName;
  }

  /**
   * Builds the root node to use for this preference.
   * 
   * @param isUser Defines if this node is a user (<code>true</code>) or system
   * (<code>false</code>) node.
   * @return The real root of this preference tree.
   */
  private String getRealRoot(boolean isUser)
  {
    // not sure about this, we should have already these permissions...
    SecurityManager security = System.getSecurityManager();

    if (security != null)
      {
        security.checkPermission(PERMISSION);
      }

    String root = null;

    if (isUser)
      {
        root = System.getProperty("gnu.java.util.prefs.gconf.user_root",
                                  DEFAULT_USER_ROOT);
      }
    else
      {
        root = System.getProperty("gnu.java.util.prefs.gconf.system_root",
                                  DEFAULT_SYSTEM_ROOT);
      }

    return root;
  }
}
