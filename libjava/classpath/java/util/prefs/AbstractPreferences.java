/* AbstractPreferences -- Partial implementation of a Preference node
   Copyright (C) 2001, 2003, 2004  Free Software Foundation, Inc.

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


package java.util.prefs;

import gnu.java.util.prefs.NodeWriter;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeSet;

/**
 * Partial implementation of a Preference node.
 *
 * @since 1.4
 * @author Mark Wielaard (mark@klomp.org)
 */
public abstract class AbstractPreferences extends Preferences {

    // protected fields

    /**
     * Object used to lock this preference node. Any thread only locks nodes
     * downwards when it has the lock on the current node. No method should
     * synchronize on the lock of any of its parent nodes while holding the
     * lock on the current node.
     */
    protected final Object lock = new Object();

    /**
     * Set to true in the contructor if the node did not exist in the backing
     * store when this preference node object was created. Should be set in
     * the contructor of a subclass. Defaults to false. Used to fire node
     * changed events.
     */
    protected boolean newNode = false;

    // private fields

    /**
     * The parent preferences node or null when this is the root node.
     */
    private final AbstractPreferences parent;

    /**
     * The name of this node.
     * Only when this is a root node (parent == null) the name is empty.
     * It has a maximum of 80 characters and cannot contain any '/' characters.
     */
    private final String name;

    /** True when this node has been remove, false otherwise. */
    private boolean removed = false;

    /**
     * Holds all the child names and nodes of this node that have been
     * accessed by earlier <code>getChild()</code> or <code>childSpi()</code>
     * invocations and that have not been removed.
     */
    private HashMap childCache = new HashMap();

    // constructor

    /**
     * Creates a new AbstractPreferences node with the given parent and name.
     * 
     * @param parent the parent of this node or null when this is the root node
     * @param name the name of this node, can not be null, only 80 characters
     *             maximum, must be empty when parent is null and cannot
     *             contain any '/' characters
     * @exception IllegalArgumentException when name is null, greater then 80
     *            characters, not the empty string but parent is null or
     *            contains a '/' character
     */
    protected AbstractPreferences(AbstractPreferences parent, String name) {
        if (  (name == null)                            // name should be given
           || (name.length() > MAX_NAME_LENGTH)         // 80 characters max
           || (parent == null && name.length() != 0)    // root has no name
           || (parent != null && name.length() == 0)    // all other nodes do
           || (name.indexOf('/') != -1))                // must not contain '/'
            throw new IllegalArgumentException("Illegal name argument '"
                                               + name
                                               + "' (parent is "
                                               + (parent == null ? "" : "not ")
                                               + "null)");
        this.parent = parent;
        this.name = name;
    }

    // identification methods

    /**
     * Returns the absolute path name of this preference node.
     * The absolute path name of a node is the path name of its parent node
     * plus a '/' plus its own name. If the node is the root node and has no
     * parent then its path name is "" and its absolute path name is "/".
     */
    public String absolutePath() {
        if (parent == null)
            return "/";
        else
            return parent.path() + '/' + name;
    }

    /**
     * Private helper method for absolutePath. Returns the empty string for a
     * root node and otherwise the parentPath of its parent plus a '/'.
     */
    private String path() {
        if (parent == null)
            return "";
        else
            return parent.path() + '/' + name;
    }

    /**
     * Returns true if this node comes from the user preferences tree, false
     * if it comes from the system preferences tree.
     */
    public boolean isUserNode() {
        AbstractPreferences root = this;
	while (root.parent != null)
	    root = root.parent;
	return root == Preferences.userRoot();
    }

    /**
     * Returns the name of this preferences node. The name of the node cannot
     * be null, can be mostly 80 characters and cannot contain any '/'
     * characters. The root node has as name "".
     */
    public String name() {
        return name;
    }

    /**
     * Returns the String given by
     * <code>
     * (isUserNode() ? "User":"System") + " Preference Node: " + absolutePath()
     * </code>
     */
    public String toString() {
        return (isUserNode() ? "User":"System")
               + " Preference Node: "
               + absolutePath();
    }

    /**
     * Returns all known unremoved children of this node.
     *
     * @return All known unremoved children of this node
     */
    protected final AbstractPreferences[] cachedChildren()
    {
      return (AbstractPreferences[]) childCache.values().toArray();
    }

    /**
     * Returns all the direct sub nodes of this preferences node.
     * Needs access to the backing store to give a meaningfull answer.
     * <p>
     * This implementation locks this node, checks if the node has not yet
     * been removed and throws an <code>IllegalStateException</code> when it
     * has been. Then it creates a new <code>TreeSet</code> and adds any
     * already cached child nodes names. To get any uncached names it calls
     * <code>childrenNamesSpi()</code> and adds the result to the set. Finally
     * it calls <code>toArray()</code> on the created set. When the call to
     * <code>childrenNamesSpi</code> thows an <code>BackingStoreException</code>
     * this method will not catch that exception but propagate the exception
     * to the caller.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException when this node has been removed
     */
    public String[] childrenNames() throws BackingStoreException {
        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node removed");

            TreeSet childrenNames = new TreeSet();

            // First get all cached node names
            childrenNames.addAll(childCache.keySet());
            
            // Then add any others
            String names[] = childrenNamesSpi();
            for (int i = 0; i < names.length; i++) {
                childrenNames.add(names[i]);
            }

            // And return the array of names
            String[] children = new String[childrenNames.size()];
            childrenNames.toArray(children);
            return children;

        }
    }

    /**
     * Returns a sub node of this preferences node if the given path is
     * relative (does not start with a '/') or a sub node of the root
     * if the path is absolute (does start with a '/').
     * <p>
     * This method first locks this node and checks if the node has not been
     * removed, if it has been removed it throws an exception. Then if the
     * path is relative (does not start with a '/') it checks if the path is
     * legal (does not end with a '/' and has no consecutive '/' characters).
     * Then it recursively gets a name from the path, gets the child node
     * from the child-cache of this node or calls the <code>childSpi()</code>
     * method to create a new child sub node. This is done recursively on the
     * newly created sub node with the rest of the path till the path is empty.
     * If the path is absolute (starts with a '/') the lock on this node is
     * droped and this method is called on the root of the preferences tree
     * with as argument the complete path minus the first '/'.
     *
     * @exception IllegalStateException if this node has been removed
     * @exception IllegalArgumentException if the path contains two or more
     * consecutive '/' characters, ends with a '/' charactor and is not the
     * string "/" (indicating the root node) or any name on the path is more
     * then 80 characters long
     */
    public Preferences node(String path) {
        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node removed");

            // Is it a relative path?
            if (!path.startsWith("/")) {

                // Check if it is a valid path
                if (path.indexOf("//") != -1 || path.endsWith("/"))
                    throw new IllegalArgumentException(path);

                return getNode(path);
            }
        }

        // path started with a '/' so it is absolute
        // we drop the lock and start from the root (omitting the first '/')
        Preferences root = isUserNode() ? userRoot() : systemRoot();
        return root.node(path.substring(1));

    }

    /**
     * Private helper method for <code>node()</code>. Called with this node
     * locked. Returns this node when path is the empty string, if it is not
     * empty the next node name is taken from the path (all chars till the
     * next '/' or end of path string) and the node is either taken from the
     * child-cache of this node or the <code>childSpi()</code> method is called
     * on this node with the name as argument. Then this method is called
     * recursively on the just constructed child node with the rest of the
     * path.
     *
     * @param path should not end with a '/' character and should not contain
     *        consecutive '/' characters
     * @exception IllegalArgumentException if path begins with a name that is
     *            larger then 80 characters.
     */
    private Preferences getNode(String path) {
        // if mark is dom then goto end

        // Empty String "" indicates this node
        if (path.length() == 0)
            return this;

        // Calculate child name and rest of path
        String childName;
        String childPath;
        int nextSlash = path.indexOf('/');
        if (nextSlash == -1) {
            childName = path;
            childPath = "";
        } else {
            childName = path.substring(0, nextSlash);
            childPath = path.substring(nextSlash+1);
        }

        // Get the child node
        AbstractPreferences child;
        child = (AbstractPreferences)childCache.get(childName);
        if (child == null) {

            if (childName.length() > MAX_NAME_LENGTH)
               throw new IllegalArgumentException(childName); 

            // Not in childCache yet so create a new sub node
            child = childSpi(childName);
            // XXX - check if node is new
            childCache.put(childName, child);
        }

        // Lock the child and go down
        synchronized(child.lock) {
            return child.getNode(childPath);
        }
    }

    /**
     * Returns true if the node that the path points to exists in memory or
     * in the backing store. Otherwise it returns false or an exception is
     * thrown. When this node is removed the only valid parameter is the
     * empty string (indicating this node), the return value in that case
     * will be false.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has been removed
     *            and the path is not the empty string (indicating this node)
     * @exception IllegalArgumentException if the path contains two or more
     * consecutive '/' characters, ends with a '/' charactor and is not the
     * string "/" (indicating the root node) or any name on the path is more
     * then 80 characters long
     */
    public boolean nodeExists(String path) throws BackingStoreException {
        synchronized(lock) {
            if (isRemoved() && path.length() != 0)
                throw new IllegalStateException("Node removed");

            // Is it a relative path?
            if (!path.startsWith("/")) {

                // Check if it is a valid path
                if (path.indexOf("//") != -1 || path.endsWith("/"))
                    throw new IllegalArgumentException(path);

                return existsNode(path);
            }
        }

        // path started with a '/' so it is absolute
        // we drop the lock and start from the root (omitting the first '/')
        Preferences root = isUserNode() ? userRoot() : systemRoot();
        return root.nodeExists(path.substring(1));

    }

    private boolean existsNode(String path) throws BackingStoreException {

        // Empty String "" indicates this node
        if (path.length() == 0)
            return(!isRemoved());

        // Calculate child name and rest of path
        String childName;
        String childPath;
        int nextSlash = path.indexOf('/');
        if (nextSlash == -1) {
            childName = path;
            childPath = "";
        } else {
            childName = path.substring(0, nextSlash);
            childPath = path.substring(nextSlash+1);
        }

        // Get the child node
        AbstractPreferences child;
        child = (AbstractPreferences)childCache.get(childName);
        if (child == null) {

            if (childName.length() > MAX_NAME_LENGTH)
               throw new IllegalArgumentException(childName);

            // Not in childCache yet so create a new sub node
            child = getChild(childName);

            if (child == null)
                return false;

            childCache.put(childName, child);
        }

        // Lock the child and go down
        synchronized(child.lock) {
            return child.existsNode(childPath);
        }
    }

    /**
     * Returns the child sub node if it exists in the backing store or null
     * if it does not exist. Called (indirectly) by <code>nodeExists()</code>
     * when a child node name can not be found in the cache.
     * <p>
     * Gets the lock on this node, calls <code>childrenNamesSpi()</code> to
     * get an array of all (possibly uncached) children and compares the
     * given name with the names in the array. If the name is found in the
     * array <code>childSpi()</code> is called to get an instance, otherwise
     * null is returned.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     */
    protected AbstractPreferences getChild(String name)
                                    throws BackingStoreException
    {
        synchronized(lock) {
            // Get all the names (not yet in the cache)
            String[] names = childrenNamesSpi();
            for (int i=0; i < names.length; i++)
                if (name.equals(names[i]))
                    return childSpi(name);
           
            // No child with that name found
            return null;
        }
    }

    /**
     * Returns true if this node has been removed with the
     * <code>removeNode()</code> method, false otherwise.
     * <p>
     * Gets the lock on this node and then returns a boolean field set by
     * <code>removeNode</code> methods.
     */
    protected boolean isRemoved() {
        synchronized(lock) {
            return removed;
        }
    }

    /**
     * Returns the parent preferences node of this node or null if this is
     * the root of the preferences tree.
     * <p>
     * Gets the lock on this node, checks that the node has not been removed
     * and returns the parent given to the constructor.
     *
     * @exception IllegalStateException if this node has been removed
     */
    public Preferences parent() {
        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node removed");

            return parent;
        }
    }

    // export methods

    /**
     * XXX
     */
    public void exportNode(OutputStream os)
                                    throws BackingStoreException,
                                           IOException
    {
        NodeWriter nodeWriter = new NodeWriter(this, os);
        nodeWriter.writePrefs();
    }

    /**
     * XXX
     */
    public void exportSubtree(OutputStream os)
                                    throws BackingStoreException,
                                           IOException
    {
        NodeWriter nodeWriter = new NodeWriter(this, os);
        nodeWriter.writePrefsTree();
    }

    // preference entry manipulation methods

    /**
     * Returns an (possibly empty) array with all the keys of the preference
     * entries of this node.
     * <p>
     * This method locks this node and checks if the node has not been
     * removed, if it has been removed it throws an exception, then it returns
     * the result of calling <code>keysSpi()</code>.
     * 
     * @exception BackingStoreException when the backing store cannot be     
     *            reached
     * @exception IllegalStateException if this node has been removed
     */
    public String[] keys() throws BackingStoreException {
        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node removed");

            return keysSpi();
        }
    }


    /**
     * Returns the value associated with the key in this preferences node. If
     * the default value of the key cannot be found in the preferences node
     * entries or something goes wrong with the backing store the supplied
     * default value is returned.
     * <p>
     * Checks that key is not null and not larger then 80 characters,
     * locks this node, and checks that the node has not been removed.
     * Then it calls <code>keySpi()</code> and returns
     * the result of that method or the given default value if it returned
     * null or throwed an exception.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public String get(String key, String defaultVal) {
        if (key.length() > MAX_KEY_LENGTH)
            throw new IllegalArgumentException(key);

        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node removed");

            String value;
            try {
                value = getSpi(key);
            } catch (ThreadDeath death) {
                throw death;
            } catch (Throwable t) {
                value = null;
            }

            if (value != null) {
                return value;
            } else {
                return defaultVal;
            }
        }
    }

    /**
     * Convenience method for getting the given entry as a boolean.
     * When the string representation of the requested entry is either
     * "true" or "false" (ignoring case) then that value is returned,
     * otherwise the given default boolean value is returned.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public boolean getBoolean(String key, boolean defaultVal) {
        String value = get(key, null);

        if ("true".equalsIgnoreCase(value))
            return true;

        if ("false".equalsIgnoreCase(value))
            return false;
        
        return defaultVal;
    }

    /**
     * Convenience method for getting the given entry as a byte array.
     * When the string representation of the requested entry is a valid
     * Base64 encoded string (without any other characters, such as newlines)
     * then the decoded Base64 string is returned as byte array,
     * otherwise the given default byte array value is returned.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public byte[] getByteArray(String key, byte[] defaultVal) {
        String value = get(key, null);

        byte[] b = null;
        if (value != null) {
            b = decode64(value);
        }

        if (b != null)
            return b;
        else
            return defaultVal;
    }
    
    /**
     * Helper method for decoding a Base64 string as an byte array.
     * Returns null on encoding error. This method does not allow any other
     * characters present in the string then the 65 special base64 chars.
     */
    private static byte[] decode64(String s) {
        ByteArrayOutputStream bs = new ByteArrayOutputStream((s.length()/4)*3);
        char[] c = new char[s.length()];
        s.getChars(0, s.length(), c, 0);

        // Convert from base64 chars
        int endchar = -1;
        for(int j = 0; j < c.length && endchar == -1; j++) {
            if (c[j] >= 'A' && c[j] <= 'Z') {
                c[j] -= 'A';
            } else if (c[j] >= 'a' && c[j] <= 'z') {
                c[j] = (char) (c[j] + 26 - 'a');
            } else if (c[j] >= '0' && c[j] <= '9') {
                c[j] = (char) (c[j] + 52 - '0');
            } else if (c[j] == '+') {
                c[j] = 62;
            } else if (c[j] == '/') {
                c[j] = 63;
            } else if (c[j] == '=') {
                endchar = j;
            } else {
                return null; // encoding exception
            }
        }

        int remaining = endchar == -1 ? c.length : endchar;
        int i = 0;
        while (remaining > 0) {
            // Four input chars (6 bits) are decoded as three bytes as
            // 000000 001111 111122 222222

            byte b0 = (byte) (c[i] << 2);
            if (remaining >= 2) {
                b0 += (c[i+1] & 0x30) >> 4;
            }
            bs.write(b0);

            if (remaining >= 3) {
                byte b1 = (byte) ((c[i+1] & 0x0F) << 4);
                b1 += (byte) ((c[i+2] & 0x3C) >> 2);
                bs.write(b1);
            }

            if (remaining >= 4) {
                byte b2 = (byte) ((c[i+2] & 0x03) << 6);
                b2 += c[i+3];
                bs.write(b2);
            }

            i += 4;
            remaining -= 4;
        }

        return bs.toByteArray();
    }

    /**
     * Convenience method for getting the given entry as a double.
     * When the string representation of the requested entry can be decoded
     * with <code>Double.parseDouble()</code> then that double is returned,
     * otherwise the given default double value is returned.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public double getDouble(String key, double defaultVal) {
        String value = get(key, null);

        if (value != null) {
            try {
                return Double.parseDouble(value);
            } catch (NumberFormatException nfe) { /* ignore */ }
        }

        return defaultVal;
    }

    /**
     * Convenience method for getting the given entry as a float.
     * When the string representation of the requested entry can be decoded
     * with <code>Float.parseFloat()</code> then that float is returned,
     * otherwise the given default float value is returned.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public float getFloat(String key, float defaultVal) {
        String value = get(key, null);

        if (value != null) {
            try {
                return Float.parseFloat(value);
            } catch (NumberFormatException nfe) { /* ignore */ }
        }

        return defaultVal;
    }

    /**
     * Convenience method for getting the given entry as an integer.
     * When the string representation of the requested entry can be decoded
     * with <code>Integer.parseInt()</code> then that integer is returned,
     * otherwise the given default integer value is returned.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public int getInt(String key, int defaultVal) {
        String value = get(key, null);

        if (value != null) {
            try {
                return Integer.parseInt(value);
            } catch (NumberFormatException nfe) { /* ignore */ }
        }

        return defaultVal;
    }

    /**
     * Convenience method for getting the given entry as a long.
     * When the string representation of the requested entry can be decoded
     * with <code>Long.parseLong()</code> then that long is returned,
     * otherwise the given default long value is returned.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public long getLong(String key, long defaultVal) {
        String value = get(key, null);

        if (value != null) {
            try {
                return Long.parseLong(value);
            } catch (NumberFormatException nfe) { /* ignore */ }
        }

        return defaultVal;
    }

    /**
     * Sets the value of the given preferences entry for this node.
     * Key and value cannot be null, the key cannot exceed 80 characters
     * and the value cannot exceed 8192 characters.
     * <p>
     * The result will be immediatly visible in this VM, but may not be
     * immediatly written to the backing store.
     * <p>
     * Checks that key and value are valid, locks this node, and checks that
     * the node has not been removed. Then it calls <code>putSpi()</code>.
     *
     * @exception NullPointerException if either key or value are null
     * @exception IllegalArgumentException if either key or value are to large
     * @exception IllegalStateException when this node has been removed
     */
    public void put(String key, String value) {
        if (key.length() > MAX_KEY_LENGTH
            || value.length() > MAX_VALUE_LENGTH)
            throw new IllegalArgumentException("key ("
                                               + key.length() + ")"
                                               + " or value ("
                                               + value.length() + ")"
                                               + " to large");
        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node removed");

            putSpi(key, value);

            // XXX - fire events
        }
            
    }

    /**
     * Convenience method for setting the given entry as a boolean.
     * The boolean is converted with <code>Boolean.toString(value)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public void putBoolean(String key, boolean value) {
        put(key, String.valueOf(value));
        // XXX - Use when using 1.4 compatible Boolean
        // put(key, Boolean.toString(value));
    }

    /**
     * Convenience method for setting the given entry as an array of bytes.
     * The byte array is converted to a Base64 encoded string
     * and then stored in the preference entry as that string.
     * <p>
     * Note that a byte array encoded as a Base64 string will be about 1.3
     * times larger then the original length of the byte array, which means
     * that the byte array may not be larger about 6 KB.
     *
     * @exception NullPointerException if either key or value are null
     * @exception IllegalArgumentException if either key or value are to large
     * @exception IllegalStateException when this node has been removed
     */
    public void putByteArray(String key, byte[] value) {
        put(key, encode64(value));
    }

    /**
     * Helper method for encoding an array of bytes as a Base64 String.
     */
    private static String encode64(byte[] b) {
        StringBuffer sb = new StringBuffer((b.length/3)*4);

        int i = 0;
        int remaining = b.length;
        char c[] = new char[4];
        while (remaining > 0) {
            // Three input bytes are encoded as four chars (6 bits) as
            // 00000011 11112222 22333333

            c[0] = (char) ((b[i] & 0xFC) >> 2);
            c[1] = (char) ((b[i] & 0x03) << 4);
            if (remaining >= 2) {
                c[1] += (char) ((b[i+1] & 0xF0) >> 4);
                c[2] = (char) ((b[i+1] & 0x0F) << 2);
                if (remaining >= 3) {
                    c[2] += (char) ((b[i+2] & 0xC0) >> 6);
                    c[3] = (char) (b[i+2] & 0x3F);
                } else {
                    c[3] = 64;
                }
            } else {
                c[2] = 64;
                c[3] = 64;
            }

            // Convert to base64 chars
            for(int j = 0; j < 4; j++) {
                if (c[j] < 26) {
                    c[j] += 'A';
                } else if (c[j] < 52) {
                    c[j] = (char) (c[j] - 26 + 'a');
                } else if (c[j] < 62) {
                    c[j] = (char) (c[j] - 52 + '0');
                } else if (c[j] == 62) {
                    c[j] = '+';
                } else if (c[j] == 63) {
                    c[j] = '/';
                } else {
                    c[j] = '=';
                }
            }

            sb.append(c);
            i += 3;
            remaining -= 3;
        }

        return sb.toString();
    }

    /**
     * Convenience method for setting the given entry as a double.
     * The double is converted with <code>Double.toString(double)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public void putDouble(String key, double value) {
        put(key, Double.toString(value));
    }

    /**
     * Convenience method for setting the given entry as a float.
     * The float is converted with <code>Float.toString(float)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public void putFloat(String key, float value) {
        put(key, Float.toString(value));
    }

    /**
     * Convenience method for setting the given entry as an integer.
     * The integer is converted with <code>Integer.toString(int)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public void putInt(String key, int value) {
        put(key, Integer.toString(value));
    }

    /**
     * Convenience method for setting the given entry as a long.
     * The long is converted with <code>Long.toString(long)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public void putLong(String key, long value) {
        put(key, Long.toString(value));
    }

    /**
     * Removes the preferences entry from this preferences node.
     * <p>     
     * The result will be immediatly visible in this VM, but may not be
     * immediatly written to the backing store.
     * <p>
     * This implementation checks that the key is not larger then 80
     * characters, gets the lock of this node, checks that the node has
     * not been removed and calls <code>removeSpi</code> with the given key.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public void remove(String key) {
        if (key.length() > MAX_KEY_LENGTH)
            throw new IllegalArgumentException(key);

        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node removed");

            removeSpi(key);
        }
    }

    /**
     * Removes all entries from this preferences node. May need access to the
     * backing store to get and clear all entries.
     * <p>
     * The result will be immediatly visible in this VM, but may not be
     * immediatly written to the backing store.
     * <p>
     * This implementation locks this node, checks that the node has not been
     * removed and calls <code>keys()</code> to get a complete array of keys
     * for this node. For every key found <code>removeSpi()</code> is called.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has been removed
     */
    public void clear() throws BackingStoreException {
        synchronized(lock) {
            if (isRemoved())
                throw new IllegalStateException("Node Removed");

            String[] keys = keys();
            for (int i = 0; i < keys.length; i++) {
                removeSpi(keys[i]);
            }
        }
    }

    /**
     * Writes all preference changes on this and any subnode that have not
     * yet been written to the backing store. This has no effect on the
     * preference entries in this VM, but it makes sure that all changes
     * are visible to other programs (other VMs might need to call the
     * <code>sync()</code> method to actually see the changes to the backing
     * store.
     * <p>
     * Locks this node, calls the <code>flushSpi()</code> method, gets all
     * the (cached - already existing in this VM) subnodes and then calls
     * <code>flushSpi()</code> on every subnode with this node unlocked and
     * only that particular subnode locked.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     */
    public void flush() throws BackingStoreException {
        flushNode(false);
    }

    /**
     * Writes and reads all preference changes to and from this and any
     * subnodes. This makes sure that all local changes are written to the
     * backing store and that all changes to the backing store are visible
     * in this preference node (and all subnodes).
     * <p>
     * Checks that this node is not removed, locks this node, calls the
     * <code>syncSpi()</code> method, gets all the subnodes and then calls
     * <code>syncSpi()</code> on every subnode with this node unlocked and
     * only that particular subnode locked.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has been removed
     */
    public void sync() throws BackingStoreException {
        flushNode(true);
    }
    

    /**
     * Private helper method that locks this node and calls either
     * <code>flushSpi()</code> if <code>sync</code> is false, or
     * <code>flushSpi()</code> if <code>sync</code> is true. Then it gets all
     * the currently cached subnodes. For every subnode it calls this method
     * recursively with this node no longer locked.
     * <p>
     * Called by either <code>flush()</code> or <code>sync()</code>
     */
    private void flushNode(boolean sync) throws BackingStoreException {
        String[] keys = null;
        synchronized(lock) {
            if (sync) {
                syncSpi();
            } else {
                flushSpi();
            }
            keys = (String[]) childCache.keySet().toArray(new String[]{});
        }

        if (keys != null) {
            for (int i = 0; i < keys.length; i++) {
                // Have to lock this node again to access the childCache
                AbstractPreferences subNode;
                synchronized(this) {
                    subNode = (AbstractPreferences) childCache.get(keys[i]);
                }

                // The child could already have been removed from the cache
                if (subNode != null) {
                    subNode.flushNode(sync);
                }
            }
        }
    }

    /**
     * Removes this and all subnodes from the backing store and clears all
     * entries. After removal this instance will not be useable (except for
     * a few methods that don't throw a <code>InvalidStateException</code>),
     * even when a new node with the same path name is created this instance
     * will not be usable again.
     * <p>
     * Checks that this is not a root node. If not it locks the parent node,
     * then locks this node and checks that the node has not yet been removed.
     * Then it makes sure that all subnodes of this node are in the child cache,
     * by calling <code>childSpi()</code> on any children not yet in the cache.
     * Then for all children it locks the subnode and removes it. After all
     * subnodes have been purged the child cache is cleared, this nodes removed
     * flag is set and any listeners are called. Finally this node is removed
     * from the child cache of the parent node.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has already been removed
     * @exception UnsupportedOperationException if this is a root node
     */
    public void removeNode() throws BackingStoreException {
        // Check if it is a root node
        if (parent == null)
            throw new UnsupportedOperationException("Cannot remove root node");

        synchronized(parent) {
            synchronized(this) {
                if (isRemoved())
                    throw new IllegalStateException("Node Removed");

                purge();
            }
            parent.childCache.remove(name);
        }
    }

    /**
     * Private helper method used to completely remove this node.
     * Called by <code>removeNode</code> with the parent node and this node
     * locked.
     * <p>
     * Makes sure that all subnodes of this node are in the child cache,
     * by calling <code>childSpi()</code> on any children not yet in the
     * cache. Then for all children it locks the subnode and calls this method
     * on that node. After all subnodes have been purged the child cache is
     * cleared, this nodes removed flag is set and any listeners are called.
     */
    private void purge() throws BackingStoreException
    {
        // Make sure all children have an AbstractPreferences node in cache
        String children[] = childrenNamesSpi();
        for (int i = 0; i < children.length; i++) {
            if (childCache.get(children[i]) == null)
                childCache.put(children[i], childSpi(children[i]));
        }

        // purge all children
        Iterator i = childCache.values().iterator();
        while (i.hasNext()) {
            AbstractPreferences node = (AbstractPreferences) i.next();
            synchronized(node) {
                node.purge();
            }
        }

        // Cache is empty now
        childCache.clear();

        // remove this node
        removeNodeSpi();
        removed = true;

        // XXX - check for listeners
    }

    // listener methods

    /**
     * XXX
     */
    public void addNodeChangeListener(NodeChangeListener listener) {
        // XXX
    }

    public void addPreferenceChangeListener(PreferenceChangeListener listener) {
        // XXX
    }

    public void removeNodeChangeListener(NodeChangeListener listener) {
        // XXX
    }

    public void removePreferenceChangeListener
                            (PreferenceChangeListener listener)
    {
        // XXX
    }

    // abstract spi methods

    /**
     * Returns the names of the sub nodes of this preference node.
     * This method only has to return any not yet cached child names,
     * but may return all names if that is easier. It must not return
     * null when there are no children, it has to return an empty array
     * in that case. Since this method must consult the backing store to
     * get all the sub node names it may throw a BackingStoreException.
     * <p>
     * Called by <code>childrenNames()</code> with this node locked.
     */
    protected abstract String[] childrenNamesSpi() throws BackingStoreException;

    /**
     * Returns a child note with the given name.
     * This method is called by the <code>node()</code> method (indirectly
     * through the <code>getNode()</code> helper method) with this node locked
     * if a sub node with this name does not already exist in the child cache.
     * If the child node did not aleady exist in the backing store the boolean
     * field <code>newNode</code> of the returned node should be set.
     * <p>
     * Note that this method should even return a non-null child node if the
     * backing store is not available since it may not throw a
     * <code>BackingStoreException</code>.
     */
    protected abstract AbstractPreferences childSpi(String name);

    /**
     * Returns an (possibly empty) array with all the keys of the preference
     * entries of this node.
     * <p>
     * Called by <code>keys()</code> with this node locked if this node has
     * not been removed. May throw an exception when the backing store cannot
     * be accessed.
     *
     * @exception BackingStoreException when the backing store cannot be     
     *            reached
     */
    protected abstract String[] keysSpi() throws BackingStoreException;
     
    /**
     * Returns the value associated with the key in this preferences node or
     * null when the key does not exist in this preferences node.
     * <p>
     * Called by <code>key()</code> with this node locked after checking that
     * key is valid, not null and that the node has not been removed.
     * <code>key()</code> will catch any exceptions that this method throws.
     */
    protected abstract String getSpi(String key);

    /**
     * Sets the value of the given preferences entry for this node.
     * The implementation is not required to propagate the change to the
     * backing store immediatly. It may not throw an exception when it tries
     * to write to the backing store and that operation fails, the failure
     * should be registered so a later invocation of <code>flush()</code>
     * or <code>sync()</code> can signal the failure.
     * <p>
     * Called by <code>put()</code> with this node locked after checking that
     * key and value are valid and non-null.
     */
    protected abstract void putSpi(String key, String value);

    /**
     * Removes the given key entry from this preferences node.
     * The implementation is not required to propagate the change to the
     * backing store immediatly.  It may not throw an exception when it tries
     * to write to the backing store and that operation fails, the failure
     * should be registered so a later invocation of <code>flush()</code>
     * or <code>sync()</code> can signal the failure.
     * <p>
     * Called by <code>remove()</code> with this node locked after checking
     * that the key is valid and non-null.
     */
    protected abstract void removeSpi(String key);

    /**
     * Writes all entries of this preferences node that have not yet been
     * written to the backing store and possibly creates this node in the
     * backing store, if it does not yet exist. Should only write changes to
     * this node and not write changes to any subnodes.
     * Note that the node can be already removed in this VM. To check if
     * that is the case the implementation can call <code>isRemoved()</code>.
     * <p>
     * Called (indirectly) by <code>flush()</code> with this node locked.
     */
    protected abstract void flushSpi() throws BackingStoreException;

    /**
     * Writes all entries of this preferences node that have not yet been
     * written to the backing store and reads any entries that have changed
     * in the backing store but that are not yet visible in this VM.
     * Should only sync this node and not change any of the subnodes.
     * Note that the node can be already removed in this VM. To check if
     * that is the case the implementation can call <code>isRemoved()</code>.
     * <p>
     * Called (indirectly) by <code>sync()</code> with this node locked.
     */
    protected abstract void syncSpi() throws BackingStoreException;

    /**
     * Clears this node from this VM and removes it from the backing store.
     * After this method has been called the node is marked as removed.
     * <p>
     * Called (indirectly) by <code>removeNode()</code> with this node locked
     * after all the sub nodes of this node have already been removed.
     */
    protected abstract void removeNodeSpi() throws BackingStoreException;
}
