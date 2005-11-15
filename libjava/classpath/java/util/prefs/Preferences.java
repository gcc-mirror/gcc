/* Preferences -- Preference node containing key value entries and subnodes
   Copyright (C) 2001, 2004, 2005  Free Software Foundation, Inc.

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

import gnu.java.util.prefs.NodeReader;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.AccessController;
import java.security.Permission;
import java.security.PrivilegedAction;

/**
 * Preference node containing key value entries and subnodes.
 * <p>
 * There are two preference node trees, a system tree which can be accessed
 * by calling <code>systemRoot()</code> containing system preferences usefull
 * for all users, and a user tree that can be accessed by calling
 * <code>userRoot()</code> containing preferences that can differ between
 * different users. How different users are identified is implementation
 * depended. It can be determined by Thread, Access Control Context or Subject.
 * <p>
 * This implementation uses the "java.util.prefs.PreferencesFactory" system
 * property to find a class that implement <code>PreferencesFactory</code>
 * and initialized that class (if it has a public no arguments contructor)
 * to get at the actual system or user root. If the system property is not set,
 * or the class cannot be initialized it uses the default implementation
 * <code>gnu.java.util.prefs.FileBasedFactory</code>.
 * <p>
 * Besides the two static method above to get the roots of the system and user
 * preference node trees there are also two convenience methods to access the
 * default preference node for a particular package an object is in. These are
 * <code>userNodeForPackage()</code> and <code>systemNodeForPackage()</code>.
 * Both methods take an Object as an argument so accessing preferences values
 * can be as easy as calling <code>Preferences.userNodeForPackage(this)</code>.
 * <p>
 * Note that if a security manager is installed all static methods check for
 * <code>RuntimePermission("preferences")</code>. But if this permission is
 * given to the code then it can access and change all (user) preference nodes
 * and entries. So you should be carefull not to store to sensitive information
 * or make security decissions based on preference values since there is no
 * more fine grained control over what preference values can be changed once
 * code has been given the correct runtime permission.
 * <p>
 * XXX
 *
 * @since 1.4
 * @author Mark Wielaard (mark@klomp.org)
 */
public abstract class Preferences {

    // Static Fields

    /**
     * Default PreferencesFactory class used when the system property
     * "java.util.prefs.PreferencesFactory" is not set.
	 * <p>
	 * XXX - Currently set to MemoryBasedFactory, should be changed
	 * when FileBasedPreferences backend works.
     */
    private static final String defaultFactoryClass
        = "gnu.java.util.prefs.MemoryBasedFactory";

    /** Permission needed to access system or user root. */
    private static final Permission prefsPermission
        = new RuntimePermission("preferences");

    /**
     * The preferences factory object that supplies the system and user root.
     * Set and returned by the getFactory() method.
     */
    private static PreferencesFactory factory;

    /** Maximum node name length. 80 characters. */
    public static final int MAX_NAME_LENGTH = 80;

    /** Maximum entry key length. 80 characters. */
    public static final int MAX_KEY_LENGTH = 80;

    /** Maximum entry value length. 8192 characters. */
    public static final int MAX_VALUE_LENGTH = 8192;

    // Constructors

    /**
     * Creates a new Preferences node. Can only be used by subclasses.
     * Empty implementation.
     */
    protected Preferences() {}

    // Static methods

    /**
     * Returns the system preferences root node containing usefull preferences
     * for all users. It is save to cache this value since it should always
     * return the same preference node.
     *
     * @return the root system preference node
     * @exception SecurityException when a security manager is installed and
     * the caller does not have <code>RuntimePermission("preferences")</code>.
     */
    public static Preferences systemRoot() throws SecurityException {
        // Get the preferences factory and check for permission
        PreferencesFactory factory = getFactory();

        return factory.systemRoot();
    }

    /**
     * Returns the user preferences root node containing preferences for the
     * the current user. How different users are identified is implementation
     * depended. It can be determined by Thread, Access Control Context or
     * Subject.
     *
     * @return the root user preference node
     * @exception SecurityException when a security manager is installed and
     * the caller does not have <code>RuntimePermission("preferences")</code>.
     */
    public static Preferences userRoot() throws SecurityException {
        // Get the preferences factory and check for permission
        PreferencesFactory factory = getFactory();
        return factory.userRoot();
    }

    /**
     * Private helper method for <code>systemRoot()</code> and
     * <code>userRoot()</code>. Checks security permission and instantiates the
     * correct factory if it has not yet been set.
     * <p>
     * When the preferences factory has not yet been set this method first
     * tries to get the system propery "java.util.prefs.PreferencesFactory"
     * and tries to initializes that class. If the system property is not set
     * or initialization fails it returns an instance of the default factory
     * <code>gnu.java.util.prefs.FileBasedPreferencesFactory</code>.
     *
     * @return the preferences factory to use
     * @exception SecurityException when a security manager is installed and
     * the caller does not have <code>RuntimePermission("preferences")</code>.
     */
    private static PreferencesFactory getFactory() throws SecurityException {

        // First check for permission
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkPermission(prefsPermission);
        }

        // Get the factory
        if (factory == null) {
            // Caller might not have enough permissions
            factory = (PreferencesFactory) AccessController.doPrivileged(
                        new PrivilegedAction() {
                            public Object run() {
                                PreferencesFactory pf = null;
                                String className = System.getProperty
                                    ("java.util.prefs.PreferencesFactory");
                                if (className != null) {
                                    try {
                                        Class fc = Class.forName(className);
                                        Object o = fc.newInstance();
                                        pf = (PreferencesFactory) o;
                                    } catch (ClassNotFoundException cnfe)
                                        {/*ignore*/}
                                    catch (InstantiationException ie)
                                        {/*ignore*/}
                                    catch (IllegalAccessException iae)
                                        {/*ignore*/}
                                    catch (ClassCastException cce)
                                        {/*ignore*/}
                                }
                                return pf;
                            }
                        });

            // Still no factory? Use our default.
            if (factory == null)
	      {
                try
		  {
                    Class cls = Class.forName (defaultFactoryClass);
                    factory = (PreferencesFactory) cls.newInstance();
                  }
		catch (Exception e)
		  {
                    throw new RuntimeException ("Couldn't load default factory"
                        + " '"+ defaultFactoryClass +"'");
                    // XXX - when using 1.4 compatible throwables add cause
                  }
              }

        }
	
        return factory;
    }

    /**
     * Returns the system preferences node for the package of a class.
     * The package node name of the class is determined by dropping the
     * final component of the fully qualified class name and
     * changing all '.' to '/' in the package name. If the class of the
     * object has no package then the package node name is "&lt;unnamed&gt;".
     * The returned node is <code>systemRoot().node(packageNodeName)</code>.
     *
     * @param c Object whose default system preference node is requested
     * @returns system preferences node that should be used by class c
     * @exception SecurityException when a security manager is installed and
     * the caller does not have <code>RuntimePermission("preferences")</code>.
     */
    public static Preferences systemNodeForPackage(Class c)
            throws SecurityException
    {
        return nodeForPackage(c, systemRoot());
    }

    /**
     * Returns the user preferences node for the package of a class.
     * The package node name of the class is determined by dropping the
     * final component of the fully qualified class name and
     * changing all '.' to '/' in the package name. If the class of the
     * object has no package then the package node name is "&lt;unnamed&gt;".
     * The returned node is <code>userRoot().node(packageNodeName)</code>.
     *
     * @param c Object whose default userpreference node is requested
     * @returns userpreferences node that should be used by class c
     * @exception SecurityException when a security manager is installed and
     * the caller does not have <code>RuntimePermission("preferences")</code>.
     */
    public static Preferences userNodeForPackage(Class c)
            throws SecurityException
    {
        return nodeForPackage(c, userRoot());
    }

    /**
     * Private helper method for <code>systemNodeForPackage()</code> and
     * <code>userNodeForPackage()</code>. Given the correct system or user
     * root it returns the correct Preference node for the package node name
     * of the given object.
     */
    private static Preferences nodeForPackage(Class c, Preferences root) {
        // Get the package path
        String className = c.getName();
        String packagePath;
        int index = className.lastIndexOf('.');
        if(index == -1) {
            packagePath = "<unnamed>";
        } else {
            packagePath = className.substring(0,index).replace('.','/');
        }

        return root.node(packagePath);
    }

    /**
     * XXX
     */
    public static void importPreferences(InputStream is) 
                                    throws InvalidPreferencesFormatException,
                                           IOException
    {
        PreferencesFactory factory = getFactory();
        NodeReader reader = new NodeReader(is, factory);
        reader.importPreferences();
    }

    // abstract methods (identification)

    /**
     * Returns the absolute path name of this preference node.
     * The absolute path name of a node is the path name of its parent node 
     * plus a '/' plus its own name. If the node is the root node and has no
     * parent then its name is "" and its absolute path name is "/".
     */
    public abstract String absolutePath();

    /**
     * Returns true if this node comes from the user preferences tree, false
     * if it comes from the system preferences tree.
     */
    public abstract boolean isUserNode();

    /**
     * Returns the name of this preferences node. The name of the node cannot
     * be null, can be mostly 80 characters and cannot contain any '/'
     * characters. The root node has as name "".
     */
    public abstract String name();

    /**
     * Returns the String given by
     * <code>
     * (isUserNode() ? "User":"System") + " Preference Node: " + absolutePath()
     * </code>
     */
    public abstract String toString();

    // abstract methods (navigation)

    /**
     * Returns all the direct sub nodes of this preferences node.
     * Needs access to the backing store to give a meaningfull answer.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException when this node has been removed
     */
    public abstract String[] childrenNames() throws BackingStoreException;

    /**
     * Returns a sub node of this preferences node if the given path is
     * relative (does not start with a '/') or a sub node of the root
     * if the path is absolute (does start with a '/').
     *
     * @exception IllegalStateException if this node has been removed
     * @exception IllegalArgumentException if the path contains two or more
     * consecutive '/' characters, ends with a '/' charactor and is not the
     * string "/" (indicating the root node) or any name on the path is more
     * then 80 characters long
     */
    public abstract Preferences node(String path);

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
    public abstract boolean nodeExists(String path)
                                throws BackingStoreException;

    /**
     * Returns the parent preferences node of this node or null if this is
     * the root of the preferences tree.
     *
     * @exception IllegalStateException if this node has been removed
     */
    public abstract Preferences parent();

    // abstract methods (export)

    /**
     * XXX
     */
    public abstract void exportNode(OutputStream os)
                                throws BackingStoreException,
                                       IOException;

    /**
     * XXX
     */
    public abstract void exportSubtree(OutputStream os)
                                throws BackingStoreException,
                                       IOException;

    // abstract methods (preference entry manipulation)

    /**
     * Returns an (possibly empty) array with all the keys of the preference
     * entries of this node.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has been removed
     */
    public abstract String[] keys() throws BackingStoreException;

    /**
     * Returns the value associated with the key in this preferences node. If
     * the default value of the key cannot be found in the preferences node
     * entries or something goes wrong with the backing store the supplied
     * default value is returned.
     *
     * @exception IllegalArgumentException if key is larger then 80 characters
     * @exception IllegalStateException if this node has been removed
     * @exception NullPointerException if key is null
     */
    public abstract String get(String key, String defaultVal);

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
    public abstract boolean getBoolean(String key, boolean defaultVal);

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
    public abstract byte[] getByteArray(String key, byte[] defaultVal);

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
    public abstract double getDouble(String key, double defaultVal);

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
    public abstract float getFloat(String key, float defaultVal);

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
    public abstract int getInt(String key, int defaultVal);

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
    public abstract long getLong(String key, long defaultVal);

    /**
     * Sets the value of the given preferences entry for this node.
     * Key and value cannot be null, the key cannot exceed 80 characters
     * and the value cannot exceed 8192 characters.
     * <p>
     * The result will be immediatly visible in this VM, but may not be
     * immediatly written to the backing store.
     *
     * @exception NullPointerException if either key or value are null
     * @exception IllegalArgumentException if either key or value are to large
     * @exception IllegalStateException when this node has been removed
     */
    public abstract void put(String key, String value);

    /**
     * Convenience method for setting the given entry as a boolean.
     * The boolean is converted with <code>Boolean.toString(value)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public abstract void putBoolean(String key, boolean value);

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
    public abstract void putByteArray(String key, byte[] value);

    /**
     * Convenience method for setting the given entry as a double.
     * The double is converted with <code>Double.toString(double)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public abstract void putDouble(String key, double value);

    /**
     * Convenience method for setting the given entry as a float.
     * The float is converted with <code>Float.toString(float)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public abstract void putFloat(String key, float value);

    /**
     * Convenience method for setting the given entry as an integer.
     * The integer is converted with <code>Integer.toString(int)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public abstract void putInt(String key, int value);

    /**
     * Convenience method for setting the given entry as a long.
     * The long is converted with <code>Long.toString(long)</code>
     * and then stored in the preference entry as that string.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public abstract void putLong(String key, long value);

    /**
     * Removes the preferences entry from this preferences node.
     * <p>
     * The result will be immediatly visible in this VM, but may not be
     * immediatly written to the backing store.
     *
     * @exception NullPointerException if the key is null
     * @exception IllegalArgumentException if the key length is to large
     * @exception IllegalStateException when this node has been removed
     */
    public abstract void remove(String key);

    // abstract methods (preference node manipulation)

    /**
     * Removes all entries from this preferences node. May need access to the
     * backing store to get and clear all entries.
     * <p>
     * The result will be immediatly visible in this VM, but may not be
     * immediatly written to the backing store.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has been removed
     */
    public abstract void clear() throws BackingStoreException;

    /**
     * Writes all preference changes on this and any subnode that have not
     * yet been written to the backing store. This has no effect on the
     * preference entries in this VM, but it makes sure that all changes
     * are visible to other programs (other VMs might need to call the
     * <code>sync()</code> method to actually see the changes to the backing
     * store.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has been removed
     */
    public abstract void flush() throws BackingStoreException;

    /**
     * Writes and reads all preference changes to and from this and any
     * subnodes. This makes sure that all local changes are written to the
     * backing store and that all changes to the backing store are visible
     * in this preference node (and all subnodes).
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has been removed
     */
    public abstract void sync() throws BackingStoreException;

    /**
     * Removes this and all subnodes from the backing store and clears all
     * entries. After removal this instance will not be useable (except for
     * a few methods that don't throw a <code>InvalidStateException</code>),
     * even when a new node with the same path name is created this instance
     * will not be usable again. The root (system or user) may never be removed.
     * <p>
     * Note that according to the specification an implementation may delay
     * removal of the node from the backing store till the <code>flush()</code>
     * method is called. But the <code>flush()</code> method may throw a 
     * <code>IllegalStateException</code> when the node has been removed.
     * So most implementations will actually remove the node and any subnodes
     * from the backing store immediatly.
     *
     * @exception BackingStoreException when the backing store cannot be
     *            reached
     * @exception IllegalStateException if this node has already been removed
     * @exception UnsupportedOperationException if this is a root node
     */
    public abstract void removeNode() throws BackingStoreException;

    // abstract methods (listeners)

    public abstract void addNodeChangeListener(NodeChangeListener listener);

    public abstract void addPreferenceChangeListener
                            (PreferenceChangeListener listener);

    public abstract void removeNodeChangeListener(NodeChangeListener listener);

    public abstract void removePreferenceChangeListener
                            (PreferenceChangeListener listener);
}

