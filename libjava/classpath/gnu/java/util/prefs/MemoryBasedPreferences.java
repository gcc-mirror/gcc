/* MemoryBasedPreferences - A Preference node which holds all entries in memory
   Copyright (C) 2001 Free Software Foundation, Inc.

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

import java.util.HashMap;

import java.util.prefs.*;

/**
 * A Preference node which holds all entries in memory
 *
 * @author Mark Wielaard (mark@klomp.org)
 */
public class MemoryBasedPreferences extends AbstractPreferences {

    /** True if this is a preference node in the user tree, false otherwise. */
    private final boolean isUser;

    /** Contains all the preference entries of this node. */
    private HashMap entries = new HashMap();

    /**
     * Creates a new preferences node with the given name and parent.
     * When isUser is true it will be user node otherwise it will be a system
     * node. It will always set the <code>newNode</code> field to true
     * since there is no real backing store, so all nodes are new.
     */
    public MemoryBasedPreferences(MemoryBasedPreferences parent,
                                  String name,
                                  boolean isUser) {
        super(parent, name);
        this.isUser = isUser;

        // Since we do not have a real backing store all nodes are new
        newNode = true;
    }

    /**
     * Returns true if this node was created as a user node.
     */
    public boolean isUserNode() {
        return isUser;
    }

    /**
     * Returns an empty array since all children names are always already
     * chached.
     */
    protected String[] childrenNamesSpi() throws BackingStoreException {
        return new String[0];
    }

    /**
     * Returns a new node with the given name with as parent this node and
     * with the <code>isUser</code> flag set to the same value as this node.
     */
    protected AbstractPreferences childSpi(String childName) {
       return new MemoryBasedPreferences(this, childName, isUser);
    }

    /**
     * Returns a (possibly empty) array of keys of the preferences entries of
     * this node.
     */
    protected String[] keysSpi() throws BackingStoreException {
        return (String[]) entries.keySet().toArray(new String[entries.size()]);
    }

    /**
     * Returns the associated value from this nodes preferences entries or
     * null when the key has not been set.
     */
    protected String getSpi(String key) {
        return (String) entries.get(key);
    }

    /**
     * Sets the value for the given key.
     */
    protected void putSpi(String key, String value) {
        entries.put(key, value);
    }

    /**
     * Removes the entry with the given key.
     */
    protected void removeSpi(String key) {
        entries.remove(key);
    }

    /**
     * Does nothing since we do not have any backing store.
     */
    protected void flushSpi() {
    }

    /**
     * Does nothing since we do not have any backing store.
     */
    protected void syncSpi() {
    }

    /**
     * Just removes the entries map of this node.
     */
    protected void removeNodeSpi() {
        entries = null;
    }
}
