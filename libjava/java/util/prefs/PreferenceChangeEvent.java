/* PreferenceChangeEvent - ObjectEvent fired when a Preferences entry changes
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

package java.util.prefs;

import java.util.EventObject;

/**
 * ObjectEvent fired when a Preferences entry changes.
 * This event is generated when a entry is added, changed or removed.
 * When an entry is removed then <code>getNewValue</code> will return null.
 * <p>
 * Preference change events are only generated for entries in one particular
 * preference node. Notification of subnode addition/removal is given by a
 * <code>NodeChangeEvent</code>.
 *
 * @since 1.4
 * @author Mark Wielaard (mark@klomp.org)
 */
public class PreferenceChangeEvent extends EventObject {

  private static final long serialVersionUID = 793724513368024975L;
  
    /**
     * The key of the changed entry.
     */
    private final String key;

    /**
     * The new value of the changed entry, or null when the entry was removed.
     */
    private final String newValue;

    /**
     * Creates a new PreferenceChangeEvent.
     *
     * @param node The source preference node for which an entry was added,
     * changed or removed
     * @param key The key of the entry that was added, changed or removed
     * @param value The new value of the entry that was added or changed, or
     * null when the entry was removed
     */
    public PreferenceChangeEvent(Preferences node, String key, String value) {
        super(node);
        this.key = key;
        this.newValue = value;
    }

    /**
     * Returns the source Preference node from which an entry was added,
     * changed or removed.
     */
    public Preferences getNode() {
        return (Preferences) source;
    }

    /**
     * Returns the key of the entry that was added, changed or removed.
     */
    public String getKey() {
        return key;
    }

    /**
     * Returns the new value of the entry that was added or changed, or
     * returns null when the entry was removed.
     */
    public String getNewValue() {
        return newValue;
    }
}
