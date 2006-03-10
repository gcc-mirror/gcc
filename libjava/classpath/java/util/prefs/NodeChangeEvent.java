/* NodeChangeEvent - ObjectEvent fired when a Preference node is added/removed
   Copyright (C) 2001, 2006 Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.NotSerializableException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.EventObject;

/**
 * ObjectEvent fired when a Preference node is added/removed.
 * This event is only generated when a new subnode is added or a subnode is
 * removed from a preference node. Changes in the entries of a preference node
 * are indicated with a <code>PreferenceChangeEvent</code>.
 * <p>
 * Note that although this class is marked as serializable, attempts to
 * serialize it will fail with NotSerializableException.
 *
 * @since 1.4
 * @author Mark Wielaard (mark@klomp.org)
 */
public class NodeChangeEvent extends EventObject {

  // We have this to placate the compiler.
  private static final long serialVersionUID =8068949086596572957L; 
  
    /**
     * The sub node that was added or removed.
     * Defined transient just like <code>EventObject.source</code> since
     * this object should be serializable, but Preferences is in general not
     * serializable.
     */
    private final transient Preferences child;

    /**
     * Creates a new NodeChangeEvent.
     *
     * @param parentNode The source preference node from which a subnode was
     * added or removed
     * @param childNode The preference node that was added or removed
     */
    public NodeChangeEvent(Preferences parentNode, Preferences childNode) {
        super(parentNode);
        child = childNode;
    }

    /**
     * Returns the source parent preference node from which a subnode was
     * added or removed.
     */
    public Preferences getParent() {
        return (Preferences) source;
    }

    /**
     * Returns the child preference subnode that was added or removed.
     * To see wether it is still a valid preference node one has to call
     * <code>event.getChild().nodeExists("")</code>.
     */
    public Preferences getChild() {
        return child;
    }

    private void readObject(ObjectInputStream ois)
      throws IOException
    {
      throw new NotSerializableException("LineEvent is not serializable");
    }
  
    private void writeObject(ObjectOutputStream oos)
      throws IOException
    {
      throw new NotSerializableException("LineEvent is not serializable");
    }
}
