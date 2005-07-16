/* NamingMap.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.NamingService;

import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.InvalidName;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * The Naming Map maps the single names components into associated objects or
 * naming contexts.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class NamingMap
{
  /**
   * The actual map.
   */
  private final TreeMap map;

  /**
   * Creates an instance of the naming map, intialising the comparator
   * to the {@link cmpNameComparator}.
   */
  public NamingMap()
  {
    map = new TreeMap(cmpNameComponent.singleton);
  }

  /**
   * Put the given CORBA object, specifying the given name as a key.
   * If the entry with the given name already exists, or if the given
   * object is already mapped under another name, the
   * {@link AlreadyBound} exception will be thrown.
   *
   * @param name the name
   * @param object the object
   */
  public void bind(NameComponent name, org.omg.CORBA.Object object)
            throws AlreadyBound, InvalidName
  {
    if (containsKey(name))
      {
        Object x = get(name);

        // Do not throw an exception if the same object is named by
        // the same name.
        if (x.equals(object))
          throw new AlreadyBound("The name is in use for another object");
      }
    else
      {
        if (containsValue(object))
          throw new AlreadyBound("Tha object has another name");
      }
  }

  /**
   * Checks if this map contains the definition of the given name.
   *
   * @param key the name to check.
   */
  public boolean containsKey(NameComponent key)
  {
    return map.containsKey(key);
  }

  /**
   * Checks if this map contains the definition of the given object.
   *
   * @param object the object to check.
   */
  public boolean containsValue(org.omg.CORBA.Object object)
  {
    return map.containsValue(object);
  }

  /**
   * Returns the map entry set.
   *
   * @return the map entry set, containing the instances of the
   * Map.Entry.
   */
  public Set entries()
  {
    return map.entrySet();
  }

  /**
   * Get the CORBA object, associated with the given name.
   *
   * @param name the name.
   *
   * @return the associated object, null if none.
   */
  public org.omg.CORBA.Object get(NameComponent name)
  {
    return (org.omg.CORBA.Object) map.get(name);
  }

  /**
   * Put the given CORBA object, specifying the given name as a key.
   * Remove all pre - existing mappings for the given name and object.
   *
   * @param name the name.
   * @param object
   */
  public void rebind(NameComponent name, org.omg.CORBA.Object object)
              throws InvalidName
  {
    // Remove the existing mapping for the given name, if present.
    remove(name);

    Iterator iter = entries().iterator();
    Map.Entry item;

    // Remove the existing mapping for the given object, if present.
    while (iter.hasNext())
      {
        item = (Map.Entry) iter.next();
        if (item.getValue().equals(object))
          iter.remove();
      }

    map.put(name, object);
  }

  /**
   * Removes the given name, if present.
   *
   * @param name a name to remove.
   */
  public void remove(NameComponent name)
  {
    map.remove(name);
  }

  /**
   * Get the size of the map.
   */
  public int size()
  {
    return map.size();
  }
}
