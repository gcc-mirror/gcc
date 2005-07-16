/* Connected_objects.java --
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


package gnu.CORBA;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * The repository of objects, that have been connected to the
 * {@link FunctionalORB} by the method
 * {@link ORB.connect(org.omg.CORBA.Object)}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Connected_objects
{
  /**
   * The reference data about the connected object.
   */
  public class cObject
  {
    /**
     * Create an initialised instance.
     */
    cObject(org.omg.CORBA.Object _object, int _port, byte[] _key)
    {
      object = _object;
      port = _port;
      key = _key;
    }

    /**
     * The object.
     */
    public final org.omg.CORBA.Object object;

    /**
     * The port on that the object is connected.
     */
    public final int port;

    /**
     * The object key.
     */
    public final byte[] key;

    public boolean equals(java.lang.Object other)
    {
      if (other instanceof cObject)
        {
          cObject o = (cObject) other;
          return o.object.equals(object) && o.port == port;
        }
      else
        return false;
    }
  }

  /**
   * The free number to give for the next instance.
   * This field is incremented each time the
   * new collection of the connected objects is created.
   * Each collection has its own unique instance number.
   */
  private static long free_object_number;

  /**
   * The map of the all connected objects, maps the object key to the
   * object.
   */
  private Map objects = new TreeMap(new ByteArrayComparator());

  /**
   * Get the record of the stored object.
   *
   * @param object the stored object
   *
   * @return the record about the stored object, null if
   * this object is not stored here.
   */
  public cObject getKey(org.omg.CORBA.Object stored_object)
  {
    Map.Entry item;
    Iterator iter = objects.entrySet().iterator();
    cObject ref;

    while (iter.hasNext())
      {
        item = (Map.Entry) iter.next();
        ref = (cObject) item.getValue();
        if (stored_object.equals(ref.object))
          return ref;
      }
    return null;
  }

  /**
   * Add the new object to the repository. The object key is
   * generated automatically.
   *
   * @param object the object to add.
   * @param port, on that the ORB will be listening to the remote
   * invocations.
   *
   * @return the newly created object record.
   */
  public cObject add(org.omg.CORBA.Object object, int port)
  {
    return add(generateObjectKey(object), object, port);
  }

  /**
   * Add the new object to the repository.
   *
   * @param key the object key.
   * @param object the object to add.
   * @param port the port, on that the ORB will be listening on the
   * remote invocations.
   */
  public cObject add(byte[] key, org.omg.CORBA.Object object, int port)
  {
    cObject rec = new cObject(object, port, key);
    objects.put(key, rec);
    return rec;
  }

  /**
   * Get the stored object.
   *
   * @param key the key (in the byte array form).
   *
   * @return the matching object, null if none is matching.
   */
  public cObject get(byte[] key)
  {
    return (cObject) objects.get(key);
  }

  /**
   * Get the map entry set.
   * @return
   */
  public Set entrySet()
  {
    return objects.entrySet();
  }

  /**
   * Remove the given object.
   *
   * @param object the object to remove.
   */
  public void remove(org.omg.CORBA.Object object)
  {
    cObject ref = getKey(object);
    if (ref != null)
      objects.remove(ref.key);
  }

  /**
   * Remove the given object, indiciating it by the key.
   *
   * @param object the object to remove.
   */
  public void remove(byte[] key)
  {
    objects.remove(key);
  }

  /**
   * Generate the object key, unique in the currently
   * running java virtual machine.
   *
   * The generated key includes the object class name
   * and the absolute instance number.
   *
   * @return the generated key.
   */
  protected byte[] generateObjectKey(org.omg.CORBA.Object object)
  {
    return (object.getClass().getName() + ":" + getFreeInstanceNumber()).getBytes();
  }

  /**
   * Get next free instance number.
   */
  private static synchronized long getFreeInstanceNumber()
  {
    long instance_number = free_object_number;
    free_object_number++;
    return instance_number;
  }
}