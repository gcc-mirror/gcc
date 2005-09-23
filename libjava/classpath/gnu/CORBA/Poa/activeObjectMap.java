/* activeObjectMap.java --
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


package gnu.CORBA.Poa;

import gnu.CORBA.ByteArrayComparator;

import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * Implements the conception of the Active Object Map.
 * If the POA supports the RETAIN policy, it maintains an Active
 * Object Map, that associates Object Ids with active servants.
 * Each association constitutes an active object. We use a single map
 * for all POAs on the given orb.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class activeObjectMap
{
  /**
   * The reference data about the object, placed on the AOM.
   */
  public class Obj
  {
    /**
     * Create an initialised instance.
     */
    Obj(org.omg.CORBA.Object _object, byte[] _key, Servant _servant, POA _poa)
    {
      object = _object;
      key = _key;
      servant = _servant;
      poa = _poa;
    }

    /**
     * The object.
     */
    public final org.omg.CORBA.Object object;

    /**
     * The servant, serving the given object.
     */
    public Servant servant;

    /**
     * The local servant that once served this object.
     * This field is used by {@link ForwardedServant} when it discovers that
     * the forwarding chaing returns back to the original location.
     * It should not be used anywhere else.
     */
    Servant primary_servant;

    /**
     * The POA, where the object is connected.
     */
    public final POA poa;

    /**
     * The object key.
     */
    public final byte[] key;

    /**
     * If true, this entry is deactivated.
     */
    public boolean deactivated;

    /**
     * Set the servant value, preserving any non null
     * value as the primary servant.
     */
    public void setServant(Servant s)
    {
      if (primary_servant == null)
        primary_servant = s;
      servant = s;
    }

    /**
     * Get the servant.
     */
    public Servant getServant()
    {
      return servant;
    }

    /**
     * Get the deactivation state.
     */
    public boolean isDeactiveted()
    {
      return deactivated;
    }

    /**
     * Set the deactivation state.
     */
    public void setDeactivated(boolean state)
    {
      deactivated = state;
    }

    public boolean equals(java.lang.Object other)
    {
      if (other instanceof Obj)
        {
          Obj o = (Obj) other;
          return o.object.equals(object);
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
  private static long free_id;

  /**
   * The map of the all connected objects, maps the object key to the
   * object.
   */
  Map objects = new TreeMap(new ByteArrayComparator());

  /**
   * Get the record of the stored object. If the object is mapped
   * several times under the different keys, one of the mappings
   * is used.
   *
   * @param object the stored object
   *
   * @return the record about the stored object, null if
   * this object is not stored here.
   */
  public Obj findObject(org.omg.CORBA.Object stored_object)
  {
    if (stored_object == null)
      return null;

    Map.Entry item;
    Iterator iter = objects.entrySet().iterator();
    Obj ref;

    while (iter.hasNext())
      {
        item = (Map.Entry) iter.next();
        ref = (Obj) item.getValue();
        if (stored_object.equals(ref.object))
          return ref;
      }
    return null;
  }

  /**
   * Find the reference info for the given servant.
   * If the servant is mapped to several objects, this
   * returns the first found occurence.
   *
   * @param servant a servant to find.
   *
   * @return the servant/object/POA binding or null if no such found.
   */
  public Obj findServant(Servant servant)
  {
    if (servant == null)
      return null;

    Map.Entry item;
    Iterator iter = objects.entrySet().iterator();
    Obj ref;

    while (iter.hasNext())
      {
        item = (Map.Entry) iter.next();
        ref = (Obj) item.getValue();
        if (servant.equals(ref.servant))
          return ref;
      }
    return null;
  }

  /**
   * Find the reference info for the given servant.
   * If the servant is mapped to several objects, this
   * returns the first found occurence.
   *
   * @param servant a servant to find.
   * @param speficies if to search for the inactive (true) or active
   * (false) servant. A servant with unmatching activity is ignored
   * by this method.
   *
   * @return the servant/object/POA binding or null if no such found.
   */
  public Obj findServant(Servant servant, boolean inactive)
  {
    if (servant == null)
      return null;

    Map.Entry item;
    Iterator iter = objects.entrySet().iterator();
    Obj ref;

    while (iter.hasNext())
      {
        item = (Map.Entry) iter.next();
        ref = (Obj) item.getValue();
        if (ref.deactivated == inactive)
          if (ref.servant != null)
            if (servant.equals(ref.servant))
              return ref;
      }
    return null;
  }

  /**
   * Add the new object to the repository. The object key is
   * generated automatically.
   *
   * @param object the object to add.
   * @param servant a servant, serving the given object.
   * @param poa the poa, where the object is connected.
   *
   * @return the newly created object record.
   */
  public Obj add(org.omg.CORBA.Object object, Servant servant, POA poa)
  {
    return add(generateObjectKey(object), object, servant, poa);
  }

  /**
   * Add the new object to the repository.
   *
   * @param key the object key.
   * @param object the object to add.
   * @param servant a servant, serving the given object.
   * @param poa the POA, where the object is connected.
   */
  public Obj add(byte[] key, org.omg.CORBA.Object object, Servant servant,
                 POA poa
                )
  {
    Obj rec = new Obj(object, key, servant, poa);
    objects.put(key, rec);
    return rec;
  }

  /**
   * Add the new object to the repository.
   *
   * @param delegate the delegate, providing data about the servant, key, POA
   * and object.
   * @param port the port that this object would take.
   */
  public Obj add(servantDelegate delegate)
  {
    Obj rec =
      new Obj(delegate.object, delegate.servant_id, delegate.servant,
              delegate.poa
             );
    objects.put(delegate.servant_id, rec);
    return rec;
  }

  /**
   * Put back the definition structure that has probably been removed earlier.
   */
  public void put(Obj obj)
  {
    objects.put(obj.key, obj);
  }

  /**
   * Get the stored object.
   *
   * @param key the key (in the byte array form).
   *
   * @return the matching object, null if none is matching.
   */
  public Obj get(byte[] key)
  {
    return (Obj) objects.get(key);
  }

  /**
   * Get the map key set.
   */
  public Set keySet()
  {
    return objects.keySet();
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
   * running java virtual machine. The passed object
   * parameter is currently not in use.
   *
   * @return the generated key.
   */
  protected byte[] generateObjectKey(org.omg.CORBA.Object object)
  {
    byte[] key;

    // The repetetive keys cannot be generated, but theoretically
    // the same keys can be passed when calling add(byte[]...).
    // Hence we check if the key is not already in the map and,
    // if it is, use the subsequent value.
    do
      {
        key = getFreeId();
      }
    while (objects.containsKey(key));
    return key;
  }

  /**
   * Get the next free 8 byte id, surely unique between calls of this
   * method for the currently running virtual machine.
   */
  public static synchronized byte[] getFreeId()
  {
    byte[] r = new byte[ 8 ];

    // Start from the faster-changing.
    r [ 0 ] = ((byte) (0xff & free_id));
    r [ 1 ] = ((byte) (0xff & (free_id >> 8)));
    r [ 2 ] = ((byte) (0xff & (free_id >> 16)));
    r [ 3 ] = ((byte) (0xff & (free_id >> 24)));
    r [ 4 ] = ((byte) (0xff & (free_id >> 32)));
    r [ 5 ] = ((byte) (0xff & (free_id >> 40)));
    r [ 6 ] = ((byte) (0xff & (free_id >> 48)));
    r [ 7 ] = ((byte) (0xff & (free_id >> 56)));

    free_id++;

    return r;
  }
}