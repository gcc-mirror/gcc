/* gnuPoaCurrent.java --
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

import org.omg.CORBA.CurrentHelper;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.PortableServer.Current;
import org.omg.PortableServer.CurrentOperations;
import org.omg.PortableServer.CurrentPackage.NoContext;
import org.omg.PortableServer.POA;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

/**
 * Supports the "Poa current" concept, providing the id and poa of
 * the object currently being served. There is only one instance
 * of this class per ORB. It maintains a thread to information map.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuPoaCurrent
  extends ObjectImpl
  implements Current
{
  /**
   * The table, mapping threads to records.
   */
  private TreeMap threads = new TreeMap();

  /**
   * Get the array of POA current repository ids.
   *
   * @return a single member array, containing value, returned
   * by the {@link CurrentHelper#id}, normally
   * "IDL:omg.org/PortableServer/Current:2.3".
   */
  public String[] _ids()
  {
    return new String[] { CurrentHelper.id() };
  }

  /**
   * Get the object id, associated with the thread currently being served.
   *
   * @throws NoContext if the current thread is not associated with any
   * object.
   */
  public byte[] get_object_id()
                       throws NoContext
  {
    CurrentOperations r;
    synchronized (threads)
      {
        r = (CurrentOperations) threads.get(Thread.currentThread().getName());
      }
    if (r != null)
      return r.get_object_id();
    else
      throw new NoContext(Thread.currentThread().getName());
  }

  /**
   * Get the object POA, associated with the thread currently being served.
   *
   * @throws NoContext if the current thread is not associated with any
   * object.
   */
  public POA get_POA()
              throws NoContext
  {
    CurrentOperations r;
    synchronized (threads)
      {
        r = (CurrentOperations) threads.get(Thread.currentThread().getName());
      }
    if (r != null)
      return r.get_POA();
    else
      throw new NoContext(Thread.currentThread().getName());
  }

  /**
   * Add the entry to the map.
   */
  public void put(Thread t, CurrentOperations record)
  {
    synchronized (threads)
      {
        threads.put(t.getName(), record);
      }
  }

  /**
   * Check if this Poa has some running threads.
   */
  public boolean has(POA poa)
  {
    synchronized (threads)
      {
        Iterator iter = threads.entrySet().iterator();
        while (iter.hasNext())
          {
            Map.Entry item = (Map.Entry) iter.next();
            try
              {
                if (((CurrentOperations) item.getValue()).get_POA() == poa)
                  {
                    return true;
                  }
              }
            catch (NoContext ex)
              {
                throw new InternalError();
              }
          }
      }
    return false;
  }

  /**
   * Check if this thread is registered.
   */
  public boolean has(Thread t)
  {
    synchronized (threads)
      {
        return threads.containsKey(t.getName());
      }
  }

  /**
   * Remove the entry from the map.
   */
  public void remove(Thread t)
  {
    synchronized (threads)
      {
        threads.remove(t.getName());
      }
  }
}