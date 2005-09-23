/* gnuIcCurrent.java --
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


package gnu.CORBA.Interceptor;

import gnu.CORBA.CDR.cdrBufOutput;
import gnu.CORBA.Poa.ORB_1_4;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.PortableInterceptor.Current;
import org.omg.PortableInterceptor.CurrentHelper;
import org.omg.PortableInterceptor.InvalidSlot;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

/**
 * Supports the "Interceptor current" concept, providing the slot value
 * information for the current thread. When making the invocation, this
 * information is copied to the Current, returned by ClientRequestInfo.
 *
 * There is only one instance of this class per ORB. It maintains a thread to
 * information map.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuIcCurrent extends ObjectImpl implements Current
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The ORB, controllin this Current. It provides data about the required size
   * of the slot array.
   */
  final ORB_1_4 orb;

  /**
   * The table, mapping threads to records.
   */
  private Hashtable threads = new Hashtable();

  /**
   * An empty array when no slots are defined, computed once.
   */
  static final Any[] NO_SLOTS = new Any[ 0 ];

  /**
   * Create the IC current.
   */
  public gnuIcCurrent(ORB_1_4 an_orb)
  {
    orb = an_orb;
  }

  /**
   * Get the array of POA current repository ids.
   *
   * @return a single member array, containing value, returned by the
   * {@link CurrentHelper#id}, normally
   * "IDL:omg.org/PortableInterceptor/Current:1.0".
   */
  public String[] _ids()
  {
    return new String[] { CurrentHelper.id() };
  }

  /**
   * Add the entry to the map.
   */
  public void put(Thread t, Any[] record)
  {
    synchronized (threads)
      {
        threads.put(t, record);

        // Remove non-running threads, avoiding memory leak.
        if (threads.size() > 12)
          {
            Iterator it = threads.entrySet().iterator();
            while (it.hasNext())
              {
                Map.Entry e = (Map.Entry) it.next();
                Thread tx = (Thread) e.getKey();
                if (!tx.isAlive())
                  {
                    it.remove();
                  }
              }
          }
      }
  }

  /**
   * Check if this thread is registered.
   */
  public boolean has(Thread t)
  {
    synchronized (threads)
      {
        return threads.containsKey(t);
      }
  }

  /**
   * Remove the entry from the map.
   */
  public void remove(Thread t)
  {
    synchronized (threads)
      {
        threads.remove(t);
      }
  }

  /**
   * Get array of all slots, as it is applicable for the current thread. If the
   * slots were not previously allocated, they are allocated during this call.
   */
  Any[] get_slots()
  {
    Any[] r;
    synchronized (threads)
      {
        r = (Any[]) threads.get(Thread.currentThread());
        if (r == null)
          {
            r = new Any[ orb.icSlotSize ];

            for (int i = 0; i < r.length; i++)
              {
                Any a = orb.create_any();
                a.type(orb.get_primitive_tc(TCKind.tk_null));
                r [ i ] = a;
              }

            put(Thread.currentThread(), r);
          }
        return r;
      }
  }

  /**
       * Get copu array of all slots, as it is applicable for the current thread. If
   * the slots were not previously allocated, they are allocated during this
   * call.
   */
  public Any[] clone_slots()
  {
    if (orb.icSlotSize == 0)
      {
        return NO_SLOTS;
      }
    else
      {
        Any[] r = get_slots();
        Any[] copy = new Any[ r.length ];

        cdrBufOutput buf = new cdrBufOutput();
        buf.setOrb(orb);

        for (int i = 0; i < copy.length; i++)
          {
            r [ i ].write_value(buf);
          }

        InputStream input = buf.create_input_stream();

        for (int i = 0; i < copy.length; i++)
          {
            copy [ i ] = orb.create_any();
            copy [ i ].read_value(input, r [ i ].type());
          }

        return copy;
      }
  }

  /**
   * Get value for the slot with the given id. If the array of Currents has not
   * been yet allocated for the current thread, it is allocated during the
   * invocation of this method.
   */
  public Any get_slot(int slot_id) throws InvalidSlot, BAD_INV_ORDER
  {
    try
      {
        return get_slots() [ slot_id ];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        throw new InvalidSlot("Slot " + slot_id);
      }
  }

  /**
   * Set value for the slot with the given id. If the array of Currents has not
   * been yet allocated for the current thread, it is allocated during the
   * invocation of this method.
   */
  public void set_slot(int slot_id, Any data)
    throws InvalidSlot, BAD_INV_ORDER
  {
    try
      {
        get_slots() [ slot_id ] = data;
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        throw new InvalidSlot("Slot " + slot_id);
      }
  }
}