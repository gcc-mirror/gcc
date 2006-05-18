/* BidiHasthable.java -- Bidirectional hash table.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.rmi.activation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * The bidirectional hash table, maps both a to b and b to a.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org) 
 */
public class BidiTable 
{
  /**
   * Use serialVerionUID for interoperability.
   */
  private static final long serialVersionUID = 1;
  
  /**
   * Maps keys to values
   */
  protected Map k2v;
  
  /**
   * Maps values to keys (in reverse)
   */
  protected Map v2k;
  
  /**
   * Create a new table that is ready to use.
   */
  public BidiTable()
  {
    k2v = new HashMap();
    v2k = new HashMap();
  }
  
  /**
   * Create a new instance where the hashtable fields are not initialised
   * (called from derivatives that intialise hashtables in they own way.
   * 
   * @param flags currently used to mark the different constructor only.
   */
  protected BidiTable(int flags)
  {
  }
  
  /**
   * Get key by value
   */
  public synchronized Object getKey(Object value)
  {
    return v2k.get(value);
  }
  
  /**
   * Put key-value pair.
   */
  public synchronized void put(Object key, Object value)
  {
    k2v.put(key, value);
    v2k.put(value, key);
  }
  
  /**
   * Get value from key
   */
  public synchronized Object get(Object key)
  {
    return k2v.get(key);
  }
  
  /**
   * Remove the key-value pair by key
   */
  public synchronized void removeKey(Object key)
  {
    Object value = k2v.get(key);
    if (value!=null)
      {
        k2v.remove(key);
        v2k.remove(value);
      }
  }
  
  /**
   * Check if the table contains this key.
   */
  public synchronized boolean containsKey(Object key)
  {
    return k2v.containsKey(key);
  }
  
  /**
   * This method is called before exit and may be used to write the database
   * to the disk. The default method does nothing.
   */
  public synchronized void shutdown()
  {
  }
  
  /**
   * Get the size.
   */
  public synchronized int size()
  {
    return k2v.size();
  }
  
  /**
   * Get the key collection.
   */
  public synchronized Object[] keys()
  {
    Collection keys = k2v.keySet();
    Object[] k = new Object[keys.size()];

    Iterator iter = keys.iterator();
    for (int i = 0; i < k.length; i++)
      k[i] = iter.next();
    
    return k;
  }
}
