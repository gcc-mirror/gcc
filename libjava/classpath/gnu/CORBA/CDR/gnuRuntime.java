/* gnuRuntime.java --
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


package gnu.CORBA.CDR;

import gnu.CORBA.Minor;

import org.omg.CORBA.LocalObject;
import org.omg.CORBA.MARSHAL;

import java.io.Serializable;
import java.util.Comparator;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Our implementation of the sending context runtime.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuRuntime
  extends LocalObject
  implements org.omg.SendingContext.RunTime
{
  /**
   * The data entry about the object that was written.
   */
  static class Entry
  {
    /**
     * The stream position, where the object was written.
     */
    int at;

    /**
     * The object that was written.
     */
    Object object;

    public String toString()
    {
      return object + "[" + at + "] "+object.getClass().getName();
    }
  }

  /**
   * The instruction that the actual object is stored at different location.
   * Used when processing chunked data where positions shifts due removing the
   * chunking tags.
   */
  static class Redirection
    extends Entry
  {
    public String toString()
    {
      return "->" + at;
    }
  }

  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The history of the written objects, maps object to records. The different
   * objects must be treated as different regardless that .equals returns.
   */
  private Map sh_objects = new IdentityHashMap();

  /**
   * The written repository Ids that can be shared.
   */
  private Map sh_ids = new TreeMap(new Comparator()
  {
    public int compare(Object a, Object b)
    {
      if (a instanceof String && b instanceof String)
        // Comparing string with string.
        return ((String) a).compareTo((String) b);
      else if (a instanceof String[] && b instanceof String[])
        {
          // Comparing array with array.
          String[] sa = (String[]) a;
          String[] sb = (String[]) b;

          if (sa.length != sb.length)
            return sa.length - sb.length;
          else
            {
              int c;
              for (int i = 0; i < sa.length; i++)
                {
                  c = sa[i].compareTo(sb[i]);
                  if (c != 0)
                    return c;
                }
              return 0;
            }
        }
      else
        // Comparing string with array.
        return a instanceof String ? 1 : -1;
    }
  });

  /**
   * The history of the written objects, maps positions to records. The
   * different objects must be treated as different regardless that .equals
   * returns.
   */
  private Map positions = new HashMap();

  /**
   * The Codebase.
   */
  private String codebase;

  /**
   * The pre-created instance of the object being written (avoid
   * re-instantiation).
   */
  public Serializable target;

  /**
   * Create Runtime.
   * 
   * @param a_id a repository Id, if only one Id was specified in the stream.
   * @param a_ids a repository Ids, if the multiple Ids were specified in te
   * stream.
   * @param a_codabase a codebase, if it was specified in the stream.
   */
  public gnuRuntime(String a_codebase, Object a_target)
  {
    if (a_target instanceof Serializable)
      target = (Serializable) a_target;

    codebase = a_codebase;
  }

  /**
   * Mark the given object as written at the given position.
   */
  public void objectWritten(Object object, int at)
  {
    if (object == null || at < 0)
      return; // No positional information provided.
    if (sh_objects.containsKey(object))
      throw new AssertionError("Repetetive writing of the same object "
        + object + " at " + at + dump());

    Entry e = new Entry();
    e.at = at;
    e.object = object;

    sh_objects.put(object, e);
    positions.put(new Integer(at), e);
  }

  /**
   * Check if the object is already written.
   * 
   * @return the position, at that the object is allready written or -1 if it is
   * not yet written.
   */
  public int isWrittenAt(Object x)
  {
    Entry e = (Entry) sh_objects.get(x);
    return e == null ? -1 : e.at;
  }

  /**
   * Set redirection, indicating that the object, searched at the p_searched
   * position can be actually found at the p_present position.
   */
  public void redirect(int p_searched, int p_present)
  {
    Redirection redirection = new Redirection();
    redirection.at = p_present;
    positions.put(new Integer(p_searched), redirection);
  }

  /**
   * Get the object, written at the given position. This returs both shared
   * objects and repository Ids.
   * 
   * @return the position, at that the object is allready written.
   * 
   * @throws MARSHAL if there is no object written at that position.
   */
  public Object isObjectWrittenAt(int x, int offset)
  {
    Entry e = (Entry) positions.get(new Integer(x));
    if (e instanceof Redirection)
      return isObjectWrittenAt(e.at, offset);
    else if (e != null)
      return e.object;
    else
      {
        MARSHAL m = new MARSHAL("No object was written at " + x + 
          " (offset " + offset + ") r " + this + dump());
        m.minor = Minor.Graph;
        throw m;
      }
  }

  /**
   * Mark the given object as written at the given position.
   */
  public void singleIdWritten(String id, int at)
  {
    if (sh_ids.containsKey(id))
      throw new InternalError("Repetetive writing of the same string " +
        id + dump());

    Entry e = new Entry();
    e.at = at;
    e.object = id;

    sh_ids.put(id, e);
    positions.put(new Integer(at), e);
  }

  /**
   * Mark the given object as written at the given position.
   */
  public void multipleIdsWritten(String[] ids, int at)
  {
    if (sh_ids.containsKey(ids))
      throw new InternalError("Repetetive writing of the same string " + 
        ids + dump());

    Entry e = new Entry();
    e.at = at;
    e.object = ids;

    sh_ids.put(ids, e);
    positions.put(new Integer(at), e);
  }

  /**
   * Check if the object is already written.
   * 
   * @return the position, at that the object is allready written or -1 if it is
   * not yet written.
   */
  public int idWrittenAt(Object x)
  {
    Entry e = (Entry) sh_ids.get(x);
    return e == null ? -1 : e.at;
  }

  /**
   * Get the codebase.
   */
  public String getCodeBase()
  {
    return codebase;
  }

  /**
   * Set the codebase, preserving the old value if the passed parameter is null
   * and forming the space delimited list if both new and old values are not
   * null.
   */
  public void addCodeBase(String base)
  {
    if (base != null)
      {
        if (codebase == null)
          codebase = base;
        else
          codebase = codebase + " " + base;
      }
  }

  /**
   * Dump all objects that are currently stored.
   */
  public String dump()
  {
    StringBuffer b = new StringBuffer(" Stream content: \n");

    // Sort by position.
    TreeSet t = new TreeSet(positions.keySet());
    Iterator p = t.iterator();

    while (p.hasNext())
      {
        Object k = p.next();
        b.append("     " + k + ": " + ((Entry) positions.get(k)).toString()
          + "\n");
      }
    return b.toString();
  }

}
