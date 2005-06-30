/* gnuStringIntMapper.java --
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


package gnu.javax.swing.text.html.parser.support;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

/**
 * A helper class, mapping between the strings and they unique integer
 * identifiers.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class gnuStringIntMapper
{
  /**
   * Maps argument integer values from DTDConstants into they string
   * names. Initialized on demand.
   */
  private Map is_Map;

  /**
   * Maps argument string names into they integer values from DTDConstants.
   * Initialized on demand.
   */
  private Map si_Map;

  /**
   *  Get string from id or null if no such id is present in the mapper.
   */
  public final String get(int id)
  {
    if (is_Map == null)
      createTheMap();

    return (String) is_Map.get(new Integer(id));
  }

  /** Get id from string or 0 if no such string is present in the mapper. */
  public final int get(String id)
  {
    if (si_Map == null)
      createTheMap();

    Integer i = (Integer) si_Map.get(id);

    return i != null ? i.intValue() : 0;
  }

  /**
   * Create the mapping table for this mapper by adding the required
   * String/int pairs. The method is invoked
   * only once for each instance, after the first invocation of the any
   * form of the <code>get</code> method. Use <code>add</code> to
   * create a map for a concrete instance.
   */
  protected abstract void create();

  /**
   * Add an id/string pair to this mapper. This is called from
   * the method <code>create</code> only.
   */
  protected void add(String name, int id)
  {
    Integer i = new Integer(id);
    si_Map.put(name, i);
    is_Map.put(i, name);
  }

  private void createTheMap()
  {
    is_Map = new HashMap();
    si_Map = new TreeMap();
    create();
  }
}
