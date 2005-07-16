/* gnuContext.java --
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

import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.omg.CORBA.Any;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.CTX_RESTRICT_SCOPE;
import org.omg.CORBA.Context;
import org.omg.CORBA.NVList;

/**
 * The working implementation of the {@link org.omg.CORBA.Context}.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class gnuContext
  extends Context
{
  /**
   * The parent context.
   */
  Context parent;

  /**
   * The collection to store the context properties.
   */
  Map properties = new Hashtable();

  /**
   * The name of this context.
   */
  String name;

  /**
   * Creates the new context with the given name and parent.
   *
   * @param a_name a name of the new context.
   * @param a_parent a parent, used to resolve the missing references.
   */
  public gnuContext(String a_name, Context a_parent)
  {
    name = a_name;
    parent = a_parent;
  }

  /** {@inheritDoc} */
  public String context_name()
  {
    return name;
  }

  /** {@inheritDoc} */
  public Context create_child(String child)
  {
    return new gnuContext(child, this);
  }

  /** {@inheritDoc} */
  public void delete_values(String property)
  {
    boolean starts = false;
    if (property.endsWith("*"))
      {
        starts = true;
        property = property.substring(0, property.length() - 1);
      }

    Set keys = properties.keySet();

    Iterator iter = keys.iterator();
    while (iter.hasNext())
      {
        String key = (String) iter.next();
        if ((starts && key.startsWith(property)) ||
            (!starts && key.equals(property))
           )
          iter.remove();
      }
  }

  /** {@inheritDoc} */
  public NVList get_values(String start_scope, int flags, String pattern)
  {
    if (start_scope != null)
      {
        Context c = this;
        while (c != null && !c.context_name().equals(start_scope))
          c = c.parent();
        if (c == null)
          return new gnuNVList();
      }

    try
      {
        gnuNVList rt = new gnuNVList();

        boolean starts = false;
        if (pattern.endsWith("*"))
          {
            starts = true;
            pattern = pattern.substring(0, pattern.length() - 1);
          }

        Set keys = properties.keySet();

        Iterator iter = keys.iterator();
        while (iter.hasNext())
          {
            String key = (String) iter.next();
            if ((starts && key.startsWith(pattern)) ||
                (!starts && key.equals(pattern))
               )
              {
                rt.add_value(key, (Any) properties.get(key), 0);
              }
          }

        if ((flags & CTX_RESTRICT_SCOPE.value) == 0 && parent != null)
          {
            NVList par = parent.get_values(start_scope, flags, pattern);
            for (int i = 0; i < par.count(); i++)
              {
                rt.list.add(par.item(i));
              }
          }

        return rt;
      }
    catch (Bounds ex)
      {
        throw new Error("Report this bug.");
      }
  }

  /** {@inheritDoc} */
  public Context parent()
  {
    return parent;
  }

  /** {@inheritDoc} */
  public void set_one_value(String name, Any value)
  {
    properties.put(name, value);
  }

  /** {@inheritDoc} */
  public void set_values(NVList values)
  {
    try
      {
        for (int i = 0; i < values.count(); i++)
          {
            properties.put(values.item(i).name(), values.item(i).value());
          }
      }
    catch (Bounds ex)
      {
        throw new Error("Please report this bug.");
      }
  }
}
