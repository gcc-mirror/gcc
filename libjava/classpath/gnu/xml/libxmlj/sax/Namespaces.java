/* Namespaces.java -
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.xml.libxmlj.sax;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Helper class for managing namespaces.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class Namespaces
{

  ArrayList stack = new ArrayList ();

  /**
   * Increments the tree depth.
   * This allocates a new potential namespace entry.
   */
  void push ()
  {
    stack.add (null);
  }

  /**
   * Decrements the tree depth.
   * This removes namespaces defined at the extremity.
   */
  void pop ()
  {
    stack.remove (stack.size() - 1);
  }

  /**
   * Searches for the namespace URI corresponding to the specified prefix.
   */
  String getURI (String prefix)
  {
    for (int i = stack.size () - 1; i >= 0; i--)
    {
      HashMap ns = (HashMap) stack.get (i);
      if (ns != null && ns.containsKey (prefix))
      {
        String ret = (String) ns.get (prefix);
        return (ret == null) ? "" : ret;
      }
    }
    return "";
  }

  /**
   * Defines the specified prefix-URI mapping at the current depth in the
   * tree.
   */
  void define (String prefix, String uri)
  {
    int index = stack.size () - 1;
    HashMap ns = (HashMap) stack.get (index);
    if (ns == null)
    {
      ns = new HashMap ();
      stack.set (index, ns);
    }
    ns.put (prefix, uri);
  }

  /**
   * Returns an iterator over the prefixes defined at the current depth.
   */
  Iterator currentPrefixes ()
  {
    HashMap ns = (HashMap) stack.get (stack.size () - 1);
    if (ns == null)
      {
        return Collections.EMPTY_LIST.iterator ();
      }
    else
      {
        return ns.keySet ().iterator ();
      }
  }

}
