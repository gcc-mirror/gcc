/* Variables.java --
   Copyright (c) 2004, 2005
   Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package gnu.classpath.tools.rmic;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

class Variables
{
  private final HashSet free = new HashSet();
  private final HashMap names = new HashMap();
  private final HashSet wides = new HashSet();
  private final HashSet declared = new HashSet();
  private boolean allocated = false;

  public void declare(Object name)
  {
    declare(name, 1);
  }

  public void declareWide(Object name)
  {
    declare(name, 2);
  }

  public void declare(Object name, int size)
  {
    if (allocated)
      throw new IllegalStateException("cannot declare after allocating");
    if (size != 1 && size != 2)
      throw new IllegalArgumentException("size must be 1 or 2");
    if (names.containsKey(name))
      throw new IllegalStateException("already allocated " + name);

    allocateNew(name, size);
    declared.add(name);
  }

  private int allocateNew(Object name, int size)
  {
    // total allocation size is first unallocated slot
    int i = free.size() + names.size() + wides.size();
    names.put(name, new Integer(i));
    if (size == 2) wides.add(name);
    return i;
  }

  public int allocate(Object name)
  {
    return allocate(name, 1);
  }

  public int allocateWide(Object name)
  {
    return allocate(name, 2);
  }

  public int allocate(Object name, int size)
  {
    allocated = true;
    if (size != 1 && size != 2)
      throw new IllegalArgumentException("size must be 1 or 2");
    if (names.containsKey(name))
      throw new IllegalStateException("already allocated " + name);

    if (size == 2)
      {
        // look for consecutive free slots
        for (Iterator it = free.iterator(); it.hasNext(); )
          {
            Integer i = (Integer) it.next();
            Integer next = new Integer(i.intValue() + 1);
            if (free.contains(next))
              {
                free.remove(i);
                free.remove(next);
                wides.add(name);
                names.put(name, i);
                return i.intValue();
              }
          }
      }
    else if (free.size() > 0)
      {
        Integer i = (Integer) free.iterator().next();
        free.remove(i);
        names.put(name, i);
        return i.intValue();
      }

    return allocateNew(name, size);
  }

  public int deallocate(Object name)
  {
    if (! names.containsKey(name))
      throw new IllegalArgumentException("no variable " + name);

    if (declared.contains(name))
      throw new IllegalStateException(name + " can't be deallocated");

    Integer i = (Integer) names.get(name);
    names.remove(name);
    free.add(i);
    if (wides.remove(name))
      free.add(new Integer(i.intValue() + 1));
    return i.intValue();
  }

  public int get(Object name)
  {
    if (! names.containsKey(name))
      throw new IllegalArgumentException("no variable " + name);

    return ((Integer) names.get(name)).intValue();
  }
}
