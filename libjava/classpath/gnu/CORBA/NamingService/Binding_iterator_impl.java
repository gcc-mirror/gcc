/* Binding_iterator.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package gnu.CORBA.NamingService;

import gnu.CORBA.SafeForDirectCalls;

import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.BindingType;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming._BindingIteratorImplBase;

/**
 * The implementation of the {@link BindingIterator}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Binding_iterator_impl
  extends _BindingIteratorImplBase implements SafeForDirectCalls
{
  /**
   * The value, returned by the {@link #next_one} when there
   * are no bindings available.
   */
  private static final Binding no_more_bindings =
    new Binding(new NameComponent[ 0 ], BindingType.nobject);

  /**
   * The collection of the available bindings.
   */
  private final Binding[] bindings;

  /**
   * The position of the internal iterator pointer.
   */
  private int p;

  public Binding_iterator_impl(Binding[] a_bindings)
  {
    bindings = a_bindings;
  }

  /**
   * Disconnect the iterator from its ORB. The iterator will
   * no longer be accessible and will be a subject of the
   * garbage collection.
   */
  public void destroy()
  {
    _orb().disconnect(this);
  }

  /**
   * Return the desired amount of bindings.
   *
   * @param amount the maximal number of bindings to return.
   * @param a_list a holder to store the returned bindings.
   *
   * @return false if there are no more bindings available,
   * true otherwise.
   */
  public boolean next_n(int amount, BindingListHolder a_list)
  {
    if (p < bindings.length)
      {
        int n = bindings.length - p;
        if (n > amount)
          n = amount;

        a_list.value = new Binding[ n ];
        for (int i = 0; i < n; i++)
          a_list.value [ i ] = bindings [ p++ ];

        return true;
      }
    else
      {
        a_list.value = new Binding[ 0 ];
        return false;
      }
  }

  /**
   * Return the next binding.
   *
   * @param a_binding a holder, where the next binding will be stored.
   *
   * @return false if there are no more bindings available, true
   * otherwise.
   */
  public boolean next_one(BindingHolder a_binding)
  {
    if (p < bindings.length)
      {
        a_binding.value = (Binding) bindings [ p++ ];
        return true;
      }
    else
      {
        a_binding.value = no_more_bindings;
        return false;
      }
  }
}
