/* ListBindingsEnumeration.java -- handles corbaname: urls
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


package gnu.javax.naming.giop;

import javax.naming.NamingEnumeration;

import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.NamingContext;

/**
 * Iterates over bindings, obtaining values first from the binding list and then
 * from the binding iterator.
 * 
 * @author Audrius Meskauskas
 */
public class ListBindingsEnumeration extends GiopNamingEnumeration implements
    NamingEnumeration
{
  /**
   * The naming service, to resolve the objects.
   */
  NamingContext service;
  
  /**
   * Create the new enumeration
   * 
   * @param bh
   *          holder, containing the first portion of the bindings
   * @param bih
   *          the iterator, containing the remaining bindings
   * @param batchSize
   *          the number of bindings the the iterator will be requested to
   *          return as a single pack
   * @param aService
   *          the naming service, used to obtain the objects, bound to the
   *          names.
   */
  public ListBindingsEnumeration(BindingListHolder bh, 
                                 BindingIteratorHolder bih, int batchSize, 
                                 NamingContext aService)
  {
    super(bh, bih, batchSize);
    service = aService;
  }
  
  /**
   * Convert from the CORBA binding into the javax.naming binding. As the CORBA
   * naming service binding does not contain the object itself, this method
   * makes the additional calls to the naming service.
   * 
   * @param binding
   *          the binding to convert
   * @return the value, that must be returned by the {@link #next()}.
   */
  public Object convert(Binding binding)
  {
    StringBuffer name = new StringBuffer();

    for (int i = 0; i < binding.binding_name.length; i++)
      {
        name.append(binding.binding_name[i]);
        if (i < binding.binding_name.length - 1)
          name.append('/');
      }
    
    try
      {
        Object object = service.resolve(binding.binding_name);
        return new javax.naming.Binding(name.toString(), object);
      }
    catch (Exception e)
      {
        // Probably was removed by the concurent thread.
        return null;
      }
  }

}
