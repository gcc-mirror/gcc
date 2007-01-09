/* TransientContextExt.java --
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

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.Object;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.NotEmpty;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.CosNaming._NamingContextExtImplBase;

/**
 * This naming context that adds the the string based extensions,
 * defined by {@link NamingContextExt}. The basic functionality
 * is handled by the enclosed instance of the {@link NamingContext}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Ext
  extends _NamingContextExtImplBase implements SafeForDirectCalls
{
  /**
   * The older version of the naming context, where all relevant calls
   * are forwarded.
   */
  private final NamingContext classic;

  /**
   * The converter class converts between string and array form of the
   * name.
   */
  private NameTransformer converter = new NameTransformer();

  /**
   * Create the extensions for the given instance of the context.
   *
   * @param previous_version the previous version of the naming context.
   */
  public Ext(NamingContext previous_version)
  {
    classic = previous_version;
  }

  /**
   * Sets a delegate to this context and, if appropriated, also
   * sets the same delegate to the enclosing 'classic' context.
   *
   * @param a_delegate a delegate to set.
   */
  public void _set_delegate(Delegate a_delegate)
  {
    super._set_delegate(a_delegate);
    if (classic instanceof ObjectImpl)
      ((ObjectImpl) classic)._set_delegate(a_delegate);
  }

  /** {@inheritDoc} */
  public void bind(NameComponent[] a_name, Object an_object)
            throws NotFound, CannotProceed, InvalidName, AlreadyBound
  {
    classic.bind(a_name, an_object);
  }

  /** {@inheritDoc} */
  public void bind_context(NameComponent[] a_name, NamingContext context)
                    throws NotFound, CannotProceed, InvalidName, AlreadyBound
  {
    classic.bind_context(a_name, context);
  }

  /** {@inheritDoc} */
  public NamingContext bind_new_context(NameComponent[] a_name)
                                 throws NotFound, AlreadyBound, CannotProceed,
                                        InvalidName
  {
    return classic.bind_new_context(a_name);
  }

  /** {@inheritDoc} */
  public void destroy()
               throws NotEmpty
  {
    classic.destroy();
  }

  /** {@inheritDoc} */
  public void list(int amount, BindingListHolder a_list,
                   BindingIteratorHolder an_iter
                  )
  {
    classic.list(amount, a_list, an_iter);
  }

  /** {@inheritDoc} */
  public NamingContext new_context()
  {
    return classic.new_context();
  }

  /** {@inheritDoc} */
  public void rebind(NameComponent[] a_name, Object an_object)
              throws NotFound, CannotProceed, InvalidName
  {
    classic.rebind(a_name, an_object);
  }

  /** {@inheritDoc} */
  public void rebind_context(NameComponent[] a_name, NamingContext context)
                      throws NotFound, CannotProceed, InvalidName
  {
    classic.rebind_context(a_name, context);
  }

  /** {@inheritDoc} */
  public Object resolve(NameComponent[] a_name)
                 throws NotFound, CannotProceed, InvalidName
  {
    return classic.resolve(a_name);
  }

  /**
   * Resolves the name, represented in the form of the string. The name
   * is first parsed into an array representation, then the call
   * is forwarded to the {@link resolve(NameComponent[])}.
   *
   * @param a_name_string a name to resolve.
   *
   * @return the resolved object.
   *
   * @throws NotFound if the name cannot be resolved.
   * @throws InvalidName if the name is invalid.
   * @throws CannotProceed on unexpected circumstances.
   */
  public Object resolve_str(String a_name_string)
                     throws NotFound, CannotProceed, InvalidName
  {
    return resolve(to_name(a_name_string));
  }

  /**
   * Convert the name string representation into array representation.
   *
   * @param a_name_string a string to convert.
   * @return a converted array of the name components
   *
   * @throws InvalidName on parsing error.
   */
  public NameComponent[] to_name(String a_name_string)
                          throws InvalidName
  {
    return converter.toName(a_name_string);
  }

  /**
   * Convert a name component array representation into string representation.
   *
   * @param a_name a name to convert.
   *
   * @return a string form.
   *
   * @throws InvalidName if the passed name is invalid.
   */
  public String to_string(NameComponent[] a_name)
                   throws InvalidName
  {
    return converter.toString(a_name);
  }

  /**
   * This method is not yet implemented.
   * FIXME TODO implement it.
   */
  public String to_url(String an_address, String a_name_string)
                throws org.omg.CosNaming.NamingContextExtPackage.InvalidAddress,
                       InvalidName
  {
    throw new NO_IMPLEMENT("Method to_url() not yet implemented.");
  }

  /** {@inheritDoc} */
  public void unbind(NameComponent[] a_name)
              throws NotFound, CannotProceed, InvalidName
  {
    classic.unbind(a_name);
  }
}
