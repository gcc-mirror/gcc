/* nContext.java -- implementation of NamingContext
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


package gnu.CORBA.NamingService;

import org.omg.CORBA.Object;
import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.BindingType;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextOperations;
import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.NotEmpty;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.CosNaming.NamingContextPackage.NotFoundReason;
import org.omg.CosNaming._NamingContextImplBase;

import gnu.CORBA.SafeForDirectCalls;

import java.util.Iterator;
import java.util.Map;

/**
 * This class implements the transient naming service, defined by
 * {@link NamingContext}. The 'transient' means that the service does
 * not store its state into the persistent memory. If the service is
 * restarted, the named objects must be re-registered again.
 *
 * TODO Write the persistent naming service.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class TransientContext
  extends _NamingContextImplBase
  implements NamingContext, NamingContextOperations, SafeForDirectCalls
{
  /**
   * Use serial version UID for interoperability.
   */
  private static final long serialVersionUID = 2;
  
  /**
   * The already named contexts.
   */
  protected final NamingMap named_contexts;

  /**
   * The already named objects.
   */
  protected final NamingMap named_objects;
  
  /**
   * Create the naming conetxt with default (transient) naming maps.
   */
  public TransientContext()
  {
    this(new NamingMap(), new NamingMap());
  }
  
  /**
   * Create the naming conetxt with the two provided naming maps.
   * 
   * @param context_map the map for contexts
   * @param object_map the map for objectss
   */
  public TransientContext(NamingMap context_map, NamingMap object_map)
  {
    named_contexts = context_map;
    named_objects = object_map;
  }

  /**
   * Gives the object a name, valid in this context.
   *
   * @param a_name the name, being given to the object.
   * @param an_object the object, being named.
   *
   * @throws AlreadyBound if the object is already named in this context.
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  public void bind(NameComponent[] a_name, Object an_object)
            throws NotFound, CannotProceed, InvalidName, AlreadyBound
  {
    if (a_name.length == 1)
      named_objects.bind(a_name [ 0 ], an_object);
    else
      {
        NamingContext context =
          (NamingContext) named_contexts.get(a_name [ 0 ]);
        context.bind(getSuffix(a_name), an_object);
      }
  }

  /**
   * Gives a child context name, valid in this context.
   *
   * @param a_name the name, being given to the child context.
   * @param a_context the child context being named.
   *
   * @throws AlreadyBound if the child context is already named in
   * the current context.
   */
  public void bind_context(NameComponent[] a_name, NamingContext a_context)
                    throws NotFound, CannotProceed, InvalidName, AlreadyBound
  {
    if (a_name.length == 1)
      named_contexts.bind(a_name [ 0 ], a_context);
    else
      {
        NamingContext context =
          (NamingContext) named_contexts.get(a_name [ 0 ]);
        context.bind_context(getSuffix(a_name), a_context);
      }
  }

  /**
   * Create a new context and give it a given name (bound it)
   * in the current context.
   *
   * The context being created is returned by calling
   * {@link #new_context()}.
   *
   * @param a_name the name being given to the new context.
   *
   * @return the newly created context.
   *
   * @throws AlreadyBound if the name is already in use.
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  public NamingContext bind_new_context(NameComponent[] a_name)
                                 throws NotFound, AlreadyBound, CannotProceed,
                                        InvalidName
  {
    if (named_contexts.containsKey(a_name [ 0 ]) ||
        named_objects.containsKey(a_name [ 0 ])
       )
      throw new AlreadyBound();

    NamingContext child = new_context();
    bind_context(a_name, child);
    return child;
  }

  /**
   * Destroy this context (must be empty).
   * @throws NotEmpty if the context being destroyed is not empty.
   */
  public void destroy()
               throws NotEmpty
  {
    if (named_contexts.size() > 0 || named_objects.size() > 0)
      throw new NotEmpty();
  }

  /**
   * Iterate over all bindings, defined in this namind context.
   *
   * @param amount the maximal number of context to return in the
   * holder a_list. The remaining bindings are accessible via iterator
   * an_iter. If the parameter amount is zero, all bindings are accessed only
   * via this iterator.
   *
   * This implementation list contexts first, then objects.
   *
   * @param a_list the holder, where the returned bindigs are stored.
   * @param an_iter the iterator that can be used to access the remaining
   * bindings.
   */
  public void list(int amount, BindingListHolder a_list,
                   BindingIteratorHolder an_iter
                  )
  {
    int nb = named_contexts.size() + named_objects.size();
    int nl = nb;
    if (nl > amount)
      nl = amount;

    a_list.value = new Binding[ nl ];

    Iterator contexts = named_contexts.entries().iterator();
    Iterator objects = named_objects.entries().iterator();

    // Create a binding list.
    for (int i = 0; i < nl; i++)
      {
        if (contexts.hasNext())
          a_list.value [ i ] = mkBinding(contexts.next(), BindingType.ncontext);
        else if (objects.hasNext())
          a_list.value [ i ] = mkBinding(objects.next(), BindingType.nobject);
        else
          throw new InternalError();
      }

    // Create an iterator.
    Binding[] remainder = new Binding[ nb - nl ];
    int p = 0;

    while (contexts.hasNext())
      remainder [ p++ ] = mkBinding(contexts.next(), BindingType.ncontext);

    while (objects.hasNext())
      remainder [ p++ ] = mkBinding(objects.next(), BindingType.nobject);

    Binding_iterator_impl bit = new Binding_iterator_impl(remainder);
    _orb().connect(bit);
    an_iter.value = bit;
  }

  /**
   * Creates a new naming context, not bound to any name.
   */
  public NamingContext new_context()
  {
    Ext context = new Ext(new TransientContext());

    // Connect the context to the current ORB:
    _orb().connect(context);
    return context;
  }

  /**
   * Names or renames the object.
   *
   * @param a_name the new name, being given to the object
   * in the scope of the current context. If the object is already
   * named in this context, it is renamed.
   *
   * @param an_object the object, being named.
   *
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  public void rebind(NameComponent[] a_name, Object an_object)
              throws NotFound, CannotProceed, InvalidName
  {
    if (a_name.length == 1)
      named_objects.rebind(a_name [ 0 ], an_object);
    else
      {
        NamingContext context =
          (NamingContext) named_contexts.get(a_name [ 0 ]);
        context.rebind(getSuffix(a_name), an_object);
      }
  }

  /**
   * Names or renames the child context.
   * If the child context is already named in
   * the current context, it is renamed. The the name being given is in
   * use, the old meaning of the name is discarded.
   *
   * @param a_name the name, being given to the child context in the scope
   * of the current context.
   *
   * @param a_context the child context being named.
   *
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  public void rebind_context(NameComponent[] a_name, NamingContext a_context)
                      throws NotFound, CannotProceed, InvalidName
  {
    if (a_name.length == 1)
      named_contexts.rebind(a_name [ 0 ], a_context);
    else
      {
        NamingContext context =
          (NamingContext) named_contexts.get(a_name [ 0 ]);
        context.rebind_context(getSuffix(a_name), a_context);
      }
  }

  /**
   * Get the object, bound to the specified name in this
   * context. The given object must match the bound
   * name.
   *
   * This implementation resolves the names as defined in specification
   * of the CORBA naming service. This means, if the beginning of the
   * name can be resolved to some naming context, the request is
   * forwarded to this context, passing the unresolved name part as a
   * parameter. In this way, it is possible to have a hierarchy of the
   * naming services. The central services resolve the the beginning
   * of the name. The local services resolve the remaining nodes of the
   * name that may be relevant to some local details. It can be three or
   * more ranks of the naming services.
   *
   * @param a_name the object name.
   *
   * @return the object, matching this name. The client
   * usually casts or narrows (using the helper) the returned value
   * to the more specific type.
   *
   * @throws NotFound if the name cannot be resolved.
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  public Object resolve(NameComponent[] a_name)
                 throws NotFound, CannotProceed, InvalidName
  {
    NameValidator.check(a_name);

    if (a_name.length > 1)
      return resolveSubContext(a_name);
    else
      {
        // A single node name.
        org.omg.CORBA.Object object;

        object = named_objects.get(a_name [ 0 ]);
        if (object != null)
          return object;

        object = named_contexts.get(a_name [ 0 ]);
        if (object != null)
          return object;
      }

    throw new NotFound(NotFoundReason.missing_node, a_name);
  }

  /**
   * Removes the name from the binding context.
   *
   * @param a_name a name to remove.
   *
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  public void unbind(NameComponent[] a_name)
              throws NotFound, CannotProceed, InvalidName
  {
    NameValidator.check(a_name);

    // Single node name - handle it.
    if (a_name.length == 1)
      {
        if (named_objects.containsKey(a_name [ 0 ]))
          named_objects.remove(a_name [ 0 ]);
        else if (named_contexts.containsKey(a_name [ 0 ]))
          named_contexts.remove(a_name [ 0 ]);
        else
          throw new NotFound(NotFoundReason.missing_node, a_name);
      }
    else
      {
        // Handle the first node and forward the command.
        NamingContext subcontext =
          (NamingContext) named_contexts.get(a_name [ 0 ]);

        if (subcontext == null)
          throw new NotFound(NotFoundReason.missing_node, a_name);

        subcontext.unbind(getSuffix(a_name));
      }
  }

  /**
   * Get the name suffix, discarding the first member.
   */
  private NameComponent[] getSuffix(NameComponent[] a_name)
  {
    NameComponent[] suffix = new NameComponent[ a_name.length - 1 ];
    System.arraycopy(a_name, 1, suffix, 0, suffix.length);
    return suffix;
  }

  /**
   * Create a binding.
   *
   * @param an_entry the entry, defining the bound object.
   * @param type the binding type.
   * @return the created binding.
   */
  private Binding mkBinding(java.lang.Object an_entry, BindingType type)
  {
    Map.Entry entry = (Map.Entry) an_entry;
    Binding b = new Binding();

    // The name component has always only one node (the current context)
    b.binding_name = new NameComponent[] { (NameComponent) entry.getKey() };
    b.binding_type = type;
    return b;
  }

  /**
   * Find the context, bound to the first name of the given
   * name, and pass the remainder (without the first node)
   * of the name for that context to resolve.
   *
   * @param a_name the name to resolve.
   *
   * @return the resolved context
   */
  private Object resolveSubContext(NameComponent[] a_name)
                            throws NotFound, CannotProceed, InvalidName
  {
    // A multiple node name.
    // This context resolves the first node only.
    NamingContext context = (NamingContext) named_contexts.get(a_name [ 0 ]);
    if (context == null)
      throw new NotFound(NotFoundReason.missing_node, a_name);

    NameComponent[] suffix = getSuffix(a_name);

    return context.resolve(suffix);
  }
}
