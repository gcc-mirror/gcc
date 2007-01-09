/* ContextContinuation.java -- handles corbaname: urls
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

import gnu.CORBA.NamingService.Ext;
import gnu.CORBA.NamingService.NameTransformer;

import java.util.Hashtable;

import javax.naming.Binding;
import javax.naming.Context;
import javax.naming.ContextNotEmptyException;
import javax.naming.InvalidNameException;
import javax.naming.Name;
import javax.naming.NameAlreadyBoundException;
import javax.naming.NameClassPair;
import javax.naming.NameNotFoundException;
import javax.naming.NameParser;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.OperationNotSupportedException;
import javax.naming.directory.InvalidAttributesException;

import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.CosNaming._NamingContextExtStub;
import org.omg.CosNaming._NamingContextStub;
import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.NotFound;

/**
 * The context to represent the corba naming service. Being the naming service,
 * the returned context supports creating the subcontexts, forwarding this task
 * to the existing naming service. When listing bindings, it uses the
 * {@link Context#BATCHSIZE} property to determine, how many bindings should
 * be returned at once (the process is transparend)
 * 
 * @author Audrius Meskauskas (audriusa@Bioinformatics.org)
 */
public class ContextContinuation implements Context
{
  /**
   * This number of bindings will be requested from the naming server at once,
   * while the subsequent bindings will be requested via binding iterator one by
   * one. Use {@link Context#BATCHSIZE} to override the value of this constant.
   */
  public int DEFAULT_BATCH_SIZE = 20;
  
  /**
   * The actual CORBA naming service.
   */
  NamingContextExt service;
  
  /**
   * The object request broker, used to access the naming service. This field
   * is only initialised when the context is constructed from the URL. 
   */
  ORB orb;
  
  /**
   * The properties.
   */
  Hashtable properties;
  
  /**
   * The parent factory.
   */
  GiopNamingServiceFactory factory;
  
  /**
   * The name transformer to obtain the name from its string representation. The
   * to_name method of the naming service is avoided as it may be remote and
   * hence expensive. The conversion rules are standard and cannot be service
   * specific.
   */
  static NameTransformer transformer = new NameTransformer();
  
  /**
   * The batch size for list operations - how many to return at once.
   */
  public final int howMany;
  
  /**
   * Creates a new naming context that uses naming service, represented by the
   * given CORBA object.
   * 
   * @param namingService
   *          the naming service object. It must be possible to narrow it into
   *          the NamingContextExt.
   * @param props
   *          the environment table.
   * @param orb
   *          the associated ORB. This reference is used during cleanup.
   * @param the
   *          parent factory. This reference is used during cleanup.
   */
  public ContextContinuation(org.omg.CORBA.Object nsObject,
                                     Hashtable props, ORB anOrb,
                                     GiopNamingServiceFactory aFactory)
  {
    factory = aFactory;
    orb = anOrb;

    Delegate delegate = ((ObjectImpl) nsObject)._get_delegate();

    // If the IOR provides the IDL ID, we can check if our name
    // service is old NamingContext or new NamingContextExt.
    // Not all forms of the URL always provide the IDL id.
    if (!nsObject._is_a(NamingContextExtHelper.id())
        && nsObject._is_a(NamingContextHelper.id()))
      {
        // We are surely working with the old version.
        _NamingContextStub stub = new _NamingContextStub();
        stub._set_delegate(delegate);
        // The Ext object will add the necessary extensions.
        service = new Ext(stub);
      }
    else
      {
        // We expecte the service to be the NamingContextExt (this is true
        // for both Sun's and our implementations). There is no easy way
        // to check the version.
        _NamingContextExtStub stub = new _NamingContextExtStub();
        stub._set_delegate(delegate);
        service = stub;
      }
    properties = props;
    howMany = getBatchSize();
  }

  /**
   * Give the specified name for the specified object. The passed name must not
   * be already bound to some other object. The components of the name are
   * mapped into the components of the CORBA name.
   * 
   * @param name
   *          the name that will be given to the object (in the scope of this
   *          context).
   * @param obj
   *          the object being named.
   * @throws NameAlreadyBoundException
   *           if this name is already used to name some object.
   * @throws InvalidAttributesException
   *           if the object does not supply all required attributes.
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void bind(Name name, Object obj) throws NamingException
  {
    try
      {
        org.omg.CORBA.Object object = (org.omg.CORBA.Object) obj;
        service.bind(toGiop(name), object);
      }
    catch (ClassCastException e)
      {
        throw new NamingException(org.omg.CORBA.Object.class + " required ");
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
    catch (AlreadyBound e)
      {
        throw new NameAlreadyBoundException();
      }
    catch (Exception e)
      {
        throw new NamingException(e.toString());
      }
  }

  /**
   * Give the specified name for the specified object. The passed name must not
   * be already bound to some other object.
   * 
   * @param name
   *          the name that will be given to the object (in the scope of this
   *          context).
   * @param obj
   *          the object being named.
   * @throws NameAlreadyBoundException
   *           if this name is already used to name some object.
   * @throws InvalidAttributesException
   *           if the object does not supply all required attributes.
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void bind(String name, Object obj) throws NamingException
  {
    try
      {
        org.omg.CORBA.Object object = (org.omg.CORBA.Object) obj;
        service.bind(transformer.toName(name), object);
      }
    catch (ClassCastException e)
      {
        throw new NamingException(org.omg.CORBA.Object.class + " required ");
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
    catch (AlreadyBound e)
      {
        throw new NameAlreadyBoundException();
      }
    catch (Exception e)
      {
        throw new NamingException(e.toString());
      }
  }

  /**
   * Releases all resources, associated with this context. The close() method
   * can be called several times, but after it has been once invoked, it is not
   * allowed to call any other method of this context. This method destroys
   * the ORB, if we have one.
   * 
   * @throws NamingException
   */
  public void close() throws NamingException
  {
    if (orb != null && factory !=null)
      {
        factory.checkIfReferenced(orb);
      }
  }

  /**
   * Not supported.
   */
  public Name composeName(Name name, Name prefix) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Not supported
   */
  public String composeName(String name1, String name2) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Creates the new naming subcontext and binds it to the current (this)
   * context. The returned object will wrap around the newly created CORBA
   * subcontext
   * 
   * @param name
   *          the name of the new context being created
   * @return the newly created context, bound to the instance of the context on
   *         that the method has been called
   * @throws NameAlreadyBoundException
   *           if this name is already bound
   * @throws InvalidAttributesException
   *           if the creation of the new context requires the missing mandatory
   *           attributes
   * @throws NamingException
   */
  public Context createSubcontext(Name subContext) throws NamingException
  {
    try
      {
        org.omg.CORBA.Object subcontext = service.bind_new_context(
          toGiop(subContext));
        Hashtable clonedProps = new Hashtable();
        clonedProps.putAll(properties);
        
        // Nulls are passed both for orb and factory, as the child contexts
        // need not to do any cleanup.
        return new ContextContinuation(subcontext, clonedProps, null, null);
      }
    catch (AlreadyBound e)
      {
        throw new NameAlreadyBoundException();
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
    catch (Exception ex)
      {
        throw new NamingException(ex.toString());
      }
  }

  /**
   * Creates the new naming subcontext and binds it to the current (this)
   * context. The returned object will wrap around the newly created CORBA
   * subcontext
   * 
   * @param name
   *          the name of the new context being created
   * @return the newly created context, bound to the instance of the context on
   *         that the method has been called
   * @throws NameAlreadyBoundException
   *           if this name is already bound
   * @throws InvalidAttributesException
   *           if the creation of the new context requires the missing mandatory
   *           attributes
   * @throws NamingException
   */
  public Context createSubcontext(String subContext) throws NamingException
  {
    try
      {
        org.omg.CORBA.Object subcontext = 
          service.bind_new_context(transformer.toName(subContext));
        Hashtable clonedProps = new Hashtable();
        clonedProps.putAll(properties);

        // Nulls are passed both for orb and factory, as the child contexts
        // need not to do any cleanup.
        return new ContextContinuation(subcontext, clonedProps, null,
                                               null);
      }
    catch (AlreadyBound e)
      {
        throw new NameAlreadyBoundException(subContext);
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException(subContext);
      }
    catch (Exception ex)
      {
        throw new NamingException(ex.toString());
      }
  }

  /**
   * Removes the naming subcontext from this naming context. Returns without
   * action if such subcontext does not exist. The context being destroyed must
   * be empty.
   * 
   * @param name
   *          the name of the subcontext beig removed.
   * @throws ContextNotEmptyException
   *           if the named context is not empty.
   * @throws NamingException
   */
  public void destroySubcontext(Name subContext) throws NamingException
  {
    unbind(subContext);
  }

  /**
   * Removes the naming subcontext from this naming context. Returns without
   * action if such subcontext does not exist. The context being destroyed must
   * be empty.
   * 
   * @param name
   *          the name of the subcontext beig removed.
   * @throws ContextNotEmptyException
   *           if the named context is not empty.
   * @throws NamingException
   */
  public void destroySubcontext(String subContext) throws NamingException
  {
    unbind(subContext);
  }

  /**
   * Returs the full name of this naming context. The returned string is not a
   * JNDI composite name and should not be passed directly to the methods of the
   * naming context. This implementation returns the IOR.
   * 
   * @return the full name of this naming context, in its own namespace.
   * @throws OperationNotSupportedException
   *           if the naming system, represented by this context, does not
   *           support the notation of the full name.
   * @throws NamingException
   */
  public String getNameInNamespace() throws NamingException
  {
    if (orb != null)
      return orb.object_to_string(service);
    else
      {
        try
          {
            ObjectImpl impl = (ObjectImpl) service;
            return impl._orb().object_to_string(impl);
          }
        catch (ClassCastException e)
          {
            throw new UnsupportedOperationException();
          }
      }
  }

  /**
   * Not supported.
   */
  public NameParser getNameParser(Name name) throws NamingException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Not supported.
   */
  public NameParser getNameParser(String name) throws NamingException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Creates and returns the enumeration over the name bindings that are present
   * the given subcontext. The enumeration elements have the type of
   * {@link NameClassPair}, providing also information about the class of the
   * bound object. The behaviour in the case if the bindings are added or
   * removed later is not defined. The contents of the subcontexts are not
   * included.
   * 
   * @param name
   *          the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  public NamingEnumeration list(Name name) throws NamingException
  {
    BindingIteratorHolder bi = new BindingIteratorHolder();
    BindingListHolder bl = new BindingListHolder();

    NamingContext subcontext;

    if (name.size() == 0)
      subcontext = service;
    else
      {
        try
          {
            subcontext = (NamingContextHelper.narrow(service.resolve(toGiop(name))));
          }
        catch (Exception e)
          {
            throw new NamingException(e.toString());
          }

      }

    subcontext.list(howMany, bl, bi);

    return new ListEnumeration(bl, bi, howMany);
  }

  /**
   * Creates and returns the enumeration over the name bindings that are present
   * the given subcontext. The enumeration elements have the type of
   * {@link NameClassPair}, providing also information about the class of the
   * bound object. The behaviour in the case if the bindings are added or
   * removed later is not defined. The contents of the subcontexts are not
   * included.
   * 
   * @param name
   *          the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  public NamingEnumeration list(String name) throws NamingException
  {
    BindingIteratorHolder bi = new BindingIteratorHolder();
    BindingListHolder bl = new BindingListHolder();

    NamingContext subcontext;

    if (name.length() == 0)
      subcontext = service;
    else
      {
        try
          {
            subcontext = (NamingContextHelper.narrow(service.resolve_str(name)));
          }
        catch (Exception e)
          {
            throw new NamingException(e.toString());
          }

      }

    subcontext.list(howMany, bl, bi);

    return new ListEnumeration(bl, bi, howMany);
  }

  /**
   * Creates and returns the enumeration over the name - object bindings that
   * are present the given subcontext. The enumeration elements have the type of
   * {@link Binding}, providing also information about the class of the bound
   * object. The behaviour in the case if the bindings are added or removed
   * later is not defined. The contents of the subcontexts are not included.
   * 
   * @param name
   *          the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  public NamingEnumeration listBindings(Name name) throws NamingException
  {
    BindingIteratorHolder bi = new BindingIteratorHolder();
    BindingListHolder bl = new BindingListHolder();

    NamingContext subcontext;

    if (name.size() == 0)
      subcontext = service;
    else
      {
        try
          {
            subcontext = (NamingContextHelper.narrow(service.resolve(toGiop(name))));
          }
        catch (Exception e)
          {
            throw new NamingException(e.toString());
          }
      }

    subcontext.list(howMany, bl, bi);

    return new ListBindingsEnumeration(bl, bi, howMany, subcontext);
  }

  /**
   * Creates and returns the enumeration over the name - object bindings that
   * are present the given subcontext. The enumeration elements have the type of
   * {@link Binding}, providing also information about the class of the bound
   * object. The behaviour in the case if the bindings are added or removed
   * later is not defined. The contents of the subcontexts are not included.
   * 
   * @param name
   *          the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  public NamingEnumeration listBindings(String name) throws NamingException
  {
    BindingIteratorHolder bi = new BindingIteratorHolder();
    BindingListHolder bl = new BindingListHolder();

    NamingContext subcontext;

    if (name.length() == 0)
      subcontext = service;
    else
      {
        try
          {
            subcontext = (NamingContextHelper.narrow(service.resolve_str(name)));
          }
        catch (Exception e)
          {
            throw new NamingException(e.toString());
          }

      }

    subcontext.list(howMany, bl, bi);

    return new ListBindingsEnumeration(bl, bi, howMany, subcontext);
  }

  /**
   * Gets the previously named object by name. If the passed name is empty, the
   * method should return a cloned instance of this naming context.
   * 
   * @param name
   *          the name of the object being searched in this context
   * @return the named object
   * @throws NameNotFountException
   *           if the name is not found
   */
  public Object lookup(Name name) throws NamingException
  {
    try
      {
        return service.resolve(toGiop(name));
      }
    catch (NotFound e)
      {
        throw new NameNotFoundException();
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
    catch (Exception e)
      {
        throw new NamingException(e.toString());
      }
  }

  /**
   * Gets the previously named object by name. If the passed name is empty, the
   * method should return a cloned instance of this naming context.
   * 
   * @param name
   *          the name of the object being searched in this context
   * @return the named object
   * @throws NamingException
   *           if the naming fails.
   */
  public Object lookup(String name) throws NamingException
  {
    try
      {
        return service.resolve_str(name);
      }
    catch (NotFound e)
      {
        throw new NameNotFoundException();
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
    catch (Exception e)
      {
        throw new NamingException(e.toString());
      }
  }

  /**
   * Not supported.
   */
  public Object lookupLink(Name name) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Not supported.
   */
  public Object lookupLink(String name) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Give the specified name for the specified object. Unlike bind, this method
   * silently replaces the existing binding for this name, if one exists.
   * 
   * @param name
   *          the name that will be given to the object (in the scope of this
   *          context).
   * @param obj
   *          the object being named.
   * @throws InvalidAttributesException
   *           if the object does not supply all required attributes.
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void rebind(Name name, Object obj) throws NamingException
  {
    try
      {
        org.omg.CORBA.Object object = (org.omg.CORBA.Object) obj;
        service.rebind(toGiop(name), object);
      }
    catch (ClassCastException e)
      {
        throw new NamingException(org.omg.CORBA.Object.class + " required ");
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
    catch (Exception e)
      {
        throw new NamingException(e.toString());
      }
  }

  /**
   * Give the specified name for the specified object. Unlike bind, this method
   * silently replaces the existing binding for this name, if one exists.
   * 
   * @param name
   *          the name that will be given to the object (in the scope of this
   *          context).
   * @param obj
   *          the object being named.
   * @throws InvalidAttributesException
   *           if the object does not supply all required attributes.
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void rebind(String name, Object obj) throws NamingException
  {
    try
      {
        org.omg.CORBA.Object object = (org.omg.CORBA.Object) obj;
        service.rebind(transformer.toName(name), object);
      }
    catch (ClassCastException e)
      {
        throw new NamingException(org.omg.CORBA.Object.class + " required ");
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
    catch (Exception e)
      {
        throw new NamingException(e.toString());
      }
  }

  /**
   * Renames the existing binding, removing the existing and giving the new name
   * for the same object.
   * 
   * @param oldName
   *          the existing name of the known object
   * @param newName
   *          the new name of the same object
   * @throws NameNotFoundException
   *           if the oldName is unknown for this context
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void rename(Name oldName, Name newName) throws NamingException
  {
    Object object = lookup(oldName);
    unbind(oldName);
    bind(newName, object);
  }

  /**
   * Renames the existing binding, removing the existing and giving the new name
   * for the same object.
   * 
   * @param oldName
   *          the existing name of the known object
   * @param newName
   *          the new name of the same object
   * @throws NameNotFoundException
   *           if the oldName is unknown for this context
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void rename(String oldName, String newName) throws NamingException
  {
    Object object = lookup(oldName);
    unbind(oldName);
    bind(newName, object);
  }

  /**
   * Removes the name - object mapping from the current context. This method
   * returns without action if the name is not bound to an object in the
   * terminal context, but throws {@link NameNotFoundException} if one of the
   * intermadiate contexts does not exist.
   * 
   * @param name
   *          the name to be removed
   * @throws NameNotFoundException
   *           if one of the intermediate naming contexts does not exist. Will
   *           not be thrown if just the terminal binding is missing.
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void unbind(Name name) throws NamingException
  {
    try
      {
        service.unbind(toGiop(name));
      }
    catch (NotFound e)
      {
        throw new NameNotFoundException();
      }
    catch (CannotProceed e)
      {
        throw new ContextNotEmptyException();
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException();
      }
  }

  /**
   * Removes the name - object mapping from the current context. This method
   * returns without action if the name is not bound to an object in the
   * terminal context, but throws {@link NameNotFoundException} if one of the
   * intermadiate contexts does not exist.
   * 
   * @param name
   *          the name to be removed
   * @throws NameNotFoundException
   *           if one of the intermediate naming contexts does not exist. Will
   *           not be thrown if just the terminal binding is missing.
   * @throws NamingException
   *           if the naming operation has failed due other reasons.
   */
  public void unbind(String name) throws NamingException
  {
    try
      {
        service.unbind(transformer.toName(name));
      }
    catch (NotFound e)
      {
        throw new NameNotFoundException(name);
      }
    catch (CannotProceed e)
      {
        throw new ContextNotEmptyException(name);
      }
    catch (InvalidName e)
      {
        throw new InvalidNameException(name);
      }
  }
  
 /**
   * Add new environment property to the environment of this context. Both name
   * and value of the new property must not be null. If the property is already
   * defined, is current value is replaced by the propVal.
   * 
   * @param propName
   *          the name of the new property
   * @param propVal
   *          the value of the new property
   * @return the previous value of this property or null if the property has not
   *         been previously defined
   * @throws NamingException
   */
  public Object addToEnvironment(String key, Object value)
      throws NamingException
  {
    if (key == null || value == null)
      throw new NullPointerException();
    return properties.put(key, value);
  }

  /**
   * Returns the environment, associated with this naming context. The returned
   * table should never be modified by the caller. Use {@link #addToEnvironment}
   * and {@link #removeFromEnvironment} to modify the environement, if needed.
   * 
   * @return the table, representing the environment of this context
   * @throws NamingException
   */
  public Hashtable getEnvironment() throws NamingException
  {
    return properties;
  }

  /**
   * Removes the property with the given name from the environment. Returns
   * without action if this property is not defined.
   * 
   * @param propName
   *          the name of the property being removed.
   * @return the value of the property that has been removed or null if the
   *         property was not defined.
   * @throws NamingException
   */
  public Object removeFromEnvironment(String propName) throws NamingException
  {
    return properties.remove(propName);
  }
  
  /**
   * Convert the {@link Name} into array of the name components, required to the
   * CORBA naming service. First the string representation is obtained, then
   * it is converted using parsing rules of the CORBA name.
   * 
   * @param name
   *          then name to convert
   * @return the converted array of components.
   */
  public NameComponent[] toGiop(Name name) throws InvalidName
  {
    return transformer.toName(name.toString());
  }
  
  /**
   * Get the batch size from the environment properties. The batch size is used
   * for listing operations.
   * 
   * @return the batch size, or some default value if not specified.
   */
  public int getBatchSize()
  {
    int batchSize = DEFAULT_BATCH_SIZE;
    Object bs = properties.get(Context.BATCHSIZE);
    if (bs != null)
      {
        try
          {
            int b = Integer.parseInt(bs.toString());
            if (b >= 0)
              batchSize = b;
          }
        catch (NumberFormatException e)
          {
            // OK, use default value.
          }
      }
    return batchSize;
  }
  
  
}
