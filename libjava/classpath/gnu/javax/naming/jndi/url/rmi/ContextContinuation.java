/* ContextContinuation.java -- RMI naming context
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

package gnu.javax.naming.jndi.url.rmi;

import java.rmi.AccessException;
import java.rmi.AlreadyBoundException;
import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import javax.naming.CommunicationException;
import javax.naming.Context;
import javax.naming.InvalidNameException;
import javax.naming.Name;
import javax.naming.NameAlreadyBoundException;
import javax.naming.NameNotFoundException;
import javax.naming.NameParser;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.OperationNotSupportedException;

/**
 * The implementation of the RMI URL context. This context connects 
 * 
 * @author Audrius Meskauskas
 */
public class ContextContinuation implements Context
{
  /**
   * The default registry location.
   */
  public static final String DEFAULT_REGISTRY_LOCATION = "rmi://localhost:1099";
  
  /**
   * The local or remote RMI registry, performing the actual work for this
   * context.
   */
  Registry registry;
  
 /**
   * The properties.
   */
  Properties properties;
  
  /**
   * The flag, indicating, that the lookup methods were called before.
   * If the lookup methods were called before, the existing ORB cannot be
   * destroyed, as references to the existing objects will become
   * unfunctional.
   */
  boolean lookupCalled;
  
  /**
   * Add new environment property to the environment of this context. Both name
   * and value of the new property must not be null. If the property is already
   * defined, is current value is replaced by the propVal. This method replaces
   * the registry. The new registry will be lazily instantiated on the first
   * call.
   * 
   * @param key
   *          the name of the new property
   * @param value
   *          the value of the new property
   * @return the previous value of this property or null if the property has not
   *         been previously defined
   */
  public Object addToEnvironment(String key, Object value)
  {
    removeRegistry();
    if (key == null || value == null)
      throw new NullPointerException();
    return properties.put(key, value);
  }

  /**
   * Returns the environment, associated with this naming context. The returned
   * table should never be modified by the caller (the registry would not be updated
   * in such case). Use {@link #addToEnvironment} and
   * {@link #removeFromEnvironment} to modify the environement, if needed.
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
   * without action if this property is not defined. Replaces the ORB,
   * constructing the new ORB with the changes set of properties (you can
   * replace the CORBA implementation provider, for instance). The new ORB will
   * be lazily instantiated on the first call.
   * 
   * @param propName
   *          the name of the property being removed.
   * @return the value of the property that has been removed or null if the
   *         property was not defined.
   * @throws NamingException
   */
  public Object removeFromEnvironment(String propName) throws NamingException
  {
    removeRegistry();
    return properties.remove(propName);
  }
  
  /**
   * Remove the current registry reference.
   */
  public void removeRegistry()
  {
    registry = null;    
  }
  
  /**
   * Get the cached or new registry reference.
   * 
   * @return the registry reference, either cached or new.
   */
  public Registry getRegistry() throws NamingException
  {
    if (registry == null)
      {
        String address = properties.getProperty(Context.PROVIDER_URL,
                                                DEFAULT_REGISTRY_LOCATION);
        
        // The format like rmi://localhost:1099 is expected. Parse.
        if (!address.startsWith("rmi://"))
          throw new InvalidNameException(address);
        
        String a = address.substring("rmi://".length());
        
        // The colon, if present, indicates the start of the port number.
        int colon = a.lastIndexOf(':');
        int port;
        
        try
          {
            if (colon >=0)
              {
                port = Integer.parseInt(a.substring(colon+1));
                a = a.substring(0, colon);
              }
            else
              port = Registry.REGISTRY_PORT;
          }
        catch (NumberFormatException e1)
          {
            throw new InvalidNameException(address);
          } 
            
        try
          {
            registry = LocateRegistry.getRegistry(a, port);
          }
        catch (RemoteException e)
          {
            throw new CommunicationException(e.toString());
          }
      }
    return registry;
  }

  /**
   * Create the rmi url context that works, talking with the given RMI registry.
   * 
   * @param props
   *          the properties for this context
   * @param initialRegistry
   *          the initial value of the registry
   */
  public ContextContinuation(Map props, Registry initialRegistry)
  {
    properties = new Properties();
    if (props != null)
      properties.putAll(props);
    registry = initialRegistry;
  }
  
  /**
   * Bind the given name into this context. The .toString() is called to
   * convert into the string representation, required by RMI registry.
   * 
   * @throws NamingException if the object is not an instance of Remote
   */
  public void bind(Name name, Object obj) throws NamingException
  {
    bind(name.toString(), obj);
  }

  /**
   * Bind the given name into this context.
   */
  public void bind(String name, Object obj) throws NamingException
  {
    try
      {
        getRegistry().bind(name, (Remote) obj);
      }
    catch (AccessException e)
      {
        throw new NamingException("access:"+e.toString());
      }
    catch (RemoteException e)
      {
        throw new CommunicationException(e.toString());
      }
    catch (AlreadyBoundException e)
      {
        throw new NameAlreadyBoundException(name);
      }
    catch (ClassCastException c)
      {
        throw new NamingException("Only Remote can be bound:"
                                  + obj.getClass().getName());
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
   * Not supported.
   */
  public String composeName(String name, String prefix) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Subcontexts are not supporte by RMI registry. The only supported case is an
   * empty name (returns the cloned instance of self).
   */
  public Context createSubcontext(Name name) throws NamingException
  {
    if (name.size() == 0)
      return new rmiURLContext(properties);
    else
      throw new OperationNotSupportedException();
  }

  /**
   * Subcontexts are not supporte by RMI registry. The only supported case is an
   * empty name (returns the cloned instance of self).
   */
  public Context createSubcontext(String name) throws NamingException
  {
    if (name.length() == 0)
      return new rmiURLContext(properties);
    else
      throw new OperationNotSupportedException();
  }

  /**
   * Subcontexts are not supporte by RMI registry.
   */
  public void destroySubcontext(Name name) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Subcontexts are not supporte by RMI registry.
   */
  public void destroySubcontext(String name) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Returns the naming service URL, same that was passed vie
   * {@link Context#PROVIDER_URL}.
   */
  public String getNameInNamespace() throws NamingException
  {
    return properties.getProperty(Context.PROVIDER_URL,
                                  DEFAULT_REGISTRY_LOCATION);
  }

  /**
   * Not supported, this context never parses any names.
   */
  public NameParser getNameParser(Name name) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * Not supported, this context never parses any names.
   */
  public NameParser getNameParser(String name) throws NamingException
  {
    throw new OperationNotSupportedException();
  }

  /**
   * List existing bindings of this context (the parameter must be empty name,
   * indicating the root context). The class name of the returned name class
   * pairs is "Remote", as this "quick preview" method should probably not call
   * the naming service again. Use listBindings if more details are required.
   */
  public NamingEnumeration list(Name name) throws NamingException
  {
    if (name.size() > 0)
      throw new OperationNotSupportedException("Only empty name is accepted");
    return list("");
  }

  /**
   * List existing bindings of this context (the parameter must be empty string,
   * indicating the root context). The class name of the returned name class
   * pairs is "Remote", as this "quick preview" method should probably not call
   * the naming service again. Use listBindings if more details are required.
   */
  public NamingEnumeration list(String name) throws NamingException
  {
    if (name.length() > 0)
      throw new OperationNotSupportedException("Only empty name is accepted");
    
    try
      {
        return new ListEnumeration(getRegistry().list());
      }
    catch (Exception e)
      {
        throw new NamingException(e.toString());
      }
  }

  /**
   * List existing bindings of this context (the parameter must be empty name,
   * indicating the root context). 
   */
  public NamingEnumeration listBindings(Name name) throws NamingException
  {
    if (name.size() > 0)
      throw new OperationNotSupportedException("Only empty name is accepted");
    return listBindings("");
  }

  /**
   * List existing bindings of this context (the parameter must be empty name,
   * indicating the root context). 
   */
  public NamingEnumeration listBindings(String name) throws NamingException
  {
    if (name.length() > 0)
      throw new OperationNotSupportedException("Only empty name is accepted");
    
    try
      {
        Registry r = getRegistry();
        return new ListBindingsEnumeration(r.list(), r);
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
   * Rebinds this object.
   * 
   * @param name
   *          the object name (.toString()) is used to convert into string
   *          representation.
   * @param obj
   *          object (must be an instance of Remote).
   */
  public void rebind(Name name, Object obj) throws NamingException
  {
    rebind(name.toString(), obj);
  }

  /**
   * Rebinds this object.
   * 
   * @param name
   *          the object name.
   * @param obj
   *          object (must be an instance of Remote).
   */
  public void rebind(String name, Object obj) throws NamingException
  {
    try
      {
        getRegistry().rebind(name, (Remote) obj);
      }
    catch (AccessException e)
      {
        throw new NamingException("access:"+e.toString());
      }
    catch (RemoteException e)
      {
        throw new CommunicationException(e.toString());
      }
    catch (ClassCastException c)
      {
        throw new NamingException("Only Remote can be bound:"
                                  + obj.getClass().getName());
      }
  }
  
  /**
   * Renames the object. If the new name is already bound in the given context,
   * the {@link AlreadyBoundException} is thrown and the oldName binding is
   * preserved.
   */
  public void rename(Name oldName, Name newName) throws NamingException
  {
    rename(oldName.toString(), newName.toString());
  }

  /**
   * Renames the object. If the new name is already bound in the given context,
   * the {@link AlreadyBoundException} is thrown and the oldName binding is
   * preserved.
   */
  public synchronized void rename(String oldName, String newName)
      throws NamingException
  {
    try
      {
        Registry r = getRegistry();
        Remote object = r.lookup(oldName);
        r.unbind(oldName);
        try
          {
            r.bind(newName, object);
          }
        catch (AlreadyBoundException e)
          {
            // Bind it back.
            try
              {
                r.bind(oldName, object);
              }
            catch (AlreadyBoundException e1)
              {
                // We have just removed this name.
                throw new InternalError();
              }
            throw new NameAlreadyBoundException(newName);
          }
      }
    catch (AccessException e)
      {
        throw new NamingException(e.toString());
      }
    catch (RemoteException e)
      {
        throw new CommunicationException(e.toString());
      }
    catch (NotBoundException e)
      {
        throw new CommunicationException(e.toString());
      }
  }

  /**
   * Unbind the object.
   */
  public void unbind(Name name) throws NamingException
  {
    unbind(name.toString());
  }

  /**
   * Unbind the object.
   */
  public void unbind(String name) throws NamingException
  {
    try
      {
        getRegistry().unbind(name);
      }
    catch (AccessException e)
      {
        throw new NamingException(e.toString());
      }
    catch (RemoteException e)
      {
        throw new CommunicationException(e.toString());
      }
    catch (NotBoundException e)
      {
        throw new CommunicationException(e.toString());
      }
  }
  
  /**
   * Release the associated resources.
   */
  public void close() throws NamingException
  {
    removeRegistry();
  }
  
  /**
   * Resolve the object by name.
   * 
   * @param name
   *          the object name, .toString() is used to get the string
   *          representation.
   */
  public Object lookup(Name name) throws NamingException
  {
    return lookup(name.toString());
  }
  
  /**
   * Resolve the object by name
   * 
   * @param name the object name.
   */
  public Object lookup(String name) throws NamingException
  {
    try
      {
        return getRegistry().lookup(name);
      }
    catch (AccessException e)
      {
        throw new NamingException(e.toString());
      }
    catch (RemoteException e)
      {
        throw new CommunicationException(e.toString());
      }
    catch (NotBoundException e)
      {
        throw new NameNotFoundException(name);
      }
  }
}
