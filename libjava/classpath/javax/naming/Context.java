/* Context.java -- A naming context
   Copyright (C) 2000, 2006 Free Software Foundation, Inc.

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


package javax.naming;

import java.util.Hashtable;

import javax.naming.directory.InvalidAttributesException;

public interface Context
{
  /**
   * Property with name of the inital context factory to use
   */
  String INITIAL_CONTEXT_FACTORY = "java.naming.factory.initial";

  /**
   * Property with colon-separated list of object factories to use.
   */
  String OBJECT_FACTORIES = "java.naming.factory.object";

  /**
   * Property with colon-separated list of state factories to use.
   */
  String STATE_FACTORIES = "java.naming.factory.state";

  /**
   * Property with colon-separated list of package prefixes to use.
   */
  String URL_PKG_PREFIXES = "java.naming.factory.url.pkgs";

  /**
   * Property with URL specifying configuration for the service provider to use.
   */
  String PROVIDER_URL = "java.naming.provider.url";

  /**
   * Property with the DNS host and domain names to use.
   */
  String DNS_URL = "java.naming.dns.url";

  /**
   * Property with the authoritativeness of the service requested.
   */
  String AUTHORITATIVE = "java.naming.authoritative";

  /**
   * Property with the batch size to use when returning data via the service's
   * protocol.
   */
  String BATCHSIZE = "java.naming.batchsize";

  /**
   * Property defining how referrals encountered by the service provider are to
   * be processed.
   */
  String REFERRAL = "java.naming.referral";

  /**
   * Property specifying the security protocol to use.
   */
  String SECURITY_PROTOCOL = "java.naming.security.protocol";

  /**
   * Property specifying the security level to use.
   */
  String SECURITY_AUTHENTICATION = "java.naming.security.authentication";

  /**
   * Property for the identity of the principal for authenticating the caller to
   * the service.
   */
  String SECURITY_PRINCIPAL = "java.naming.security.principal";

  /**
   * Property specifying the credentials of the principal for authenticating the
   * caller to the service.
   */
  String SECURITY_CREDENTIALS = "java.naming.security.credentials";

  /**
   * Property for specifying the preferred language to use with the service.
   */
  String LANGUAGE = "java.naming.language";

  /**
   * Property for the initial context constructor to use when searching for
   * other properties.
   */
  String APPLET = "java.naming.applet";

  /**
   * Give the specified name for the specified object. The passed name must not
   * be already bound to some other object.
   *
   * @param name the name that will be given to the object (in the scope of this
   *          context).
   * @param obj the object being named.
   * @throws NameAlreadyBoundException if this name is already used to name some
   *           object.
   * @throws InvalidAttributesException if the object does not supply all
   *           required attributes.
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void bind(Name name, Object obj) throws NamingException;

  /**
   * Give the specified name for the specified object. The passed name must not
   * be already bound to some other object.
   *
   * @param name the name that will be given to the object (in the scope of this
   *          context).
   * @param obj the object being named.
   * @throws NameAlreadyBoundException if this name is already used to name some
   *           object.
   * @throws InvalidAttributesException if the object does not supply all
   *           required attributes.
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void bind(String name, Object obj) throws NamingException;

  /**
   * Gets the previously named object by name. If the passed name is empty, the
   * method should return a cloned instance of this naming context.
   *
   * @param name the name of the object being searched in this context
   * @return the named object
   * @throws NamingException if the naming fails.
   */
  Object lookup(Name name) throws NamingException;

  /**
   * Gets the previously named object by name. If the passed name is empty, the
   * method should return a cloned instance of this naming context.
   *
   * @param name the name of the object being searched in this context
   * @return the named object
   * @throws NamingException if the naming fails.
   */
  Object lookup(String name) throws NamingException;

  /**
   * Give the specified name for the specified object. Unlike bind, this method
   * silently replaces the existing binding for this name, if one exists.
   *
   * @param name the name that will be given to the object (in the scope of this
   *          context).
   * @param obj the object being named.
   * @throws InvalidAttributesException if the object does not supply all
   *           required attributes.
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void rebind(Name name, Object obj) throws NamingException;

  /**
   * Give the specified name for the specified object. Unlike bind, this method
   * silently replaces the existing binding for this name, if one exists.
   *
   * @param name the name that will be given to the object (in the scope of this
   *          context).
   * @param obj the object being named.
   * @throws InvalidAttributesException if the object does not supply all
   *           required attributes.
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void rebind(String name, Object obj) throws NamingException;

  /**
   * Removes the name - object mapping from the current context. This method
   * returns without action if the name is not bound to an object in the
   * terminal context, but throws {@link NameNotFoundException} if one of the
   * intermadiate contexts does not exist.
   *
   * @param name the name to be removed
   * @throws NameNotFoundException if one of the intermediate naming contexts
   *           does not exist. Will not be thrown if just the terminal binding
   *           is missing.
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void unbind(Name name) throws NamingException;

  /**
   * Removes the name - object mapping from the current context. This method
   * returns without action if the name is not bound to an object in the
   * terminal context, but throws {@link NameNotFoundException} if one of the
   * intermadiate contexts does not exist.
   *
   * @param name the name to be removed
   * @throws NameNotFoundException if one of the intermediate naming contexts
   *           does not exist. Will not be thrown if just the terminal binding
   *           is missing.
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void unbind(String name) throws NamingException;

  /**
   * Renames the existing binding, removing the existing and giving the new name
   * for the same object.
   *
   * @param oldName the existing name of the known object
   * @param newName the new name of the same object
   * @throws NameNotFoundException if the oldName is unknown for this context
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void rename(Name oldName, Name newName) throws NamingException;

  /**
   * Renames the existing binding, removing the existing and giving the new name
   * for the same object.
   *
   * @param oldName the existing name of the known object
   * @param newName the new name of the same object
   * @throws NameNotFoundException if the oldName is unknown for this context
   * @throws NamingException if the naming operation has failed due other
   *           reasons.
   */
  void rename(String oldName, String newName) throws NamingException;

  /**
   * Creates and returns the enumeration over the name bindings that are present
   * the given subcontext. The enumeration elements have the type of
   * {@link NameClassPair}, providing also information about the class of the
   * bound object. The behaviour in the case if the bindings are added or
   * removed later is not defined. The contents of the subcontexts are not
   * included.
   *
   * @param name the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  NamingEnumeration<NameClassPair> list(Name name) throws NamingException;

  /**
   * Creates and returns the enumeration over the name bindings that are present
   * the given subcontext. The enumeration elements have the type of
   * {@link NameClassPair}, providing also information about the class of the
   * bound object. The behaviour in the case if the bindings are added or
   * removed later is not defined. The contents of the subcontexts are not
   * included.
   *
   * @param name the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  NamingEnumeration<NameClassPair> list(String name) throws NamingException;

  /**
   * Creates and returns the enumeration over the name - object bindings that
   * are present the given subcontext. The enumeration elements have the type of
   * {@link Binding}, providing also information about the class of the bound
   * object. The behaviour in the case if the bindings are added or removed
   * later is not defined. The contents of the subcontexts are not included.
   *
   * @param name the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  NamingEnumeration<Binding> listBindings(Name name) throws NamingException;

  /**
   * Creates and returns the enumeration over the name - object bindings that
   * are present the given subcontext. The enumeration elements have the type of
   * {@link Binding}, providing also information about the class of the bound
   * object. The behaviour in the case if the bindings are added or removed
   * later is not defined. The contents of the subcontexts are not included.
   *
   * @param name the name of the subcontext
   * @return the enumeration over the names, known for the given subcontext.
   * @throws NamingException
   */
  NamingEnumeration<Binding> listBindings(String name) throws NamingException;

  /**
   * Creates the new naming subcontext and binds it to the current (this)
   * context.
   *
   * @param name the name of the new context being created
   * @return the newly created context, bound to the instance of the context on
   *         that the method has been called
   * @throws NameAlreadyBoundException if this name is already bound
   * @throws InvalidAttributesException if the creation of the new context
   *           requires the missing mandatory attributes
   * @throws NamingException
   */
  Context createSubcontext(Name name) throws NamingException;

  /**
   * Creates the new naming subcontext and binds it to the current (this)
   * context.
   *
   * @param name the name of the new context being created
   * @return the newly created context, bound to the instance of the context on
   *         that the method has been called
   * @throws NameAlreadyBoundException if this name is already bound
   * @throws InvalidAttributesException if the creation of the new context
   *           requires the missing mandatory attributes
   * @throws NamingException
   */
  Context createSubcontext(String name) throws NamingException;

  /**
   * Removes the naming subcontext from this naming context. Returns without
   * action if such subcontext does not exist. The context being destroyed must
   * be empty.
   *
   * @param name the name of the subcontext beig removed.
   * @throws ContextNotEmptyException if the named context is not empty.
   * @throws NamingException
   */
  void destroySubcontext(Name name) throws NamingException;

  /**
   * Removes the naming subcontext from this naming context. Returns without
   * action if such subcontext does not exist. The context being destroyed must
   * be empty.
   *
   * @param name the name of the subcontext beig removed.
   * @throws ContextNotEmptyException if the named context is not empty.
   * @throws NamingException
   */
  void destroySubcontext(String name) throws NamingException;

  /**
   * Retrieves the named object, not following the link of the terminal atomic
   * component of the name. If the object, named by the passed name, is not a
   * link, returns that object itself. The intermediate links, if present, are
   * followed.
   *
   * @param name the name of the object that may be a link, leading to another
   *          object.
   * @return the named object, not following the terminal link (if present).
   * @throws NamingException
   */
  Object lookupLink(Name name) throws NamingException;

  /**
   * Retrieves the named object, not following the link of the terminal atomic
   * component of the name. If the object, named by the passed name, is not a
   * link, returns that object itself. The intermediate links, if present, are
   * followed.
   *
   * @param name the name of the object that may be a link, leading to another
   *          object.
   * @return the named object, not following the terminal link (if present).
   * @throws NamingException
   */
  Object lookupLink(String name) throws NamingException;

  /**
   * Obtains the name parser for parsing the names of the given naming
   * subcontext.
   *
   * @param name the name of the subcontext for that the parser must be obtained
   * @return the parser to parse the names of that context
   * @throws NamingException
   */
  NameParser getNameParser(Name name) throws NamingException;

  /**
   * Obtains the name parser for parsing the names of the given naming
   * subcontext.
   *
   * @param name the name of the subcontext for that the parser must be obtained
   * @return the parser to parse the names of that context
   * @throws NamingException
   */
  NameParser getNameParser(String name) throws NamingException;

  /**
   * Composes the name of this context together with another name, related to
   * this context.
   *
   * @param name a name, defined in the scope of this context
   * @param prefix a name of this context itself, defined in the scope of some
   *          ancestor
   * @return the name of the same object as named by the first parameter, but
   *         related to the context of the specified ancestor.
   * @throws NamingException
   */
  Name composeName(Name name, Name prefix) throws NamingException;

  /**
   * Composes the name of this context together with another name, related to
   * this context.
   *
   * @param name a name, defined in the scope of this context
   * @param prefix a name of this context itself, defined in the scope of some
   *          ancestor
   * @return the name of the same object as named by the first parameter, but
   *         related to the context of the specified ancestor.
   * @throws NamingException
   */
  String composeName(String name, String prefix) throws NamingException;

  /**
   * Add new environment property to the environment of this context. Both name
   * and value of the new property must not be null. If the property is already
   * defined, is current value is replaced by the propVal.
   *
   * @param propName the name of the new property
   * @param propVal the value of the new property
   * @return the previous value of this property or null if the property has not
   *         been previously defined
   * @throws NamingException
   */
  Object addToEnvironment(String propName, Object propVal)
      throws NamingException;

  /**
   * Removes the property with the given name from the environment. Returns
   * without action if this property is not defined.
   *
   * @param propName the name of the property being removed.
   * @return the value of the property that has been removed or null if the
   *         property was not defined.
   * @throws NamingException
   */
  Object removeFromEnvironment(String propName) throws NamingException;

  /**
   * Returns the environment, associated with this naming context. The returned
   * table should never be modified by the caller. Use {@link #addToEnvironment}
   * and {@link #removeFromEnvironment} to modify the environement, if needed.
   *
   * @return the table, representing the environment of this context
   * @throws NamingException
   */
  Hashtable<?,?> getEnvironment() throws NamingException;

  /**
   * Releases all resources, associated with this context. The close() method
   * can be called several times, but after it has been once invoked, it is not
   * allowed to call any other method of this context,
   *
   * @throws NamingException
   */
  void close() throws NamingException;

  /**
   * Returs the full name of this naming context. The returned string is not a
   * JNDI composite name and should not be passed directly to the methods of the
   * naming context.
   *
   * @return the full name of this naming context, in its own namespace.
   * @throws OperationNotSupportedException if the naming system, represented by
   *           this context, does not support the notation of the full name.
   * @throws NamingException
   */
  String getNameInNamespace() throws NamingException;
}
