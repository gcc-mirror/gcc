/* Context.java --
   Copyright (C) 2000 Free Software Foundation, Inc.

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


package javax.naming;

import java.util.Hashtable;

public interface Context
{
  // Property with name of the inital context factory to use
  public static final String INITIAL_CONTEXT_FACTORY 
    = "java.naming.factory.initial";

  // Property with colon-separated list of object factories to use.
  public static final String OBJECT_FACTORIES
    = "java.naming.factory.object";

  // Property with colon-separated list of state factories to use.
  public static final String STATE_FACTORIES
    = "java.naming.factory.state";

  // Property with colon-separated list of package prefixes to use.
  public static final String URL_PKG_PREFIXES
    = "java.naming.factory.url.pkgs";

  // Property with URL specifying configuration for the service
  // provider to use.
  public static final String PROVIDER_URL 
    = "java.naming.provider.url";
  
  // Property with the DNS host and domain names to use.
  public static final String DNS_URL 
    = "java.naming.dns.url";
  
  // Property with the authoritativeness of the service requested.
  public static final String AUTHORITATIVE 
    = "java.naming.authoritative";
  
  // Property with the batch size to use when returning data via the
  // service's protocol.
  public static final String BATCHSIZE
    = "java.naming.batchsize";
  
  // Property defining how referrals encountered by the service
  // provider are to be processed.
  public static final String REFERRAL
    = "java.naming.referral";

  // Property specifying the security protocol to use.
  public static final String SECURITY_PROTOCOL
    = "java.naming.security.protocol";

  // Property specifying the security level to use.
  public static final String SECURITY_AUTHENTICATION
    = "java.naming.security.authentication";

  // Property for the identity of the principal for authenticating
  // the caller to the service.
  public static final String SECURITY_PRINCIPAL
    = "java.naming.security.principal";

  // Property specifying the credentials of the principal for
  // authenticating the caller to the service.
  public static final String SECURITY_CREDENTIALS
    = "java.naming.security.credentials";

  // Property for specifying the preferred language to use with the
  // service.
  public static final String LANGUAGE
    = "java.naming.language";

  // Property for the initial context constructor to use when searching
  // for other properties.
  public static final String APPLET
    = "java.naming.applet";

  public void bind (Name name, Object obj) throws NamingException;
  public void bind (String name, Object obj) throws NamingException;

  public Object lookup (Name name) throws NamingException;
  public Object lookup (String name) throws NamingException;

  public void rebind (Name name, Object obj) throws NamingException;
  public void rebind (String name, Object obj) throws NamingException;

  public void unbind (Name name) throws NamingException;
  public void unbind (String name) throws NamingException;

  public void rename (Name oldName, Name newName) throws NamingException;
  public void rename (String oldName, String newName) throws NamingException;

  public NamingEnumeration list (Name name) throws NamingException;
  public NamingEnumeration list (String name) throws NamingException;

  public NamingEnumeration listBindings (Name name) throws NamingException;
  public NamingEnumeration listBindings (String name) throws NamingException;

  public void destroySubcontext (Name name) throws NamingException;
  public void destroySubcontext (String name) throws NamingException;

  public Context createSubcontext (Name name) throws NamingException;
  public Context createSubcontext (String name) throws NamingException;

  public Object lookupLink (Name name) throws NamingException;
  public Object lookupLink (String name) throws NamingException;

  public NameParser getNameParser (Name name) throws NamingException;
  public NameParser getNameParser (String name) throws NamingException;

  public Name composeName (Name name, Name prefix) throws NamingException;
  public String composeName (String name, 
			     String prefix) throws NamingException;

  public Object addToEnvironment (String propName, 
				  Object propVal) throws NamingException;

  public Object removeFromEnvironment (String propName) throws NamingException;

  public Hashtable getEnvironment () throws NamingException;

  public void close () throws NamingException;

  public String getNameInNamespace () throws NamingException;
}

