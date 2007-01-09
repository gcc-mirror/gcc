/* InitialDirContext.java --
   Copyright (C) 2000, 2001, 2004  Free Software Foundation, Inc.

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


package javax.naming.directory;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.Name;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.NoInitialContextException;
import javax.naming.NotContextException;

/**
 * @author Tom Tromey (tromey@redhat.com)
 * @date June 25, 2001
 */
public class InitialDirContext extends InitialContext implements DirContext
{
  public InitialDirContext ()
    throws NamingException
  {
    this (null);
  }

  protected InitialDirContext (boolean lazy)
    throws NamingException
  {
    super (lazy);
  }

  public InitialDirContext (Hashtable<?, ?> environment)
    throws NamingException
  {
    super (environment);
  }

  // The InitialContext docs suggest that this exist.  And it does
  // seem like a good idea.  but the InitialDirContext docs indicate
  // it cannot be non-private.
  private DirContext getURLOrDefaultInitDirCtx (Name name)
    throws NamingException
  {
    Context c = getURLOrDefaultInitCtx (name);
    if (c == null)
      throw new NoInitialContextException ();
    else if (! (c instanceof DirContext))
      throw new NotContextException ();
    return (DirContext) c;
  }

  private DirContext getURLOrDefaultInitDirCtx (String name)
    throws NamingException
  {
    Context c = getURLOrDefaultInitCtx (name);
    if (c == null)
      throw new NoInitialContextException ();
    else if (! (c instanceof DirContext))
      throw new NotContextException ();
    return (DirContext) c;
  }

  public Attributes getAttributes (String name)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getAttributes (name);
  }

  public Attributes getAttributes (String name, String[] attrIds)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getAttributes (name, attrIds);
  }

  public Attributes getAttributes (Name name)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getAttributes (name);
  }

  public Attributes getAttributes(Name name, String[] attrIds)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getAttributes (name, attrIds);
  }

  public void modifyAttributes(Name name, int mod_op, Attributes attrs)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).modifyAttributes (name, mod_op, attrs);
  }

  public void modifyAttributes(String name, int mod_op, Attributes attrs)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).modifyAttributes (name, mod_op, attrs);
  }

  public void modifyAttributes(Name name, ModificationItem[] mods)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).modifyAttributes (name, mods);
  }

  public void modifyAttributes(String name, ModificationItem[] mods)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).modifyAttributes (name, mods);
  }

  public void bind(Name name, Object obj, Attributes attrs)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).bind (name, obj, attrs);
  }

  public void bind(String name, Object obj, Attributes attrs)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).bind (name, obj, attrs);
  }

  public void rebind(Name name, Object obj, Attributes attrs)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).rebind (name, obj, attrs);
  }

  public void rebind(String name, Object obj, Attributes attrs)
    throws NamingException
  {
    getURLOrDefaultInitDirCtx (name).rebind (name, obj, attrs);
  }

  public DirContext createSubcontext(Name name, Attributes attrs)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).createSubcontext (name, attrs);
  }

  public DirContext createSubcontext(String name, Attributes attrs)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).createSubcontext (name, attrs);
  }

  public DirContext getSchema(Name name)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getSchema (name);
  }

  public DirContext getSchema(String name)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getSchema (name);
  }

  public DirContext getSchemaClassDefinition(Name name)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getSchemaClassDefinition (name);
  }

  public DirContext getSchemaClassDefinition(String name)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).getSchemaClassDefinition (name);
  }

  public NamingEnumeration<SearchResult> search(Name name,
                                                Attributes matchingAttributes,
                                                String[] attributesToReturn)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes,
						    attributesToReturn);
  }

  public NamingEnumeration<SearchResult> search(String name,
                                                Attributes matchingAttributes,
                                                String[] attributesToReturn)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes,
						    attributesToReturn);
  }

  public NamingEnumeration<SearchResult> search(Name name,
                                                Attributes matchingAttributes)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes);
  }

  public NamingEnumeration<SearchResult> search(String name,
                                                Attributes matchingAttributes)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes);
  }

  public NamingEnumeration<SearchResult> search(Name name, String filter,
                                                SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filter, cons);
  }

  public NamingEnumeration<SearchResult> search(String name, String filter,
                                                SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filter, cons);
  }

  public NamingEnumeration<SearchResult> search(Name name, String filterExpr,
                                                Object[] filterArgs,
                                                SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filterExpr,
						    filterArgs, cons);
  }

  public NamingEnumeration<SearchResult> search(String name,
                                                String filterExpr,
                                                Object[] filterArgs,
                                                SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filterExpr,
						    filterArgs, cons);
  }
}
