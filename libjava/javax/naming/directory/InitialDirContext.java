/* Copyright (C) 2000, 2001 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming.directory;

import javax.naming.*;
import java.util.Hashtable;

/**
 * @author Tom Tromey <tromey@redhat.com>
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

  public InitialDirContext (Hashtable environment)
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

  public NamingEnumeration search(Name name, Attributes matchingAttributes,
				  String[] attributesToReturn)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes,
						    attributesToReturn);
  }

  public NamingEnumeration search(String name, Attributes matchingAttributes,
				  String[] attributesToReturn)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes,
						    attributesToReturn);
  }

  public NamingEnumeration search(Name name, Attributes matchingAttributes)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes);
  }

  public NamingEnumeration search(String name, Attributes matchingAttributes)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, matchingAttributes);
  }

  public NamingEnumeration search(Name name, String filter,
				  SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filter, cons);
  }

  public NamingEnumeration search(String name, String filter,
				  SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filter, cons);
  }

  public NamingEnumeration search(Name name, String filterExpr,
				  Object[] filterArgs, SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filterExpr,
						    filterArgs, cons);
  }

  public NamingEnumeration search(String name, String filterExpr,
				  Object[] filterArgs, SearchControls cons)
    throws NamingException
  {
    return getURLOrDefaultInitDirCtx (name).search (name, filterExpr,
						    filterArgs, cons);
  }
}
