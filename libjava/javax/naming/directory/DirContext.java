/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;

import javax.naming.*;

/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 13, 2001
 */

public interface DirContext extends Context
{
  public static final int ADD_ATTRIBUTE = 1;
  public static final int REPLACE_ATTRIBUTE = 2;
  public static final int REMOVE_ATTRIBUTE = 3;

  public Attributes getAttributes (String name) throws NamingException;
  public Attributes getAttributes (String name, String[] attrIds) throws NamingException;
  public Attributes getAttributes (Name name) throws NamingException;
  public Attributes getAttributes(Name name, String[] attrIds) throws NamingException;
  public void modifyAttributes(Name name, int mod_op, Attributes attrs) throws NamingException;
  public void modifyAttributes(String name, int mod_op, Attributes attrs) throws NamingException;
  public void modifyAttributes(Name name, ModificationItem[] mods) throws NamingException;
  public void modifyAttributes(String name, ModificationItem[] mods) throws NamingException;
  public void bind(Name name, Object obj, Attributes attrs) throws NamingException;
  public void bind(String name, Object obj, Attributes attrs) throws NamingException;
  public void rebind(Name name, Object obj, Attributes attrs) throws NamingException;
  public void rebind(String name, Object obj, Attributes attrs) throws NamingException;
  public DirContext createSubcontext(Name name, Attributes attrs) throws NamingException;
  public DirContext createSubcontext(String name, Attributes attrs) throws NamingException;
  public DirContext getSchema(Name name) throws NamingException;
  public DirContext getSchema(String name) throws NamingException;
  public DirContext getSchemaClassDefinition(Name name) throws NamingException;
  public DirContext getSchemaClassDefinition(String name) throws NamingException;
  public NamingEnumeration search(Name name, Attributes matchingAttributes, String[] attributesToReturn) throws NamingException;
  public NamingEnumeration search(String name, Attributes matchingAttributes, String[] attributesToReturn) throws NamingException;
  public NamingEnumeration search(Name name, Attributes matchingAttributes) throws NamingException;
  public NamingEnumeration search(String name, Attributes matchingAttributes) throws NamingException;
  public NamingEnumeration search(Name name, String filter, SearchControls cons) throws NamingException;
  public NamingEnumeration search(String name, String filter, SearchControls cons) throws NamingException;
  public NamingEnumeration search(Name name, String filterExpr, Object[] filterArgs, SearchControls cons) throws NamingException;
  public NamingEnumeration search(String name, String filterExpr, Object[] filterArgs, SearchControls cons) throws NamingException;
}
