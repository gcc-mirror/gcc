/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;

import javax.naming.*;
import java.io.Serializable;

/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 14, 2001
 */

public interface Attribute extends Cloneable, Serializable
{
  // FIXME: Need to set value from JNDI 1.1.1 fro interoperability.
  // public static final long serialVersionUID = ;

  public NamingEnumeration getAll() throws NamingException;
  public Object get() throws NamingException;
  public int size();
  public String getID();
  public boolean contains(Object attrVal);
  public boolean add(Object attrVal);
  public boolean remove(Object attrval);
  public void clear();
  public DirContext getAttributeSyntaxDefinition() throws NamingException;
  public DirContext getAttributeDefinition() throws NamingException;
  public Object clone();
  public boolean isOrdered();
  public Object get(int ix) throws NamingException;
  public Object remove(int ix);
  public void add(int ix, Object attrVal);
  public Object set(int ix, Object attrVal);
}
