/* ResolveResult.java --
   Copyright (C) 2001 Free Software Foundation, Inc.

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


package javax.naming.spi;

import java.io.Serializable;
import java.util.EventObject;
import javax.naming.Name;
import javax.naming.CompositeName;
import javax.naming.InvalidNameException;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 5, 2001
 */

public class ResolveResult implements Serializable
{
  // Serialized fields.
  protected Object resolvedObj;
  protected Name remainingName;

  protected ResolveResult()
  {
    resolvedObj = null;
    remainingName = null;
  }

  public ResolveResult(Object robj, String rcomp)
  {
    if (robj == null || rcomp == null)
      throw new IllegalArgumentException ();
    resolvedObj = robj;
    remainingName = new CompositeName ();
    try
      {
	remainingName.add (rcomp);
      }
    catch (InvalidNameException _)
      {
      }
  }

  public ResolveResult(Object robj, Name rname)
  {
    resolvedObj = robj;
    remainingName = rname;
  }

  public Name getRemainingName()
  {
    return remainingName;
  }

  public Object getResolvedObj()
  {
    return resolvedObj;
  }

  public void setRemainingName(Name name)
  {
    remainingName = (Name) name.clone();
  }

  public void appendRemainingName(Name name)
  {
    try
      {
	remainingName.addAll(name);
      }
    catch (InvalidNameException _)
      {
      }
  }

  public void appendRemainingComponent(String name)
  {
    try
      {
	remainingName.add(name);
      }
    catch (InvalidNameException _)
      {
      }
  }

  public void setResolvedObj(Object obj)
  {
    resolvedObj = obj;
  }
}
