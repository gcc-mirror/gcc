/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.spi;
import javax.naming.*;
import java.util.EventObject;
import java.io.Serializable;
 
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
