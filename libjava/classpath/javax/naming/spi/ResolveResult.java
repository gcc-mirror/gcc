/* ResolveResult.java --
   Copyright (C) 2001, 2004, 2005, 2006  Free Software Foundation, Inc.

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


package javax.naming.spi;

import java.io.Serializable;

import javax.naming.CompositeName;
import javax.naming.InvalidNameException;
import javax.naming.Name;
 
/**
 * Stores the partial resolution of the name. This class contains the
 * object to which part of the name has been resolved and the remaining,
 * unresolved part of this name. 
 * 
 * @author Warren Levy (warrenl@redhat.com)
 */

public class ResolveResult implements Serializable
{
  private static final long serialVersionUID = - 4552108072002407559L;

  // Serialized fields.
  /**
   * The object, to that part of the name has been resolved.
   */
  protected Object resolvedObj;
  
  /**
   * The remaining, unresolved part of the name.
   */
  protected Name remainingName;
  
  /**
   * Create the unitialised instance with both parts being null.
   */
  protected ResolveResult()
  {
  }
  
  /**
   * Create the initialised instance
   * 
   * @param resolved the object, to that the name is partially resolved
   * @param remaining the remaining unresolved part of the name.
   */
  public ResolveResult(Object resolved, String remaining)
  {
    if (resolved == null || remaining == null)
      throw new IllegalArgumentException ();
    resolvedObj = resolved;
    remainingName = new CompositeName ();
    try
      {
	remainingName.add (remaining);
      }
    catch (InvalidNameException _)
      {
      }
  }

  /**
   * Create the initialised instance
   * 
   * @param resolved the object, to that the name is partially resolved
   * @param remaining the remaining unresolved part of the name.
   */
  public ResolveResult(Object resolved, Name remaining)
  {
    resolvedObj = resolved;
    remainingName = remaining;
  }

  /**
   * Get the remaining unresolved part of the name
   * 
   * @return the remaining unresolved part of the name.
   */
  public Name getRemainingName()
  {
    return remainingName;
  }

  /**
   * Get the object to that the name was partially resolved
   * 
   * @return the object, to that the name is partially resolved
   */
  public Object getResolvedObj()
  {
    return resolvedObj;
  }
  
  /**
   * Set the remaining unresolved name.
   * 
   * @param name the name being set. The passed parameter is cloned, so the
   *          caller can reuse or modify it after the method returns.
   */
  public void setRemainingName(Name name)
  {
    remainingName = (Name) name.clone();
  }
  
  /**
   * Append the name to the end of the resolved name.
   * 
   * @param name the name to append
   */
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

  /**
   * Append the name to the end of the resolved name.
   * 
   * @param name the name to append
   */
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

  /**
   * Set the object to that the part of the name has been resolved.
   * 
   * @param obj the object, to that the name has been partially resolved.
   */
  public void setResolvedObj(Object obj)
  {
    resolvedObj = obj;
  }
}
