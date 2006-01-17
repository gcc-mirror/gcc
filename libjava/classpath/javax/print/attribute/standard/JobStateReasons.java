/* JobStateReasons.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import javax.print.attribute.PrintJobAttribute;

/**
 * The <code>JobStateReasons</code> attribute provides the set of 
 * additional informations available about the current state of a print job. 
 * <p>
 * <b>IPP Compatibility:</b> JobStateReasons is an IPP 1.1 attribute.
 * </p>
 * @see javax.print.attribute.standard.JobState
 * @see javax.print.attribute.standard.JobStateReason
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class JobStateReasons extends HashSet
  implements PrintJobAttribute
{
  private static final long serialVersionUID = 8849088261264331812L;

  /**
   * Constructs an empty <code>JobStateReasons</code> attribute.
   */  
  public JobStateReasons()
  {
    super();
  }

  /**
   * Constructs an empty <code>JobStateReasons</code> attribute
   * with the given initial capacity and load factor.
   * 
   * @param initialCapacity the intial capacity.
   * @param loadFactor the load factor of the underlying HashSet.
   * 
   * @throws IllegalArgumentException if initialCapacity &lt; 0
   * @throws IllegalArgumentException if initialCapacity or loadFactor &lt; 0
   */
  public JobStateReasons(int initialCapacity, float loadFactor)
  {
    super(initialCapacity, loadFactor);
  }

  /**
   * Constructs an empty <code>JobStateReasons</code> attribute
   * with the given initial capacity and the default load factor.
   * 
   * @param initialCapacity the intial capacity.
   * 
   * @throws IllegalArgumentException if initialCapacity &lt; 0
   */
  public JobStateReasons(int initialCapacity)
  {
    super(initialCapacity);
  }

  /**
   * Constructs a <code>JobStateReasons</code> attribute
   * with the content of the given collection.
   * 
   * @param collection the collection for the initial values.
   * 
   * @throws NullPointerException if collection or any value is 
   * <code>null</code>.
   * @throws ClassCastException if values of collection are not of type 
   * <code>JobStateReason</code>.
   */
  public JobStateReasons(Collection collection)
  {
    super(collection.size(), 0.75f);
    Iterator it = collection.iterator();
    while (it.hasNext())
      add(it.next());
  }

  /**
   * Adds the given job state reason object to the set.
   * 
   * @param o the reason of type <code>JobStateReason</code>.
   * @return <code>true</code> if set changed, <code>false</code> otherwise.
   * 
   * @throws NullPointerException if given object is <code>null</code>.
   * @throws ClassCastException if given object is not an instance of
   * <code>JobStateReason</code>.
   */
  public boolean add(Object o)
  {
    if (o == null)
      throw new NullPointerException("reason is null");  
    
    return add((JobStateReason) o);
  }
  
  /**
   * Returns category of this class.
   *
   * @return The class <code>JobStateReasons</code> itself.
   */
  public Class getCategory()
  {
    return JobStateReasons.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "job-state-reasons".
   */
  public String getName()
  {
    return "job-state-reasons";
  }
}
