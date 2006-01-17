/* Element.java -- Base class for object tree elements.
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.java.beans.encoder.elements;

import java.util.Iterator;
import java.util.LinkedList;

import gnu.java.beans.encoder.ObjectId;
import gnu.java.beans.encoder.Writer;

/** <code>Element</code> is the base class for the object tree elements.
 * 
 * <p>It provides the neccessary infrastructure every element subclass
 * needs in order to interact with the {@link gnu.java.beans.encoder.Root}
 * class.</p>
 * 
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public abstract class Element
{
  /**
   * Stores the child elements.
   */
  private LinkedList children = new LinkedList();
  
  /**
   * An optional ObjectId instance which is needed for certain subclasses
   * only.
   */
  private ObjectId objectId;
  
  /** Sets an {@link gnu.java.beans.encoder.ObjectId} instance in this
   * <code>Element</code>.
   * 
   * <p>This can only be done once.</p>
   * 
   * @param objectId An ObjectId instance.
   */
  public final void initId(ObjectId objectId)
  {
    assert (this.objectId == null);
    assert (objectId != null);
    
    this.objectId = objectId;
  }

  /** Adds a child element to this <code>Element</code>.
   * 
   * @param elem The new child.
   */
  public final void addChild(Element elem)
  {
    children.add(elem);
  }
  
  /** Removes the child element added last.
   */
  public final void removeLast()
  {
    children.removeLast();
  }
  
  /** Provides access to the child elements via an iterator.
   * 
   * @return An iterator for the child elements.
   */
  public final Iterator iterator(){
    return children.iterator();
  }
  
  /** Clears all the stored child elements.
   * 
   */
  public final void clear()
  {
   children.clear(); 
  }
  
  /** Returns whether this element contains child elements.
   * 
   * <p>This method is useful to decide which formatting variant
   * for the XML element can be chosen.</p>
   * 
   * @return Whether the element has child elements.
   */
  public final boolean isEmpty()
  {
    return children.isEmpty(); 
  }
  
  /** Retrieves the element's {@link gnu.java.beans.encoder.ObjectId} instance
   * if it has one.
   * 
   * @return The ObjectId instance or <code>null</code>.
   */
  public final ObjectId getId()
  {
    return objectId;
  }
  
  /** Writes the opening XML tag.
   * 
   * @param writer The writer to be used for XML writing.
   */
  public abstract void writeStart(Writer writer);
  
  /** Writes the closing XML tag.
   * 
   * <p>By default this does <code>writer.writeEnd(children.isEmpty())</code>.
   * Override if neccessary, for example when using the
   * {@link gnu.java.beans.encoder.Writer#writeNoChildren}</code> method
   * variants. 
   * 
   * @param writer The writer to be used for XML writing.
   */
  public void writeEnd(Writer writer)
  {
    writer.writeEnd(children.isEmpty());
  }
  
}
