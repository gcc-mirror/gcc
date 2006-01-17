/* Root.java -- The root of an object tree.
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


package gnu.java.beans.encoder;

import java.beans.XMLEncoder;
import java.util.Iterator;
import java.util.Stack;

import gnu.java.beans.encoder.elements.Element;

/** <p><code>Root</code> provides a simple interface to a tree of
 * objects.</p>
 * 
 * <p>Using an instance of this class a logical representation of
 * the real object tree that is serialized can be built. When the
 * actual data should be written as XML <code>Root</code> and
 * {@link gnu.java.beans.encoder.elements.Element} class can provide
 * context information which is used to write the best fitting
 * XML representation.</p>
 *   
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class Root
{
  private Stack parents = new Stack();

  private Element rootElement, current;
  
  private boolean started;

  public Root()
  {
    rootElement = current = new RootElement();
  }

  /** <p>Adds another child element to the tree.</p>
   * 
   * <p>The new element automatically becomes the current
   * element.</p>
   * 
   * @param elem The new child element.
   */
  public void addChild(Element elem)
  {
    current.addChild(elem);

    parents.push(current);
    current = elem;
  }

  /**
   * <p>Marks that the end of the current element
   * is reached and that no more childs are added to
   * it.</p>
   * 
   * <p>The behavior is to return to the nearest parent
   * element.</p>
   */
  public void end()
  {
    current = (Element) parents.pop();
  }

  /**
   * <p>Goes back to the nearest parent element but
   * deletes the just created child.</p>
   * 
   * <p>This is used if something went wrong while
   * processing the child element's {@link java.beans.Expression}
   * or {@link java.beans.Statement}.</p>
   *
   */
  public void deleteLast()
  {
    current = (Element) parents.pop();

    current.removeLast();
  }

  /**
   * <p>Traverses the elements in the object tree
   * and creates their XML representation in the output
   * stream of the given {@link Writer}.</p>
   * 
   * <p>Finally the <code>Writer</code> is flushed.</p>
   *  
   * @param writer The Writer instance that generates the XML representation.
   */
  public void traverse(Writer writer)
  {
    if (!started)
      {
        writer.writePreamble();
        rootElement.writeStart(writer);
      }
    started = true;
    
    traverse(writer, rootElement.iterator());
    
    rootElement.clear();
    
    writer.flush();
  }

  /** Writes the closing element and closes the {@link Writer}
   * 
   * @param writer The Writer instance that generates the XML representation.
   */
  public void close(Writer writer)
  {
    rootElement.writeEnd(writer);
    writer.close();
  }

  /** Recursively traverses the object tree.
   * 
   * @param writer The Writer instance that generates the XML representation.
   * @param ite An Iterator returning Element instances.
   */
  private void traverse(Writer writer, Iterator ite)
  {
    while (ite.hasNext())
      {
        Element e = (Element) ite.next();
        e.writeStart(writer);

        traverse(writer, e.iterator());

        e.writeEnd(writer);
        
        e.clear();
      }
  }

  /** <p>A special Element implementation that represents the
   * encoder's context.</p>
   * 
   * <p>This element is written only once per Writer.</p>
   * 
   * <p>It is assumed that this element is never empty to simplify
   * the implementation.</p>
   * 
   * @author Robert Schuster (robertschuster@fsfe.org);
   *
   */
  static class RootElement extends Element
  {
    public void writeStart(Writer writer)
    {
      writer.write("java", new String[] { "version", "class" },
                   new String[] { System.getProperty("java.version"),
                                 XMLEncoder.class.getName() }, false);
    }
    
    public void writeEnd(Writer writer)
    {
      writer.writeEnd(false);
    }

  }

}
