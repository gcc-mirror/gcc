/* ContentModel.java --
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


package javax.swing.text.html.parser;

import gnu.javax.swing.text.html.parser.models.transformer;

import java.io.Serializable;

import java.util.Vector;

/**
 * A representation of the element content. The instances of this class
 * can be arranged into the linked list, representing a BNF expression.
 * The content model is constructed as a branched tree structure in the
 * following way:
 * <pre>
 * a = new ContentModel('+', A, null); // a reprensents A+
 * b = new ContentModel('&amp;', B, a); // b represents B &amp; A+
 * c = new ContentModel('*', b, null); // c represents ( B &amp; A+) *
 * d = new ContentModel('|', new ContentModel('*', A, null),
 *          new ContentModel('?', B, null)); // d represents ( A* | B? )
 * </pre>
 * where the valid operations are:
 * <ul>
 * <li><code>E* </code> E occurs zero or more times</li>
 * <li><code>E+ </code> E occurs one or more times</li>
 * <li><code>E? </code> E occurs once or not atl all</li>
 * <li><code>A,B</code> A occurs before B</li>
 * <li><code>A|B</code> both A and B are permitted in any order.
 * The '|' alone does not permit the repetetive occurence of A or B
 * (use <code>(A|B)*</code>.</li>
 * <li><code>A&amp;B</code> both A and B must occur once (in any order)</li>
 * </ul>
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class ContentModel
  implements Serializable
{
  /** Use serialVersionUID for interoperability. */
  private static final long serialVersionUID = -1130825523866321257L;

  /**
   * The next content model model ( = pointer to the next element of
   * the linked list) for the binary expression (',','&amp;' or '|'). Null
   * for the last element in the list.
   */
  public ContentModel next;

  /**
   * The document content, containing either Element or the enclosed
   * content model (that would be in the parentheses in BNF expression).
   */
  public Object content;

  /**
   * Specifies the BNF operation between this node and the node,
   * stored in the field <code>next</code> (or for this node, if it is
   * an unary operation.
   */
  public int type;

  /**
   * Create a content model initializing all fields to default values.
   */
  public ContentModel()
  {
    // Nothing to do here.
  }

  /**
   * Create a content model, consisting of the single element.
   * Examples:
   *<code>
   * a = new ContentModel('+', A, null); // a reprensents A+
   * b = new ContentModel('&amp;', B, a);    // b represents  B &amp; A+
   * c = new ContentModel('*', b, null); // c represents  ( B &amp; A+) *
   * d = new ContentModel('|', A,
   *    new ContentModel('?',b, null);
   *     // d represents
   * </code>
   */
  public ContentModel(Element a_content)
  {
    content = a_content;
  }

  /**
   * Create a content model, involving expression of the given type.
   * @param a_type The expression operation type ('*','?' or '+'
   * @param a_content The content for that the expression is applied.
   */
  public ContentModel(int a_type, ContentModel a_content)
  {
    content = a_content;
    type = a_type;
  }

  /**
   * Create a content model, involving binary expression of the given type.
   * @param a_type The expression operation type ( ',', '|' or '&amp;').
   * @param a_content The content of the left part of the expression.
   * @param a_next The content model, representing the right part of the
   * expression.
   */
  public ContentModel(int a_type, Object a_content, ContentModel a_next)
  {
    content = a_content;
    type = a_type;
    next = a_next;
  }

  /**
   * Adds all list elements to the given vector, ignoring the
   * operations between the elements. The old vector values are not
   * discarded.
   * @param elements - a vector to add the values to.
   */
  public void getElements(Vector<Element> elements)
  {
    ContentModel c = this;

    while (c != null)
      {
        // FIXME: correct?
        if (c.content instanceof Element)
          elements.add((Element) c.content);
        c = c.next;
      }
  }

  /**
   * Checks if the content model matches an empty input stream.
   * The empty content is created using SGML DTD keyword EMPTY.
   * The empty model is a model with the content field equal to null.
   *
   * @return true if the content field is equal to null.
   */
  public boolean empty()
  {
    return content == null;
  }

  /**
   * Get the element, stored in the <code>next.content</code>.
   * The method is programmed as the part of the standard API, but not
   * used in this implementation.
   * @return the value of the field <code>next</code>.
   */
  public Element first()
  {
    return (Element) next.content;
  }

  /**
   * Checks if this object can potentially be the first token in the
   * ContenModel list. The method is programmed as the part of the
   *  standard API, but not used in this implementation.
   */
  public boolean first(Object token)
  {
    ContentModel c = this;
    while (c.next != null)
      {
        if (c.content != null && c.content.toString().equals(token.toString()) &&
            c.type != ','
           )

          // Agree if the operation with the preceeding element
          // is not the comma operation.
          return true;
        c = c.next;
      }
    return false;
  }

  /**
   * Returns a string representation (an expression) of this content model.
   * The expression has BNF-like syntax, except the absence of the
   * unary operator is additionally indicated by " ' ". It is
   * advisable to check the created models for correctness using this
   * method.
   */
  public String toString()
  {
    return transformer.transform(this).toString();
  }
}
