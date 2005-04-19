/* transformer.java -- Content model transforms.
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


package gnu.javax.swing.text.html.parser.models;

import java.io.Serializable;

import javax.swing.text.html.parser.ContentModel;
import javax.swing.text.html.parser.DTD;

/**
 * Transforms the standard ContentModel tree into the internal representation,
 * used in this implementation.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class transformer
  implements Serializable
{
  private static final long serialVersionUID = 1;

  /**
   * All binary operators.
   */
  protected static String binary = "&|,";

  /**
   * All unary operators.
   */
  protected static String unary = "+*?";

  /**
   * Measure length of the linked list of the content models.
   * @param c The heading element of the linked list.
   * @return the length of the list (0 for null 1 if c!=null and c.next==null,
   * etc.
   */
  public static int measureChainLength(ContentModel c)
  {
    if (c == null)
      return 0;
    else
      return measureChainLength(c.next) + 1;
  }

  /**
   * Transform into internal representation without usind dtd.
   * This should be used only for testing.
   */
  public static node transform(ContentModel c)
  {
    return transform(c, null);
  }

  /**
   * Transform into internal representation.
   * @param c a model to transform
   * @return a transformed model
   * @throws Error if the model structure contains errors.
   */
  public static node transform(ContentModel c, DTD dtd)
  {
    // Handle the special cases first.
    if (c.content instanceof node)
      return (node) c.content;

    // Do the typical transform.
    node n;

    /* Case with the single token */
    if (c.next == null)
      {
        n = optionalTransform(c, dtd);
      }
    else /* Case with the chain of the multiple tokens. */
      {
        node[] l = new node[ measureChainLength(c) ];
        ContentModel m = c;
        for (int i = 0; i < l.length; i++)
          {
            if (m.content instanceof ContentModel)
              {
                ContentModel nested = (ContentModel) m.content;
                if (nested.next == null &&
                    !(nested.content instanceof ContentModel)
                   )
                  {
                    l [ i ] =
                      new node((char) m.type, (char) nested.type, nested.content);
                  }
                else
                  {
                    l [ i ] = transform(nested, dtd);
                  }
              }
            else
              l [ i ] = new node((char) 0, (char) 0, m.content);
            addtype(l [ i ], (char) m.type);
            m = m.next;
          }

        if (isBinary(c.type))
          for (int i = 0; i < l.length; i++)
            {
              l [ i ].binary = (char) c.type;
            }

        n = new list(l);
      }

    addtype(n, (char) c.type);

    return n;
  }

  /**
   * True for binary operator
   * @param c a character to test
   * @return true for [ ,&| ], false otherwise.
   */
  private static boolean isBinary(int c)
  {
    return binary.indexOf((char) c) >= 0;
  }

  /**
   * True for unary operator.
   * @param c a character to test
   * @return true for [ +?* ], false otherwise.
   */
  private static boolean isUnary(int c)
  {
    return unary.indexOf((char) c) >= 0;
  }

  /**
   * Assign an operation type for the given node.
   * @param n A node to set the operation to.
   * @param type Either binary or unary operation, is assigned to the
   * corresponding field of the node.
   * @throws error if the operation type is not
   * representing a valid unary or binary operation.
   */
  private static void addtype(node n, char type)
  {
    if (isBinary(type))
      n.binary = type;

    else if (isUnary(type))
      n.unary = type;

    else if (type != 0)
      throw new Error("Invalid operation '" + (char) type + "'");
  }

  private static node optionalTransform(ContentModel c, DTD dtd)
  {
    node n;
    if (c.content instanceof ContentModel)
      n = transform((ContentModel) c.content, dtd);
    else

      /* A single token with the specified operation */
      n = new node((char) 0, (char) 0, c.content);
    return n;
  }
}
