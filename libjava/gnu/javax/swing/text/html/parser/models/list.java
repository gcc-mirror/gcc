/* list.java --
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


package gnu.javax.swing.text.html.parser.models;

import java.io.Serializable;

/**
 * Part of the internal representation of the content model.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class list
  extends node
  implements Serializable
{
  private static final long serialVersionUID = 1;

  /**
   * Setting to true means that the list nodes must always be connected
   * by the same operation. This is far safer and clearer, but not
   * required by default standard.
   */
  public static boolean CLEAR;

  /**
   * A list of nodes.
   */
  public final node[] nodes;

  /**
   * Creates a new model list that is a member of some enclosing list.
   * @param binary_operator An operator with that this list is connected
   * with other members of the enclosing list.
   * @param unary_operator The unary operator for this list.
   * @param a_nodes The nodes inside this list.
   */
  public list(char binary_operator, char unary_operator, node[] a_nodes)
  {
    super(binary_operator, unary_operator, a_nodes);
    nodes = a_nodes;
  }

  /**
   * Creates a new model list. Assigns the previous field.
   * @param a_nodes The nodes for this list.
   * @throws an error if the node elements are connected by the
   * different operations. This is not supported, use grouping.
   */
  public list(node[] a_nodes)
       throws Error
  {
    this(',', (char) 0, a_nodes);

    int operation = nodes [ 0 ].binary;

    for (int i = 0; i < nodes.length; i++)
      {
        if (CLEAR && nodes [ i ].binary != operation)
          throw new Error("List members can only be connected by " +
                          "the same operation, use grouping"
                         );

        if (i > 0)
          nodes [ i ].previous = nodes [ i - 1 ];
      }
  }

  /**
   * Returns true if all members in the list are closed.
   */
  public boolean isClosed()
  {
    if (super.isClosed())
      return true;
    for (int i = 0; i < nodes.length; i++)
      {
        if (!nodes [ i ].isClosed())
          return false;
      }
    return true;
  }

  /**
   * Find the token that could match as the next token in
   * the token list.
   *
   * @return Such token object or null if none is found.
   */
  public Object findFreeNode()
  {
    Object fn;
    for (int j = 0; j < nodes.length; j++)
      {
        if (!nodes [ j ].isClosed())
          {
            fn = nodes [ j ].findFreeNode();
            if (fn != null)
              return fn;
          }
      }
    return null;
  }

  /**
   * Tries to match this list agains the given token sequence.
   * @param tokens the sequence of the tokens to match.
   * @return true if the valid match is found.
   */
  public boolean matches(Object[] tokens)
  {
    reset();

    Object x;
    boolean m;
    boolean matched = false;

    for (int i = 0; i < tokens.length; i++)
      {
        matched = false;
        x = tokens [ i ];

        nodescan: 
        for (int j = 0; j < nodes.length; j++)
          {
            if (!nodes [ j ].isClosed())
              {
                m = nodes [ j ].performMatch(x);

                if (m)
                  {
                    matched = true;
                    break nodescan;
                  }
              }
          }
        if (!matched)
          return false;
      }

    boolean valid = true;

    for (int i = 0; i < nodes.length; i++)
      {
        if (!nodes [ i ].valid())
          valid = false;
      }

    return valid;
  }

  /**
   * The list never closes, despite it is trated as closed
   * if all members in the list are closed.
   * @return false.
   */
  public boolean mustClose()
  {
    return false;
  }

  /**
   * Perform a match operation for the single token
   * against this list.
   * @param token a token to match.
   * @return true if the match is found.
   */
  public boolean performMatch(Object token)
  {
    boolean ok = false;
    Matching: 
    for (int i = 0; i < nodes.length; i++)
      {
        ok = nodes [ i ].performMatch(token);

        if (ok)
          break Matching;
      }

    if (ok)
      matches();

    return ok;
  }

  /**
   * Prepeares the list for the next matching operation.
   */
  public void reset()
  {
    super.reset();
    for (int i = 0; i < nodes.length; i++)
      nodes [ i ].reset();
  }

  /**
   * Check if the provided token can match as a next token in the
   * list. In the case of match, the list state changes, moving
   * current position after the matched token. However if this method
   * returns a suggested new token to insert before the provided one,
   * the state of the list does not change.
   * @return Boolean.TRUE if the match is found,
   * Boolean.FALSE if the match is not possible and no token can be
   * inserted to make the match valid. Otherwise, returns the
   * token object that can be inserted before the last token in the
   * list, probably (not for sure) making the match valid.
   * If the object is an instance of Element or TagElement,
   * it is first ensured that the object flag "omit start" is set.
   */
  public Object show(Object x)
  {
    boolean m;
    boolean matched = false;

    nodescan: 
    for (int j = 0; j < nodes.length; j++)
      {
        if (!nodes [ j ].isClosed())
          {
            m = nodes [ j ].performMatch(x);

            if (m)
              {
                matched = true;
                break nodescan;
              }
            else
              {
                // For comma operation, only first not closed
                // node must be tested for a match.
                // unless it allows matching zero times.
                if (binary == ',' &&
                    !(nodes [ j ].unary == '?' || nodes [ j ].unary == '*')
                   )
                  break nodescan;
              }
          }
      }

    if (!matched)
      {
        // Find and return that would be matched.
        Object freeNode = findFreeNode();
        if (freeNode == null)
          return Boolean.FALSE;
        else
          return freeNode;
      }

    for (int i = 0; i < nodes.length; i++)
      if (!nodes [ i ].validPreliminary())
        {
          return Boolean.FALSE;
        }

    return Boolean.TRUE;
  }

  /**
   * Returns a string representation of the list.
   * @return String representation, similar to BNF expression.
   */
  public String toString()
  {
    StringBuffer b = new StringBuffer();
    b.append(" ( ");
    for (int i = 0; i < nodes.length; i++)
      {
        if (i > 0)
          b.append(" " + (char) nodes [ i ].binary + " ");
        b.append(nodes [ i ]);
      }

    b.append(" )");
    if (unary != 0)
      b.append((char) unary);
    else
      b.append(' ');
    return b.toString();
  }

  /**
   * Returns true if all memebers in the list are valid.
   */
  public boolean valid()
  {
    for (int i = 0; i < nodes.length; i++)
      {
        if (!nodes [ i ].valid())
          return false;
      }
    return true;
  }

  /**
   * Returns true if all memebers in the list are either valid
   * or unvisited. The unvisited members can become valid after
   * more tokens will be shown.
   */
  public boolean validPreliminary()
  {
    if (silenceAllowed())
      {
        boolean everVisited = false;
        for (int i = 0; i < nodes.length; i++)
          {
            if (nodes [ i ].visits > 0)
              {
                everVisited = true;
                break;
              }
          }
        if (!everVisited)
          return true;
      }

    for (int i = 0; i < nodes.length; i++)
      {
        if (!nodes [ i ].validPreliminary())
          return false;
      }
    return true;
  }

  /**
   * Closes all members in the list.
   */
  protected void close()
  {
    super.close();
    for (int i = 0; i < nodes.length; i++)
      {
        nodes [ i ].close();
      }
  }

  /**
   * Compare given token with the token of this node.
   * If the token represents a <code>list</code>, the call may be
   * delegeted to the child subnodes.
   * @param a_token A token to compare.
   * @return True if the token matches the token of this node.
   */
  protected boolean compare(Object a_token)
  {
    return performMatch(a_token);
  }
}
