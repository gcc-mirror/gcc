/* node.java --
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

/**
 * Part of the internal representation of the content model.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class node
  implements Serializable
{
  private static final long serialVersionUID = 1;

  /**
   * The token to match (can be instance of list).
   */
  public Object token;

  /**
   * True for the node that cannot be visited again.
   */
  public boolean _closed;

  /**
   * The binary operation for this node.
   */
  public char binary;

  /**
   * The unary opeation for this node.
   */
  public char unary;

  /**
   * The number of times the node already was visited.
   */
  public int visits;

  /**
   * The previous node in content model (used for closing nodes).
   */
  public node previous;

  /**
   * Creates a new node.
   * @param binary_operator The operator, connecting all nodes in the list.
   * The nodes, connected by the different operators, must be arranged into
   * the different lists.
   * @param unary_operator The unary operator for this node or zero if
   * no such was specified.
   * @param token The token to match. This can be either a string or
   * the new instance of the list.
   * @param a_previous The previous node in the list, null for the first
   * node. This is used for propagating the closing operation for the
   * comma delimited list.
   */
  public node(char binary_operator, char unary_operator, Object a_token)
  {
    if (a_token != null)
      if (a_token.getClass().equals(node.class))
        throw new Error("Creating node in node is redundant and ineffective.");

    binary = binary_operator;
    unary = unary_operator;
    token = a_token;
  }

  /**
   * Checks if this node is in the closed state.
   * @return True if the node is closed.
   */
  public boolean isClosed()
  {
    return _closed;
  }

  /**
   * Check if closing this node means closing the previous node.
   */
  public boolean closePrevious()
  {
    return binary == ',';
  }

  /**
   * Return the token object if it could match as a next token in
   * a token list of null if it could not.
   * @return
   */
  public Object findFreeNode()
  {
    boolean ok;
    if (isClosed() || silenceAllowed())
      return null;

    // Try if the node would stay valid after a one more visit.
    visits++;
    ok = valid();
    visits--;

    if (ok)
      {
        if (token instanceof node)
          return ((node) token).findFreeNode();
        else
          return token;
      }
    else
      return null;
  }

  /**
   * Check if the current situation is such that the node must be closed
   * now.
   */
  public boolean mustClose()
  {
    switch (unary)
      {
        case 0 :
          return true;

        case '*' :
          return false;

        case '+' :
          return false;

        case '?' :
          return visits <= 1;

        default :
          throw new Error("Invalid unary operation " + unary + " ( '" +
                          (char) unary + "' )"
                         );
      }
  }

  /**
   * Do the match operation with the given token. This sets various
   * flags.
   * @param token The token to match.
   * @return true if the the token matches node, false if it does not match
   * or if the node is closed.
   */
  public boolean performMatch(Object a_token)
  {
    if (isClosed())
      return false;

    boolean matches = compare(a_token);
    if (matches)
      matches();

    return matches;
  }

  /**
   * Prepares the node for matching against a new list of tokens.
   */
  public void reset()
  {
    _closed = false;
    visits = 0;
  }

  /**
   * Check if the provided token can match this node.
   * In the case of match, the node state changes, moving
   * current position after the matched token. However if this method
   * returns a suggested new token to insert before the provided one,
   * the state of the list does not change.
   * @return Boolean.TRUE if the match is found,
   * Boolean.FALSE if the match is not possible and no token can be
   * inserted to make the match valid. Otherwise, returns the
   * token object that can be inserted before the last token in the
   * list, probably (not for sure) making the match valid.
   */
  public Object show(Object x)
  {
    if (compare(x))
      return performMatch(x) ? Boolean.TRUE : Boolean.FALSE;

    Object recommended = findFreeNode();
    return recommended != null ? recommended : Boolean.FALSE;
  }

  /**
   * Check if it would be a valid case if this node is visited zero times.
   * Nodes with unary operator * or ? need not be matched to make a
   * model valid.
   */
  public boolean silenceAllowed()
  {
    return unary == '?' || unary == '*';
  }

  /**
   * Returns a string representation of the list.
   * @return String representation, similar to BNF expression.
   */
  public String toString()
  {
    StringBuffer b = new StringBuffer();

    b.append(token);
    if (unary != 0)
      b.append((char) unary);
    else
      b.append('\'');

    return b.toString();
  }

  /**
   * Check if the node state is valid.
   */
  public boolean valid()
  {
    switch (unary)
      {
        case 0 :
          if (binary == '|')
            return true;
          else
            return visits == 1;

        case '*' :
          return true;

        case '+' :
          return visits > 0;

        case '?' :
          return visits <= 1;

        default :
          throw new Error("Invalid unary operation " + unary + " ( '" +
                          (char) unary + "' )"
                         );
      }
  }

  public boolean validPreliminary()
  {
    return visits == 0 || valid();
  }

  /**
  * Closes this node and, if closePrevious() returs true, calls close() for
  * the previous node.
  */
  protected void close()
  {
    _closed = true;
    if (previous != null && closePrevious())
      previous.close();
  }

  /**
   * Compare the provided token object with the token object of this node.
   */
  protected boolean compare(Object a_token)
  {
    if (token instanceof Object[])
      throw new Error("Invalid token object, probably the 'list' " +
                      "should be used. "
                     );

    if (token instanceof node[])
      throw new Error("Do not use 'node' for the array of nodes, use 'list'. ");

    if (token instanceof node)
      {
        return ((node) token).performMatch(a_token);
      }

    boolean rt = false;

    if (token == a_token)
      rt = true;
    if (token.equals(a_token))
      rt = true;
    if (token.toString().equalsIgnoreCase(a_token.toString()))
      rt = true;

    return rt;
  }

  /**
   * Fire the changes that must happen then the token matches this node.
   */
  protected void matches()
  {
    visits++;
    if (mustClose())
      close();
  }
}
