/* Steps.java -- 
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.xml.xpath;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Set;
import org.w3c.dom.Attr;
import org.w3c.dom.Node;

/**
 * A list of transitions between components in a location path.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class Steps
  extends Path
{

  final LinkedList path;

  public Steps()
  {
    this(new LinkedList());
  }

  Steps(LinkedList path)
  {
    this.path = path;
  }

  public boolean matches(Node context)
  {
    // Right to left
    return matches(context, path.size() - 1);
  }

  boolean matches(Node context, int pos)
  {
    Pattern right = (Pattern) path.get(pos);
    if (!right.matches(context))
      {
        return false;
      }
    if (pos > 0)
      {
        Pattern left = (Pattern) path.get(pos - 1);
        Iterator j = possibleContexts(right, context).iterator();
        while (j.hasNext())
          {
            Node candidate = (Node) j.next();
            if (left.matches(candidate) &&
                matches(candidate, pos - 1))
              {
                return true;
              }
            // keep going, there may be another candidate
          }
        return false;
      }
    return true;
  }

  /**
   * Essentially the reverse of Selector.addCandidates.
   * The idea is to determine possible context nodes for a match.
   */
  Collection possibleContexts(Pattern pattern, Node context)
  {
    if (pattern instanceof Selector)
      {
        Selector s = (Selector) pattern;
        Collection candidates = new LinkedHashSet();
        switch (s.axis)
          {
          case Selector.PARENT:
            s.addChildNodes(context, candidates, false);
            break;
          case Selector.ANCESTOR:
            s.addChildNodes(context, candidates, true);
            break;
          case Selector.ANCESTOR_OR_SELF:
            candidates.add (context);
            s.addChildNodes(context, candidates, true);
            break;
          case Selector.CHILD:
            s.addParentNode(context, candidates, false);
            break;
          case Selector.DESCENDANT:
            s.addParentNode(context, candidates, true);
            break;
          case Selector.DESCENDANT_OR_SELF:
            candidates.add(context);
            s.addParentNode(context, candidates, true);
            break;
          case Selector.PRECEDING_SIBLING:
            s.addFollowingNodes(context, candidates, false);
            break;
          case Selector.FOLLOWING_SIBLING:
            s.addPrecedingNodes(context, candidates, false);
            break;
          case Selector.PRECEDING:
            s.addFollowingNodes(context, candidates, true);
            break;
          case Selector.FOLLOWING:
            s.addPrecedingNodes(context, candidates, true);
            break;
          case Selector.ATTRIBUTE:
          case Selector.NAMESPACE:
            if (context.getNodeType() == Node.ATTRIBUTE_NODE)
              {
                candidates.add(((Attr) context).getOwnerElement());
              }
            break;
          case Selector.SELF:
            candidates.add(context);
            break;
          }
        return candidates;
      }
    return Collections.EMPTY_SET;
  }

  public Object evaluate(Node context, int pos, int len)
  {
    //System.err.println(toString()+" evaluate");
    // Left to right
    Iterator i = path.iterator();
    Expr lhs = (Expr) i.next();
    Object val = lhs.evaluate(context, pos, len);
    //System.err.println("\tevaluate "+lhs+" = "+val);
    while (val instanceof Collection && i.hasNext())
      {
        Path rhs = (Path) i.next();
        val = rhs.evaluate(context, (Collection) val);
        //System.err.println("\tevaluate "+rhs+" = "+val);
      }
    return val;
  }

  Collection evaluate(Node context, Collection ns)
  {
    // Left to right
    Iterator i = path.iterator();
    Expr lhs = (Expr) i.next();
    if (lhs instanceof Path)
      {
        ns = ((Path) lhs).evaluate(context, ns);
      }
    else
      {
        Set acc = new LinkedHashSet();
        int pos = 1, len = ns.size();
        for (Iterator j = ns.iterator(); j.hasNext(); )
          {
            Node node = (Node) j.next();
            Object ret = lhs.evaluate(node, pos++, len);
            if (ret instanceof Collection)
              {
                acc.addAll((Collection) ret);
              }
          }
        ns = acc;
      }
    while (i.hasNext())
      {
        Path rhs = (Path) i.next();
        ns = rhs.evaluate(context, ns);
      }
    return ns;
  }

  public Expr clone(Object context)
  {
    int len = path.size();
    LinkedList path2 = new LinkedList();
    for (int i = 0; i < len; i++)
      {
        path2.add(((Expr) path.get(i)).clone(context));
      }
    return new Steps(path2);
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer();
    Iterator i = path.iterator();
    Expr expr = (Expr) i.next();
    if (!(expr instanceof Root))
      {
        buf.append(expr);
      }
    while (i.hasNext())
      {
        expr = (Expr) i.next();
        buf.append('/');
        buf.append(expr);
      }
    return buf.toString();
  }

}
