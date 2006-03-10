/* Template.java -- 
   Copyright (C) 2004,2006 Free Software Foundation, Inc.

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

package gnu.xml.transform;

import java.io.PrintStream;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.NameTest;
import gnu.xml.xpath.NodeTypeTest;
import gnu.xml.xpath.Pattern;
import gnu.xml.xpath.Selector;
import gnu.xml.xpath.Test;

/**
 * A template in an XSL stylesheet.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class Template
  implements Comparable
{

  static final double DEFAULT_PRIORITY = 0.5d;

  final Stylesheet stylesheet;
  final QName name;
  final Pattern match;
  final TemplateNode node;
  final double priority;
  final int precedence;
  final QName mode;
  final boolean isAnyNode; // is the match simply "node()"?

  Template(Stylesheet stylesheet, 
           QName name, Pattern match, TemplateNode node,
           int precedence, String priorityAttr, QName mode)
  {
    this.stylesheet = stylesheet;
    this.name = name;
    this.match = match;
    this.node = node;
    this.precedence = precedence;
    this.mode = mode;
    
    double p = DEFAULT_PRIORITY;
    boolean a = false;
    if (priorityAttr != null)
      p = Double.parseDouble(priorityAttr);
    else
      {
        // adjust priority if necessary
        // see XSLT section 5.5
        if (match instanceof Selector)
          {
            Selector selector = (Selector) match;
            Test[] tests = selector.getTests();
            if (tests.length > 0)
              {
                Test test = tests[0];
                if (test instanceof NameTest)
                  {
                    NameTest nameTest = (NameTest) test;
                    if (nameTest.matchesAny())
                      p = -0.25d;
                    else if (nameTest.matchesAnyLocalName())
                      p = -0.20d;
                    else
                      p = 0.0d;
                  }
                else
                  {
                    NodeTypeTest nodeTypeTest = (NodeTypeTest) test;
                    if (nodeTypeTest.getNodeType() ==
                        Node.PROCESSING_INSTRUCTION_NODE &&
                        nodeTypeTest.getData() != null)
                      p = 0.0d;
                    else
                      p = -0.5d;
                    a = (nodeTypeTest.getNodeType() == 0);
                  }
                // Add a small difference for predicates
                if (tests.length > 1)
                  p += ((double) tests.length - 1) * 0.001;
              }
          }
      }
    this.priority = p;
    this.isAnyNode = a;
  }
  
  private Template(Stylesheet stylesheet, 
           QName name, Pattern match, TemplateNode node,
           int precedence, double priority, QName mode, boolean isAnyNode)
  {
    this.stylesheet = stylesheet;
    this.name = name;
    this.match = match;
    this.node = node;
    this.precedence = precedence;
    this.priority = priority;
    this.mode = mode;
    this.isAnyNode = isAnyNode;
  }

  Template clone(Stylesheet stylesheet)
  {
    // FIXME by cloning we lose the imports() functionality, so
    // apply-imports will be broken.
    return new Template(stylesheet,
                        name,
                        (match == null) ? null :
                        (Pattern) match.clone(stylesheet),
                        (node == null) ? null : node.clone(stylesheet),
                        precedence,
                        priority,
                        mode,
                        isAnyNode);
  }
  
  public int compareTo(Object other)
  {
    if (other instanceof Template)
      {
        Template t = (Template) other;
        int d = t.precedence - precedence;
        if (d != 0)
          return d;
        double d2 = t.priority - priority;
        if (d2 != 0.0d)
          return (int) Math.round(d2 * 1000.0d);
      }
    return 0;
  }

  Test getNodeTest(Expr expr)
  {
    return null;
  }

  boolean matches(QName mode, Node node)
  {
    if ((mode == null && this.mode != null) ||
        (mode != null && !mode.equals(this.mode)))
      return false;
    if (match == null)
      return false;
    if (isAnyNode && node.getNodeType() == Node.DOCUMENT_NODE)
      return false; // don't match document node
    return match.matches(node);
  }

  boolean matches(QName name)
  {
    return name.equals(this.name);
  }

  boolean imports(Template other)
  {
    for (Stylesheet ctx = other.stylesheet.parent;
         ctx != null;
         ctx = ctx.parent)
      {
        if (ctx == stylesheet)
          return true;
      }
    return false;
  }

  /**
   * @param stylesheet the stylesheet
   * @param parent the parent of result nodes
   * @param context the context node in the source document
   * @param pos the context position
   * @param len the context size
   * @param nextSibling if non-null, add result nodes before this node
   */
  void apply(Stylesheet stylesheet, QName mode,
             Node context, int pos, int len,
             Node parent, Node nextSibling)
    throws TransformerException
  {
    if (stylesheet.debug)
      System.err.println("...applying " + toString() + " to " + context);
    if (node != null)
      node.apply(stylesheet, mode,
                 context, pos, len,
                 parent, nextSibling);
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(getClass().getName());
    buf.append('[');
    if (name != null)
      {
        buf.append("name=");
        buf.append(name);
      }
    else if (match != null)
      {
        buf.append("match=");
        buf.append(match);
      }
    if (mode != null)
      {
        buf.append(",mode=");
        buf.append(mode);
      }
    buf.append(']');
    return buf.toString();
    
    //return (name != null) ? name.toString() : match.toString();
  }

  void list(PrintStream out)
  {
    out.println(toString());
    if (node != null)
      node.list(1, out, true);
  }

}
