/* Expr.java -- 
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

package gnu.xml.xpath;

import gnu.java.lang.CPStringBuilder;

import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.StringTokenizer;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * An XPath expression.
 * This can be evaluated in the context of a node to produce a result.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class Expr
  implements XPathExpression
{

  protected static final Comparator<Node> documentOrderComparator =
    new DocumentOrderComparator();

  protected static final DecimalFormat decimalFormat =
    new DecimalFormat("####################################################" +
                      ".####################################################",
                      new DecimalFormatSymbols(Locale.US));

  static class ExprNodeSet implements NodeList
  {

    private ArrayList<Node> list;

    ExprNodeSet(Collection<Node> collection)
    {
      if (collection instanceof ArrayList)
        list = (ArrayList<Node>) collection;
      else
        list = new ArrayList<Node>(collection);
    }

    public int getLength()
    {
      return list.size();
    }

    public Node item(int index)
    {
      try
        {
          return list.get(index);
        }
      catch (ArrayIndexOutOfBoundsException e)
        {
          return null;
        }
    }
    
  }

  public Object evaluate(Object item, QName returnType)
    throws XPathExpressionException
  {
    Object ret = null;
    Node context = null;
    if (item instanceof Node)
      {
        context = (Node) item;
        ret = evaluate(context, 1, 1);
        if (XPathConstants.STRING == returnType &&
            !(ret instanceof String))
          {
            ret = _string(context, ret);
          }
        else if (XPathConstants.NUMBER == returnType &&
                 !(ret instanceof Double))
          {
            ret = new Double(_number(context, ret));
          }
        else if (XPathConstants.BOOLEAN == returnType &&
                 !(ret instanceof Boolean))
          {
            ret = _boolean(context, ret) ? Boolean.TRUE : Boolean.FALSE;
          }
        else if (XPathConstants.NODE == returnType)
          {
            if (ret instanceof Collection)
              {
		/* Suppression is safe, as we know context
		   produces Collection<Node> */
		@SuppressWarnings("unchecked") 
		  Collection<Node> ns = (Collection<Node>) ret;
                switch (ns.size())
                  {
                  case 0:
                    ret = null;
                    break;
                  case 1:
                    ret = ns.iterator().next();
                    break;
                  default:
                    throw new XPathExpressionException("multiple nodes in node-set");
                  }
              }
            else if (ret != null)
              {
                throw new XPathExpressionException("return value is not a node-set");
              }
          }
        else if (XPathConstants.NODESET == returnType)
          {
            if (ret != null && !(ret instanceof Collection))
              {
                throw new XPathExpressionException("return value is not a node-set");
              }
            if (ret != null)
	      {
		/* Suppression is safe, as we know context produces Collection<Node> */
		@SuppressWarnings("unchecked")
		  Collection<Node> nodes = (Collection<Node>) ret;
		ret = new ExprNodeSet(nodes);
	      }
          }
      }
    return ret;
  }

  public String evaluate(Object item)
    throws XPathExpressionException
  {
    return (String) evaluate(item, XPathConstants.STRING); 
  }

  public Object evaluate(InputSource source, QName returnType)
    throws XPathExpressionException
  {
    try
      {
        DocumentBuilderFactory factory =
          new gnu.xml.dom.JAXPFactory();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document doc = builder.parse(source);
        return evaluate(doc, returnType);
      }
    catch (ParserConfigurationException e)
      {
        throw new XPathExpressionException(e); 
      }
    catch (SAXException e)
      {
        throw new XPathExpressionException(e); 
      }
    catch (IOException e)
      {
        throw new XPathExpressionException(e); 
      }
  }

  public String evaluate(InputSource source)
    throws XPathExpressionException
  {
    return (String) evaluate(source, XPathConstants.STRING);
  }

  public abstract Object evaluate(Node context, int pos, int len);

  public abstract Expr clone(Object context);

  public abstract boolean references(QName var);
  
  /* -- 4.1 Node Set Functions -- */

  /**
   * The id function selects elements by their unique ID.
   * When the argument to id is of type node-set, then the result is
   * the union of the result of applying id to the string-value of each of
   * the nodes in the argument node-set. When the argument to id is of any
   * other type, the argument is converted to a string as if by a call to
   * the string function; the string is split into a whitespace-separated
   * list of tokens (whitespace is any sequence of characters matching the
   * production S); the result is a node-set containing the elements in the
   * same document as the context node that have a unique ID equal to any of
   * the tokens in the list.
   */
  public static Collection<Node> _id(Node context, Object object)
  {
    Set<Node> ret = new HashSet<Node>();
    if (object instanceof Collection)
      {
	/* Suppression is safe, as the iteration will check each value is a Node */
	@SuppressWarnings("unchecked")
	  Collection<Node> nodeSet = (Collection<Node>) object;
        for (Iterator<Node> i = nodeSet.iterator(); i.hasNext(); )
          {
            String string = stringValue(i.next());
            ret.addAll(_id (context, string));
          }
      }
    else
      {
        Document doc = (context instanceof Document) ? (Document) context :
          context.getOwnerDocument();
        String string = _string(context, object);
        StringTokenizer st = new StringTokenizer(string, " \t\r\n");
        while (st.hasMoreTokens())
          {
            Node element = doc.getElementById(st.nextToken());
            if (element != null)
              {
                ret.add(element);
              }
          }
      }
    return ret;
  }

  /**
   * The local-name function returns the local part of the expanded-name of
   * the node in the argument node-set that is first in document order. If
   * the argument node-set is empty or the first node has no expanded-name,
   * an empty string is returned. If the argument is omitted, it defaults to
   * a node-set with the context node as its only member.
   */
  public static String _local_name(Node context, Collection<Node> nodeSet)
  {
    if (nodeSet == null || nodeSet.isEmpty())
      return "";
    Node node = firstNode(nodeSet);
    String ret = node.getLocalName();
    return (ret == null) ? "" : ret;
  }

  /**
   * The namespace-uri function returns the namespace URI of the
   * expanded-name of the node in the argument node-set that is first in
   * document order. If the argument node-set is empty, the first node has
   * no expanded-name, or the namespace URI of the expanded-name is null, an
   * empty string is returned. If the argument is omitted, it defaults to a
   * node-set with the context node as its only member.
   */
  public static String _namespace_uri(Node context, Collection<Node> nodeSet)
  {
    if (nodeSet == null || nodeSet.isEmpty())
      return "";
    Node node = firstNode(nodeSet);
    String ret = node.getNamespaceURI();
    return (ret == null) ? "" : ret;
  }
  
  /**
   * The name function returns a string containing a QName representing the
   * expanded-name of the node in the argument node-set that is first in
   * document order. The QName must represent the expanded-name with respect
   * to the namespace declarations in effect on the node whose expanded-name
   * is being represented. Typically, this will be the QName that occurred
   * in the XML source. This need not be the case if there are namespace
   * declarations in effect on the node that associate multiple prefixes
   * with the same namespace. However, an implementation may include
   * information about the original prefix in its representation of nodes;
   * in this case, an implementation can ensure that the returned string is
   * always the same as the QName used in the XML source. If the argument
   * node-set is empty or the first node has no expanded-name, an empty
   * string is returned. If the argument it omitted, it defaults to a
   * node-set with the context node as its only member.
   */
  public static String _name(Node context, Collection<Node> nodeSet)
  {
    if (nodeSet == null || nodeSet.isEmpty())
      return "";
    Node node = firstNode(nodeSet);
    String ret = null;
    switch (node.getNodeType())
      {
      case Node.ATTRIBUTE_NODE:
      case Node.ELEMENT_NODE:
      case Node.PROCESSING_INSTRUCTION_NODE:
        ret = node.getNodeName();
      }
    return (ret == null) ? "" : ret;
  }

  /**
   * Returns the first node in the set in document order.
   */
  static Node firstNode(Collection<Node> nodeSet)
  {
    List<Node> list = new ArrayList<Node>(nodeSet);
    Collections.sort(list, documentOrderComparator);
    return list.get(0);
  }

  /* -- 4.2 String Functions -- */

  /**
   * Implementation of the XPath <code>string</code> function.
   */
  public static String _string(Node context, Object object)
  {
    if (object == null)
      {
        return stringValue(context);
      }
    if (object instanceof String)
      {
        return (String) object;
      }
    if (object instanceof Boolean)
      {
        return object.toString();
      }
    if (object instanceof Double)
      {
        double d = ((Double) object).doubleValue();
        if (Double.isNaN(d))
          {
            return "NaN";
          }
        else if (d == 0.0d)
          {
            return "0";
          }
        else if (Double.isInfinite(d))
          {
            if (d < 0)
              {
                return "-Infinity";
              }
            else
              {
                return "Infinity";
              }
          }
        else
          {
            String ret = decimalFormat.format(d);
            if (ret.endsWith (".0"))
              { 
                ret = ret.substring(0, ret.length() - 2);
              }
            return ret;
          }
      }
    if (object instanceof Collection)
      {
	/* Suppression is safe, as we fail immediately if the
	 * first element is not a Node and don't use the rest */
	@SuppressWarnings("unchecked") 
	  Collection<Node> nodeSet = (Collection<Node>) object;
        if (nodeSet.isEmpty())
          {
            return "";
          }
        Node node = firstNode(nodeSet);
        return stringValue(node);
      }
    throw new IllegalArgumentException(object.toString());
  }

  /* -- 4.3 Boolean Functions -- */
  
  /**
   * Implementation of the XPath <code>boolean</code> function.
   */
  public static boolean _boolean(Node context, Object object)
  {
    if (object instanceof Boolean)
      {
        return ((Boolean) object).booleanValue();
      }
    if (object instanceof Double)
      {
        Double value = (Double) object;
        if (value.isNaN())
          return false;
        return value.doubleValue() != 0.0;
      }
    if (object instanceof String)
      {
        return ((String) object).length() != 0;
      }
    if (object instanceof Collection)
      {
        return ((Collection<?>) object).size() != 0;
      }
    return false; // TODO user defined types
  }

  /* -- 4.4 Number Functions -- */

  /**
   * Implementation of the XPath <code>number</code> function.
   */
  public static double _number(Node context, Object object)
  {
    if (object == null)
      {
        object = Collections.singleton(context);
      }
    if (object instanceof Double)
      {
        return ((Double) object).doubleValue();
      }
    if (object instanceof Boolean)
      {
        return ((Boolean) object).booleanValue() ? 1.0 : 0.0;
      }
    if (object instanceof Collection)
      {
	/* Suppression is safe, as we fail immediately if one
	 * of the elements is not a Node */
	@SuppressWarnings("unchecked") 
	  Collection<Node> nodeSet = (Collection<Node>) object;
        // Convert node-set to string
        object = stringValue(nodeSet);
      }
    if (object instanceof String)
      {
        String string = ((String) object).trim();
        try
          {
            return Double.parseDouble(string);
          }
        catch (NumberFormatException e)
          {
            return Double.NaN;
          }
      }
    return Double.NaN; // TODO user-defined types
  }

  /**
   * Computes the XPath string-value of the specified node-set.
   */
  public static String stringValue(Collection<Node> nodeSet)
  {
    CPStringBuilder buf = new CPStringBuilder();
    for (Iterator<Node> i = nodeSet.iterator(); i.hasNext(); )
      {
        buf.append(stringValue(i.next()));
      }
    return buf.toString();
  }

  /**
   * Computes the XPath string-value of the specified node.
   */
  public static String stringValue(Node node)
  {
    return stringValue(node, false);
  }
  
  static String stringValue(Node node, boolean elementMode)
  {
    switch (node.getNodeType())
      {
      case Node.DOCUMENT_NODE: // 5.1 Root Node
      case Node.DOCUMENT_FRAGMENT_NODE:
      case Node.ELEMENT_NODE: // 5.2 Element Nodes
        CPStringBuilder buf = new CPStringBuilder();
        for (Node ctx = node.getFirstChild(); ctx != null;
             ctx = ctx.getNextSibling())
          {
            buf.append(stringValue(ctx, true));
          }
        return buf.toString();
      case Node.TEXT_NODE: // 5.7 Text Nodes
      case Node.CDATA_SECTION_NODE:
        return node.getNodeValue();
      case Node.ATTRIBUTE_NODE: // 5.3 Attribute Nodes
      case Node.PROCESSING_INSTRUCTION_NODE: // 5.5 Processing Instruction
      case Node.COMMENT_NODE: // 5.6 Comment Nodes
        if (!elementMode)
          {
            return node.getNodeValue();
          }
      default:
        return "";
      }
  }

  static int intValue(Object val)
  {
    if (val instanceof Double)
      {
        Double d = (Double) val;
        return d.isNaN() ? 0 : d.intValue();
      }
    else
      return (int) Math.ceil(_number(null, val));
  }

}
