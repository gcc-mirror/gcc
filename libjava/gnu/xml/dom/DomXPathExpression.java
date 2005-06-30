/* DomXPathExpression.java -- 
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

package gnu.xml.dom;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.DOMException;
import org.w3c.dom.Node;
import org.w3c.dom.xpath.XPathException;
import org.w3c.dom.xpath.XPathNSResolver;
import org.w3c.dom.xpath.XPathResult;
import gnu.xml.xpath.DocumentOrderComparator;

/**
 * An XPath expression.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class DomXPathExpression
implements org.w3c.dom.xpath.XPathExpression
{

  final DomDocument doc;
  final XPathExpression expression;
  final XPathNSResolver resolver;

  DomXPathExpression(DomDocument doc, String expression,
                     XPathNSResolver resolver)
    throws XPathException
  {
    this.doc = doc;
    this.resolver = resolver;
    
		XPathFactory factory = XPathFactory.newInstance();
		XPath xpath = factory.newXPath();
		if (resolver != null)
		  {
				xpath.setNamespaceContext(new DomNSResolverContext(resolver));
	 	  }
    try
      {
        this.expression = xpath.compile(expression);
      }
    catch (XPathExpressionException e)
      {
        throw new XPathException(XPathException.INVALID_EXPRESSION_ERR,
						                     e.getMessage ());
      }
  }

  public Object evaluate(Node contextNode, short type, Object result)
    throws XPathException, DOMException
  {
		try
		  {
				QName typeName = null;
				switch (type)
				  {
						case XPathResult.BOOLEAN_TYPE:
							typeName = XPathConstants.BOOLEAN;
							break;
						case XPathResult.NUMBER_TYPE:
							typeName = XPathConstants.NUMBER;
							break;
						case XPathResult.STRING_TYPE:
							typeName = XPathConstants.STRING;
							break;
						case XPathResult.ANY_UNORDERED_NODE_TYPE:
						case XPathResult.FIRST_ORDERED_NODE_TYPE:
							typeName = XPathConstants.NODE;
							break;
						case XPathResult.UNORDERED_NODE_ITERATOR_TYPE:
						case XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE:
						case XPathResult.ORDERED_NODE_ITERATOR_TYPE:
						case XPathResult.ORDERED_NODE_SNAPSHOT_TYPE:
							typeName = XPathConstants.NODESET;
							break;
						default:
							throw new XPathException(XPathException.TYPE_ERR, null);
					}
				Object val = expression.evaluate(contextNode, typeName);
				switch (type)
				  {
						case XPathResult.ORDERED_NODE_ITERATOR_TYPE:
						case XPathResult.ORDERED_NODE_SNAPSHOT_TYPE:
							// Sort the nodes
							List ns = new ArrayList((Collection) val);
							Collections.sort(ns, new DocumentOrderComparator());
							val = ns;
					}
				return new DomXPathResult(val, type);
			}
		catch (javax.xml.xpath.XPathException e)
		  {
				throw new XPathException(XPathException.TYPE_ERR, e.getMessage());
			}
  }

  public String toString ()
  {
    return getClass ().getName () + "[expression=" + expression + "]";
  }
  
}
