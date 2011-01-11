/* XPath.java --
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

package javax.xml.xpath;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import org.xml.sax.InputSource;

/**
 * Interface to the XPath evaluation environment.
 * @since 1.3
 */
public interface XPath
{

  /**
   * Resets the environment.
   */
  void reset();

  void setXPathVariableResolver(XPathVariableResolver resolver);

  XPathVariableResolver getXPathVariableResolver();

  void setXPathFunctionResolver(XPathFunctionResolver resolver);

  XPathFunctionResolver getXPathFunctionResolver();

  void setNamespaceContext(NamespaceContext nsContext);

  NamespaceContext getNamespaceContext();

  /**
   * Compiles an XPath expression for future evaluation.
   * @param expression the expression
   */
  XPathExpression compile(String expression)
    throws XPathExpressionException;

  /**
   * Evaluates an expression.
   * @param expression the expression
   * @param item the expression context
   * @param returnType the desired return type
   */
  Object evaluate(String expression,
                  Object item,
                  QName returnType)
    throws XPathExpressionException;

  /**
   * Evaluates an expression and returns the result as a string.
   * @param expression the expression
   * @param item the expression context
   */
  String evaluate(String expression,
                  Object item)
    throws XPathExpressionException;

  /**
   * Evaluates an expression.
   * @param expression the expression
   * @param source the source to load the expression context from
   * @param returnType the desired return type
   */
  Object evaluate(String expression,
                  InputSource source,
                  QName returnType)
    throws XPathExpressionException;

  /**
   * Evaluates an expression and returns the result as a string.
   * @param expression the expression
   * @param source the source to load the expression context from
   */
  String evaluate(String expression,
                  InputSource source)
    throws XPathExpressionException;

}
