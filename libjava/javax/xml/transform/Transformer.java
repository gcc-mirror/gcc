/* Transformer.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

package javax.xml.transform;

import java.util.Properties;

/**
 * An XSL transformation.
 * Instances of this class may be reused, but the same instance may not be
 * used concurrently by different threads.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public abstract class Transformer
{

  protected Transformer()
  {
  }

  /**
   * Transforms the source XML to a result tree.
   * @param xmlSource the XML source
   * @param outputTarget the result of the transformation
   */
  public abstract void transform(Source xmlSource, Result outputTarget) 
    throws TransformerException;
  
  /**
   * Sets a parameter value for the transformation.
   * Parameters may be referenced in the XSLT stylesheet.
   * @param name the parameter name (an XML Name, or a namespace-prefixed
   * XML Name of the form <code>{<i>namespaceURI</i>}<i>localName</i></code>
   * @param value the value to assign
   */
  public abstract void setParameter(String name, Object value);

  /**
   * Returns the specified parameter value.
   * @param name the parameter name (an XML Name, or a namespace-prefixed
   * XML Name of the form <code>{<i>namespaceURI</i>}<i>localName</i></code>
   */
  public abstract Object getParameter(String name);

  /**
   * Clears all parameter values.
   */
  public abstract void clearParameters();

  /**
   * Sets the callback used to resolve entities referenced by
   * <code>xsl:include</code>, <code>xsl:import</code>, or the XPath
   * <code>document()</code> function.
   */
  public abstract void setURIResolver(URIResolver resolver);

  /**
   * Returns the callback used to resolve entities referenced by
   * <code>xsl:include</code>, <code>xsl:import</code>, or the XPath
   * <code>document()</code> function.
   */
  public abstract URIResolver getURIResolver();

  /**
   * Sets the output properties for the transformation, overriding any
   * properties defined in the stylesheet.
   * The format of property keys is as in the
   * {@link #setOutputProperty(java.lang.String,java.lang.String)} method.
   * @param oformat a set of output properties, or null to reset all the
   * properties to their default values
   */
  public abstract void setOutputProperties(Properties oformat) 
    throws IllegalArgumentException;

  /**
   * Returns a copy of the output properties for the transformation.
   * Missing properties are defaulted according the
   * <a href='http://www.w3.org/TR/xslt#output'>XSLT Recommendation, section
   * 16</a>: <code>getProperty(String)</code> returns all properties
   * including defaulted ones, and <code>get(Object)</code> returns only the
   * properties explicitly set in the stylesheet.
   */
  public abstract Properties getOutputProperties();

  /**
   * Sets an output property for the transformation, overriding any property
   * of the same name defined in the stylesheet.
   * @param name the property name (an XML Name, or a namespace-prefixed
   * XML Name of the form <code>{<i>namespaceURI</i>}<i>localName</i></code>
   * @param value the string value of the property
   * @exception IllegalArgumentException if the property is not supported
   */
  public abstract void setOutputProperty(String name, String value) 
    throws IllegalArgumentException;

  /**
   * Returns the value of an output property for the transformation.
   * Only explicit properties set programmatically or defined in the
   * stylesheet, not defaulted properties, are returned by this method.
   * @param name the property name (an XML Name, or a namespace-prefixed
   * XML Name of the form <code>{<i>namespaceURI</i>}<i>localName</i></code>
   * @exception IllegalArgumentException if the property is not supported
   */
  public abstract String getOutputProperty(String name) 
    throws IllegalArgumentException;

  /**
   * Sets the callback used to report errors during the transformation.
   *  @exception IllegalArgumentException if the listener is null
   */
  public abstract void setErrorListener(ErrorListener listener)
    throws IllegalArgumentException;
  
  /**
   * Returns the callback used to report errors during the transformation.
   */
  public abstract ErrorListener getErrorListener();

  // -- JAXP 1.3 methods --

  /**
   * Reset this Transformer to its original configuration.
   * @since 1.3
   */
  public void reset()
  {
  }

}
