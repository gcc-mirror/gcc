/* SAXTransformerFactory.java -- 
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

package javax.xml.transform.sax;

import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import org.xml.sax.XMLFilter;

/**
 * Specialized transformer factory with support for SAX-specific factory
 * methods.
 * This factory provides SAX content handlers that can create transformation
 * templates and transformers.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public abstract class SAXTransformerFactory extends TransformerFactory
{

  /**
   * Factory feature indicating that the factory can be cast to this class.
   */
  public static final String FEATURE =
    "http://javax.xml.transform.sax.SAXTransformerFactory/feature";

  /**
   * Factory feature indicating that this factory can create new XMLFilters.
   */
  public static final String FEATURE_XMLFILTER =
    "http://javax.xml.transform.sax.SAXTransformerFactory/feature/xmlfilter";

  protected SAXTransformerFactory()
  {
  }

  /**
   * Returns a content handler that can process SAX events into a result,
   * using the specified transformation.
   * @param src the source stylesheet
   */
  public abstract TransformerHandler newTransformerHandler(Source src)
    throws TransformerConfigurationException;

  /**
   * Returns a content handler that can process SAX events into a result,
   * using the specified transformation.
   * @param templates the compiled stylesheet
   */
  public abstract TransformerHandler newTransformerHandler(Templates templates)
    throws TransformerConfigurationException;

  /**
   * Returns a content handler that can process SAX events into a result,
   * using the identity transform.
   */
  public abstract TransformerHandler newTransformerHandler()
    throws TransformerConfigurationException;

  /**
   * Returns a content handler that can process SAX events into a
   * transformation template.
   */
  public abstract TemplatesHandler newTemplatesHandler()
    throws TransformerConfigurationException;

  /**
   * Creates an XML filter for the specified source.
   */
  public abstract XMLFilter newXMLFilter(Source src)
    throws TransformerConfigurationException;

  /**
   * Creates an XML filter for the specified compiled stylesheet.
   */
  public abstract XMLFilter newXMLFilter(Templates templates)
    throws TransformerConfigurationException;

}
