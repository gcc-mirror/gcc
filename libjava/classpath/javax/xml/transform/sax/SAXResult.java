/* SAXResult.java -- 
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

import javax.xml.transform.Result;
import org.xml.sax.ContentHandler;
import org.xml.sax.ext.LexicalHandler;

/**
 * Specifies SAX handlers to be used as a result sink during a
 * transformation.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class SAXResult implements Result
{
  
  /**
   * Factory feature indicating that SAX results are supported.
   */
  public static final String FEATURE =
    "http://javax.xml.transform.sax.SAXResult/feature";

  private ContentHandler handler;
  private LexicalHandler lexicalHandler;
  private String systemId;

  /**
   * Default constructor.
   */
  public SAXResult()
  {
  }

  /**
   * Constructor specifying a content handler.
   */
  public SAXResult(ContentHandler handler)
  {
    this.handler = handler;
  }

  /**
   * Sets the content handler to which result document events will be
   * propagated.
   */
  public void setHandler(ContentHandler handler)
  {
    this.handler = handler;
  }

  /**
   * Returns the content handler to which result document events will be
   * propagated.
   */
  public ContentHandler getHandler()
  {
    return handler;
  }

  /**
   * Sets the lexical handler to which lexical events will be propagated.
   * If a lexical handler is not set, the transformer should attempt to cast
   * the content handler to a lexical handler.
   */
  public void setLexicalHandler(LexicalHandler handler)
  {
    lexicalHandler = handler;
  }
  
  /**
   * Returns the lexical handler to which lexical events will be propagated.
   * If a lexical handler is not set, the transformer should attempt to cast
   * the content handler to a lexical handler.
   */
  public LexicalHandler getLexicalHandler()
  {
    return lexicalHandler;
  }

  /**
   * Sets the system ID which this result represents.
   */
  public void setSystemId(String systemId)
  {
    this.systemId = systemId;
  }

  /**
   * Returns the system ID which this result represnts.
   */
  public String getSystemId()
  {
    return systemId;
  }

}
