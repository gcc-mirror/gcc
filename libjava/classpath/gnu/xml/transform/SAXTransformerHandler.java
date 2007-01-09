/* SAXTransformerHandler.java -- 
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import javax.xml.transform.Result;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.TransformerHandler;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import gnu.xml.dom.ls.SAXEventSink;

/**
 * A SAX event sink that processes an XML source represented as a stream of
 * SAX events into a result tree.
 * This works by simply buffering all the events into a DOM tree and then
 * using this DOM tree as the source of the transformation.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
class SAXTransformerHandler
  extends SAXEventSink
  implements TransformerHandler
{

  final TransformerFactoryImpl factory;
  final Transformer transformer;
  String systemId;
  Result result;

  SAXTransformerHandler(TransformerFactoryImpl factory, Transformer transformer)
  {
    this.factory = factory;
    this.transformer = transformer;
  }

  public String getSystemId()
  {
    return systemId;
  }

  public void setSystemId(String systemId)
  {
    this.systemId = systemId;
  }

  public Transformer getTransformer()
  {
    return transformer;
  }

  public void setResult(Result result)
  {
    this.result = result;
  }

  public void endDocument()
    throws SAXException
  {
    super.endDocument();
    try
      {
        Document doc = getDocument();
        DOMSource ds = new DOMSource(doc, systemId);
        transformer.transform(ds, result);
      }
    catch (TransformerException e)
      {
        SAXException e2 = new SAXException(e.getMessage());
        e2.initCause(e);
        throw e2;
      }
  }
  
}
