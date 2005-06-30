/* TransformerHandler.java -- 
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
import javax.xml.transform.Transformer;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.ext.LexicalHandler;

/**
 * A content handler that transforms SAX events into a result tree.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public interface TransformerHandler
  extends ContentHandler, LexicalHandler, DTDHandler
{
  
  /**
   * Sets the result sink to be used as the output of the transformation.
   * @exception IllegalArgumentException if the result is not a valid target
   */
  public void setResult(Result result) 
    throws IllegalArgumentException;

  /**
   * Sets the system ID relative to which URLs will be resolved.
   */
  public void setSystemId(String systemID);

  /**
   * Returns the system ID relative to which URLs will be resolved.
   */
  public String getSystemId();

  /**
   * Returns the transformer associated with this handler in order to set
   * parameters and output properties.
   */
  public Transformer getTransformer();

}
