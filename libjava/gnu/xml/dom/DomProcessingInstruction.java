/* DomProcessingInstruction.java -- 
   Copyright (C) 1999,2000,2001,2004 Free Software Foundation, Inc.

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

package gnu.xml.dom;

import org.w3c.dom.DOMException;
import org.w3c.dom.ProcessingInstruction;

/**
 * <p> "ProcessingInstruction" (PI) implementation.
 * This is a non-core DOM class, supporting the "XML" feature. </p>
 *
 * <p> Unlike other DOM APIs in the "XML" feature, this one fully
 * exposes the functionality it describes.  So there is no reason
 * inherent in DOM to avoid using this API, unless you want to rely
 * on NOTATION declarations to associate meaning with your PIs;
 * there is no vendor-neutal way to record those notations in DOM.</p>
 *
 * <p> Also of note is that PI support is part of SAX, so that XML
 * systems using PIs can choose among multiple APIs. </p>
 *
 * @see DomNotation
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomProcessingInstruction
  extends DomNode
  implements ProcessingInstruction
{
  
  private String target;
  private String data;
  
  /**
   * Constructs a ProcessingInstruction node associated with the
   * specified document, with the specified data.
   *
   * <p>This constructor should only be invoked by a Document object as
   * part of its createProcessingInstruction functionality, or through
   * a subclass which is similarly used in a "Sub-DOM" style layer. 
   */
  protected DomProcessingInstruction(DomDocument owner,
                                     String target, String data)
  {
    super(PROCESSING_INSTRUCTION_NODE, owner);
    this.target = target;
    this.data = data;
  }

  /**
   * <b>DOM L1</b>
   * Returns the target of the processing instruction.
   */
  public final String getTarget()
  {
    return target;
  }

  /**
   * <b>DOM L1</b>
   * Returns the target of the processing instruction
   * (same as getTarget).
   */
  public final String getNodeName()
  {
    return target;
  }

  /**
   * <b>DOM L1</b>
   * Returns the data associated with the processing instruction.
   */
  public final String getData()
  {
    return data;
  }

  /**
   * <b>DOM L1</b>
   * Returns the data associated with the processing instruction
   * (same as getData).
   */
  public final String getNodeValue()
  {
    return data;
  }

  /**
   * <b>DOM L1</b>
   * Assigns the data associated with the processing instruction;
   * same as setNodeValue.
   */
  public final void setData(String data)
  {
    setNodeValue(data);
  }
  
  /**
   * <b>DOM L1</b>
   * Assigns the data associated with the processing instruction.
   */
  public final void setNodeValue(String data)
  {
    if (isReadonly())
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    this.data = data;
  }
  
}

