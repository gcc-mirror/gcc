/* gnu.java.beans.decoder.SimpleHandler
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

package gnu.java.beans.decoder;

import java.beans.ExceptionListener;

import org.xml.sax.Attributes;

/** XML element handler that is specialized on tags that contains a simple string in their
 * body which has to be parsed in a specific way.
 * <p>All of these tags have in common that they do not accept attributes. A warning is
 * send to the parser's ExceptionListener when one or more attributes exist.</p>
 *
 * @author Robert Schuster
 */
abstract class SimpleHandler extends AbstractElementHandler
{
  private ObjectContext context;

  /**
   * @param PersistenceParser
   */
  SimpleHandler(ElementHandler parent)
  {
    super(parent, false);

    // SimpleHandler do not accept any subelements
  }

  protected final Context startElement(Attributes attributes, ExceptionListener exceptionListener)
    throws AssemblyException
  {
  	
    // note: simple elements should not have any attributes. We inform
    // the user of this syntactical but uncritical problem by sending
    // an IllegalArgumentException for each unneccessary attribute
    int size = attributes.getLength();
    for (int i = 0; i < size; i++) {
            String attributeName = attributes.getQName(i);
            Exception e =
                    new IllegalArgumentException(
                            "Unneccessary attribute '"
                                    + attributeName
                                    + "' discarded.");
            exceptionListener.exceptionThrown(e);
    }
    
    return context = new ObjectContext();
  }

  public void endElement(String characters)
    throws AssemblyException, AssemblyException
  {
    // reports the number when the character data can be parsed
    try
      {
	context.setObject(parse(characters));
      }
    catch (NumberFormatException nfe)
      {
	throw new AssemblyException(nfe);
      }
  }

  /** Returns an object that is created from the given characters. If the string is
   * converted into a number a NumberFormatException is cathed and reported
   * appropriately.
   *
   * @param characters A string of characters that has to be processed in some way.
   * @return An Object instance generated from the given data.
   * @throws AssemblerException When the string was invalid.
   * @throws NumberFormatException When the string could not be parsed into a number.
   */
  protected abstract Object parse(String characters)
    throws AssemblyException, NumberFormatException;
}
