/* gnu.java.beans.decoder.ElementHandler
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

/** ElementHandler manages a Context instance and interacts with
 * its parent and child handlers.
 *
 * @author Robert Schuster
 */
interface ElementHandler
{
  /** Evaluates the attributes and creates a Context instance.
   * If the creation of the Context instance fails the ElementHandler
   * is marked as failed which may affect the parent handler other.
   *
   * @param attributes Attributes of the XML tag.
   */
  void start(Attributes attributes, ExceptionListener exceptionListener);

  /** Post-processes the Context.
   */
  void end(ExceptionListener exceptionListener);

  /** Adds characters from the body of the XML tag to the buffer.
   *
   * @param ch
   * @param start
   * @param length
   * @throws SAXException
   */
  void characters(char[] ch, int start, int length);

  /** Returns whether a subelement of the given name is allowed. The rules
   * for evaluating this are derived from the javabeans.dtd which can be found
   * here: <a href="http://java.sun.com/products/jfc/tsc/articles/persistence3">Java Persistence Article</a>.
   * 
   * @param subElementName 
   * @return
   */
  boolean isSubelementAllowed(String subElementName);

  /** Provides the same functionality as Class.forName() but allows the decoder
   * to use a different class loader.
   * 
   * @param className
   * @return
   * @throws ClassNotFoundException
   */ 
  Class instantiateClass(String className) throws ClassNotFoundException;

  /** Notifies the handler's Context that its child Context will not return
   * a value back. Some Context variants need this information to know when
   * a method or a constructor call can be made.
   *
   * This method is called by a child handler.
   */
  void notifyStatement(ExceptionListener exceptionListener);

  /** Returns whether this handler has failed.
   *
   * This is used to skip child elements.
   *
   * @return Whether this handler has failed.
   */
  boolean hasFailed();

  /** Returns the Context instance this handler is working on.
   * 
   * @return The handler's Context instance.
   */
  Context getContext();

  /** Notifies the handler that its Context failed and starts a recursive
   * invocation of the parent handler if it is affected by that failure.
   * 
   * Although the method is a public API member it is only used internally.
   */
  void notifyContextFailed();

  /** Stores the object under the given id. The object is not stored if the
   * id is null.
   * 
   * @param objectId
   * @param o
   */
  void putObject(String objectId, Object o);

  Object getObject(String objectId) throws AssemblyException;

  ElementHandler getParent();
}
