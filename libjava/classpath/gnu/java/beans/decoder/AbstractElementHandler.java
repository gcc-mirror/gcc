/* gnu.java.beans.decoder.AbstractElementHandler
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

package gnu.java.beans.decoder;

import java.beans.ExceptionListener;

import org.xml.sax.Attributes;

/** ElementHandler manages a Context instance and interacts with
 * its parent and child handlers.
 *
 * @author Robert Schuster
 */
abstract class AbstractElementHandler implements ElementHandler
{
  /** The Context instance of this handler. The instance is available after the startElement()
   * method was called. Otherwise the handler is marked as failed.
   */
  private Context context;

  /** The parent handler. */
  private ElementHandler parent;

  /** Stores whether this handler is marked as failed. */
  private boolean hasFailed;

  /** Stores the character data which is contained in the body of the XML tag. */
  private StringBuffer buffer = new StringBuffer();

  /** Stores whether this ElementHandler can have subelements. The information for this is taken from
   * javabeans.dtd which can be found here:
   * <a href="http://java.sun.com/products/jfc/tsc/articles/persistence3/">Java Persistence Article</a>
   */
  private boolean allowsSubelements;

  /** Creates a new ElementHandler with the given ElementHandler instance
   * as parent.
   *
   * @param parentHandler The parent handler.
   */
  protected AbstractElementHandler(ElementHandler parentHandler,
                                   boolean allowsSubs)
  {
    parent = parentHandler;
    allowsSubelements = allowsSubs;
  }

  /** Evaluates the attributes and creates a Context instance.
   * If the creation of the Context instance fails the ElementHandler
   * is marked as failed which may affect the parent handler other.
   *
   * @param attributes Attributes of the XML tag.
   */
  public final void start(Attributes attributes,
                          ExceptionListener exceptionListener)
  {
    try
      {
        // lets the subclass create the appropriate Context instance
        context = startElement(attributes, exceptionListener);
      }
    catch (AssemblyException pe)
      {
        Throwable t = pe.getCause();

        if (t instanceof Exception)
          exceptionListener.exceptionThrown((Exception) t);
        else
          throw new InternalError("Unexpected Throwable type in AssemblerException. Please file a bug report.");

        notifyContextFailed();

        return;
      }
  }

  /** Analyses the content of the Attributes instance and creates a Context
   * object accordingly.
   * An AssemblerException is thrown when the Context instance could not
   * be created.
   *
   * @param attributes Attributes of the XML tag.
   * @return A Context instance.
   * @throws AssemblerException when Context instance could not be created.
   */
  protected abstract Context startElement(Attributes attributes, ExceptionListener exceptionListener)
    throws AssemblyException;

  /** Post-processes the Context.
   */
  public final void end(ExceptionListener exceptionListener)
  {
    // skips processing if the handler is marked as failed (because the Context
    // is then invalid or may not exist at all)
    if (!hasFailed)
      {
        try
          {
            // note: the order of operations is very important here
            // sends the stored character data to the Context
            endElement(buffer.toString());

            // reports to the parent handler if this handler's Context is a
            // statement (returning no value BACK to the parent's Context)
            if (context.isStatement())
              {
                // This may create a valid result in the parent's Context
                // or let it fail
                parent.notifyStatement(exceptionListener);

                // skips any further processing if the parent handler is now marked
                // as failed
                if (parent.hasFailed())
                  return;
              }

            // processes the Context and stores the result
            putObject(context.getId(), context.endContext(parent.getContext()));

            // transfers the Context's results to the parent's Context
            // if it is an expression (rather than a statement)
            if (! context.isStatement())
              parent.getContext().addParameterObject(context.getResult());
          }
        catch (AssemblyException pe)
          {
            // notifies that an exception was thrown in this handler's Context
            Throwable t = pe.getCause();

            if (t instanceof Exception)
              exceptionListener.exceptionThrown((Exception) t);
            else
              throw (InternalError) new InternalError("Severe problem while decoding XML data.")
                    .initCause(t);

            // marks the handler as failed
            notifyContextFailed();
          }
      }
  }

  /** Notifies the handler's Context that its child Context will not return
   * a value back. Some Context variants need this information to know when
   * a method or a constructor call can be made.
   *
   * This method is called by a child handler.
   */
  public void notifyStatement(ExceptionListener exceptionListener)
  {
    try
      {

        // propagates to parent handler first to generate objects
        // needed by this Context instance
        if(context.isStatement())
        {
                parent.notifyStatement(exceptionListener);
        }

        // Some Context instances do stuff which can fail now. If that
        // happens this handler is marked as failed.
        context.notifyStatement(parent.getContext());
      }
    catch (AssemblyException ae)
      {
        // notifies that an exception was thrown in this handler's Context
        Throwable t = ae.getCause();

        if (t instanceof Exception)
          exceptionListener.exceptionThrown((Exception) t);
        else
          throw (InternalError) new InternalError("Severe problem while decoding XML data.")
                .initCause(t);

        // marks the handler as failed
        notifyContextFailed();
      }
  }

  /** Marks this and any depending parent handlers as failed. Which means that on their end
   * no result is calculated.
   *
   * When a handler has failed no more handlers are accepted within it.
   */
  public final void notifyContextFailed()
  {
    hasFailed = true;

    // marks the parent handler as failed if its Context
    // is affected by the failure of this handler's Context
    if (parent.getContext().subContextFailed())
      parent.notifyContextFailed();
  }

  /** Returns whether this handler has failed.
   *
   * This is used to skip child elements.
   *
   * @return Whether this handler has failed.
   */
  public final boolean hasFailed()
  {
    return hasFailed;
  }

  /** Processes the character data when the element ends.
   *
   * The default implementation does nothing for convenience.
   *
   * @param characters
   * @throws AssemblerException
   */
  protected void endElement(String characters) throws AssemblyException
  {
    // XXX: throw an exception when unexpected character data is available?
  }

  /** Adds characters from the body of the XML tag to the buffer.
   *
   * @param ch
   * @param start
   * @param length
   * @throws SAXException
   */
  public final void characters(char[] ch, int start, int length)
  {
    // simply appends character data
    buffer.append(ch, start, length);
  }

  /** Stores an object globally under a unique id. If the id is
   * null the object is not stored.
   *
   * @param objectId
   * @param o
   */
  public void putObject(String objectId, Object o)
  {
    if (objectId != null)
      parent.putObject(objectId, o);
  }

  /** Returns a previously stored object. If the id is null the
   * result is null, too.
   *
   * @param objectId
   * @return Returns a previously stored object or null.
   */
  public Object getObject(String objectId) throws AssemblyException
  {
    return objectId == null ? null : parent.getObject(objectId);
  }

  /** Returns the Class instance as if called Class.forName() but
   * uses a ClassLoader given by the user.
   *
   * @param className
   * @return
   * @throws ClassNotFoundException
   */
  public Class instantiateClass(String className)
    throws ClassNotFoundException
  {
    return parent.instantiateClass(className);
  }

  public final boolean isSubelementAllowed(String subElementName)
  {
    return allowsSubelements && ! subElementName.equals("java");
  }

  public final Context getContext()
  {
    return context;
  }

  public final ElementHandler getParent()
  {
    return parent;
  }
}
