/* gnu.java.beans.decoder.AbstractCreatableContext
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


/** AbstractCreatableObjectContext is the base class for all Context implementations
 * which create a result object in their lifetime. It provides means for preventing
 * to create the object twice.
 *
 * @author Robert Schuster
 *
 */
abstract class AbstractCreatableObjectContext extends AbstractObjectContext
{
  AbstractCreatableObjectContext()
  {
  }

  /** Adds a parameter object to this Context if the result object has not been
   * created yet. Otherwise an AssemblyException is thrown that indicates a wrong
   * behavior of the decoder.
   */
  public final void addParameterObject(Object o) throws AssemblyException
  {
    if (object == null)
      addParameterObjectImpl(o);
    else
      throw new AssemblyException(new IllegalStateException("No more parameter objects are allowed when the object as already been created."));
  }

  /** Adds a parameter object to this Context. Implement this without caring
   * for illegal states because this has been done already.
   *
   * @param obj The parameter object to be added. 
   */
  protected abstract void addParameterObjectImpl(Object obj);

  /** Creates the result object if it does not exist already.
   */
  public final void notifyStatement(Context outerContext)
    throws AssemblyException
  {
    if (object != null)
      return;

    object = createObject(outerContext);
  }

  /** Creates the result object. This method is called only once. Implement this
   * without checking for double invocations as this is already being prevented.
   *
   * @param outerContext The Context that exists around this one. 
   * @return The result object.
   * @throws AssemblerException if the object creation fails somehow.
   */
  protected abstract Object createObject(Context outerContext)
    throws AssemblyException;

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
   */
  public final Object endContext(Context outerContext)
    throws AssemblyException
  {
    notifyStatement(outerContext);
    return object;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#subContextFailed()
   */
  public boolean subContextFailed()
  {
    /* Returns true when the AbstractCreatableObjectContext has not created the result object yet
     * (A failed subcontext automatically lets this context fail too.)
     */
    return object == null;
  }
}
