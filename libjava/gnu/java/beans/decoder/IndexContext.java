/* gnu.java.beans.decoder.IndexContext
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

/** IndexContext is Context implementation that senses whether it is an indexed get or set
 * operation and invokes this operation.
 *
 * <p>An IndexContent is a get operation when no argument is provided and a set operation if one
 * argument is provided.</p>
 *
 * @author Robert Schuster
 */
class IndexContext extends AbstractContext
{
  private Object result;
  private Object argument;
  private int index;
  private boolean isSetter;

  IndexContext(String id, int newIndex)
  {
    setId(id);
    index = newIndex;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
   */
  public void addParameterObject(Object o) throws AssemblyException
  {
    if (! isSetter)
      {
	argument = o;
	isSetter = true;
      }
    else
      throw new AssemblyException(new IllegalStateException("More than one argument for indiced access is not possible."));
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#reportStatement()
   */
  public void notifyStatement(Context outerContext) throws AssemblyException
  {
    throw new AssemblyException(new IllegalStateException("Statements inside indiced access are not allowed."));
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
   */
  public Object endContext(Context outerContext) throws AssemblyException
  {
    if (isSetter)
      {
	// setter
	outerContext.set(index, argument);

	return null;
      }
    else
      // getter
      return result = outerContext.get(index);
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#subContextFailed()
   */
  public boolean subContextFailed()
  {
    // returns true to indicate that indiced access assembly fails when one of its
    // argument could not be assembled
    return true;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#set(int, java.lang.Object)
   */
  public void set(int index, Object o) throws AssemblyException
  {
    throw new AssemblyException(new IllegalStateException("Setter is not allowed inside indiced access."));
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#get(int)
   */
  public Object get(int index) throws AssemblyException
  {
    throw new AssemblyException(new IllegalStateException("getter is not allowed insided indiced access."));
  }

  public Object getResult()
  {
    return result;
  }
}
