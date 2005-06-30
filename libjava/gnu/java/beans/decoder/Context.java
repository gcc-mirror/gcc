/* gnu.java.beans.decoder.Context
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

/** A Context is the environment for an object which is being assembler. If there
 * are no errors each handler creates one Context.
 * <p>Depending on the result of isStatement() a Context can be statement or an
 * expression. An expression returns a value to the Context of its parent handler,
 * a statement does not. Whenever a Context is a statement the parent handler's
 * Context is informed about that through the {@link notifyStatement}-method.</p>
 *
 * @author Robert Schuster
 */
interface Context
{
  /** Adds a parameter object to the context. This method is used when
   * sub-Contexts return their result.
   *
   * Some Contexts do not accept more than a certain amount of objects
   * and throw an AssemblerException if the amount is exceeded.
   *
   * @param o The object added to this context.
   */
  void addParameterObject(Object o) throws AssemblyException;

  /** Notifies that the next element is a statement. This can mean
   * that an argument list is complete to be called.
   *
   */
  void notifyStatement(Context outerContext) throws AssemblyException;

  /** Notifies that the context ends and the returns the appropriate result
   * object.
   *
   * @param outerContext
   * @return
   */
  Object endContext(Context outerContext) throws AssemblyException;

  /** Notifies that the assembly of a subcontext failed and returns
   * whether this Context is affected in a way that it fails too.
   *
   * @return Whether the failure of a subcontext lets this context fail, too.
   */
  boolean subContextFailed();

  /** Calls an appropriate indexed set method if it is available or
   * throws an AssemblerException if that is not allowed on this Context.
   *
   * The behaviour of this method is equal to List.set(int, Object).
   *
   * @param index Index position to be set.
   * @param o Object to be set at the given index position.
   * @throws AssemblerException Indexed set is not allowed or otherwise failed.
   */
  void set(int index, Object o) throws AssemblyException;

  /** Calls an appropriate indexed get method if it is available or
   * throws an AssemblerException if that is not allowed on this Context.
   *
   * The behaviour of this method is equal to List.get(int).
   *
   * @param index Index position of the object return.
   * @throws AssemblerException Indexed get is not allowed or otherwise failed.
   */
  Object get(int index) throws AssemblyException;

  /** Returns the result which was calculated by calling endContext() or reportStatement().
   * Its the handler's responsibility to care that any of these two methods was called.
   *
   * This is used by sub-Contexts to access this Context's result.
   *
   * @return
   */
  Object getResult();

  /** Gives this Context a unique id. For convenience the id may be null which means
   * that no id exists at all.
   *
   * @param id
   */
  void setId(String id);

  /** Returns this Context's unique id or null if does not have such an id.
   *
   * @return This Context's id or null.
   */
  String getId();

  /** Returns whether this Context is a statement (not returning result back
   * to parent handler's Context) or not (= expression).
   *
   * @return
   */
  boolean isStatement();

  /** Sets whether this Context is a statement or not.
   *
   * @param b
   */
  void setStatement(boolean b);
}
