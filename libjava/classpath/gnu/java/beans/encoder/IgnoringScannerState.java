/* IgnoringScannerState.java -- A ScannerState that does nothing.
 Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.java.beans.encoder;

/** A special {@link ScannerState} implementation that ignores all child
 * elements.
 *
 * <p>Consider the call hierarchy:
 * <code>
 * methodInvocation
 *   objectInstantiation
 *     classResolution*
 *       objectInstantiation
 *         classResolution
 * </code>
 * </p>
 *
 * <p>When the ignoring state is active one can filter the elements of
 * one level. One has to set up the state machine that a transition
 * via "class resolution" from a state that was reached via "object
 * instantation" reaches an <code>IgnoringScannerState</code>.</p>
 *
 * <p>Setting the default successor of a <code>IgnoringScannerState</code>
 * to itself causes all elements of the call hierarchy to be skipped
 * until another state is reached by going back.</p>
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 *
 */
class IgnoringScannerState extends ScannerState
{

  void methodInvocation(String methodName)
  {
  }

  void staticMethodInvocation(String className, String methodName)
  {
  }

  void staticFieldAccess(String className, String fieldName)
  {
  }

  void classResolution(String className)
  {
  }

  void objectInstantiation(String className, ObjectId objectId)
  {
  }

  void primitiveInstantiation(String primitiveName, String valueAsString)
  {
  }

  void objectArrayInstantiation(String arrayClassName, String lengthAsString, ObjectId objectId)
  {
  }

  void primitiveArrayInstantiation(String arrayClassName, String lengthAsString, ObjectId objectId)
  {
  }

  void arraySet(String indexAsString)
  {
  }

  void arrayGet(String indexAsString)
  {
  }

  void listGet()
  {
  }

  void listSet()
  {
  }

  void nullObject()
  {
  }

  void stringReference(String string)
  {
  }

  void objectReference(ObjectId id)
  {
  }

  void end()
  {
  }

}
