/* DynAnyFactoryOperations.java --
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


package org.omg.DynamicAny;

import org.omg.CORBA.Any;
import org.omg.CORBA.TypeCode;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;

/**
 * Defines the operations, applicable for DynAnyFactory. These operations
 * produce new DynAny's either from Any, serving as a template and value
 * provider, or from the given typecode.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynAnyFactoryOperations
{
  /**
   * Create DynAny for holding the data of the given type. The returned DynAny
   * is initialised to its agreed default value. The agreed default values are:
   * <table border='1'>
   * <tr>
   * <th>Type</th>
   * <th>Value</th>
   * <th>Creates</th>
   * </tr>
   *
   * <tr>
   * <td>boolean</td>
   * <td>false</td>
   * <td>{@link DynAny}</td>
   * </tr>
   * <tr>
   * <td>numeric types, octet, fixed</td>
   * <td>0</td>
   * <td>{@link DynAny}</td>
   * </tr>
   * <tr>
   * <td>char, wchar</td>
   * <td>(char) 0</td>
   * <td>{@link DynAny}</td>
   * </tr>
   * <tr>
   * <td>string, wstring</td>
   * <td>empty string ("", not <code>null<code>)</td>
   * <td>{@link DynAny}</td>
   * </tr>
   * <tr>
   * <td>{@link Any}</td>
   * <td>{@link Any} with no value and typecode of kind {@link TCKind.tk_null}</td>
   * <td>{@link DynAny}</td>
   * </tr>
   * <tr>
   * <td>Sequence</td>
   * <td>Empty (zero size) sequence</td>
   * <td>{@link DynSequence}</td>
   * </tr>
   * <tr>
   * <td>Array</td>
   * <td>All members of array are recursively initialised to default values.</td>
   * <td>{@link DynArray}</td>
   * </tr>
   * <tr>
   * <td>Structure, exception</td>
   * <td>All fields of the structure (if any) are recursively initialised to
   * default values.</td>
   * <td>{@link DynStruct}</td>
   * </tr>
   * <tr>
   * <td>Enumeration</td>
   * <td>Default value, indicated by typecode.</td>
   * <td>{@link DynEnum}</td>
   * </tr>
   * <tr>
   * <td>Union</td>
   * <td>Default variant (indicated by typecode), recursively initialised to
   * its default value.</td>
   * <td>{@link DynUnion}</td>
   * </tr>
   * <tr>
   * <td>Value, ValueBox</td>
   * <td>null</td>
   * <td>{@link DynValue}, {@link DynValueBox}</td>
   * </tr>
   * <tr>
   * <td>TypeCode</td>
   * <td>Typecode of kind <code>TCKind.tk_null</code></td>
   * <td>{@link DynValue}, {@link DynValueBox}</td>
   * </tr>
   *
   * </table>
   *
   * @param type the type of the data being stored.
   *
   * @return the created DynAny, having the passed type.
   *
   * @throws InconsistentTypeCode if type.kind() is tk_Principal, tk_native or
   * tk_abstract_interface. These types cannot be stored in DynAny.
   */
  DynAny create_dyn_any_from_type_code(TypeCode type)
    throws InconsistentTypeCode;

  /**
   * Create DynAny using the given Any as template.
   *
   * @param value the Any, providing type and value for the DynAny being
   * created.
   *
   * @return the created DynAny, having the same type and storing the same value
   * as the passed Any.
   *
   * @throws InconsistentTypeCode if value.type().kind() is tk_Principal,
   * tk_native or tk_abstract_interface. These types cannot be stored in DynAny.
   */
  DynAny create_dyn_any(Any value)
    throws InconsistentTypeCode;
}
