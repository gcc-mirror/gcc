/* AbstractAny.java --
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


package gnu.CORBA.DynAn;

import gnu.CORBA.TypeKindNamer;

import org.omg.CORBA.Any;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

import java.io.Serializable;

/**
 * The top of our DynAny implementation, this class provides ORB that is
 * required to create anys and factory that is required to initialise DynAnys.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class AbstractAny
  extends LocalObject
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The "initial final_type" that can be an alias of the known final_type.
   */
  public TypeCode official_type;

  /**
   * The "basic" final_type to that the final_type finally evaluates.
   */
  public final TypeCode final_type;

  /**
   * The DynAny factory, required in initializations.
   */
  public final gnuDynAnyFactory factory;

  /**
   * The ORB, to that this DynAny belongs.
   */
  public final ORB orb;

  /**
   * The minor code, indicating the error, related to work with non - GNU
   * Classpath DynAny.
   */
  short MINOR = 8148;

  /**
   * The message about the empty structure or exception.
   */
  static final String EMPTY = "Empty structure with no fields.";

  /**
   * The message about the structure or exception size mismatch.
   */
  static final String SIZE = "Size mismatch.";

  /**
   * The message about the content of this DynAny being equal to
   * <code>null</code>
   */
  static final String ISNULL = "The content is null";

  /**
   * The change value listener.
   */
  ValueChangeListener listener;

  /**
   * Create the abstract dyn any.
   */
  public AbstractAny(TypeCode oType, TypeCode aType,
                        gnuDynAnyFactory aFactory, ORB anOrb
                       )
  {
    official_type = oType;
    final_type = aType;
    factory = aFactory;
    orb = anOrb;
  }

  /**
   * Get the typecode.
   */
  public TypeCode type()
  {
    return official_type;
  }

  /**
   * Create the Any.
   */
  public Any createAny()
  {
    return orb.create_any();
  }

  /**
   * The "value changed" listener.
   */
  protected void valueChanged()
  {
    if (listener != null)
      listener.changed();
  }

  /**
   * Check the type.
   */
  void checkType(TypeCode expected, TypeCode actual)
          throws TypeMismatch
  {
    if (!expected.equal(actual))
      throw new TypeMismatch(typeMismatch(expected, actual));
  }

  /**
   * Format "Type mismatch" string.
   */
  String typeMismatch(TypeCode expected, TypeCode actual)
  {
    return TypeKindNamer.nameIt(expected) + " expected " +
           TypeKindNamer.nameIt(actual);
  }

  /**
   * Format "size mismatch" string.
   */
  String sizeMismatch(int here, int other)
  {
    return "Size mismatch, " + other + " (expected " + here + ")";
  }
}