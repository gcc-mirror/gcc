/* gnuDynAnyFactory.java --
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

import gnu.CORBA.Poa.ORB_1_4;
import gnu.CORBA.Unexpected;
import gnu.CORBA.HolderLocator;
import gnu.CORBA.TypeKindNamer;

import org.omg.CORBA.Any;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.Streamable;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactory;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynArray;
import org.omg.DynamicAny.DynEnum;
import org.omg.DynamicAny.DynFixed;
import org.omg.DynamicAny.DynSequence;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.DynUnion;
import org.omg.DynamicAny.DynValue;
import org.omg.DynamicAny.DynValueBox;

/**
 * This class is returned by ORB when resolving
 * initial reference "DynAnyFactory".
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynAnyFactory
  extends LocalObject
  implements DynAnyFactory
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The ORB, to that the factory belongs.
   */
  final ORB_1_4 orb;

  /**
   * Create a new factory, specifying the ORB to that the factory belongs.
   *
   * @param anOrb
   */
  public gnuDynAnyFactory(ORB_1_4 anOrb)
  {
    orb = anOrb;
  }

  /**
   * Get the orb.
   */
  public ORB_1_4 getOrb()
  {
    return orb;
  }

  /**
   * Create an initialised array.
   */
  public DynArray create_array(TypeCode official, TypeCode type)
  {
    return new gnuDynArray(official, type, this, orb, true);
  }

  /**
   * Create an empty sequence.
   */
  public DynSequence create_sequence(TypeCode official, TypeCode type)
  {
    return new gnuDynSequence(official, type, this, orb);
  }

  /**
   * Create structure.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynStruct create_structure(TypeCode official, TypeCode type)
  {
    return new gnuDynStruct(official, type, this, orb);
  }

  /**
   * Create union.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynUnion create_union(TypeCode official, TypeCode type)
  {
    try
      {
        return new gnuDynUnion(official, type, this, orb);
      }
    catch (Exception ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Create value.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynValue create_value(TypeCode official, TypeCode type)
  {
    return new gnuDynValue(official, type, this, orb);
  }

  /**
   * Create value box.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynValueBox create_value_box(TypeCode official, TypeCode type)
  {
    return new gnuDynValueBox(official, type, this, orb);
  }

  /**
   * Create enumeration.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynEnum create_enumeration(TypeCode official, TypeCode type)
  {
    return new gnuDynEnum(official, type, this, orb);
  }

  /**
   * Create fixed.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynFixed create_fixed(TypeCode official, TypeCode type)
  {
    return new gnuDynFixed(official, type, this, orb);
  }

  /**
   * Create alias.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynAny create_alias(TypeCode official, TypeCode type)
                      throws InconsistentTypeCode
  {
    try
      {
        return create_dyn_any_from_type_code(official, type.content_type());
      }
    catch (BadKind e)
      {
        throw new Unexpected(e);
      }
  }

  /**
   * Create the undivideable DynAny.
   */
  public DynAny create_simple(TypeCode official, TypeCode type)
  {
    Streamable holder = HolderLocator.createHolder(type);
    return new gnuDynAny(holder, official, type, this, orb);
  }

  /**
   * Create the DynAny from typecode.
   */
  public DynAny create_dyn_any_from_type_code(TypeCode type)
                                       throws InconsistentTypeCode
  {
    return create_dyn_any_from_type_code(type, type);
  }

  /**
   * Create the DynAny from typecode.
   *
   * @param official the type that was originally passed as a parameter by user.
   * May be alias of some other type.
   * @param type the type into that the "official type" evaluates during alias
   * resolving. Initially equal to "official type".
   */
  public DynAny create_dyn_any_from_type_code(TypeCode official, TypeCode type)
                                       throws InconsistentTypeCode
  {
    DynAny d;
    try
      {
        switch (type.kind().value())
          {
            case TCKind._tk_array :
              return create_array(official, type);

            case TCKind._tk_sequence :
              return create_sequence(official, type);

            case TCKind._tk_struct :
            case TCKind._tk_except :
              return create_structure(official, type);

            case TCKind._tk_union :
              return create_union(official, type);

            case TCKind._tk_value :
              return create_value(official, type);

            case TCKind._tk_value_box :
              return create_value_box(official, type);

            case TCKind._tk_enum :
              return create_enumeration(official, type);

            case TCKind._tk_fixed :
              return create_fixed(official, type);

            case TCKind._tk_alias :
              return create_alias(official, type);

            case TCKind._tk_null :
              return new gnuDynAny(null, official, type, this, orb);

            case TCKind._tk_TypeCode :
              d = create_simple(official, type);
              d.insert_typecode(orb.get_primitive_tc(TCKind.tk_null));
              return d;

            case TCKind._tk_any :
              d = create_simple(official, type);

              Any empty_any = orb.create_any();
              empty_any.type(orb.get_primitive_tc(TCKind.tk_null));
              d.insert_any(empty_any);
              return d;

            case TCKind._tk_wstring :
              d = create_simple(official, type);
              d.insert_wstring("");
              return d;

            case TCKind._tk_string :
              d = create_simple(official, type);
              d.insert_string("");
              return d;

            case TCKind._tk_native :
            case TCKind._tk_Principal :
            case TCKind._tk_abstract_interface :
              throw new InconsistentTypeCode("Following API, the " +
                                             TypeKindNamer.nameIt(type) +
                                             " must not be supported."
                                            );

            default :
              return create_simple(official, type);
          }
      }
    catch (UserException uex)
      {
        InconsistentTypeCode it = new InconsistentTypeCode();
        it.initCause(uex);
        throw it;
      }
  }

  /**
   * Create the DynAny using the passed value as template and assign this value.
   */
  public DynAny create_dyn_any(Any value)
                        throws InconsistentTypeCode
  {
    DynAny created = create_dyn_any_from_type_code(value.type());
    try
      {
        created.from_any(value);
      }
    catch (UserException uex)
      {
        InconsistentTypeCode t = new InconsistentTypeCode("Inconsistent Any");
        t.initCause(uex);
        throw t;
      }
    catch (Exception e)
      {
        throw new Unexpected(e);
      }
    return created;
  }
}