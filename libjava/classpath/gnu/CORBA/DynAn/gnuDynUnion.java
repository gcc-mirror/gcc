/* gnuDynUnion.java --
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

import gnu.CORBA.Unexpected;

import org.omg.CORBA.Any;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynUnion;

import java.io.Serializable;

/**
 * Implementation of DynUnion.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynUnion
  extends DivideableAny
  implements DynUnion, Serializable, ValueChangeListener
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The discrimintor of this union.
   */
  DynAny discriminator;

  /**
   * The message string that occurs several times throwing exception.
   */
  static String NOAM = "No active member";

  /**
   * Create a new instance with the given typecode.
   *
   * @param aType the final_type, must be final_type of the union.
   */
  public gnuDynUnion(TypeCode oType, TypeCode aType, gnuDynAnyFactory aFactory,
                     ORB anOrb
                    )
              throws InconsistentTypeCode
  {
    super(oType, aType, aFactory, anOrb);
    try
      {
        discriminator =
          factory.create_dyn_any_from_type_code(final_type.discriminator_type());

        ((AbstractAny) discriminator).listener = this;

        if (final_type.default_index() >= 0)
          set_to_default_member();
        else
          set_to_no_active_member();
      }
    catch (Exception ex)
      {
        InconsistentTypeCode inc = new InconsistentTypeCode("discriminator");
        inc.initCause(ex);
        throw inc;
      }
  }

  /*
   * (non-Javadoc)
   *
   * @see gnu.CORBA.DynAn.DivideableAny#to_any()
   */
  public Any to_any()
  {
    Any a = createAny();
    OutputStream ou = a.create_output_stream();
    discriminator.to_any().write_value(ou);
    if (array.length == 2)
      array [ 1 ].to_any().write_value(ou);
    a.read_value(ou.create_input_stream(), final_type);
    return a;
  }

  /**
   * Assign from another identical structure.
   */
  public void assign(DynAny from)
              throws TypeMismatch
  {
    checkType(official_type, from.type());
    if (!(from instanceof DynUnion))
      throw new TypeMismatch("DynUnion required");
    else
      {
        try
          {
            DynUnion u = (DynUnion) from;
            discriminator.assign(u.get_discriminator());
            if (u.has_no_active_member())
              {
                if (array.length != 1)
                  array = new DynAny[] { discriminator };
              }
            else
              {
                if (array.length != 2)
                  array = new DynAny[] { discriminator, u.member().copy() };
                else
                  array [ 1 ] = u.member().copy();
              }
          }
        catch (InvalidValue e)
          {
            throw new Unexpected(e);
          }
      }
    valueChanged();
  }

  /** @inheritDoc */
  public DynAny copy()
  {
    try
      {
        gnuDynUnion other =
          new gnuDynUnion(official_type, final_type, factory, orb);
        other.discriminator = discriminator.copy();
        ((AbstractAny) other.discriminator).listener = other;
        if (array.length == 1)
          {
            other.array = new DynAny[] { other.discriminator };
          }
        else
          {
            other.array =
              new DynAny[] { other.discriminator, array [ 1 ].copy() };
          }
        return other;
      }
    catch (InconsistentTypeCode ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Done via reading from stream.
   */
  public void from_any(Any an_any)
                throws TypeMismatch, InvalidValue
  {
    checkType(official_type, an_any.type());

    Any adis = createAny();
    try
      {
        InputStream stream = an_any.create_input_stream();
        adis.read_value(stream, final_type.discriminator_type());

        DynAny nd = factory.create_dyn_any(adis);

        set_discriminator(nd);
        if (array.length == 2)
          {
            // Reusing the same Any <code>adis</code>.
            adis.read_value(stream, array [ 1 ].type());
            array [ 1 ].from_any(adis);
          }
      }
    catch (InconsistentTypeCode it)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(it);
        throw t;
      }
    catch (MARSHAL m)
      {
        InvalidValue t = new InvalidValue();
        t.initCause(m);
        throw t;
      }
    catch (BadKind b)
      {
        throw new Unexpected(b);
      }
    valueChanged();
  }

  /** @inheritDoc */
  public TCKind discriminator_kind()
  {
    return discriminator.type().kind();
  }

  /** @inheritDoc */
  public DynAny get_discriminator()
  {
    return discriminator;
  }

  /** @inheritDoc */
  public boolean has_no_active_member()
  {
    return array.length == 1;
  }

  /** @inheritDoc */
  public TCKind member_kind()
                     throws InvalidValue
  {
    return member().type().kind();
  }

  /**
   * Get the name of the current variant of the union.
   */
  public String member_name()
                     throws InvalidValue
  {
    if (array.length == 1)
      throw new InvalidValue(NOAM);
    try
      {
        Any da = discriminator.to_any();


        // Get the discriminator variant.
        Variants:
        for (int i = 0; i < final_type.member_count(); i++)
          {
            if (final_type.member_label(i).equal(da))
              return final_type.member_name(i);
          }
        throw new InvalidValue(NOAM);
      }
    catch (Exception e)
      {
        InvalidValue t = new InvalidValue("Err");
        t.initCause(e);
        throw t;
      }
  }

  /** @inheritDoc */
  public DynAny member()
                throws InvalidValue
  {
    if (array.length < 2)
      throw new InvalidValue(NOAM);
    else
      return array [ 1 ];
  }

  /**
   * Set the union discriminator.
   */
  public void set_discriminator(DynAny aDiscriminator)
                         throws TypeMismatch
  {
    try
      {
        if (!aDiscriminator.type().equal(final_type.discriminator_type()))
          throw new TypeMismatch("Wrong discriminator final_type for " +
                                 final_type.name()
                                );

        // Seting the same discriminator value again should not change
        // the fields of the current member.
        if (!discriminator.equal(aDiscriminator))
          {
            discriminator.assign(aDiscriminator);
            updateMember();
          }
        else
          {
            pos = array.length == 2 ? 1 : 0;
          }
      }
    catch (Exception e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
  }

  /**
   * Set to default member, if one exists.
   */
  public void set_to_default_member()
                             throws TypeMismatch
  {
    try
      {
        int di = final_type.default_index();
        if (di < 0)
          throw new TypeMismatch("Union " + final_type.name() +
                                 "has no default index"
                                );

        Any da = final_type.member_label(di);
        discriminator.from_any(da);
        updateMember();
      }
    catch (TypeMismatch m)
      {
        // This one OK.
        throw m;
      }
    catch (Exception e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
  }

  /** @inheritDoc */
  public void set_to_no_active_member()
                               throws TypeMismatch
  {
    try
      {
        if (final_type.default_index() >= 0)
          {
            throw new TypeMismatch("Explicit default case defined.");
          }
      }
    catch (BadKind ex)
      {
        // The default index is not set.
      }
    array = new DynAny[] { discriminator };
    valueChanged();
  }

  /**
   * Update member, in accordance with discriminator value.
   */
  public void updateMember()
                    throws TypeMismatch
  {
    try
      {
        Any da = discriminator.to_any();


        // Get the discriminator variant.
        Variants:
        for (int i = 0; i < final_type.member_count(); i++)
          {
            if (final_type.member_label(i).equal(da))
              {
                array =
                  new DynAny[]
                  {
                    discriminator,
                    factory.create_dyn_any_from_type_code(final_type.member_type(i))
                  };
                pos = 1;
                valueChanged();
                return;
              }
          }
      }
    catch (Exception e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }

    // Discrimintator does not point to valid member.
    array = new DynAny[] { discriminator };
    pos = 0;
    valueChanged();
  }

  /**
   * Called when the discriminator is changed.
   */
  public void changed()
  {
    try
      {
        updateMember();
      }
    catch (TypeMismatch ex)
      {
        throw new Unexpected(ex);
      }
  }
}