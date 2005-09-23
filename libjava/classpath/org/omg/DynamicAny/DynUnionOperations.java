/* DynUnionOperations.java --
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

import org.omg.CORBA.TCKind;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

/**
 * Defines the operations, applicable to the DynUnion. The DynUnion has only two
 * valid positions:
 * <ul>
 * <li>0 - contains the discriminator of the union. The discriminator defines,
 * which of the union variants is currently active.</li>
 * <li>1 - contains the currently active variant of the union content (a union
 * member). </li>
 * </ul>
 * The size of the union is normally 2. If the discriminator value defines no
 * valid variant, the union consists of discriminator only, having the size 1.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynUnionOperations
  extends DynAnyOperations
{
  /**
   * <p>Get the value of discriminator, defining which content variant
   * (member) is active.
   * </p><p>
   * In the current implementation, the later changes on the returned value
   * alter the state of the union via implemented internal listener.
   * </p>
   */
  DynAny get_discriminator();

  /**
   * <p>Set the value of discriminator, activating the member variant that is
   * consistent with the discriminator value. If the current member variant
   * matches the discriminator being set, it is unchanged. Otherwise, it is
   * replaced by the matching member variant with fields, initialised to default
   * values. The current position is set to 0 if the discriminator value does
   * not match any member variant. Otherwise, the current position is set to 1,
   * index of the member variant.
   * </p>
   * @throws TypeMismatch if the discriminator has a wrong type of this union.
   */
  void set_discriminator(DynAny aDiscriminator)
                  throws TypeMismatch;

  /**
   * Get the kind of the union descriminator.
   *
   * @return the TCKind value of the discriminator typecode.
   */
  TCKind discriminator_kind();

  /**
   * Get the current variant of the union content.
   *
   * @return the current member of the union. This reference is only valid as
   * long as the current member does not change.
   *
   * @throws InvalidValue if the union has no active member.
   */
  DynAny member()
         throws InvalidValue;

  /**
   * Returns the kind of the currently active union member.
   *
   * @return the TCKind value of the union member.
   *
   * @throws InvalidValue if the union has no active member.
   */
  TCKind member_kind()
              throws InvalidValue;

  /**
   * Returns the name of the currently active union member.
   *
   * @return the TCKind value of the union member.
   *
   * @throws InvalidValue if the union has no active member.
   */
  String member_name()
              throws InvalidValue;

  /**
   * Returns true if the union has no active member. This happens if If the
   * discriminator value defines no valid variant. Such union consists of
   * discriminator only, having the size 1.
   */
  boolean has_no_active_member();

  /**
   * Set the discriminator to default value. The current position is set to 0.
   * This also sets the content variant to the default variant, and the size of
   * the union becomes 2.
   *
   * @throws TypeMismatch if the default case is not defined for this union.
   */
  void set_to_default_member()
                      throws TypeMismatch;

  /**
   * Set the discriminator to value that does not correspond any content variant
   * (any union <code>case</code> label). The current position is set to 0.
   * The size of the union becomes 0.
   *
   * @throws TypeMismatch if the union has explicit default case.
   */
  void set_to_no_active_member()
                        throws TypeMismatch;
}