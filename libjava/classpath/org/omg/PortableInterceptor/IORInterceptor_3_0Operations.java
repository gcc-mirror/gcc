/* IORInterceptor_3_0Operations.java --
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


package org.omg.PortableInterceptor;

/**
 * Defines the operations, applicable to the IORInterceptor_3_0.
 * 
 * @since 1.5 
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface IORInterceptor_3_0Operations
  extends IORInterceptorOperations
{
  /**
   * This method is invoked on all registered IORInterceptor_3_0 instances when
   * the state of the adapter manager changes. 
   * 
   * @param adapterManagerId the Id of the adapter manager that has changed the
   * state. The same value is returned by 
   * {@link IORInfoOperations#manager_id()}.
   * 
   * @param adapterState the new state of the adapter manager, one of the
   * {@link HOLDING#value}, {@link DISCARDING#value}, {@link INACTIVE#value}
   * or {@link NON_EXISTENT#value}.
   */
  void adapter_manager_state_changed(int adapterManagerId, short adapterState);

  /**
   * Notifies the interceptor about the adapter state changes that are unrelated
   * to adapter manager state changes. This method is invoked on all registered
   * IORInterceptor_3_0 instances. The only currently possible change of state
   * is when POA is destroyed. In this case, the method is invoked passing the
   * single element array witn the reference template of the POA being destroyed
   * and the {@link NON_EXISTENT#value} state.
   * 
   * @param adapters identifies the object adapters that have changed they
   * state.
   * @param adaptersState the new state of the adapters, one of the
   * {@link HOLDING#value}, {@link DISCARDING#value}, {@link INACTIVE#value}
   * or {@link NON_EXISTENT#value}.
   */
  void adapter_state_changed(ObjectReferenceTemplate[] adapters,
    short adaptersState);

  /**
   * This metod is invoked after the
   * {@link IORInterceptorOperations#establish_components} have been called on
   * all registered interceptor instances. At this stage, it is possible to set
   * the object reference factory using
   * {@link IORInfo#current_factory(ObjectReferenceFactory )}.
   */
  void components_established(IORInfo info);
}