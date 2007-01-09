/* Component.java -- a component log level.
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under terms
of your choice, provided that you also meet, for each linked independent
module, the terms and conditions of the license of that module.  An
independent module is a module which is not derived from or based on
this library.  If you modify this library, you may extend this exception
to your version of the library, but you are not obligated to do so.  If
you do not wish to do so, delete this exception statement from your
version.  */


package gnu.classpath.debug;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.logging.Level;

public final class Component extends Level
{

  /*
   * HOW TO ADD NEW COMPONENTS:
   *
   * If you want to add a new, simple component, that you will use in
   * logging statements, simply create a new class variable that
   * instantiates this class, and choose an appropriate string name
   * and a integer constant not used by any other component level.
   *
   * For example, if my component had to do with 'frobbing', I would
   * add this entry below:
   *
   *   private static final Component FROBBING = new Component ("FROBBING", 7);
   *
   * Then, I would update the component 'EVERYTHING' to have and end
   * index ONE GREATER THAN the index of the new component.
   *
   * ADDING NEW COMPONENT CLASSES:
   *
   * A "component class" is a run of more than one component, which can
   * be enabled all at once. EVERYTHING and SSL are examples of component
   * classes. To add a new class, create a new component with a start index
   * equal to the index of the first member component, and with an end
   * index equal to the index of the last member component plus one.
   */

  /**
   * Signifies that everything should be logged. This should be used to
   * enable or disable levels only; logging code should not use it.
   */
  public static final Component EVERYTHING = new Component ("*", 0, 11);

  /**
   * Signifies that all SSL related messages should be logged. This should
   * be used to enable or disable levels only; logging code should not use
   * it.
   */
  public static final Component SSL = new Component ("SSL", 0, 5);

  /**
   * Traces the progression of an SSL handshake.
   */
  public static final Component SSL_HANDSHAKE = new Component ("SSL HANDSHAKE", 0);

  /**
   * Traces record layer messages during SSL communications.
   */
  public static final Component SSL_RECORD_LAYER = new Component ("SSL RECORD LAYER", 1);

  /**
   * Trace details about the SSL key exchange.
   */
  public static final Component SSL_KEY_EXCHANGE = new Component ("SSL KEY EXCHANGE", 2);
  
  /**
   * Trace running of delegated tasks.
   */
  public static final Component SSL_DELEGATED_TASK = new Component ("SSL DELEGATED TASK", 3);

  /* Index 4 reserved for future use by SSL components. */

  /**
   * Trace the operation of cryptographic primitives.
   */
  public static final Component CRYPTO = new Component ("CRYPTO", 5);

  /**
   * Trace the parsing of X.509 certificates and related objects.
   */
  public static final Component X509 = new Component ("X.509", 6);

  /**
   * Trace access control policies, including the parsing of
   * java.policy files.
   */
  public static final Component POLICY = new Component ("POLICY", 7);
  
  /**
   * Trace ipp implementation.
   */
  public static final Component IPP = new Component ("IPP", 10);

  private final int startIndex;
  private final int endIndex;

  private Component (final String name, final int bitIndex)
  {
    this (name, bitIndex, bitIndex + 1);
  }

  private Component (final String name, final int startIndex, final int endIndex)
  {
    super (name, Level.FINE.intValue ());
    this.startIndex = startIndex;
    this.endIndex = endIndex;
  }

  /**
   * Return the component for the given name.
   *
   * @param name The name of the component to get.
   * @return The named component, or null if there is no such component.
   */
  public static Component forName (final String name)
  {
    try
      {
	Field f = Component.class.getField (name.toUpperCase ());
	if (!Modifier.isStatic (f.getModifiers ())
	    || Component.class.isAssignableFrom (f.getClass ()))
	  return null;
	return (Component) f.get (null);
      }
    catch (Throwable _)
      {
	return null;
      }
  }

  public int startIndex ()
  {
    return startIndex;
  }

  public int endIndex ()
  {
    return endIndex;
  }
}