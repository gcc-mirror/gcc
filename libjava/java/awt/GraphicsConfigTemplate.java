/* GraphicsConfigTemplate.java -- a template for selecting configurations
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt;

import java.io.Serializable;

/**
 * This allows filtering an array of GraphicsConfigurations for the best
 * one based on various requirements. The resulting configuration has had
 * all non-default attributes set as required to meet or exceed the request.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see GraphicsConfiguration
 * @see GraphicsDevice
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class GraphicsConfigTemplate implements Serializable
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = -8061369279557787079L;

  /** States that a feature is required to select a configuration. */
  public static final int REQUIRED = 1;

  /**
   * States that a feature is preferred, but not required, to select a
   * configuration. In the case of multiple valid configurations, the tie
   * breaks in favor of the one with the feature.
   */
  public static final int PREFERRED = 2;

  /**
   * States that a feature is not necessary in the configuration. In the case
   * of multiple valid configurations, the tie breaks in favor of the one
   * without the feature, to reduce overhead.
   */
  public static final int UNNECESSARY = 3;

  /**
   * The default constructor.
   */
  public GraphicsConfigTemplate()
  {
  }

  /**
   * Returns the "best" match among the array of possible configurations, given
   * the criteria of this template.
   *
   * @param array the array to choose from
   * @return the best match
   * @throws NullPointerException if array is null
   */
  public abstract GraphicsConfiguration getBestConfiguration
    (GraphicsConfiguration[] array);

  /**
   * Returns true if the given configuration supports all the features required
   * by this template.
   *
   * @param config the configuration to test
   * @return true if it is a match
   * @throws NullPointerException if config is null
   */
  public abstract boolean isGraphicsConfigSupported
    (GraphicsConfiguration config);
} // class GraphicsConfigTemplate
