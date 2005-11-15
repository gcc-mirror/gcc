/* Reverb attributes 
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


package javax.sound.sampled;

/**
 * This represents a reverb effect which can be applied to an audio signal.
 * @since 1.3
 */
public class ReverbType
{
  private String name;
  private int earlyReflectionDelay;
  private float earlyReflectionIntensity;
  private int lateReflectionDelay;
  private float lateReflectionIntensity;
  private int decayTime;

  /**
   * Create a new ReverbType given its attributes.
   * @param name the name of this type
   * @param earlyDelay the early delay time in microseconds
   * @param earlyInten the early intensity in decibels
   * @param lateDelay the late delay time in microseconds
   * @param lateInten the late intensity in decibels
   * @param decay the decay time in microseconds
   */
  protected ReverbType(String name, int earlyDelay, float earlyInten,
		       int lateDelay, float lateInten, int decay)
  {
    this.name = name;
    this.earlyReflectionDelay = earlyDelay;
    this.earlyReflectionIntensity = earlyInten;
    this.lateReflectionDelay = lateDelay;
    this.lateReflectionIntensity = lateInten;
    this.decayTime = decay;
  }

  public final boolean equals(Object o)
  {
    return super.equals(o);
  }

  public final int hashCode()
  {
    return super.hashCode();
  }

  /**
   * Return the decay time.
   */
  public int getDecayTime()
  {
    return decayTime;
  }

  /**
   * Return the early reflection delay.
   */
  public int getEarlyReflectionDelay()
  {
    return earlyReflectionDelay;
  }

  /**
   * Return the early reflection intensity.
   */
  public float getEarlyReflectionIntensity()
  {
    return earlyReflectionIntensity;
  }

  /**
   * Return the late reflection delay.
   */
  public int getLateReflectionDelay()
  {
    return lateReflectionDelay;
  }

  /**
   * Return the late reflection intensity.
   */
  public float getLateReflectionIntensity()
  {
    return lateReflectionIntensity;
  }

  /**
   * Return the name of this ReverbType.
   * @since 1.5
   */
  public String getName()
  {
    return name;
  }

  /**
   * Return a description of this ReverbType.
   */
  public String toString()
  {
    return ("name=" + name + "; earlyReflectionDelay=" + earlyReflectionDelay
	    + "; earlyReflectionIntensity=" + earlyReflectionIntensity
	    + "; lateReflectionDelay=" + lateReflectionDelay
	    + "; lateReflectionIntensity=" + lateReflectionIntensity
	    + "; decayTime=" + decayTime);
  }
}
