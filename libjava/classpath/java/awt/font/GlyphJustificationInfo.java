/* GlyphJustificationInfo.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package java.awt.font;

/**
 * @author Michael Koch
 */
public final class GlyphJustificationInfo
{
  public static final int PRIORITY_KASHIDA = 0;
  public static final int PRIORITY_WHITESPACE = 1;
  public static final int PRIORITY_INTERCHAR = 2;
  public static final int PRIORITY_NONE = 3;

  public final float weight;
  public final int growPriority;
  public final boolean growAbsorb;
  public final float growLeftLimit;
  public final float growRightLimit;
  public final int shrinkPriority;
  public final boolean shrinkAbsorb;
  public final float shrinkLeftLimit;
  public final float shrinkRightLimit;

  public GlyphJustificationInfo (float weight, boolean growAbsorb,
                                 int growPriority, float growLeftLimit,
                                 float growRightLimit, boolean shrinkAbsorb,
                                 int shrinkPriority, float shrinkLeftLimit,
                                 float shrinkRightLimit)
  {
    this.weight = weight;
    this.growAbsorb = growAbsorb;
    this.growPriority = growPriority;
    this.growLeftLimit = growLeftLimit;
    this.growRightLimit = growRightLimit;
    this.shrinkAbsorb = shrinkAbsorb;
    this.shrinkPriority = shrinkPriority;
    this.shrinkLeftLimit = shrinkLeftLimit;
    this.shrinkRightLimit = shrinkRightLimit;
  }
}
