/* IIOParam.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio;

import java.awt.Point;
import java.awt.Rectangle;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class IIOParam
{
  protected IIOParamController controller;
  protected IIOParamController defaultController;
  protected Point destinationOffset = new Point(0, 0);
  protected ImageTypeSpecifier destinationType;
  protected int[] sourceBands;
  protected Rectangle sourceRegion;
  protected int sourceXSubsampling;
  protected int sourceYSubsampling;
  protected int subsamplingXOffset;
  protected int subsamplingYOffset;

  /**
   * Initializes an <code>IIOParam</code> object.
   */
  protected IIOParam()
  {
    // Do nothing here.
  }

  public boolean activateController()
  {
    if (controller == null)
      return false;
    
    return controller.activate(this);
  }
  
  public IIOParamController getController()
  {
    return controller;
  }

  public IIOParamController getDefaultController()
  {
    return defaultController;
  }

  public Point getDestinationOffset()
  {
    return destinationOffset;
  }

  public ImageTypeSpecifier getDestinationType()
  {
    return destinationType;
  }

  public int[] getSourceBands()
  {
    return sourceBands;
  }

  public Rectangle getSourceRegion()
  {
    return sourceRegion;
  }

  public int getSourceXSubsampling()
  {
    return sourceXSubsampling;
  }
  
  public int getSourceYSubsampling()
  {
    return sourceYSubsampling;
  }

  public int getSubsamplingXOffset()
  {
    return subsamplingXOffset;
  }
  
  public int getSubsamplingYOffset()
  {
    return subsamplingYOffset;
  }

  public boolean hasController()
  {
    return getController() != null;
  }

  public void setController(IIOParamController controller)
  {
    this.controller = controller;
  }

  public void setDestinationOffset(Point destinationOffset)
  {
    if (destinationOffset == null)
      throw new IllegalArgumentException("destinationOffset is null");

    this.destinationOffset = destinationOffset;
  }

  public void setSourceBands(int[] sourceBands)
  {
    this.sourceBands = sourceBands;
  }

  public void setSourceRegion(Rectangle sourceRegion)
  {
    if (sourceRegion != null
	&& (sourceRegion.x < 0
	    || sourceRegion.y < 0
	    || sourceRegion.width <= 0
	    || sourceRegion.height <= 0))
      throw new IllegalArgumentException("illegal source region");
    
    // FIXME: Throw IllegalStateException.

    this.sourceRegion = sourceRegion;
  }

  public void setSourceSubsampling(int sourceXSubsampling, int sourceYSubsampling,
				   int subsamplingXOffset, int subsamplingYOffset)
  {
    this.sourceXSubsampling = sourceXSubsampling;
    this.sourceYSubsampling = sourceYSubsampling;
    this.subsamplingXOffset = subsamplingXOffset;
    this.subsamplingYOffset = subsamplingYOffset;
  }
}
