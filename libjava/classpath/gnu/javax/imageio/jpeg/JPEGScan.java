/* JPEGScan.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.javax.imageio.jpeg;

import java.util.ArrayList;

public class JPEGScan
{
  private int maxHeight = 0, maxWidth = 0, maxV = 0, maxH = 0;
  private int numOfComponents = 0, numOfComponentBlocks = 0;
  private ArrayList components = new ArrayList();

  public JPEGScan()
  {
    // Nothing to do here.
  }

  public JPEGScan(int h, int w)
  {
    maxHeight=h;
    maxWidth=w;
  }

  private void recalculateDimensions()
  {
    JPEGComponent comp;

    // Compute the maximum H, maximum V factors defined in Annex A of the ISO
    // DIS 10918-1.
    for(int i=0; i < components.size() ; i++)
      {
        comp = (JPEGComponent)components.get(i);
        if(comp.factorH > maxH)
          maxH=comp.factorH;
        if(comp.factorV > maxV)
          maxV=comp.factorV;
      }

    for(int i=0; i < components.size() ; i++)
      {
        comp = (JPEGComponent)components.get(i);
        comp.maxH = maxH;
        comp.maxV = maxV;
      }

  }

  public void addComponent(byte id, byte factorHorizontal, byte factorVertical,
                           byte quantizationID)
  {
    JPEGComponent component = new JPEGComponent(id, factorHorizontal, factorVertical, quantizationID);
    components.add((Object)component);
    recalculateDimensions();
    numOfComponents++;
    numOfComponentBlocks += factorHorizontal*factorVertical;
  }

  public JPEGComponent getComponentByID(byte id)
  {
    JPEGComponent comp = (JPEGComponent)components.get(0);
    for(int i=0; i < components.size() ; i++)
      {
        comp=(JPEGComponent)components.get(i);
        if(comp.component_id==id)
          break;
      }
    return(comp);
  }

  public JPEGComponent get(int id)
  {
    return((JPEGComponent)components.get(id));
  }

  public int getX(byte id)
  {
    JPEGComponent comp = getComponentByID(id);
    return(comp.width);
  }

  public int getY(byte id)
  {
    JPEGComponent comp = getComponentByID(id);
    return(comp.height);
  }

  public int getMaxV()
  {
    return(maxV);
  }

  public int getMaxH()
  {
    return(maxH);
  }

  public void setWidth(int w)
  {
    maxWidth=w;
  }

  public void setHeight(int h)
  {
    maxHeight=h;
  }

  public int size()
  {
    return(numOfComponents);
  }

  public int sizeComponentBlocks()
  {
    return(numOfComponentBlocks);
  }
}
