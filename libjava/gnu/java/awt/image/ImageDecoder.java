/* ImageDecoder.java
   Copyright (C) 1999, 2000, 2004  Free Software Foundation, Inc.

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

package gnu.java.awt.image;

import java.awt.image.ImageConsumer;
import java.awt.image.ImageProducer;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Vector;

public abstract class ImageDecoder implements ImageProducer 
{
  Vector consumers = new Vector ();
  String filename;
  URL url;
  byte[] data;
  int offset;
  int length;
  InputStream input;

  static
  {
    // FIXME: there was some broken code here that looked like
    // it wanted to rely on this property.  I don't have any idea
    // what it was intended to do.
    // String endian = System.getProperties ().getProperty ("gnu.cpu.endian");
  }

  public ImageDecoder (String filename)
  {
    this.filename = filename;
  }

  public ImageDecoder (URL url)
  {
    this.url = url;
  }

  public ImageDecoder (InputStream is)
  {
    this.input = is;
  }

  public ImageDecoder (byte[] imagedata, int imageoffset, int imagelength)
  {
    data = imagedata;
    offset = imageoffset;
    length = imagelength;
  }

  public void addConsumer (ImageConsumer ic) 
  {
    consumers.addElement (ic);
  }

  public boolean isConsumer (ImageConsumer ic)
  {
    return consumers.contains (ic);
  }
  
  public void removeConsumer (ImageConsumer ic)
  {
    consumers.removeElement (ic);
  }

  public void startProduction (ImageConsumer ic)
  {
    if (!isConsumer(ic))
      addConsumer(ic);

    Vector list = (Vector) consumers.clone ();
    try 
      {
	// Create the input stream here rather than in the
	// ImageDecoder constructors so that exceptions cause
	// imageComplete to be called with an appropriate error
	// status.
        if (input == null)
          {
            try 
              {
                if (url != null)
                  input = url.openStream();
                else
                  {
                    if (filename != null)
                      input = new FileInputStream (filename);
                    else
                      input = new ByteArrayInputStream (data, offset, length);
                  }
                produce (list, input);
              } 
            finally 
              {
                input = null;
              }
          }
        else
          {
            produce (list, input);
          }
      }
    catch (Exception e)
      {
	for (int i = 0; i < list.size (); i++)
	  {
	    ImageConsumer ic2 = (ImageConsumer) list.elementAt (i);
	    ic2.imageComplete (ImageConsumer.IMAGEERROR);
	  }
      }
  }

  public void requestTopDownLeftRightResend (ImageConsumer ic) 
  { 
  }

  public abstract void produce (Vector v, InputStream is) throws IOException;
}
