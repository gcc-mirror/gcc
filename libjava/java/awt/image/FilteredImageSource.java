/* FilteredImageSource.java -- Java class for providing image data 
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.awt.image;

import java.util.Hashtable;

/**
 *
 * @see ImageConsumer
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public class FilteredImageSource implements ImageProducer
{
    ImageProducer ip;
    ImageFilter filter;
    Hashtable consumers = new Hashtable();

    /**
     * The given filter is applied to the given image producer
     * to create a new image producer.  
     */
    public FilteredImageSource(ImageProducer ip, ImageFilter filter) {
	this.ip = ip;
	this.filter = filter;
    }

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code>.  
     */
    public synchronized void addConsumer(ImageConsumer ic) {
	if (consumers.containsKey(ic))
	    return;

	ImageFilter f = filter.getFilterInstance(ic);
	consumers.put(ic, f);
	ip.addConsumer(f);
    }

    /**
     * Used to determine if the given <code>ImageConsumer</code> is
     * already registered with this <code>ImageProducer</code>.  
     */
    public synchronized boolean isConsumer(ImageConsumer ic) {
	ImageFilter f = (ImageFilter)consumers.get(ic);
	if (f != null)
	    return ip.isConsumer(f);
	return false;
    }

    /**
     * Used to remove an <code>ImageConsumer</code> from the list of
     * registered consumers for this <code>ImageProducer</code>.  
     */
    public synchronized void removeConsumer(ImageConsumer ic) {
	ImageFilter f = (ImageFilter)consumers.remove(ic);
	if (f != null)
	    ip.removeConsumer(f);
    }

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code> and then immediately start
     * reconstruction of the image data to be delivered to all
     * registered consumers.  
     */
    public void startProduction(ImageConsumer ic) {
	ImageFilter f;
	if (!(consumers.containsKey(ic))) {
	    f = filter.getFilterInstance(ic);
	    consumers.put(ic, f);
	    ip.addConsumer(f);
	} else { 
	    f = (ImageFilter)consumers.get( ic );
	}
	ip.startProduction(f);
    }

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code> and then request that this producer
     * resend the image data in the order top-down, left-right.  
     */
    public void requestTopDownLeftRightResend(ImageConsumer ic) {
	ImageFilter f = (ImageFilter)consumers.get(ic);
	ip.requestTopDownLeftRightResend(f);
    }
}

