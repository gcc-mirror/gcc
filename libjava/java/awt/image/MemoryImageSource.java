/* MemoryImageSource.java -- Java class for providing image data 
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

import java.awt.Image;
import java.util.Enumeration;
import java.util.Hashtable;

public class MemoryImageSource implements ImageProducer 
{
    private boolean animated = false;
    private boolean fullbuffers = false;
    private int pixeli[], width, height, offset, scansize;
    private byte pixelb[];
    private ColorModel cm;
    private Hashtable props, consumers = new Hashtable();

    /**
       Constructs an ImageProducer from memory
    */
    public MemoryImageSource(int w, int h, ColorModel cm,
			     byte pix[], int off, int scan)
    {
	this ( w, h, cm, pix, off, scan, null );
    }
    /**
       Constructs an ImageProducer from memory
    */
    public MemoryImageSource( int w, int h, ColorModel cm,
			      byte pix[], int off, int scan,
			      Hashtable props)
    {
	width = w;
	height = h;
	this.cm = cm;
	offset = off;
	scansize = scan;
	this.props = props;
	int max = (( scansize > width ) ? scansize : width );
	pixelb = new byte[ max  * height ];
	System.arraycopy( pix, 0, pixelb, 0, max * height );
    }
    /**
       Constructs an ImageProducer from memory
    */
    public MemoryImageSource(int w, int h, ColorModel cm,
			     int pix[], int off, int scan)
    {
	this ( w, h, cm, pix, off, scan, null );
    }

    /**
       Constructs an ImageProducer from memory
    */
    public MemoryImageSource(int w, int h, ColorModel cm,
			     int pix[], int off, int scan,
			     Hashtable props)
    {
	width = w;
	height = h;
	this.cm = cm;
	offset = off;
	scansize = scan;
	this.props = props;
	int max = (( scansize > width ) ? scansize : width );
	pixeli = new int[ max  * height ];
	System.arraycopy( pix, 0, pixeli, 0, max * height );
    }
    /**
       Constructs an ImageProducer from memory using the default RGB ColorModel
    */
    public MemoryImageSource(int w, int h,
			     int pix[], int off, int scan,
			     Hashtable props)
    {
	this ( w, h, ColorModel.getRGBdefault(), pix, off, scan, props);
    }

    /**
       Constructs an ImageProducer from memory using the default RGB ColorModel
    */
    public MemoryImageSource(int w, int h,
			     int pix[], int off, int scan)
    {
	this ( w, h, ColorModel.getRGBdefault(), pix, off, scan, null);
    }

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code>.  
     */
    public synchronized void addConsumer(ImageConsumer ic) {
	if (consumers.containsKey(ic))
	    return;

	consumers.put(ic, ic);
    }

    /**
     * Used to determine if the given <code>ImageConsumer</code> is
     * already registered with this <code>ImageProducer</code>.  
     */
    public synchronized boolean isConsumer(ImageConsumer ic) {
	if (consumers.containsKey(ic))
	    return true;
	return false;
    }

    /**
     * Used to remove an <code>ImageConsumer</code> from the list of
     * registered consumers for this <code>ImageProducer</code>.  
     */
    public synchronized void removeConsumer(ImageConsumer ic) {
	consumers.remove(ic);
    }

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code> and then immediately start
     * reconstruction of the image data to be delivered to all
     * registered consumers.  
     */
    public void startProduction(ImageConsumer ic) {
	if (!(consumers.containsKey(ic))) {
	    consumers.put(ic, ic);
	}        
	Enumeration e = consumers.elements();
	for( ; e.hasMoreElements(); ) {
		ic = (ImageConsumer)e.nextElement();
		sendPicture( ic );
		ic.imageComplete( ImageConsumer.SINGLEFRAME );
	    }	

    }

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code> and then request that this producer
     * resend the image data in the order top-down, left-right.  
     */
    public void requestTopDownLeftRightResend(ImageConsumer ic) {
	startProduction ( ic );
    }


    /**
       Changes a flag to indicate whether this MemoryImageSource supports
       animations.

       @param animated A flag indicating whether this class supports animations
     */    
    public synchronized void setAnimated(boolean animated)
    {
	this.animated = animated;
    }


    /**
       A flag to indicate whether or not to send full buffer updates when
       sending animation. If this flag is set then full buffers are sent
       in the newPixels methods instead of just regions.

       @param fullbuffers - a flag indicating whether to send the full buffers 
     */
    public synchronized void setFullBufferUpdates(boolean fullbuffers)
    {
	this.fullbuffers = fullbuffers;
    }

    /**
       Send an animation frame to the image consumers.
     */
    public void newPixels()
    {
	if( animated == true ) {
		ImageConsumer ic;
		Enumeration e = consumers.elements();
		for( ; e.hasMoreElements(); ) {
			ic = (ImageConsumer)e.nextElement();
			sendPicture( ic );
			ic.imageComplete( ImageConsumer.SINGLEFRAME );
		    }	
	    }
    }

    
    private void sendPicture ( ImageConsumer ic )
    {
	ic.setHints( ImageConsumer.TOPDOWNLEFTRIGHT );
	if( props != null ) {
	    ic.setProperties( props );
	}
	ic.setDimensions(width, height);
	if( pixeli != null ) {
	    ic.setPixels( 0, 0, width, height, cm, pixeli, offset, scansize );
	} else {
	    ic.setPixels( 0, 0, width, height, cm, pixelb, offset, scansize );
	}
    }

    /**
       Send an animation frame to the image consumers containing the specified
       pixels unless setFullBufferUpdates is set.
     */
    public synchronized void newPixels(int x,
				       int y,
				       int w,
				       int h)
    {
	if( animated == true )
	    {
		if( fullbuffers ) {
		    newPixels();
		} else {
		    ImageConsumer ic;
		    Enumeration e = consumers.elements();
		    for( ; e.hasMoreElements(); ) {
			    ic = (ImageConsumer)e.nextElement();
			    ic.setHints( ImageConsumer.TOPDOWNLEFTRIGHT );
			    if( props != null ) {
				ic.setProperties( props );
			    }
			    if( pixeli != null ) {
				ic.setPixels( 0, 0, width, height, cm, pixeli, offset, scansize );
			    } else {
				ic.setPixels( 0, 0, width, height, cm, pixelb, offset, scansize );
			    }
			    ic.imageComplete( ImageConsumer.SINGLEFRAME );
		    }
		}     
	    }
    }



    /**
       Send an animation frame to the image consumers containing the specified
       pixels unless setFullBufferUpdates is set.

       If framenotify is set then a notification is sent when the frame 
       is sent otherwise no status is sent.
     */
    public synchronized void newPixels(int x,
				       int y,
				       int w,
				       int h,
				       boolean framenotify)
    {
	if( animated == true )
	    {
		if( fullbuffers ) {
		    newPixels();
		} else {
		    ImageConsumer ic;
		    Enumeration e = consumers.elements();
		    for( ; e.hasMoreElements(); ) {
			    ic = (ImageConsumer)e.nextElement();
			    ic.setHints( ImageConsumer.TOPDOWNLEFTRIGHT );
			    if( props != null ) {
				ic.setProperties( props );
			    }
			    if( pixeli != null ) {
				ic.setPixels( 0, 0, width, height, cm, pixeli, offset, scansize );
			    } else {
				ic.setPixels( 0, 0, width, height, cm, pixelb, offset, scansize );
			    }
			    if( framenotify == true )
				ic.imageComplete( ImageConsumer.SINGLEFRAME );
		    }
		}     
	    }
    }

    public synchronized void newPixels(byte newpix[],
				       ColorModel newmodel,
				       int offset,
				       int scansize)

    {
	if( animated == true )
	    {
		//FIXME
	    }
    }

    public synchronized void newPixels(int newpix[],
				       ColorModel newmodel,
				       int offset,
				       int scansize)

    {
	if( animated == true )
	    {
		//FIXME
	    }
    }

}
