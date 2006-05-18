/* BMPImageReader.java --
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

package gnu.javax.imageio.bmp;

import java.io.IOException;
import javax.imageio.*;
import javax.imageio.spi.*;
import javax.imageio.metadata.*;
import javax.imageio.stream.ImageInputStream;
import java.util.Iterator;
import java.awt.image.BufferedImage;

public class BMPImageReader extends ImageReader {
    private BMPInfoHeader infoHeader;
    private BMPFileHeader fileHeader;
    private BMPDecoder decoder;

    protected BMPImageReader(ImageReaderSpi originatingProvider){
	super(originatingProvider);
	infoHeader = null;
	fileHeader = null;
	decoder = null;
    }

    private void validateIndex(int imageIndex) 
	throws IndexOutOfBoundsException {
	if (imageIndex != 0) 
	    throw new IndexOutOfBoundsException("Invalid image index.");
    }

    public void setInput(Object input) {
	super.setInput(input);
    }

    public void setInput(Object input, 
			 boolean seekForwardOnly, 
			 boolean ignoreMetadata) {
	super.setInput(input, seekForwardOnly, ignoreMetadata);
    }
	
    public void setInput(Object input, boolean isStreamable) {
	super.setInput(input, isStreamable);
	
	if (!(input instanceof ImageInputStream))
	    throw new IllegalArgumentException("Input not an ImageInputStream.");
    }

    private void checkStream() throws IOException {
	if (!(input instanceof ImageInputStream)) 
	    throw new IllegalStateException("Input not an ImageInputStream.");
	if(input == null)
	    throw new IllegalStateException("No input stream.");

    }

    private void readHeaders() throws IOException, IIOException {
	if(fileHeader != null)
	    return;

	checkStream();

	fileHeader = new BMPFileHeader((ImageInputStream)input);
	infoHeader = new BMPInfoHeader((ImageInputStream)input);
	decoder = BMPDecoder.getDecoder(fileHeader, infoHeader);
    }
    
    public int getWidth(int imageIndex) throws IOException {
	validateIndex(imageIndex);
	readHeaders();
	return infoHeader.getWidth();
    }

    public int getHeight(int imageIndex) throws IOException {
	validateIndex(imageIndex);
	readHeaders();
	return infoHeader.getHeight();
    }

    public Iterator getImageTypes(int imageIndex){
	validateIndex(imageIndex);
	return null;
    }

    /**
     * Returns the number of images. BMP files can only contain a single one.
     */
    public int getNumImages(boolean allowSearch){
	return 1;
    }


    // FIXME: Support metadata
    public IIOMetadata getImageMetadata(int imageIndex){
	validateIndex(imageIndex);
	return null;
    }

    // FIXME: Support metadata
    public IIOMetadata getStreamMetadata(){
	return null;
    }

    /**
     * Reads the image indexed by imageIndex and returns it as 
     * a complete BufferedImage, using a supplied ImageReadParam.
    */	      
    public BufferedImage read(int imageIndex, ImageReadParam param) 
	throws IOException, IIOException {
	validateIndex(imageIndex);
	readHeaders();
	return decoder.decode((ImageInputStream)input);
    }
}



