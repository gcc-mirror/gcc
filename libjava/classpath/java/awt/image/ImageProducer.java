/* ImageProducer.java -- Java interface for image production
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


package java.awt.image;

/**
 * An object implementing the <code>ImageProducer</code> interface can
 * produce data for images.  Each image has a corresponding
 * <code>ImageProducer</code> which is needed for things such as
 * resizing the image.
 *
 * @see ImageConsumer
 * @author C. Brian Jones (cbj@gnu.org)
 */
public interface ImageProducer
{
    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code>.
     */
    void addConsumer(ImageConsumer ic);

    /**
     * Used to determine if the given <code>ImageConsumer</code> is
     * already registered with this <code>ImageProducer</code>.
     */
    boolean isConsumer(ImageConsumer ic);

    /**
     * Used to remove an <code>ImageConsumer</code> from the list of
     * registered consumers for this <code>ImageProducer</code>.
     */
    void removeConsumer(ImageConsumer ic);

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code> and then immediately start
     * reconstruction of the image data to be delivered to all
     * registered consumers.
     */
    void startProduction(ImageConsumer ic);

    /**
     * Used to register an <code>ImageConsumer</code> with this
     * <code>ImageProducer</code> and then request that this producer
     * resend the image data in the order top-down, left-right.
     */
    void requestTopDownLeftRightResend(ImageConsumer ic);
}
