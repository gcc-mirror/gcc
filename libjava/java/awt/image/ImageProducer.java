/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.image;

public interface ImageProducer
{
  void addConsumer(ImageConsumer ic);
  boolean isConsumer(ImageConsumer ic);
  void removeConsumer(ImageConsumer ic);
  void startProduction(ImageConsumer ic);
  void requestTopDownLeftRightResend(ImageConsumer ic);
}
