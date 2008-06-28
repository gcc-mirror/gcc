/*gstclasspathsrc.h - Header file for the GstClasspathPlugin
 Copyright (C) 2007 Free Software Foundation, Inc.

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

#ifndef __GST_CLASSPATH_SRC_H__
#define __GST_CLASSPATH_SRC_H__

#include <gst/gst.h>
#include <gst/base/gstpushsrc.h>

#include "gst_input_stream.h"

G_BEGIN_DECLS

/* #defines don't like whitespacey bits */
#define GST_TYPE_CLASSPATH_SRC (gst_classpath_src_get_type())

#define GST_CLASSPATH_SRC(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST((obj),GST_TYPE_CLASSPATH_SRC,GstClasspathSrc))
  
#define GST_CLASSPATH_SRC_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_CAST((klass),GST_TYPE_CLASSPATH_SRC,GstClasspathSrcClass))
  
#define GST_IS_CLASSPATH_SRC(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE((obj),GST_TYPE_CLASSPATH_SRC))
  
#define GST_IS_CLASSPATH_SRC_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_TYPE((klass),GST_TYPE_CLASSPATH_SRC))

typedef struct _GstClasspathSrcPrivate GstClasspathSrcPrivate;
typedef struct _GstClasspathSrc GstClasspathSrc;
typedef struct _GstClasspathSrcClass GstClasspathSrcClass;

struct _GstClasspathSrc
{
  GstPushSrc element;
  
  /* instance members */
  GstClasspathSrcPrivate *priv;
};

struct _GstClasspathSrcClass
{
  GstPushSrcClass parent_class;
};

GType gst_classpath_src_get_type (void);

/* exported properties */

#define GST_CLASSPATH_SRC_ISTREAM "input-stream"

G_END_DECLS

#endif /* __GST_CLASSPATH_SRC_H__ */
