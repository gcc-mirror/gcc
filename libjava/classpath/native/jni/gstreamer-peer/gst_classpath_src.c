/*gstclasspathsrc.c - Class file for the GstClasspathPlugin
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

/*
 * We don't really use version numbering here, we give it the same version
 * number of classpath, so that gstreamer is happy.
 * TODO: Maybe this should be moved in config.h instead?
 */
#define CLASSPATH_GST_PLUGIN_VERSION PACKAGE_VERSION

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <gst/gst.h>
#include <gst/base/gstbasesrc.h>
#include <gst/base/gstpushsrc.h>

#include <glib.h>
#include <glib/gprintf.h>

#include <gdk/gdk.h>

#include "gst_classpath_src.h"
#include "gst_input_stream.h"

GST_DEBUG_CATEGORY_STATIC (gst_classpath_src_debug);
#define GST_CAT_DEFAULT gst_classpath_src_debug

enum
{
  ARG_0,
  ARG_INPUTSTREAM
};

struct _GstClasspathSrcPrivate
{
  GstInputStream *istream;
  GstCaps *caps;
};

static const GstElementDetails gst_classpath_src_details =
GST_ELEMENT_DETAILS ("ClasspathSrc",
  "Source/Network",
  "Read from a java input stream",
  "Mario Torre <neugens@limasoftware.net>");

static GstStaticPadTemplate _template =
GST_STATIC_PAD_TEMPLATE ("src",
  GST_PAD_SRC,
  GST_PAD_ALWAYS,
  GST_STATIC_CAPS_ANY);

/* ***** plugin init ***** */

static void
_do_init (GType filesrc_type __attribute__ ((unused)))
{
  GST_DEBUG_CATEGORY_INIT (gst_classpath_src_debug, "classpathsrc",
         0, "classpathsrc");
}

GST_BOILERPLATE_FULL (GstClasspathSrc, gst_classpath_src, GstPushSrc,
                  GST_TYPE_PUSH_SRC, _do_init);

static gboolean
plugin_init (GstPlugin *plugin)
{
  return gst_element_register (plugin, "classpathsrc",
                               GST_RANK_NONE, GST_TYPE_CLASSPATH_SRC);
}

GST_PLUGIN_DEFINE_STATIC (GST_VERSION_MAJOR,
  GST_VERSION_MINOR,
  "classpathsrc",
  "Java InputStream Reader",
  plugin_init, CLASSPATH_GST_PLUGIN_VERSION,
  GST_LICENSE_UNKNOWN, /* GPL + Exception */
  "Classpath", "http://www.classpath.org/")
        
/* ***** public class methods ***** */

static void gst_classpath_src_set_property (GObject *object,
                                            guint prop_id,
                                            const GValue *value,
                                            GParamSpec *pspec);

static void gst_classpath_src_get_property (GObject *object,
                                            guint prop_id,
                                            GValue *value,
                                            GParamSpec *pspec);

static void gst_classpath_src_finalize (GObject *object);

static GstCaps *gst_classpath_src_getcaps (GstBaseSrc *basesrc);

static gboolean gst_classpath_src_start (GstBaseSrc *basesrc);

static gboolean gst_classpath_src_stop (GstBaseSrc *basesrc);

static GstFlowReturn gst_classpath_src_create (GstPushSrc *src,
                                               GstBuffer **buffer);

static GstFlowReturn
gst_classpath_src_create_stream (GstClasspathSrc *src, GstBuffer **buffer);

static GstFlowReturn
check_read (GstClasspathSrc *src, int read, int buffer_size,
            GstBuffer **buffer);

/* ***** public class methods: end ***** */

static void
gst_classpath_src_base_init (gpointer gclass)
{
  GstElementClass *gstelement_class = GST_ELEMENT_CLASS (gclass);

  gst_element_class_add_pad_template (gstelement_class,
                                      gst_static_pad_template_get (&_template));

  gst_element_class_set_details (gstelement_class, &gst_classpath_src_details);
}

static void
gst_classpath_src_class_init (GstClasspathSrcClass *klass)
{
  GObjectClass *gobject_class;
  GstBaseSrcClass *gstbasesrc_class;
  GstPushSrcClass *gstpushsrc_class;
  
  GParamSpec *pspec;

  gobject_class = G_OBJECT_CLASS (klass);
  gstbasesrc_class = GST_BASE_SRC_CLASS (klass);
  gstpushsrc_class = GST_PUSH_SRC_CLASS (klass);
  
  g_type_class_add_private (klass, sizeof (GstClasspathSrcPrivate));
  
  /* getter and setters */

  gobject_class->set_property = gst_classpath_src_set_property;
  gobject_class->get_property = gst_classpath_src_get_property;

  /* register properties */    
  pspec = g_param_spec_pointer (GST_CLASSPATH_SRC_ISTREAM,
                                "GstInputStream instance",
                                "GstInputStream instance",
                                G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class, ARG_INPUTSTREAM, pspec);

  /* register callbacks */
  gobject_class->finalize = GST_DEBUG_FUNCPTR (gst_classpath_src_finalize);

  gstbasesrc_class->get_caps = GST_DEBUG_FUNCPTR (gst_classpath_src_getcaps);
  gstbasesrc_class->start = GST_DEBUG_FUNCPTR (gst_classpath_src_start);
  gstbasesrc_class->stop = GST_DEBUG_FUNCPTR (gst_classpath_src_stop);

  gstpushsrc_class->create = GST_DEBUG_FUNCPTR (gst_classpath_src_create);
}

/* ***** */

static void
gst_classpath_src_init (GstClasspathSrc *src,
                        GstClasspathSrcClass * g_class __attribute__ ((unused)))
{
  src->priv = G_TYPE_INSTANCE_GET_PRIVATE (src, GST_TYPE_CLASSPATH_SRC,
                                           GstClasspathSrcPrivate);
  
  src->priv->istream = NULL;
  src->priv->caps = NULL;
}

static void
gst_classpath_src_finalize (GObject *object)
{
  G_OBJECT_CLASS (parent_class)->finalize (object);
}

/* ************************************************************************** */

static void
gst_classpath_src_set_property (GObject *object,
                                guint prop_id,
                                const GValue *value,
                                GParamSpec *pspec)
{
  GstClasspathSrc *src;
  
  g_return_if_fail (GST_IS_CLASSPATH_SRC (object));
  
  src = GST_CLASSPATH_SRC (object);
  
  GST_OBJECT_LOCK (src);
  switch (prop_id)
    {
      case ARG_INPUTSTREAM:
        {
          GST_STATE_LOCK (src);
            {
              GstState state;
              state = GST_STATE (src);
              
              if (state != GST_STATE_READY && state != GST_STATE_NULL)
                {
                  GST_DEBUG_OBJECT (src, "setting reader in wrong state");
                  GST_STATE_UNLOCK (src);
                  break;
                }
            }
          GST_STATE_UNLOCK (src);
          
          /* FIXME: check if this is a valid instance of GstInputStream */
          src->priv->istream = g_value_get_pointer (value);
        }
        break;
        
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);   
        break;
    }
  GST_OBJECT_UNLOCK (src);
}

static void
gst_classpath_src_get_property (GObject *object,
                                guint prop_id __attribute__ ((unused)),
                                GValue *value __attribute__ ((unused)),
                                GParamSpec *pspec __attribute__ ((unused)))
{
  /* TODO */
}

/* ************************************************************************** */

static GstCaps *gst_classpath_src_getcaps (GstBaseSrc *basesrc)
{
  GstClasspathSrc *src;
  GstCaps *caps = NULL;

  src = GST_CLASSPATH_SRC (basesrc);

  if (src->priv->caps)
    caps = gst_caps_copy (src->priv->caps);
  else
    caps = gst_caps_new_any ();
  
  GST_DEBUG_OBJECT (src, "returning caps %" GST_PTR_FORMAT, caps);
  g_assert (GST_IS_CAPS (caps));
  
  return caps;
}

static GstFlowReturn
gst_classpath_src_create_stream (GstClasspathSrc *src, GstBuffer **buffer)
{
  int buffer_size = 2048;
  int read = -1; 
  
  buffer_size = gst_input_stream_available (src->priv->istream);
  if (buffer_size < 0)
    return GST_FLOW_ERROR;
  else if (buffer_size == 0)
    return GST_FLOW_WRONG_STATE;
  
  *buffer = gst_buffer_new_and_alloc (buffer_size);
  if (*buffer == NULL)
    {
      return GST_FLOW_ERROR;
    }
  
  read = gst_input_stream_read (src->priv->istream,
                                (int *) GST_BUFFER_DATA (*buffer),
                                0,
                                buffer_size);
  
  return check_read (src, read, buffer_size, buffer);
}

GstFlowReturn
check_read (GstClasspathSrc *src, int read, int buffer_size, GstBuffer **buffer)
{
  if (G_UNLIKELY (read < 0))
    {
      g_warning("GST_FLOW_UNEXPECTED (read < 0)");
      
      gst_buffer_unref (*buffer);
      *buffer = NULL;
      
      return GST_FLOW_ERROR;
    }
  else if (G_UNLIKELY (read == 0))
    {
      g_warning("GST_FLOW_WRONG_STATE (read == 0)");
      
      gst_buffer_unref (*buffer);
      *buffer = NULL;
      
      return GST_FLOW_WRONG_STATE;
    }
  else if (G_UNLIKELY (read < buffer_size))
    {
      g_warning("shorter read");
      gst_buffer_unref (*buffer);
      *buffer = NULL;
      
      return GST_FLOW_ERROR;
    }

  GST_BUFFER_SIZE (*buffer) = read;
  gst_buffer_set_caps (*buffer, src->priv->caps);
  
  return GST_FLOW_OK;
}

static GstFlowReturn
gst_classpath_src_create (GstPushSrc *basesrc, GstBuffer **buffer)
{
  GstClasspathSrc *src = NULL;
  GstFlowReturn ret = GST_FLOW_OK;
  
  src = GST_CLASSPATH_SRC (basesrc);
   
  /* create the buffer */
  ret = gst_classpath_src_create_stream (src, buffer);
  
  return ret;
}

static gboolean
gst_classpath_src_start (GstBaseSrc *basesrc)
{
  GstClasspathSrc *src;

  src = GST_CLASSPATH_SRC (basesrc);
   
  if (src->priv->istream == NULL)
    {
      g_warning("GstInputStream is still null. You need to " \
                "pass a valid InputStream object");
          
      GST_ELEMENT_ERROR (src, RESOURCE, OPEN_READ, (NULL),
                         ("GstInputStream is still null. You need to " \
                          "pass a valid InputStream"));
      return FALSE;
    }
          
  return TRUE;
}

static gboolean
gst_classpath_src_stop (GstBaseSrc *basesrc)
{
  GstClasspathSrc *src;

  src = GST_CLASSPATH_SRC (basesrc);
  
  /* clean the stream */
  if (src->priv->istream != NULL)
    gst_input_stream_clean (src->priv->istream);

  if (src->priv->caps) {
    gst_caps_unref (src->priv->caps);
    src->priv->caps = NULL;
  }

  return TRUE;
}
