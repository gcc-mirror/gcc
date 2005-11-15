/* gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.c - DSSI Provider
   Copyright (C) 2005 Free Software Foundation, Inc.

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

#include <config.h>
#include <gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.h> 

#include "dssi_data.h"

void
Java_gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider_dlclose_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), jlong sohandle)
{
  dssi_data *data = (dssi_data *) (long) sohandle;
  dlclose (data->dlhandle);
  JCL_free (env, data);  
}

jlong
Java_gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider_dlopen_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), jstring name)
{
  const char *filename;
  void *handle;
  DSSI_Descriptor_Function fn;
  dssi_data *data = 0;
  
  filename = JCL_jstring_to_cstring (env, name);
  if (filename == NULL)
    return (0);
  
  handle = dlopen(filename, RTLD_NOW);
  
  if (handle == 0)
    goto done;
  
  fn = (DSSI_Descriptor_Function) dlsym(handle, "dssi_descriptor");
  
  if (fn == 0)
    {
      dlclose(handle);
      goto done;
    }
  
  data = (dssi_data *) JCL_malloc(env, sizeof(dssi_data));
  data->dlhandle = handle;
  data->fn = fn;
  data->midiEventReadIndex = 0;
  data->midiEventWriteIndex = 0;
  
 done:
  JCL_free_cstring (env, name, filename);
  return PTR_TO_JLONG(data);
}

jlong
Java_gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider_getDSSIHandle_1 
  (JNIEnv *env __attribute__((unused)), 
   jclass clazz __attribute__((unused)), jlong handle, jlong index)
{ 
  dssi_data *data = JLONG_TO_PTR(dssi_data,handle);
  data->desc = (data->fn)(index);
  return PTR_TO_JLONG(data->desc);
}

jstring
Java_gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider_getDSSIName_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), jlong handle)
{
  DSSI_Descriptor *desc = JLONG_TO_PTR(DSSI_Descriptor,handle);
  const char *str = desc->LADSPA_Plugin->Name;
  
  return (*env)->NewStringUTF (env, str);
} 

jstring
Java_gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider_getDSSICopyright_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), jlong handle)
{
  DSSI_Descriptor *desc = JLONG_TO_PTR(DSSI_Descriptor,handle);
  const char *str = desc->LADSPA_Plugin->Copyright;
  
  return (*env)->NewStringUTF (env, str);
} 

jstring
Java_gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider_getDSSIVendor_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), jlong handle)
{
  DSSI_Descriptor *desc = JLONG_TO_PTR(DSSI_Descriptor,handle);
  const char *str = desc->LADSPA_Plugin->Maker;
  
  return (*env)->NewStringUTF (env, str);
}

jstring
Java_gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider_getDSSILabel_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), jlong handle)
{
  DSSI_Descriptor *desc = JLONG_TO_PTR(DSSI_Descriptor,handle);
  const char *str = desc->LADSPA_Plugin->Label;
  
  return (*env)->NewStringUTF (env, str);
}


