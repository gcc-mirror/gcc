/* gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider.c
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
#include <gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider.h>

#include <alsa/asoundlib.h>

static snd_seq_t *seq;

JNIEXPORT void JNICALL
Java_gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider_init_1 
(JNIEnv *env __attribute__((unused)), jobject this __attribute__((unused)))
{
  int rc = snd_seq_open (&seq, "default", SND_SEQ_OPEN_DUPLEX, SND_SEQ_NONBLOCK);
  if (rc < 0)
    abort ();
  snd_seq_set_client_name(seq, "gnu.javax.sound.midi.alsa");
}

/**
 * Return a CLAZZ[] filled with ports matching TYPE. 
 */
static jobjectArray 
getPortDeviceInfo (JNIEnv *env, unsigned type, const char *clazz)
{
  jobjectArray rarray;
  snd_seq_client_info_t *cinfo;
  snd_seq_port_info_t *pinfo;
  int client;
  int count = 0;
  jclass icls;
  jmethodID mid;

  snd_seq_client_info_alloca(&cinfo);
  snd_seq_port_info_alloca(&pinfo);

  /* First, count the number of input devices.  */
  snd_seq_client_info_set_client (cinfo, -1);
  while (snd_seq_query_next_client (seq, cinfo) >= 0)
    {
      client = snd_seq_client_info_get_client (cinfo);
      if (client == 0)
	continue;
      snd_seq_port_info_set_client (pinfo, client);
      snd_seq_port_info_set_port (pinfo, -1);
      while (snd_seq_query_next_port (seq, pinfo) >= 0)
	{
	  if ((snd_seq_port_info_get_capability (pinfo) & type) != type)
	    continue;
	  count++;
	}
    }

  icls = (*env)->FindClass(env, clazz);
  mid = (*env)->GetMethodID(env, icls, "<init>", 
			    "(Ljava/lang/String;Ljava/lang/String;JJ)V");

  rarray = (jobjectArray) (*env)->NewObjectArray(env, count, icls, NULL); 

  /* Now, populate our array.  */
  count = 0;
  snd_seq_client_info_set_client (cinfo, -1);
  while (snd_seq_query_next_client (seq, cinfo) >= 0)
    {
      const char *client_name;
      client = snd_seq_client_info_get_client (cinfo);
      if (client == 0)
	continue;
      snd_seq_port_info_set_client (pinfo, client);
      snd_seq_port_info_set_port (pinfo, -1);

      client_name = snd_seq_client_info_get_name (cinfo);

      while (snd_seq_query_next_port (seq, pinfo) >= 0)
	{
	  const char *port_name;

	  if ((snd_seq_port_info_get_capability (pinfo) & type) != type)
	    continue;

	  port_name = snd_seq_port_info_get_name (pinfo);

	  (*env)->SetObjectArrayElement(env, rarray, count,
					(*env)->NewObject (env, icls, mid,
							   (*env)->NewStringUTF (env, client_name),
							   (*env)->NewStringUTF (env, port_name),
							   (jlong) snd_seq_port_info_get_client(pinfo), 
							   (jlong) snd_seq_port_info_get_port(pinfo)));
	  count++;
	}
    }

  return rarray;
}

JNIEXPORT jobjectArray JNICALL
Java_gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider_getInputDeviceInfo_1 
  (JNIEnv *env, jobject this __attribute__((unused)))
{
  return getPortDeviceInfo (env, 
			    (SND_SEQ_PORT_CAP_READ | SND_SEQ_PORT_CAP_SUBS_READ), 
			    "gnu/javax/sound/midi/alsa/AlsaMidiDeviceProvider$AlsaInputPortInfo");
}

JNIEXPORT jobjectArray JNICALL
Java_gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider_getOutputDeviceInfo_1 
  (JNIEnv *env, jobject this __attribute__((unused)))
{
  return getPortDeviceInfo (env, 
			    (SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE), 
			    "gnu/javax/sound/midi/alsa/AlsaMidiDeviceProvider$AlsaOutputPortInfo");
}

