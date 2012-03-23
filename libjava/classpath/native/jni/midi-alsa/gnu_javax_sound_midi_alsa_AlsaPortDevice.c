/* gnu_javax_sound_midi_alsa_AlsaPortDevice.c - Native support
   Copyright (C) 2005, 2010, 2011
   Free Software Foundation, Inc.
   
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
#include <gnu_javax_sound_midi_alsa_AlsaPortDevice.h>
#include <unistd.h>

#include <jcl.h>
#include <alsa/asoundlib.h>

JNIEXPORT void JNICALL
Java_gnu_javax_sound_midi_alsa_AlsaPortDevice_run_1receiver_1thread_1
  (JNIEnv *env, jobject this __attribute__((unused)), 
   jlong client, jlong port, jobject receiver)
{
  int rc;
  snd_seq_port_info_t *pinfo, *sinfo;
  snd_seq_port_subscribe_t *subs;
  snd_seq_addr_t sender, dest;
  snd_seq_t *seq;

  snd_seq_port_info_alloca (&pinfo);
  snd_seq_port_info_alloca (&sinfo);
  snd_seq_port_subscribe_alloca (&subs);

  rc = snd_seq_open (&seq, "default", SND_SEQ_OPEN_DUPLEX, SND_SEQ_NONBLOCK);
  if (rc < 0)
    JCL_ThrowException (env, "java/lang/InternalError", snd_strerror (rc));

  snd_seq_port_info_set_capability (pinfo, SND_SEQ_PORT_CAP_WRITE);
  snd_seq_port_info_set_type (pinfo, SND_SEQ_PORT_TYPE_MIDI_GENERIC);

  rc = snd_seq_create_port (seq, pinfo);
  if (rc < 0)
    JCL_ThrowException (env, "java/lang/InternalError", snd_strerror (rc));

  sender.client = (int) client;
  sender.port = (int) port;
  dest.client = snd_seq_port_info_get_client(pinfo);
  dest.port = snd_seq_port_info_get_port(pinfo);

  snd_seq_port_subscribe_set_sender (subs, &sender);
  snd_seq_port_subscribe_set_dest (subs, &dest);
  rc = snd_seq_subscribe_port(seq, subs);
  if (rc < 0)
    JCL_ThrowException (env, "java/lang/InternalError", snd_strerror (rc));

  {
    int npfd;
    struct pollfd *pfd;
    jclass smcls, rcls;
    jmethodID sminit, rsend;
    jbyteArray mba;
    jbyte *ba;
    jobject msg;
    jlong jtimestamp;

    npfd = snd_seq_poll_descriptors_count (seq, POLLIN);
    pfd = (struct pollfd *) alloca (npfd * sizeof (struct pollfd));
    snd_seq_poll_descriptors (seq, pfd, npfd, POLLIN);

    smcls = (*env)->FindClass(env, "javax/sound/midi/ShortMessage");
    sminit = (*env)->GetMethodID(env, smcls, "<init>", "([B)V");

    rcls = (*env)->FindClass(env, "javax/sound/midi/Receiver");
    rsend = (*env)->GetMethodID(env, rcls, "send", "(Ljavax/sound/midi/MidiMessage;J)V");

    while (1)
      {
	if (poll (pfd, npfd, 100000) > 0)
	  {
	    snd_seq_event_t *ev;
	    
	    do
	      {
		snd_seq_event_input (seq, &ev);

		if ((ev->flags & SND_SEQ_TIME_STAMP_MASK) == SND_SEQ_TIME_STAMP_TICK)
		  jtimestamp = (jlong) ev->time.tick;
		else
		  jtimestamp = (jlong) ev->time.time.tv_sec * (jlong) 1000000000 
		    + (jlong) ev->time.time.tv_nsec;

		switch (ev->type)
		  {
		  case SND_SEQ_EVENT_NOTEON:
		    mba = (*env)->NewByteArray (env, 3);
		    ba = (*env)->GetByteArrayElements (env, mba, 0);
		    ba[0] = 0x90 + ev->data.control.channel;
		    ba[1] = ev->data.note.note;
		    ba[2] = ev->data.note.velocity;
		    (*env)->ReleaseByteArrayElements (env, mba, ba, 0);
		    msg = (*env)->NewObject(env, smcls, sminit, mba);
		    (*env)->CallObjectMethod(env, receiver, 
					     rsend, msg, jtimestamp);
		    break;
		    
		  case SND_SEQ_EVENT_CONTROLLER:
		    mba = (*env)->NewByteArray (env, 3);
		    ba = (*env)->GetByteArrayElements (env, mba, 0);
		    ba[0] = 0xB0 + ev->data.control.channel;
		    ba[1] = ev->data.control.param;
		    ba[2] = ev->data.control.value;
		    (*env)->ReleaseByteArrayElements (env, mba, ba, 0);
		    msg = (*env)->NewObject(env, smcls, sminit, mba);
		    (*env)->CallObjectMethod(env, receiver, 
					     rsend, msg, jtimestamp);
		    break;

		  default:
		    printf ("UNKNOWN EVENT 0x%x\n", ev->type);
		    break;
		  }

		snd_seq_free_event(ev);
	      }
	    while (snd_seq_event_input_pending (seq, 0) > 0);
	  }
      }
  }
}
