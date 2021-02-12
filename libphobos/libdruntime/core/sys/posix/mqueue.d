/**
 * D header file for Posix Message Queues
 *
 * Defines external functions required to manage Posix Message Queues
 *
 * mq_open(3)          open a message queue
 * mq_close(3)         close a message queue
 * mq_unlink(3)        remove a message queue
 * mq_send(3)          send a message
 * mq_receive(3)       receive a message
 * mq_timedsend(3)     send a message with a timeout (linux specific)
 * mq_timedreceive(3)  receive a message with a timeout (linux specific)
 * mq_getattr(3)       get message queue attributes
 * mq_setattr(3)       set message queue attributes
 * mq_notify(3)        register asynchronous notify
 *
 * Copyright: Copyright (c) 2016 sociomantic labs. All rights reserved
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Andreas Bok Andersen, Mathias Lang
 * Standards: POSIX.1-2001.
 * See_Also:  $(HTTP pubs.opengroup.org/onlinepubs/9699919799/basedefs/mqueue.h.html, Standard)
 * Source: $(DRUNTIMESRC core/sys/posix/mqueue.d)
 */
module core.sys.posix.mqueue;

import core.sys.posix.config;
import core.sys.posix.signal;
import core.sys.posix.time;

version (Posix):
version (CRuntime_Glibc):
extern (C):
@nogc nothrow:
@system:


/// Message queue descriptor.
alias int mqd_t;

/**
 * Used in getting and setting the attributes of a message queue.
 */
struct mq_attr
{
    /// Message queue flags.
    c_long mq_flags;
    /// Maximum number of messages.
    c_long mq_maxmsg;
    /// Maximum message size.
    c_long mq_msgsize;
    /// Number of messages currently queued.
    c_long mq_curmsgs;
}


/**
 * Establish connection between a process and a message queue `name`.
 *
 * Note:
 * Linux prototypes are:
 * mqd_t mq_open (const(char)* name, int oflag);
 * mqd_t mq_open (const(char)* name, int oflag, mode_t mode, mq_attr* attr);
 *
 * Params:
 *   name   = Name of the message queue to open.
 *   oflag  = determines the type of access used.
 *            If `O_CREAT` is on `oflag`, the third argument is taken as a
 *            `mode_t`, the mode of the created message queue.
 *            If `O_CREAT` is on `oflag`, the fourth argument is taken as
 *            a pointer to a `mq_attr' (message queue attributes).
 *            If the fourth argument is `null`, default attributes are used.
 *   ...    = varargs matching the function prototypes
 *
 * Returns:
 *  Message queue descriptor or (mqd_t) -1 on error.
 */
mqd_t mq_open(const(char)* name, int oflag, ...);


/**
 * Closes the message queue descriptor mqdes.
 *
 * Params:
 *   mqdes = Message queue descriptor to close.
 *
 * Returns:
 *   On success mq_close() returns 0; on error, -1 is returned, with errno
 *   set to indicate the error.
 */
int mq_close (mqd_t mqdes);


/**
 * Query status and attributes of message queue `mqdes`.
 *
 * Params:
 *   mqdes  = Message queue descriptor.
 *   mqstat = Buffer to fill with the message queue's attributes.
 *
 * Returns:
 *   On success mq_getattr() return 0; on error, -1 is returned, with errno
 *   set to indicate the error.
 */
int mq_getattr (mqd_t mqdes, mq_attr* mqstat);


/*
 * Set attributes associated with message queue `mqdes`
 *
 * Params:
 *   mqdes   = Message queue descriptor.
 *   newstat = non-null pointer to fill with attributes for `mqdes`.
 *   oldstat = if not `null` it is filled with the old attributes.
 *
 * Returns:
 *   On success mq_setattr() return 0; on error, -1 is returned, with errno
 *   set to indicate the error.
 */
int mq_setattr (mqd_t mqdes, const(mq_attr)* newstat, mq_attr* oldstat);


/**
 * Remove the specified message queue `name`.
 *
 * Params:
 *   name = Name of the queue to remove.
 *
 * Returns:
 *   On success mq_unlink() returns 0; on error, -1 is returned, with errno
 *   set to indicate the error.
 */
int mq_unlink (const(char)* name);


/**
 * Register for notification when a message is available
 *
 * Params:
 *   mqdes        = Message queue descriptor.
 *   notification = See `man 3 mq_notify` for details.
 *
 * Returns:
 *   On success mq_notify() returns 0; on error, -1 is returned, with errno
 *   set to indicate the error.
 */
int mq_notify (mqd_t mqdes, const(sigevent)* notification);


/**
 * Receive the oldest message with the highest priority the the message queue
 *
 * Params:
 *   mqdes      = Message queue descriptor.
 *   msg_ptr    = Buffer to write the message to
 *   msg_len    = Size of the buffer provided as `msg_ptr`. Must be greater
 *                than the mq_msgsize attribute of the queue.
 *   msg_prio   = If not `null`, set to the priority of this message.
 *
 * Returns:
 *   On success, mq_receive() returns the number of bytes in the received
 *   message; on error, -1 is returned, with errno set to indicate the error
 */
ssize_t mq_receive (mqd_t mqdes, char* msg_ptr, size_t msg_len, uint* msg_prio);


/**
 * Receive the oldest message with the highest priority the the message queue,
 * wait up to a certain timeout.
 *
 * Params:
 *   mqdes       = Message queue descriptor.
 *   msg_ptr     = Buffer to write the message to
 *   msg_len     = Size of the buffer provided as `msg_ptr`. Must be greater
 *                 than the mq_msgsize attribute of the queue.
 *   msg_prio    = If not `null`, set to the priority of this message.
 *   abs_timeout = Specify a ceiling on the time to block if the queue is empty.
 *
 * Returns:
 *   On success, mq_receive() returns the number of bytes in the received
 *   message; on error, -1 is returned, with errno set to indicate the error
 */
ssize_t mq_timedreceive (mqd_t mqdes, char* msg_ptr, size_t msg_len,
                         uint* msg_prio, const(timespec)* abs_timeout);


/**
 * Add a message to a message queue.
 *
 * Params:
 *   mqdes      = Message queue descriptor.
 *   msg_ptr    = Buffer to read the message from
 *   msg_len    = Size of the message provided via `msg_ptr`. Must be lower
 *                or equal to the mq_msgsize attribute of the queue.
 *   msg_prio   = Priority of this message.
 *
 * Returns:
 *   On success, mq_send() return zero; on error, -1 is returned, with errno
 *   set to indicate the error.
 */
int mq_send (mqd_t mqdes, const(char)* msg_ptr, size_t msg_len, uint msg_prio);


/**
 * Add a message to a message queue, block up to a certain time if the queue
 * is full.
 *
 * Params:
 *   mqdes      = Message queue descriptor.
 *   msg_ptr    = Buffer to read the message from
 *   msg_len    = Size of the message provided via `msg_ptr`. Must be lower
 *                or equal to the mq_msgsize attribute of the queue.
 *   msg_prio   = Priority of this message.
 *   abs_timeout = Specify a ceiling on the time to block if the queue is empty.
 *
 * Returns:
 *   On success, mq_timedsend() return zero; on error, -1 is returned,
 *   with errno set to indicate the error.
 *
 */
int mq_timedsend (mqd_t mqdes, const(char)* msg_ptr, size_t msg_len,
                   uint msg_prio, const(timespec)* abs_timeout);
