// socket_epoll.go -- GNU/Linux epoll handling.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Support for GNU/Linux epoll.
// Only for implementing net package.
// DO NOT USE DIRECTLY.

package syscall

// We don't take this type directly from the header file because it
// uses a union.  FIXME.

type EpollEvent struct {
  Events uint32;
  Fd int32;
  Pad int32;
};

func libc_epoll_create(size int) int __asm__ ("epoll_create");
func libc_epoll_ctl(epfd, op, fd int, event *EpollEvent) int __asm__ ("epoll_ctl");
func libc_epoll_wait(epfd int, events *EpollEvent, maxevents int,
                     timeout int) int __asm__ ("epoll_wait");


func EpollCreate(size int) (fd int, errno int) {
  fd = libc_epoll_create(int(size));
  if fd < 0 { errno = GetErrno() }
  return;
}

func EpollCtl(epfd, op, fd int, ev *EpollEvent) (errno int) {
  r := libc_epoll_ctl(epfd, op, fd, ev);
  if r < 0 { errno = GetErrno() }
  return;
}

func EpollWait(epfd int, ev []EpollEvent, msec int) (n int, errno int) {
  var events *EpollEvent;
  var maxevents int;
  if len(ev) > 0 {
    maxevents = len(ev);
    events = &ev[0]
  }
  n = libc_epoll_wait(epfd, events, maxevents, msec);
  if n < 0 { errno = GetErrno() }
  return;
}
