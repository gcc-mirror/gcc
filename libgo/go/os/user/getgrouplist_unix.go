// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build (dragonfly || freebsd || hurd || (!android && linux) || netbsd || openbsd || (solaris && !illumos)) && cgo && !osusergo

package user

import (
	"fmt"
	"syscall"
)

//extern-sysinfo getgrouplist
func getGroupList(name *byte, userGID syscall.Gid_t, gids *syscall.Gid_t, n *int32) int32

// groupRetry retries getGroupList with much larger size for n. The result is
// stored in gids.
func groupRetry(username string, name []byte, userGID syscall.Gid_t, gids *[]syscall.Gid_t, n *int32) error {
	// More than initial buffer, but now n contains the correct size.
	if *n > maxGroups {
		return fmt.Errorf("user: %q is a member of more than %d groups", username, maxGroups)
	}
	*gids = make([]syscall.Gid_t, *n)
	rv := getGroupList(&name[0], userGID, &(*gids)[0], n)
	if rv == -1 {
		return fmt.Errorf("user: list groups for %s failed", username)
	}
	return nil
}
