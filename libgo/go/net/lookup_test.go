// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package net

import (
	"bytes"
	"context"
	"fmt"
	"internal/testenv"
	"reflect"
	"runtime"
	"sort"
	"strings"
	"testing"
	"time"
)

func lookupLocalhost(ctx context.Context, fn func(context.Context, string) ([]IPAddr, error), host string) ([]IPAddr, error) {
	switch host {
	case "localhost":
		return []IPAddr{
			{IP: IPv4(127, 0, 0, 1)},
			{IP: IPv6loopback},
		}, nil
	default:
		return fn(ctx, host)
	}
}

// The Lookup APIs use various sources such as local database, DNS or
// mDNS, and may use platform-dependent DNS stub resolver if possible.
// The APIs accept any of forms for a query; host name in various
// encodings, UTF-8 encoded net name, domain name, FQDN or absolute
// FQDN, but the result would be one of the forms and it depends on
// the circumstances.

var lookupGoogleSRVTests = []struct {
	service, proto, name string
	cname, target        string
}{
	{
		"xmpp-server", "tcp", "google.com",
		"google.com.", "google.com.",
	},
	{
		"xmpp-server", "tcp", "google.com.",
		"google.com.", "google.com.",
	},

	// non-standard back door
	{
		"", "", "_xmpp-server._tcp.google.com",
		"google.com.", "google.com.",
	},
	{
		"", "", "_xmpp-server._tcp.google.com.",
		"google.com.", "google.com.",
	},
}

func TestLookupGoogleSRV(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	for _, tt := range lookupGoogleSRVTests {
		cname, srvs, err := LookupSRV(tt.service, tt.proto, tt.name)
		if err != nil {
			testenv.SkipFlakyNet(t)
			t.Fatal(err)
		}
		if len(srvs) == 0 {
			t.Error("got no record")
		}
		if !strings.HasSuffix(cname, tt.cname) {
			t.Errorf("got %s; want %s", cname, tt.cname)
		}
		for _, srv := range srvs {
			if !strings.HasSuffix(srv.Target, tt.target) {
				t.Errorf("got %v; want a record containing %s", srv, tt.target)
			}
		}
	}
}

var lookupGmailMXTests = []struct {
	name, host string
}{
	{"gmail.com", "google.com."},
	{"gmail.com.", "google.com."},
}

func TestLookupGmailMX(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	defer dnsWaitGroup.Wait()

	for _, tt := range lookupGmailMXTests {
		mxs, err := LookupMX(tt.name)
		if err != nil {
			t.Fatal(err)
		}
		if len(mxs) == 0 {
			t.Error("got no record")
		}
		for _, mx := range mxs {
			if !strings.HasSuffix(mx.Host, tt.host) {
				t.Errorf("got %v; want a record containing %s", mx, tt.host)
			}
		}
	}
}

var lookupGmailNSTests = []struct {
	name, host string
}{
	{"gmail.com", "google.com."},
	{"gmail.com.", "google.com."},
}

func TestLookupGmailNS(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	defer dnsWaitGroup.Wait()

	for _, tt := range lookupGmailNSTests {
		nss, err := LookupNS(tt.name)
		if err != nil {
			testenv.SkipFlakyNet(t)
			t.Fatal(err)
		}
		if len(nss) == 0 {
			t.Error("got no record")
		}
		for _, ns := range nss {
			if !strings.HasSuffix(ns.Host, tt.host) {
				t.Errorf("got %v; want a record containing %s", ns, tt.host)
			}
		}
	}
}

var lookupGmailTXTTests = []struct {
	name, txt, host string
}{
	{"gmail.com", "spf", "google.com"},
	{"gmail.com.", "spf", "google.com"},
}

func TestLookupGmailTXT(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	defer dnsWaitGroup.Wait()

	for _, tt := range lookupGmailTXTTests {
		txts, err := LookupTXT(tt.name)
		if err != nil {
			t.Fatal(err)
		}
		if len(txts) == 0 {
			t.Error("got no record")
		}
		for _, txt := range txts {
			if !strings.Contains(txt, tt.txt) || (!strings.HasSuffix(txt, tt.host) && !strings.HasSuffix(txt, tt.host+".")) {
				t.Errorf("got %s; want a record containing %s, %s", txt, tt.txt, tt.host)
			}
		}
	}
}

var lookupGooglePublicDNSAddrTests = []struct {
	addr, name string
}{
	{"8.8.8.8", ".google.com."},
	{"8.8.4.4", ".google.com."},

	{"2001:4860:4860::8888", ".google.com."},
	{"2001:4860:4860::8844", ".google.com."},
}

func TestLookupGooglePublicDNSAddr(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !supportsIPv6() || !*testIPv4 || !*testIPv6 {
		t.Skip("both IPv4 and IPv6 are required")
	}

	defer dnsWaitGroup.Wait()

	for _, tt := range lookupGooglePublicDNSAddrTests {
		names, err := LookupAddr(tt.addr)
		if err != nil {
			t.Fatal(err)
		}
		if len(names) == 0 {
			t.Error("got no record")
		}
		for _, name := range names {
			if !strings.HasSuffix(name, tt.name) {
				t.Errorf("got %s; want a record containing %s", name, tt.name)
			}
		}
	}
}

func TestLookupIPv6LinkLocalAddr(t *testing.T) {
	if !supportsIPv6() || !*testIPv6 {
		t.Skip("IPv6 is required")
	}

	defer dnsWaitGroup.Wait()

	addrs, err := LookupHost("localhost")
	if err != nil {
		t.Fatal(err)
	}
	found := false
	for _, addr := range addrs {
		if addr == "fe80::1%lo0" {
			found = true
			break
		}
	}
	if !found {
		t.Skipf("not supported on %s", runtime.GOOS)
	}
	if _, err := LookupAddr("fe80::1%lo0"); err != nil {
		t.Error(err)
	}
}

var lookupCNAMETests = []struct {
	name, cname string
}{
	{"www.iana.org", "icann.org."},
	{"www.iana.org.", "icann.org."},
	{"www.google.com", "google.com."},
}

func TestLookupCNAME(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	defer dnsWaitGroup.Wait()

	for _, tt := range lookupCNAMETests {
		cname, err := LookupCNAME(tt.name)
		if err != nil {
			t.Fatal(err)
		}
		if !strings.HasSuffix(cname, tt.cname) {
			t.Errorf("got %s; want a record containing %s", cname, tt.cname)
		}
	}
}

var lookupGoogleHostTests = []struct {
	name string
}{
	{"google.com"},
	{"google.com."},
}

func TestLookupGoogleHost(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	defer dnsWaitGroup.Wait()

	for _, tt := range lookupGoogleHostTests {
		addrs, err := LookupHost(tt.name)
		if err != nil {
			t.Fatal(err)
		}
		if len(addrs) == 0 {
			t.Error("got no record")
		}
		for _, addr := range addrs {
			if ParseIP(addr) == nil {
				t.Errorf("got %q; want a literal IP address", addr)
			}
		}
	}
}

func TestLookupLongTXT(t *testing.T) {
	if runtime.GOOS == "plan9" {
		t.Skip("skipping on plan9; see https://golang.org/issue/22857")
	}
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	defer dnsWaitGroup.Wait()

	txts, err := LookupTXT("golang.rsc.io")
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(txts)
	want := []string{
		strings.Repeat("abcdefghijklmnopqrstuvwxyABCDEFGHJIKLMNOPQRSTUVWXY", 10),
		"gophers rule",
	}
	if !reflect.DeepEqual(txts, want) {
		t.Fatalf("LookupTXT golang.rsc.io incorrect\nhave %q\nwant %q", txts, want)
	}
}

var lookupGoogleIPTests = []struct {
	name string
}{
	{"google.com"},
	{"google.com."},
}

func TestLookupGoogleIP(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	defer dnsWaitGroup.Wait()

	for _, tt := range lookupGoogleIPTests {
		ips, err := LookupIP(tt.name)
		if err != nil {
			t.Fatal(err)
		}
		if len(ips) == 0 {
			t.Error("got no record")
		}
		for _, ip := range ips {
			if ip.To4() == nil && ip.To16() == nil {
				t.Errorf("got %v; want an IP address", ip)
			}
		}
	}
}

var revAddrTests = []struct {
	Addr      string
	Reverse   string
	ErrPrefix string
}{
	{"1.2.3.4", "4.3.2.1.in-addr.arpa.", ""},
	{"245.110.36.114", "114.36.110.245.in-addr.arpa.", ""},
	{"::ffff:12.34.56.78", "78.56.34.12.in-addr.arpa.", ""},
	{"::1", "1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.ip6.arpa.", ""},
	{"1::", "0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.1.0.0.0.ip6.arpa.", ""},
	{"1234:567::89a:bcde", "e.d.c.b.a.9.8.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.7.6.5.0.4.3.2.1.ip6.arpa.", ""},
	{"1234:567:fefe:bcbc:adad:9e4a:89a:bcde", "e.d.c.b.a.9.8.0.a.4.e.9.d.a.d.a.c.b.c.b.e.f.e.f.7.6.5.0.4.3.2.1.ip6.arpa.", ""},
	{"1.2.3", "", "unrecognized address"},
	{"1.2.3.4.5", "", "unrecognized address"},
	{"1234:567:bcbca::89a:bcde", "", "unrecognized address"},
	{"1234:567::bcbc:adad::89a:bcde", "", "unrecognized address"},
}

func TestReverseAddress(t *testing.T) {
	defer dnsWaitGroup.Wait()
	for i, tt := range revAddrTests {
		a, err := reverseaddr(tt.Addr)
		if len(tt.ErrPrefix) > 0 && err == nil {
			t.Errorf("#%d: expected %q, got <nil> (error)", i, tt.ErrPrefix)
			continue
		}
		if len(tt.ErrPrefix) == 0 && err != nil {
			t.Errorf("#%d: expected <nil>, got %q (error)", i, err)
		}
		if err != nil && err.(*DNSError).Err != tt.ErrPrefix {
			t.Errorf("#%d: expected %q, got %q (mismatched error)", i, tt.ErrPrefix, err.(*DNSError).Err)
		}
		if a != tt.Reverse {
			t.Errorf("#%d: expected %q, got %q (reverse address)", i, tt.Reverse, a)
		}
	}
}

func TestDNSFlood(t *testing.T) {
	if !*testDNSFlood {
		t.Skip("test disabled; use -dnsflood to enable")
	}

	defer dnsWaitGroup.Wait()

	var N = 5000
	if runtime.GOOS == "darwin" {
		// On Darwin this test consumes kernel threads much
		// than other platforms for some reason.
		// When we monitor the number of allocated Ms by
		// observing on runtime.newm calls, we can see that it
		// easily reaches the per process ceiling
		// kern.num_threads when CGO_ENABLED=1 and
		// GODEBUG=netdns=go.
		N = 500
	}

	const timeout = 3 * time.Second
	ctxHalfTimeout, cancel := context.WithTimeout(context.Background(), timeout/2)
	defer cancel()
	ctxTimeout, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	c := make(chan error, 2*N)
	for i := 0; i < N; i++ {
		name := fmt.Sprintf("%d.net-test.golang.org", i)
		go func() {
			_, err := DefaultResolver.LookupIPAddr(ctxHalfTimeout, name)
			c <- err
		}()
		go func() {
			_, err := DefaultResolver.LookupIPAddr(ctxTimeout, name)
			c <- err
		}()
	}
	qstats := struct {
		succeeded, failed         int
		timeout, temporary, other int
		unknown                   int
	}{}
	deadline := time.After(timeout + time.Second)
	for i := 0; i < 2*N; i++ {
		select {
		case <-deadline:
			t.Fatal("deadline exceeded")
		case err := <-c:
			switch err := err.(type) {
			case nil:
				qstats.succeeded++
			case Error:
				qstats.failed++
				if err.Timeout() {
					qstats.timeout++
				}
				if err.Temporary() {
					qstats.temporary++
				}
				if !err.Timeout() && !err.Temporary() {
					qstats.other++
				}
			default:
				qstats.failed++
				qstats.unknown++
			}
		}
	}

	// A high volume of DNS queries for sub-domain of golang.org
	// would be coordinated by authoritative or recursive server,
	// or stub resolver which implements query-response rate
	// limitation, so we can expect some query successes and more
	// failures including timeout, temporary and other here.
	// As a rule, unknown must not be shown but it might possibly
	// happen due to issue 4856 for now.
	t.Logf("%v succeeded, %v failed (%v timeout, %v temporary, %v other, %v unknown)", qstats.succeeded, qstats.failed, qstats.timeout, qstats.temporary, qstats.other, qstats.unknown)
}

func TestLookupDotsWithLocalSource(t *testing.T) {
	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	defer dnsWaitGroup.Wait()

	for i, fn := range []func() func(){forceGoDNS, forceCgoDNS} {
		fixup := fn()
		if fixup == nil {
			continue
		}
		names, err := LookupAddr("127.0.0.1")
		fixup()
		if err != nil {
			t.Logf("#%d: %v", i, err)
			continue
		}
		mode := "netgo"
		if i == 1 {
			mode = "netcgo"
		}
	loop:
		for i, name := range names {
			if strings.Index(name, ".") == len(name)-1 { // "localhost" not "localhost."
				for j := range names {
					if j == i {
						continue
					}
					if names[j] == name[:len(name)-1] {
						// It's OK if we find the name without the dot,
						// as some systems say 127.0.0.1 localhost localhost.
						continue loop
					}
				}
				t.Errorf("%s: got %s; want %s", mode, name, name[:len(name)-1])
			} else if strings.Contains(name, ".") && !strings.HasSuffix(name, ".") { // "localhost.localdomain." not "localhost.localdomain"
				t.Errorf("%s: got %s; want name ending with trailing dot", mode, name)
			}
		}
	}
}

func TestLookupDotsWithRemoteSource(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}

	if !supportsIPv4() || !*testIPv4 {
		t.Skip("IPv4 is required")
	}

	defer dnsWaitGroup.Wait()

	if fixup := forceGoDNS(); fixup != nil {
		testDots(t, "go")
		fixup()
	}
	if fixup := forceCgoDNS(); fixup != nil {
		testDots(t, "cgo")
		fixup()
	}
}

func testDots(t *testing.T, mode string) {
	names, err := LookupAddr("8.8.8.8") // Google dns server
	if err != nil {
		testenv.SkipFlakyNet(t)
		t.Errorf("LookupAddr(8.8.8.8): %v (mode=%v)", err, mode)
	} else {
		for _, name := range names {
			if !strings.HasSuffix(name, ".google.com.") {
				t.Errorf("LookupAddr(8.8.8.8) = %v, want names ending in .google.com. with trailing dot (mode=%v)", names, mode)
				break
			}
		}
	}

	cname, err := LookupCNAME("www.mit.edu")
	if err != nil {
		testenv.SkipFlakyNet(t)
		t.Errorf("LookupCNAME(www.mit.edu, mode=%v): %v", mode, err)
	} else if !strings.HasSuffix(cname, ".") {
		t.Errorf("LookupCNAME(www.mit.edu) = %v, want cname ending in . with trailing dot (mode=%v)", cname, mode)
	}

	mxs, err := LookupMX("google.com")
	if err != nil {
		testenv.SkipFlakyNet(t)
		t.Errorf("LookupMX(google.com): %v (mode=%v)", err, mode)
	} else {
		for _, mx := range mxs {
			if !strings.HasSuffix(mx.Host, ".google.com.") {
				t.Errorf("LookupMX(google.com) = %v, want names ending in .google.com. with trailing dot (mode=%v)", mxString(mxs), mode)
				break
			}
		}
	}

	nss, err := LookupNS("google.com")
	if err != nil {
		testenv.SkipFlakyNet(t)
		t.Errorf("LookupNS(google.com): %v (mode=%v)", err, mode)
	} else {
		for _, ns := range nss {
			if !strings.HasSuffix(ns.Host, ".google.com.") {
				t.Errorf("LookupNS(google.com) = %v, want names ending in .google.com. with trailing dot (mode=%v)", nsString(nss), mode)
				break
			}
		}
	}

	cname, srvs, err := LookupSRV("xmpp-server", "tcp", "google.com")
	if err != nil {
		testenv.SkipFlakyNet(t)
		t.Errorf("LookupSRV(xmpp-server, tcp, google.com): %v (mode=%v)", err, mode)
	} else {
		if !strings.HasSuffix(cname, ".google.com.") {
			t.Errorf("LookupSRV(xmpp-server, tcp, google.com) returned cname=%v, want name ending in .google.com. with trailing dot (mode=%v)", cname, mode)
		}
		for _, srv := range srvs {
			if !strings.HasSuffix(srv.Target, ".google.com.") {
				t.Errorf("LookupSRV(xmpp-server, tcp, google.com) returned addrs=%v, want names ending in .google.com. with trailing dot (mode=%v)", srvString(srvs), mode)
				break
			}
		}
	}
}

func mxString(mxs []*MX) string {
	var buf bytes.Buffer
	sep := ""
	fmt.Fprintf(&buf, "[")
	for _, mx := range mxs {
		fmt.Fprintf(&buf, "%s%s:%d", sep, mx.Host, mx.Pref)
		sep = " "
	}
	fmt.Fprintf(&buf, "]")
	return buf.String()
}

func nsString(nss []*NS) string {
	var buf bytes.Buffer
	sep := ""
	fmt.Fprintf(&buf, "[")
	for _, ns := range nss {
		fmt.Fprintf(&buf, "%s%s", sep, ns.Host)
		sep = " "
	}
	fmt.Fprintf(&buf, "]")
	return buf.String()
}

func srvString(srvs []*SRV) string {
	var buf bytes.Buffer
	sep := ""
	fmt.Fprintf(&buf, "[")
	for _, srv := range srvs {
		fmt.Fprintf(&buf, "%s%s:%d:%d:%d", sep, srv.Target, srv.Port, srv.Priority, srv.Weight)
		sep = " "
	}
	fmt.Fprintf(&buf, "]")
	return buf.String()
}

func TestLookupPort(t *testing.T) {
	// See http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
	//
	// Please be careful about adding new test cases.
	// There are platforms having incomplete mappings for
	// restricted resource access and security reasons.
	type test struct {
		network string
		name    string
		port    int
		ok      bool
	}
	var tests = []test{
		{"tcp", "0", 0, true},
		{"udp", "0", 0, true},
		{"udp", "domain", 53, true},

		{"--badnet--", "zzz", 0, false},
		{"tcp", "--badport--", 0, false},
		{"tcp", "-1", 0, false},
		{"tcp", "65536", 0, false},
		{"udp", "-1", 0, false},
		{"udp", "65536", 0, false},
		{"tcp", "123456789", 0, false},

		// Issue 13610: LookupPort("tcp", "")
		{"tcp", "", 0, true},
		{"tcp4", "", 0, true},
		{"tcp6", "", 0, true},
		{"udp", "", 0, true},
		{"udp4", "", 0, true},
		{"udp6", "", 0, true},
	}

	switch runtime.GOOS {
	case "android":
		if netGo {
			t.Skipf("not supported on %s without cgo; see golang.org/issues/14576", runtime.GOOS)
		}
	default:
		tests = append(tests, test{"tcp", "http", 80, true})
	}

	for _, tt := range tests {
		port, err := LookupPort(tt.network, tt.name)
		if port != tt.port || (err == nil) != tt.ok {
			t.Errorf("LookupPort(%q, %q) = %d, %v; want %d, error=%t", tt.network, tt.name, port, err, tt.port, !tt.ok)
		}
		if err != nil {
			if perr := parseLookupPortError(err); perr != nil {
				t.Error(perr)
			}
		}
	}
}

// Like TestLookupPort but with minimal tests that should always pass
// because the answers are baked-in to the net package.
func TestLookupPort_Minimal(t *testing.T) {
	type test struct {
		network string
		name    string
		port    int
	}
	var tests = []test{
		{"tcp", "http", 80},
		{"tcp", "HTTP", 80}, // case shouldn't matter
		{"tcp", "https", 443},
		{"tcp", "ssh", 22},
		{"tcp", "gopher", 70},
		{"tcp4", "http", 80},
		{"tcp6", "http", 80},
	}

	for _, tt := range tests {
		port, err := LookupPort(tt.network, tt.name)
		if port != tt.port || err != nil {
			t.Errorf("LookupPort(%q, %q) = %d, %v; want %d, error=nil", tt.network, tt.name, port, err, tt.port)
		}
	}
}

func TestLookupProtocol_Minimal(t *testing.T) {
	type test struct {
		name string
		want int
	}
	var tests = []test{
		{"tcp", 6},
		{"TcP", 6}, // case shouldn't matter
		{"icmp", 1},
		{"igmp", 2},
		{"udp", 17},
		{"ipv6-icmp", 58},
	}

	for _, tt := range tests {
		got, err := lookupProtocol(context.Background(), tt.name)
		if got != tt.want || err != nil {
			t.Errorf("LookupProtocol(%q) = %d, %v; want %d, error=nil", tt.name, got, err, tt.want)
		}
	}

}

func TestLookupNonLDH(t *testing.T) {
	if runtime.GOOS == "nacl" {
		t.Skip("skip on nacl")
	}

	defer dnsWaitGroup.Wait()

	if fixup := forceGoDNS(); fixup != nil {
		defer fixup()
	}

	// "LDH" stands for letters, digits, and hyphens and is the usual
	// description of standard DNS names.
	// This test is checking that other kinds of names are reported
	// as not found, not reported as invalid names.
	addrs, err := LookupHost("!!!.###.bogus..domain.")
	if err == nil {
		t.Fatalf("lookup succeeded: %v", addrs)
	}
	if !strings.HasSuffix(err.Error(), errNoSuchHost.Error()) {
		t.Fatalf("lookup error = %v, want %v", err, errNoSuchHost)
	}
}

func TestLookupContextCancel(t *testing.T) {
	if testenv.Builder() == "" {
		testenv.MustHaveExternalNetwork(t)
	}
	if runtime.GOOS == "nacl" {
		t.Skip("skip on nacl")
	}

	defer dnsWaitGroup.Wait()

	ctx, ctxCancel := context.WithCancel(context.Background())
	ctxCancel()
	_, err := DefaultResolver.LookupIPAddr(ctx, "google.com")
	if err != errCanceled {
		testenv.SkipFlakyNet(t)
		t.Fatal(err)
	}
	ctx = context.Background()
	_, err = DefaultResolver.LookupIPAddr(ctx, "google.com")
	if err != nil {
		testenv.SkipFlakyNet(t)
		t.Fatal(err)
	}
}
