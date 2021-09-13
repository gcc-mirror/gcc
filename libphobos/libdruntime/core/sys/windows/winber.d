/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_winber.d)
 */
module core.sys.windows.winber;
version (Windows):
@system:

/* Comment from MinGW
  winber.h - Header file for the Windows LDAP Basic Encoding Rules API

  Written by Filip Navara <xnavara@volny.cz>

  References:
    The C LDAP Application Program Interface
    http://www.watersprings.org/pub/id/draft-ietf-ldapext-ldap-c-api-05.txt

    Lightweight Directory Access Protocol Reference
    http://msdn.microsoft.com/library/en-us/netdir/ldap/ldap_reference.asp

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

 import core.sys.windows.basetsd;

/* Opaque structure
 *  http://msdn.microsoft.com/library/en-us/ldap/ldap/berelement.asp
 */
struct BerElement;

alias int ber_int_t, ber_slen_t;
alias uint ber_uint_t, ber_len_t, ber_tag_t;

align(4):
struct BerValue {
    ber_len_t bv_len;
    char*     bv_val;
}
alias BerValue LDAP_BERVAL, BERVAL;
alias BerValue* PLDAP_BERVAL, PBERVAL;

enum ber_tag_t
    LBER_ERROR   = -1,
    LBER_DEFAULT = -1,
    LBER_USE_DER =  1;

/*  FIXME: In MinGW, these are WINBERAPI == DECLSPEC_IMPORT.  Linkage
 *  attribute?
 */
extern (C) {
    BerElement* ber_init(const(BerValue)*);
    int ber_printf(BerElement*, const(char)*, ...);
    int ber_flatten(BerElement*, BerValue**);
    ber_tag_t ber_scanf(BerElement*, const(char)*, ...);
    ber_tag_t ber_peek_tag(BerElement*, ber_len_t*);
    ber_tag_t ber_skip_tag(BerElement*, ber_len_t*);
    ber_tag_t ber_first_element(BerElement*, ber_len_t*, char**);
    ber_tag_t ber_next_element(BerElement*, ber_len_t*, char*);
    void ber_bvfree(BerValue*);
    void ber_bvecfree(BerValue**);
    void ber_free(BerElement*, int);
    BerValue* ber_bvdup(BerValue*);
    BerElement* ber_alloc_t(int);
}
