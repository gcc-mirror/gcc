// { dg-do compile { target correct_iso_cpp_string_wchar_protos } }
// { dg-options "-O2" }

#include <cstring>

const char *cc1, *cc2;
char *c1, *c2;
const void *cv1, *cv2;
void *v1, *v2;

void
test01 ()
{
  v1 = std::memchr (v2, '/', 3);
  c1 = std::strchr (c2, '/');
  c1 = std::strrchr (c2, 'c');
  c1 = std::strpbrk (c2, "abc");
  c1 = std::strstr (c2, "abc");

  cv1 = std::memchr (v2, '/', 3);
  cc1 = std::strchr (c2, '/');
  cc1 = std::strrchr (c2, 'c');
  cc1 = std::strpbrk (c2, "abc");
  cc1 = std::strstr (c2, "abc");

  v1 = std::memchr (cv2, '/', 3);	// { dg-error "invalid conversion" }
  c1 = std::strchr (cc2, '/');		// { dg-error "invalid conversion" }
  c1 = std::strrchr (cc2, 'c');		// { dg-error "invalid conversion" }
  c1 = std::strpbrk (cc2, "abc");	// { dg-error "invalid conversion" }
  c1 = std::strstr (cc2, "abc");	// { dg-error "invalid conversion" }

  cv1 = std::memchr (cv2, '/', 3);
  cc1 = std::strchr (cc2, '/');
  cc1 = std::strrchr (cc2, 'c');
  cc1 = std::strpbrk (cc2, "abc");
  cc1 = std::strstr (cc2, "abc");
}
