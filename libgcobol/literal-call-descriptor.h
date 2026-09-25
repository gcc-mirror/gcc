/* Shared layout for compiler-generated literal CALL descriptors.
   Field order and C/GCC type mappings are defined together here. */
#ifndef GCOBOL_LITERAL_CALL_DESCRIPTOR_H
#define GCOBOL_LITERAL_CALL_DESCRIPTOR_H

/* X(member name, runtime C++ type, compiler target tree type).
   The third argument is only expanded by the GENERIC builder. */
#define GCOBOL_LITERAL_CALL_DESCRIPTOR_FIELDS(X) \
  X(literal, const char *, const_char_pointer) \
  X(field, const void *, const_void_pointer) \
  X(warning_filename, const char *, const_char_pointer) \
  X(program_id, int, INT) \
  X(call_convention, int, INT) \
  X(warning_line, int, INT)

struct cblc_literal_call_descriptor
  {
#define GCOBOL_DECLARE_CALL_FIELD(name, c_type, tree_type) c_type name;
  GCOBOL_LITERAL_CALL_DESCRIPTOR_FIELDS(GCOBOL_DECLARE_CALL_FIELD)
#undef GCOBOL_DECLARE_CALL_FIELD
  };

/* literal and warning_filename use the existing NUL-terminated resolver
   and diagnostic interfaces. field points to a counted cblc_field_t.
   A null warning_filename suppresses the warning only.
   The descriptor contains metadata, never a cached resolved address. */
extern "C" void *
__gg__resolve_literal_call_descriptor(
  const cblc_literal_call_descriptor *descriptor);

#endif
