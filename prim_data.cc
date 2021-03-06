#include "prim_data_incs.h"

Prim *prim_operator = 0;
Prim *prim_period = 0;
Prim *prim_setter = 0;
Prim *prim_pow = 0;
Prim *prim_mult = 0;
Prim *prim_div = 0;
Prim *prim_mod = 0;
Prim *prim_add = 0;
Prim *prim_subtract = 0;
Prim *prim_lsh = 0;
Prim *prim_rsh = 0;
Prim *prim_less = 0;
Prim *prim_lessorequal = 0;
Prim *prim_greater = 0;
Prim *prim_greaterorequal = 0;
Prim *prim_equal = 0;
Prim *prim_notequal = 0;
Prim *prim_and = 0;
Prim *prim_xor = 0;
Prim *prim_or = 0;
Prim *prim_land = 0;
Prim *prim_lor = 0;
Prim *prim_assign = 0;
Prim *prim_apply = 0;
Prim *prim_by = 0;
Prim *prim_seqcat = 0;
Prim *prim_plus = 0;
Prim *prim_minus = 0;
Prim *prim_not = 0;
Prim *prim_lnot = 0;
Prim *prim_deref = 0;
Prim *prim_cast = 0;
Prim *prim_strcat = 0;
Prim *prim_ref = 0;
Prim *prim_primitive = 0;
Prim *prim_reply = 0;
Prim *prim_make = 0;
Prim *prim_vector = 0;
Prim *prim_new = 0;
Prim *prim_clone = 0;
Prim *prim_clone_vector = 0;
Prim *prim_isinstance = 0;
Prim *prim_issubclass = 0;
Prim *prim_index_object = 0;
Prim *prim_set_index_object = 0;
Prim *prim_meta_apply = 0;
Prim *prim_destruct = 0;
Prim *prim_coerce = 0;
Prim *prim_merge = 0;
Prim *prim_merge_in = 0;
Prim *prim_type_assert = 0;
Prim *prim_len = 0;
Prim *prim_sizeof = 0;
Prim *prim_sizeof_element = 0;
Prim *prim_typeof = 0;
Prim *prim_typeof_element = 0;

void prim_init(Primitives *p, IF1 *if1) {
  char *n;
  static PrimType prim_operator_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_operator_ret_types[] = {PRIM_TYPE_ANY};
  prim_operator =
      new Prim(0, "operator", "prim_operator", -2, 0, 1, prim_operator_arg_types, prim_operator_ret_types, 0);
  n = (char *)if1->strings.put((char *)"operator");
  p->prims.add(prim_operator);
  p->prim_map[0][0].put(n, prim_operator);
  static PrimType prim_period_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_period_ret_types[] = {PRIM_TYPE_ANY};
  prim_period =
      new Prim(1, ".", "prim_period", 3, 1, 1, prim_period_arg_types, prim_period_ret_types, PRIM_NON_FUNCTIONAL);
  n = (char *)if1->strings.put((char *)".");
  p->prims.add(prim_period);
  p->prim_map[1][1].put(n, prim_period);
  static PrimType prim_setter_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_setter_ret_types[] = {PRIM_TYPE_ANY};
  prim_setter =
      new Prim(2, ".=", "prim_setter", 4, 1, 1, prim_setter_arg_types, prim_setter_ret_types, PRIM_NON_FUNCTIONAL);
  n = (char *)if1->strings.put((char *)".=");
  p->prims.add(prim_setter);
  p->prim_map[2][1].put(n, prim_setter);
  static PrimType prim_pow_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_pow_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_pow = new Prim(3, "**", "prim_pow", 3, 1, 1, prim_pow_arg_types, prim_pow_ret_types, 0);
  n = (char *)if1->strings.put((char *)"**");
  p->prims.add(prim_pow);
  p->prim_map[1][1].put(n, prim_pow);
  static PrimType prim_mult_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_mult_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_mult = new Prim(4, "*", "prim_mult", 3, 1, 1, prim_mult_arg_types, prim_mult_ret_types, 0);
  n = (char *)if1->strings.put((char *)"*");
  p->prims.add(prim_mult);
  p->prim_map[1][1].put(n, prim_mult);
  static PrimType prim_div_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_div_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_div = new Prim(5, "/", "prim_div", 3, 1, 1, prim_div_arg_types, prim_div_ret_types, 0);
  n = (char *)if1->strings.put((char *)"/");
  p->prims.add(prim_div);
  p->prim_map[1][1].put(n, prim_div);
  static PrimType prim_mod_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_mod_ret_types[] = {PRIM_TYPE_ANY_INT_A};
  prim_mod = new Prim(6, "%", "prim_mod", 3, 1, 1, prim_mod_arg_types, prim_mod_ret_types, 0);
  n = (char *)if1->strings.put((char *)"%");
  p->prims.add(prim_mod);
  p->prim_map[1][1].put(n, prim_mod);
  static PrimType prim_add_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_add_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_add = new Prim(7, "+", "prim_add", 3, 1, 1, prim_add_arg_types, prim_add_ret_types, 0);
  n = (char *)if1->strings.put((char *)"+");
  p->prims.add(prim_add);
  p->prim_map[1][1].put(n, prim_add);
  static PrimType prim_subtract_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_subtract_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_subtract = new Prim(8, "-", "prim_subtract", 3, 1, 1, prim_subtract_arg_types, prim_subtract_ret_types, 0);
  n = (char *)if1->strings.put((char *)"-");
  p->prims.add(prim_subtract);
  p->prim_map[1][1].put(n, prim_subtract);
  static PrimType prim_lsh_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_lsh_ret_types[] = {PRIM_TYPE_ANY_NUM_A};
  prim_lsh = new Prim(9, "<<", "prim_lsh", 3, 1, 1, prim_lsh_arg_types, prim_lsh_ret_types, 0);
  n = (char *)if1->strings.put((char *)"<<");
  p->prims.add(prim_lsh);
  p->prim_map[1][1].put(n, prim_lsh);
  static PrimType prim_rsh_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_rsh_ret_types[] = {PRIM_TYPE_ANY_NUM_A};
  prim_rsh = new Prim(10, ">>", "prim_rsh", 3, 1, 1, prim_rsh_arg_types, prim_rsh_ret_types, 0);
  n = (char *)if1->strings.put((char *)">>");
  p->prims.add(prim_rsh);
  p->prim_map[1][1].put(n, prim_rsh);
  static PrimType prim_less_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_less_ret_types[] = {PRIM_TYPE_BOOL};
  prim_less = new Prim(11, "<", "prim_less", 3, 1, 1, prim_less_arg_types, prim_less_ret_types, 0);
  n = (char *)if1->strings.put((char *)"<");
  p->prims.add(prim_less);
  p->prim_map[1][1].put(n, prim_less);
  static PrimType prim_lessorequal_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_lessorequal_ret_types[] = {PRIM_TYPE_BOOL};
  prim_lessorequal =
      new Prim(12, "<=", "prim_lessorequal", 3, 1, 1, prim_lessorequal_arg_types, prim_lessorequal_ret_types, 0);
  n = (char *)if1->strings.put((char *)"<=");
  p->prims.add(prim_lessorequal);
  p->prim_map[1][1].put(n, prim_lessorequal);
  static PrimType prim_greater_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_greater_ret_types[] = {PRIM_TYPE_BOOL};
  prim_greater = new Prim(13, ">", "prim_greater", 3, 1, 1, prim_greater_arg_types, prim_greater_ret_types, 0);
  n = (char *)if1->strings.put((char *)">");
  p->prims.add(prim_greater);
  p->prim_map[1][1].put(n, prim_greater);
  static PrimType prim_greaterorequal_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_greaterorequal_ret_types[] = {PRIM_TYPE_BOOL};
  prim_greaterorequal = new Prim(14, ">=", "prim_greaterorequal", 3, 1, 1, prim_greaterorequal_arg_types,
                                 prim_greaterorequal_ret_types, 0);
  n = (char *)if1->strings.put((char *)">=");
  p->prims.add(prim_greaterorequal);
  p->prim_map[1][1].put(n, prim_greaterorequal);
  static PrimType prim_equal_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_equal_ret_types[] = {PRIM_TYPE_BOOL};
  prim_equal = new Prim(15, "==", "prim_equal", 3, 1, 1, prim_equal_arg_types, prim_equal_ret_types, 0);
  n = (char *)if1->strings.put((char *)"==");
  p->prims.add(prim_equal);
  p->prim_map[1][1].put(n, prim_equal);
  static PrimType prim_notequal_arg_types[] = {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B};
  static PrimType prim_notequal_ret_types[] = {PRIM_TYPE_BOOL};
  prim_notequal = new Prim(16, "!=", "prim_notequal", 3, 1, 1, prim_notequal_arg_types, prim_notequal_ret_types, 0);
  n = (char *)if1->strings.put((char *)"!=");
  p->prims.add(prim_notequal);
  p->prim_map[1][1].put(n, prim_notequal);
  static PrimType prim_and_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_and_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_and = new Prim(17, "&", "prim_and", 3, 1, 1, prim_and_arg_types, prim_and_ret_types, 0);
  n = (char *)if1->strings.put((char *)"&");
  p->prims.add(prim_and);
  p->prim_map[1][1].put(n, prim_and);
  static PrimType prim_xor_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_xor_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_xor = new Prim(18, "^", "prim_xor", 3, 1, 1, prim_xor_arg_types, prim_xor_ret_types, 0);
  n = (char *)if1->strings.put((char *)"^");
  p->prims.add(prim_xor);
  p->prim_map[1][1].put(n, prim_xor);
  static PrimType prim_or_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_or_ret_types[] = {PRIM_TYPE_ANY_NUM_AB};
  prim_or = new Prim(19, "|", "prim_or", 3, 1, 1, prim_or_arg_types, prim_or_ret_types, 0);
  n = (char *)if1->strings.put((char *)"|");
  p->prims.add(prim_or);
  p->prim_map[1][1].put(n, prim_or);
  static PrimType prim_land_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_land_ret_types[] = {PRIM_TYPE_BOOL};
  prim_land = new Prim(20, "&&", "prim_land", 3, 1, 1, prim_land_arg_types, prim_land_ret_types, 0);
  n = (char *)if1->strings.put((char *)"&&");
  p->prims.add(prim_land);
  p->prim_map[1][1].put(n, prim_land);
  static PrimType prim_lor_arg_types[] = {PRIM_TYPE_ANY_INT_A, PRIM_TYPE_ANY_INT_B};
  static PrimType prim_lor_ret_types[] = {PRIM_TYPE_BOOL};
  prim_lor = new Prim(21, "||", "prim_lor", 3, 1, 1, prim_lor_arg_types, prim_lor_ret_types, 0);
  n = (char *)if1->strings.put((char *)"||");
  p->prims.add(prim_lor);
  p->prim_map[1][1].put(n, prim_lor);
  static PrimType prim_assign_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_assign_ret_types[] = {PRIM_TYPE_ANY};
  prim_assign = new Prim(22, "=", "prim_assign", 3, 1, 1, prim_assign_arg_types, prim_assign_ret_types, 0);
  n = (char *)if1->strings.put((char *)"=");
  p->prims.add(prim_assign);
  p->prim_map[1][1].put(n, prim_assign);
  static PrimType prim_apply_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_apply_ret_types[] = {PRIM_TYPE_ANY};
  prim_apply = new Prim(23, "^^", "prim_apply", 3, 1, 1, prim_apply_arg_types, prim_apply_ret_types, 0);
  n = (char *)if1->strings.put((char *)"^^");
  p->prims.add(prim_apply);
  p->prim_map[1][1].put(n, prim_apply);
  static PrimType prim_by_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_by_ret_types[] = {PRIM_TYPE_ANY};
  prim_by = new Prim(24, "by", "prim_by", 3, 1, 1, prim_by_arg_types, prim_by_ret_types, 0);
  n = (char *)if1->strings.put((char *)"by");
  p->prims.add(prim_by);
  p->prim_map[1][1].put(n, prim_by);
  static PrimType prim_seqcat_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_seqcat_ret_types[] = {PRIM_TYPE_ANY};
  prim_seqcat = new Prim(25, "#", "prim_seqcat", 3, 1, 1, prim_seqcat_arg_types, prim_seqcat_ret_types, 0);
  n = (char *)if1->strings.put((char *)"#");
  p->prims.add(prim_seqcat);
  p->prim_map[1][1].put(n, prim_seqcat);
  static PrimType prim_plus_arg_types[] = {PRIM_TYPE_ANY_NUM_A};
  static PrimType prim_plus_ret_types[] = {PRIM_TYPE_A};
  prim_plus = new Prim(26, "+", "prim_plus", 2, 0, 1, prim_plus_arg_types, prim_plus_ret_types, 0);
  n = (char *)if1->strings.put((char *)"+");
  p->prims.add(prim_plus);
  p->prim_map[0][0].put(n, prim_plus);
  static PrimType prim_minus_arg_types[] = {PRIM_TYPE_ANY_NUM_A};
  static PrimType prim_minus_ret_types[] = {PRIM_TYPE_ANY_NUM_A};
  prim_minus = new Prim(27, "-", "prim_minus", 2, 0, 1, prim_minus_arg_types, prim_minus_ret_types, 0);
  n = (char *)if1->strings.put((char *)"-");
  p->prims.add(prim_minus);
  p->prim_map[0][0].put(n, prim_minus);
  static PrimType prim_not_arg_types[] = {PRIM_TYPE_ANY_INT_A};
  static PrimType prim_not_ret_types[] = {PRIM_TYPE_ANY_NUM_A};
  prim_not = new Prim(28, "~", "prim_not", 2, 0, 1, prim_not_arg_types, prim_not_ret_types, 0);
  n = (char *)if1->strings.put((char *)"~");
  p->prims.add(prim_not);
  p->prim_map[0][0].put(n, prim_not);
  static PrimType prim_lnot_arg_types[] = {PRIM_TYPE_ANY_INT_A};
  static PrimType prim_lnot_ret_types[] = {PRIM_TYPE_BOOL};
  prim_lnot = new Prim(29, "!", "prim_lnot", 2, 0, 1, prim_lnot_arg_types, prim_lnot_ret_types, 0);
  n = (char *)if1->strings.put((char *)"!");
  p->prims.add(prim_lnot);
  p->prim_map[0][0].put(n, prim_lnot);
  static PrimType prim_deref_arg_types[] = {PRIM_TYPE_REF};
  static PrimType prim_deref_ret_types[] = {PRIM_TYPE_ANY};
  prim_deref = new Prim(30, "*", "prim_deref", 2, 0, 1, prim_deref_arg_types, prim_deref_ret_types, 0);
  n = (char *)if1->strings.put((char *)"*");
  p->prims.add(prim_deref);
  p->prim_map[0][0].put(n, prim_deref);
  static PrimType prim_cast_arg_types[] = {PRIM_TYPE_SYMBOL, PRIM_TYPE_ANY};
  static PrimType prim_cast_ret_types[] = {PRIM_TYPE_ANY};
  prim_cast = new Prim(31, "(", "prim_cast", 3, 0, 1, prim_cast_arg_types, prim_cast_ret_types, 0);
  n = (char *)if1->strings.put((char *)"(");
  p->prims.add(prim_cast);
  p->prim_map[1][0].put(n, prim_cast);
  static PrimType prim_strcat_arg_types[] = {PRIM_TYPE_STRING, PRIM_TYPE_STRING};
  static PrimType prim_strcat_ret_types[] = {PRIM_TYPE_STRING};
  prim_strcat = new Prim(32, "::", "prim_strcat", 3, 1, 1, prim_strcat_arg_types, prim_strcat_ret_types, 0);
  n = (char *)if1->strings.put((char *)"::");
  p->prims.add(prim_strcat);
  p->prim_map[1][1].put(n, prim_strcat);
  static PrimType prim_ref_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_ref_ret_types[] = {PRIM_TYPE_ANY};
  prim_ref = new Prim(33, "&", "prim_ref", 2, 0, 1, prim_ref_arg_types, prim_ref_ret_types, 0);
  n = (char *)if1->strings.put((char *)"&");
  p->prims.add(prim_ref);
  p->prim_map[0][0].put(n, prim_ref);
  static PrimType prim_primitive_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_primitive_ret_types[] = {PRIM_TYPE_ANY};
  prim_primitive =
      new Prim(34, "primitive", "prim_primitive", -2, 0, -1, prim_primitive_arg_types, prim_primitive_ret_types, 0);
  n = (char *)if1->strings.put((char *)"primitive");
  p->prims.add(prim_primitive);
  p->prim_map[0][0].put(n, prim_primitive);
  static PrimType prim_reply_arg_types[] = {PRIM_TYPE_CONT, PRIM_TYPE_ALL};
  static PrimType prim_reply_ret_types[] = {PRIM_TYPE_ALL};
  prim_reply =
      new Prim(35, "reply", "prim_reply", -3, 0, 1, prim_reply_arg_types, prim_reply_ret_types, PRIM_NON_FUNCTIONAL);
  n = (char *)if1->strings.put((char *)"reply");
  p->prims.add(prim_reply);
  p->prim_map[0][0].put(n, prim_reply);
  static PrimType prim_make_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_make_ret_types[] = {PRIM_TYPE_ANY};
  prim_make = new Prim(36, "make", "prim_make", -3, 0, 1, prim_make_arg_types, prim_make_ret_types, 0);
  n = (char *)if1->strings.put((char *)"make");
  p->prims.add(prim_make);
  p->prim_map[0][0].put(n, prim_make);
  static PrimType prim_vector_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_vector_ret_types[] = {PRIM_TYPE_ANY};
  prim_vector = new Prim(37, "make_vector", "prim_vector", -2, 0, 1, prim_vector_arg_types, prim_vector_ret_types, 0);
  n = (char *)if1->strings.put((char *)"make_vector");
  p->prims.add(prim_vector);
  p->prim_map[0][0].put(n, prim_vector);
  static PrimType prim_new_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_new_ret_types[] = {PRIM_TYPE_ANY};
  prim_new = new Prim(38, "new", "prim_new", -2, 0, 1, prim_new_arg_types, prim_new_ret_types, 0);
  n = (char *)if1->strings.put((char *)"new");
  p->prims.add(prim_new);
  p->prim_map[0][0].put(n, prim_new);
  static PrimType prim_clone_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_clone_ret_types[] = {PRIM_TYPE_ANY};
  prim_clone = new Prim(39, "clone", "prim_clone", -2, 0, 1, prim_clone_arg_types, prim_clone_ret_types, 0);
  n = (char *)if1->strings.put((char *)"clone");
  p->prims.add(prim_clone);
  p->prim_map[0][0].put(n, prim_clone);
  static PrimType prim_clone_vector_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY_INT_A};
  static PrimType prim_clone_vector_ret_types[] = {PRIM_TYPE_ANY};
  prim_clone_vector = new Prim(40, "clone_vector", "prim_clone_vector", -3, 0, 1, prim_clone_vector_arg_types,
                               prim_clone_vector_ret_types, 0);
  n = (char *)if1->strings.put((char *)"clone_vector");
  p->prims.add(prim_clone_vector);
  p->prim_map[0][0].put(n, prim_clone_vector);
  static PrimType prim_isinstance_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_isinstance_ret_types[] = {PRIM_TYPE_ANY};
  prim_isinstance =
      new Prim(41, "isinstance", "prim_isinstance", -3, 0, 1, prim_isinstance_arg_types, prim_isinstance_ret_types, 0);
  n = (char *)if1->strings.put((char *)"isinstance");
  p->prims.add(prim_isinstance);
  p->prim_map[0][0].put(n, prim_isinstance);
  static PrimType prim_issubclass_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_issubclass_ret_types[] = {PRIM_TYPE_ANY};
  prim_issubclass =
      new Prim(42, "issubclass", "prim_issubclass", -3, 0, 1, prim_issubclass_arg_types, prim_issubclass_ret_types, 0);
  n = (char *)if1->strings.put((char *)"issubclass");
  p->prims.add(prim_issubclass);
  p->prim_map[0][0].put(n, prim_issubclass);
  static PrimType prim_index_object_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_index_object_ret_types[] = {PRIM_TYPE_ANY};
  prim_index_object = new Prim(43, "index_object", "prim_index_object", -3, 0, 1, prim_index_object_arg_types,
                               prim_index_object_ret_types, 0);
  n = (char *)if1->strings.put((char *)"index_object");
  p->prims.add(prim_index_object);
  p->prim_map[0][0].put(n, prim_index_object);
  static PrimType prim_set_index_object_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_set_index_object_ret_types[] = {PRIM_TYPE_ANY};
  prim_set_index_object =
      new Prim(44, "set_index_object", "prim_set_index_object", -4, 0, 1, prim_set_index_object_arg_types,
               prim_set_index_object_ret_types, PRIM_NON_FUNCTIONAL);
  n = (char *)if1->strings.put((char *)"set_index_object");
  p->prims.add(prim_set_index_object);
  p->prim_map[0][0].put(n, prim_set_index_object);
  static PrimType prim_meta_apply_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_meta_apply_ret_types[] = {PRIM_TYPE_ANY};
  prim_meta_apply =
      new Prim(45, "meta_apply", "prim_meta_apply", 3, 0, 1, prim_meta_apply_arg_types, prim_meta_apply_ret_types, 0);
  n = (char *)if1->strings.put((char *)"meta_apply");
  p->prims.add(prim_meta_apply);
  p->prim_map[1][0].put(n, prim_meta_apply);
  static PrimType prim_destruct_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_destruct_ret_types[] = {PRIM_TYPE_ANY};
  prim_destruct = new Prim(46, "destruct", "prim_destruct", 2, 0, 1, prim_destruct_arg_types, prim_destruct_ret_types,
                           PRIM_NON_FUNCTIONAL);
  n = (char *)if1->strings.put((char *)"destruct");
  p->prims.add(prim_destruct);
  p->prim_map[0][0].put(n, prim_destruct);
  static PrimType prim_coerce_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_coerce_ret_types[] = {PRIM_TYPE_ANY};
  prim_coerce = new Prim(47, "coerce", "prim_coerce", -3, 0, 1, prim_coerce_arg_types, prim_coerce_ret_types, 0);
  n = (char *)if1->strings.put((char *)"coerce");
  p->prims.add(prim_coerce);
  p->prim_map[0][0].put(n, prim_coerce);
  static PrimType prim_merge_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_merge_ret_types[] = {PRIM_TYPE_ANY};
  prim_merge = new Prim(48, "merge", "prim_merge", -3, 0, 1, prim_merge_arg_types, prim_merge_ret_types, 0);
  n = (char *)if1->strings.put((char *)"merge");
  p->prims.add(prim_merge);
  p->prim_map[0][0].put(n, prim_merge);
  static PrimType prim_merge_in_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_merge_in_ret_types[] = {PRIM_TYPE_ANY};
  prim_merge_in =
      new Prim(49, "merge_in", "prim_merge_in", -3, 0, 1, prim_merge_in_arg_types, prim_merge_in_ret_types, 0);
  n = (char *)if1->strings.put((char *)"merge_in");
  p->prims.add(prim_merge_in);
  p->prim_map[0][0].put(n, prim_merge_in);
  static PrimType prim_type_assert_arg_types[] = {PRIM_TYPE_ANY, PRIM_TYPE_ANY};
  static PrimType prim_type_assert_ret_types[] = {PRIM_TYPE_ANY};
  prim_type_assert = new Prim(50, "type_assert", "prim_type_assert", 3, 0, 1, prim_type_assert_arg_types,
                              prim_type_assert_ret_types, 0);
  n = (char *)if1->strings.put((char *)"type_assert");
  p->prims.add(prim_type_assert);
  p->prim_map[1][0].put(n, prim_type_assert);
  static PrimType prim_len_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_len_ret_types[] = {PRIM_TYPE_ANY};
  prim_len = new Prim(51, "len", "prim_len", 2, 0, 1, prim_len_arg_types, prim_len_ret_types, 0);
  n = (char *)if1->strings.put((char *)"len");
  p->prims.add(prim_len);
  p->prim_map[0][0].put(n, prim_len);
  static PrimType prim_sizeof_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_sizeof_ret_types[] = {PRIM_TYPE_ANY};
  prim_sizeof = new Prim(52, "sizeof", "prim_sizeof", 2, 0, 1, prim_sizeof_arg_types, prim_sizeof_ret_types, 0);
  n = (char *)if1->strings.put((char *)"sizeof");
  p->prims.add(prim_sizeof);
  p->prim_map[0][0].put(n, prim_sizeof);
  static PrimType prim_sizeof_element_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_sizeof_element_ret_types[] = {PRIM_TYPE_SIZE};
  prim_sizeof_element = new Prim(53, "sizeof_element", "prim_sizeof_element", 2, 0, 1, prim_sizeof_element_arg_types,
                                 prim_sizeof_element_ret_types, 0);
  n = (char *)if1->strings.put((char *)"sizeof_element");
  p->prims.add(prim_sizeof_element);
  p->prim_map[0][0].put(n, prim_sizeof_element);
  static PrimType prim_typeof_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_typeof_ret_types[] = {PRIM_TYPE_ANY};
  prim_typeof = new Prim(54, "typeof", "prim_typeof", 2, 0, 1, prim_typeof_arg_types, prim_typeof_ret_types, 0);
  n = (char *)if1->strings.put((char *)"typeof");
  p->prims.add(prim_typeof);
  p->prim_map[0][0].put(n, prim_typeof);
  static PrimType prim_typeof_element_arg_types[] = {PRIM_TYPE_ANY};
  static PrimType prim_typeof_element_ret_types[] = {PRIM_TYPE_ANY};
  prim_typeof_element = new Prim(55, "typeof_element", "prim_typeof_element", 2, 0, 1, prim_typeof_element_arg_types,
                                 prim_typeof_element_ret_types, 0);
  n = (char *)if1->strings.put((char *)"typeof_element");
  p->prims.add(prim_typeof_element);
  p->prim_map[0][0].put(n, prim_typeof_element);
}
