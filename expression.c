// Make sure xcb expression arithmetic behaves EXACTLY like it would
// in C... by implementing it in C!

#include <libguile.h>


SCM
plus_wrapper (SCM val1, SCM val2)
{
  return scm_from_int(scm_to_int(val1) + scm_to_int(val2));
}

SCM
minus_wrapper (SCM val1, SCM val2)
{
  return scm_from_int(scm_to_int(val1) - scm_to_int(val2));
}

SCM
multiply_wrapper (SCM val1, SCM val2)
{
  return scm_from_int(scm_to_int(val1) * scm_to_int(val2));
}

SCM
divide_wrapper (SCM val1, SCM val2)
{
  return scm_from_int(scm_to_int(val1) / scm_to_int(val2));
}

SCM
logand_wrapper (SCM val1, SCM val2)
{
  return scm_from_int(scm_to_int(val1) & scm_to_int(val2));
}

SCM
lshift_wrapper (SCM val1, SCM val2)
{
  return scm_from_int(scm_to_int(val1) << scm_to_int(val2));
}

SCM
lognot_wrapper (SCM val)
{
  return scm_from_int(~scm_to_int(val));
}


SCM
popcount_wrapper (SCM val)
{
  scm_t_int32 int_val;
  int popcount, bit;

  int_val = scm_to_int(val);
  popcount = int_val & 0x1;

  while(int_val = int_val >> 1) { 
    popcount += int_val & 0x1;
  }

  return scm_from_int(popcount);
}

void
init_xcb_expressions ()
{
  scm_c_define_gsubr ("xcb-add", 2, 0, 0, plus_wrapper);
  scm_c_define_gsubr ("xcb-subtract", 2, 0, 0, minus_wrapper);
  scm_c_define_gsubr ("xcb-multiply", 2, 0, 0, multiply_wrapper);
  scm_c_define_gsubr ("xcb-divide", 2, 0, 0, divide_wrapper);
  scm_c_define_gsubr ("xcb-lsh", 2, 0, 0, lshift_wrapper);
  scm_c_define_gsubr ("xcb-not", 1, 0, 0, lognot_wrapper);
  scm_c_define_gsubr ("xcb-and", 2, 0, 0, logand_wrapper);
  scm_c_define_gsubr ("xcb-popcount", 1, 0, 0, popcount_wrapper);
}
