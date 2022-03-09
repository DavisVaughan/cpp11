#pragma once

#include <ostream>
#include <complex> // for complex
#include <type_traits>  // for is_convertible, enable_if

#include "R_ext/Arith.h"      // for NA_REAL
#include "R_ext/Complex.h"    // for Rcomplex
#include "cpp11/R.hpp"        // for SEXP, SEXPREC, ...
#include "cpp11/as.hpp"       // for as_sexp
#include "cpp11/protect.hpp"  // for unwind_protect, preserved
#include "cpp11/sexp.hpp"     // for sexp

namespace cpp11 {

class r_complex {
 public:
  r_complex() = default;

  r_complex(SEXP data) {
    if (Rf_isComplex(data) && Rf_xlength(data) == 1) {
      Rcomplex elt = COMPLEX_ELT(data, 0);
      real_ = elt.r;
      imag_ = elt.i;
    } else {
      throw std::invalid_argument("Invalid r_complex value");
    }
  }

  r_complex(double real, double imag) :
    real_(real), imag_(imag) {}

  explicit r_complex(Rcomplex value) :
    real_(value.r), imag_(value.i) {}
  explicit r_complex(std::complex<double> value) :
    real_(value.real()), imag_(value.imag()) {}

  explicit operator Rcomplex() const {
    return Rcomplex{real_, imag_};
  }
  explicit operator std::complex<double>() const {
    return std::complex<double>(real_, imag_);
  }

  double real() const {
    return real_;
  }
  double imag() const {
    return imag_;
  }
 private:
  double real_;
  double imag_;
};

inline bool operator==(const r_complex& x, const r_complex& y) {
  return (x.real() == y.real()) && (x.imag() == y.imag());
}

inline std::ostream& operator<<(std::ostream& os, const r_complex& value) {
  os << value.real() << "+" << value.imag() << "i" ;
  return os;
}

template <>
inline r_complex na() {
  return r_complex(NA_REAL, NA_REAL);
}

template <>
inline bool is_na(const r_complex& x) {
  return ISNA(x.real()) || ISNA(x.imag());
}

template <typename T, typename R = void>
using enable_if_r_complex = enable_if_t<std::is_same<T, r_complex>::value, R>;

template <typename T>
enable_if_r_complex<T, SEXP> as_sexp(T from) {
  sexp res = Rf_allocVector(CPLXSXP, 1);
  unwind_protect([&] { SET_COMPLEX_ELT(res.data(), 0, static_cast<Rcomplex>(from)); });
  return res;
}

namespace traits {
template <>
struct get_underlying_type<r_complex> {
  using type = Rcomplex;
};
}  // namespace traits

}  // namespace cpp11
