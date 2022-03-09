#pragma once

#include <algorithm>         // for min
#include <array>             // for array
#include <initializer_list>  // for initializer_list

#include "cpp11/R.hpp"                // for SEXP, SEXPREC, Rf_allocVector
#include "cpp11/as.hpp"               // for as_sexp
#include "cpp11/attribute_proxy.hpp"  // for attribute_proxy
#include "cpp11/named_arg.hpp"        // for named_arg
#include "cpp11/protect.hpp"          // for preserved
#include "cpp11/r_complex.hpp"        // for r_complex
#include "cpp11/r_vector.hpp"         // for r_vector, r_vector<>::proxy
#include "cpp11/sexp.hpp"             // for sexp

// Specializations for complex

namespace cpp11 {

template <>
inline SEXP r_vector<r_complex>::valid_type(SEXP data) {
  if (data == nullptr) {
    throw type_error(CPLXSXP, NILSXP);
  }
  if (TYPEOF(data) != CPLXSXP) {
    throw type_error(CPLXSXP, TYPEOF(data));
  }
  return data;
}

template <>
inline r_complex r_vector<r_complex>::operator[](const R_xlen_t pos) const {
  // NOPROTECT: likely too costly to unwind protect every elt
  return static_cast<r_complex>(is_altrep_ ? COMPLEX_ELT(data_, pos) : data_p_[pos]);
}

template <>
inline typename r_vector<r_complex>::underlying_type* r_vector<r_complex>::get_p(
    bool is_altrep, SEXP data) {
  if (is_altrep) {
    return nullptr;
  } else {
    return COMPLEX(data);
  }
}

template <>
inline void r_vector<r_complex>::const_iterator::fill_buf(R_xlen_t pos) {
  length_ = std::min(64_xl, data_->size() - pos);
  COMPLEX_GET_REGION(data_->data_, pos, length_, buf_.data());
  block_start_ = pos;
}

typedef r_vector<r_complex> complexes;

namespace writable {

template <>
inline typename r_vector<r_complex>::proxy& r_vector<r_complex>::proxy::operator=(
    const r_complex& rhs) {
  if (is_altrep_) {
    // NOPROTECT: likely too costly to unwind protect every set elt
    SET_COMPLEX_ELT(data_, index_, static_cast<Rcomplex>(rhs));
  } else {
    *p_ = static_cast<Rcomplex>(rhs);
  }
  return *this;
}

template <>
inline r_vector<r_complex>::proxy::operator r_complex() const {
  if (p_ == nullptr) {
    // NOPROTECT: likely too costly to unwind protect every elt
    return static_cast<r_complex>(COMPLEX_ELT(data_, index_));
  } else {
    return static_cast<r_complex>(*p_);
  }
}

inline bool operator==(const r_vector<r_complex>::proxy& lhs, r_complex rhs) {
  return static_cast<r_complex>(lhs) == rhs;
}

template <>
inline r_vector<r_complex>::r_vector(std::initializer_list<r_complex> il)
    : cpp11::r_vector<r_complex>(Rf_allocVector(CPLXSXP, il.size())), capacity_(il.size()) {
  protect_ = preserved.insert(data_);
  auto it = il.begin();
  for (R_xlen_t i = 0; i < capacity_; ++i, ++it) {
    SET_COMPLEX_ELT(data_, i, static_cast<Rcomplex>(*it));
  }
}

template <>
inline void r_vector<r_complex>::reserve(R_xlen_t new_capacity) {
  data_ = data_ == R_NilValue ? safe[Rf_allocVector](CPLXSXP, new_capacity)
                              : safe[Rf_xlengthgets](data_, new_capacity);
  SEXP old_protect = protect_;

  // Protect the new data
  protect_ = preserved.insert(data_);

  // Release the old protection;
  preserved.release(old_protect);

  data_p_ = COMPLEX(data_);
  capacity_ = new_capacity;
}

template <>
inline r_vector<r_complex>::r_vector(std::initializer_list<named_arg> il)
    : cpp11::r_vector<r_complex>(safe[Rf_allocVector](CPLXSXP, il.size())),
      capacity_(il.size()) {
  protect_ = preserved.insert(data_);
  int n_protected = 0;

  try {
    unwind_protect([&] {
      Rf_setAttrib(data_, R_NamesSymbol, Rf_allocVector(STRSXP, capacity_));
      SEXP names = PROTECT(Rf_getAttrib(data_, R_NamesSymbol));
      ++n_protected;
      auto it = il.begin();
      for (R_xlen_t i = 0; i < capacity_; ++i, ++it) {
        data_p_[i] = COMPLEX_ELT(it->value(), 0);
        SET_STRING_ELT(names, i, Rf_mkCharCE(it->name(), CE_UTF8));
      }
      UNPROTECT(n_protected);
    });
  } catch (const unwind_exception& e) {
    preserved.release(protect_);
    UNPROTECT(n_protected);
    throw e;
  }
}

template <>
inline void r_vector<r_complex>::push_back(r_complex value) {
  while (length_ >= capacity_) {
    reserve(capacity_ == 0 ? 1 : capacity_ *= 2);
  }
  if (is_altrep_) {
    // NOPROTECT: likely too costly to unwind protect every elt
    SET_COMPLEX_ELT(data_, length_, static_cast<Rcomplex>(value));
  } else {
    data_p_[length_] = static_cast<Rcomplex>(value);
  }
  ++length_;
}

typedef r_vector<r_complex> complexes;

}  // namespace writable

}  // namespace cpp11
