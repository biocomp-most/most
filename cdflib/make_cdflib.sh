#!/bin/bash
ar rc cdflib.a \
	biomath_constants_mod.o biomath_interface_mod.o biomath_mathlib_mod.o \
	biomath_sort_mod.o biomath_strings_mod.o cdf_aux_mod.o cdf_beta_mod.o \
	cdf_binomial_mod.o cdf_chisq_mod.o cdf_f_mod.o cdf_gamma_mod.o \
	cdf_nc_chisq_mod.o cdf_nc_f_mod.o cdf_nc_t_mod.o \
	cdf_neg_binomial_mod.o cdf_normal_mod.o cdf_poisson_mod.o cdf_t_mod.o \
	zero_finder.o
