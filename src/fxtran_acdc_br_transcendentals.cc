#include <cmath>
#include <limits>
#include <cstdio>
#ifdef BITREPCPP11
# include <cstdint>
#else
# include <stdint.h>
#endif

// Copyright (c) 2013, Andrea Arteaga
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
// 
//   Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
// 
//   Redistributions in binary form must reproduce the above copyright notice, this
//   list of conditions and the following disclaimer in the documentation and/or
//   other materials provided with the distribution.
// 
//   Neither the name of the {organization} nor the names of its
//   contributors may be used to endorse or promote products derived from
//   this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
// ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#if (defined BITREPFMA && defined BITREPCPP11)
# define __BITREPFMA(a,b,c) std::fma(a,b,c)
#else
# define __BITREPFMA(a,b,c) (a*b + c)
#endif

namespace fxtran_acdc_br
{

/*************
 * CONSTANTS *
 *************/
 
static const double const_2_over_pi = 6.3661977236758138e-1;
static const double halfLogTwoPi = 0.91893853320467274178032973640562;
static const double squareRootOfPi = 1.77245385090551602729816748334;

/*****************************************
 * FORWARD DECLARATION OF SOME FUNCTIONS *
 *****************************************/

double __internal_exp_kernel(double x, int scale);
double __internal_expm1_kernel(double x);
double log1p(double);
double log(double);
double log_gamma(double);
double gamma(double);
double __cheb_eval(const double  cs[],int order,double x);
double __erfseries(double x);
double erfc(double x);


/********************
 * HELPER FUNCTIONS *
 ********************/

#pragma acc routine seq
double __internal_copysign_pos(double a, double b)
{
    union {
        int32_t i[2];
        double d;
    } aa, bb;
    aa.d = a;
    bb.d = b;
    aa.i[1] = (bb.i[1] & 0x80000000) | aa.i[1];
    return aa.d;
}

#pragma acc routine seq
double __internal_old_exp_kernel(double x, int scale)
{ 
    double t, z;
    int i, j, k;

    union {
        int32_t i[2];
        double d;
    } zz;

    t = std::floor (__BITREPFMA(x, 1.4426950408889634e+0, 4.99999999999999945e-1));
    i = (int)t;
    z = __BITREPFMA (t, -6.9314718055994529e-1, x);
    z = __BITREPFMA (t, -2.3190468138462996e-17, z);
    t = __internal_expm1_kernel (z);
    k = ((i + scale) << 20) + (1023 << 20);

    if (std::abs(i) < 1021) {
        zz.i[0] = 0; zz.i[1] = k;
        z = zz.d;
        z = __BITREPFMA (t, z, z);
    } else {
        j = 0x40000000;
        if (i < 0) {
            k += (55 << 20);
            j -= (55 << 20);
        }
        k = k - (1 << 20);

        zz.i[0] = 0; zz.i[1] = j; /* 2^-54 if a is denormal, 2.0 otherwise */
        z = zz.d;
        t = __BITREPFMA (t, z, z);
        
        zz.i[0] = 0; zz.i[1] = k; /* 2^-54 if a is denormal, 2.0 otherwise */
        z = zz.d;
        z = t * z;
    }
    return z;
}   


/***************************
 * TRIGONOMETRIC FUNCTIONS *
 ***************************/

/**
 * \param x The number whose sin or cos must be computed
 * \param q Represents the quadrant as integer
 */
#pragma acc routine seq
static double __internal_sin_cos_kerneld(double x, int q)

{
    const double __sin_cos_coefficient[16] =
    {
       1.590307857061102704e-10,  /* sin0 */
      -2.505091138364548653e-08,  /* sin1 */
       2.755731498463002875e-06,  /* sin2 */
      -1.984126983447703004e-04,  /* sin3 */
       8.333333333329348558e-03,  /* sin4 */
      -1.666666666666666297e-01,  /* sin5 */
       0.00000000000000000,       /* sin6 */
       0.00000000000000000,       /* unused */
    
      -1.136781730462628422e-11,  /* cos0 */
       2.087588337859780049e-09,  /* cos1 */
      -2.755731554299955694e-07,  /* cos2 */
       2.480158729361868326e-05,  /* cos3 */
      -1.388888888888066683e-03,  /* cos4 */
       4.166666666666663660e-02,  /* cos5 */
      -5.000000000000000000e-01,  /* cos6 */
       1.000000000000000000e+00,  /* cos7 */
    };

    const double *coeff = __sin_cos_coefficient + 8*(q&1);
    double x2 = x*x;

    double z = (q & 1) ? -1.136781730462628422e-11 : 1.590307857061102704e-10;

    z = __BITREPFMA(z, x2, coeff[1]);
    z = __BITREPFMA(z, x2, coeff[2]);
    z = __BITREPFMA(z, x2, coeff[3]);
    z = __BITREPFMA(z, x2, coeff[4]);
    z = __BITREPFMA(z, x2, coeff[5]);
    z = __BITREPFMA(z, x2, coeff[6]);

    x = __BITREPFMA(z, x, x);

    if (q & 1) x = __BITREPFMA(z, x2, 1.);
    if (q & 2) x = __BITREPFMA(x, -1., 0.);

    return x;
}


#pragma acc routine seq
double __internal_tan_kernel(double x, int i)
{
    double x2, z, q;
    x2 = x*x;
    z = 9.8006287203286300E-006;

    z = __BITREPFMA(z, x2, -2.4279526494179897E-005);
    z = __BITREPFMA(z, x2,  4.8644173130937162E-005);
    z = __BITREPFMA(z, x2, -2.5640012693782273E-005);
    z = __BITREPFMA(z, x2,  6.7223984330880073E-005);
    z = __BITREPFMA(z, x2,  8.3559287318211639E-005);
    z = __BITREPFMA(z, x2,  2.4375039850848564E-004);
    z = __BITREPFMA(z, x2,  5.8886487754856672E-004);
    z = __BITREPFMA(z, x2,  1.4560454844672040E-003);
    z = __BITREPFMA(z, x2,  3.5921008885857180E-003);
    z = __BITREPFMA(z, x2,  8.8632379218613715E-003);
    z = __BITREPFMA(z, x2,  2.1869488399337889E-002);
    z = __BITREPFMA(z, x2,  5.3968253972902704E-002);
    z = __BITREPFMA(z, x2,  1.3333333333325342E-001);
    z = __BITREPFMA(z, x2,  3.3333333333333381E-001);
    z = z * x2;
    q = __BITREPFMA(z, x, x);

    if (i) {
        double s = q - x; 
        double w = __BITREPFMA(z, x, -s); // tail of q
        z = - (1. / q);
        s = __BITREPFMA(q, z, 1.0);
        q = __BITREPFMA(__BITREPFMA(z,w,s), z, z);
    }           

    return q;
}


#pragma acc routine seq
static double __internal_trig_reduction_kerneld(double x, int *q_)
{
    double j, t;
    int& q = *q_;

    //q = static_cast<int>(x * const_2_over_pi + .5);
    q = static_cast<int>(std::floor(x * const_2_over_pi + .5));
    j = q;

    t = (-j) * 1.5707963267948966e+000 + x;
    t = (-j) * 6.1232339957367574e-017 + t;
    t = (-j) * 8.4784276603688985e-032 + t;

    // TODO: support huge values (fabs(a) > 2147483648.0)
    
    return t;
}

#pragma acc routine seq
double sin(double x)
{
    double z;
    int q;

    // TODO: support infinite x

    z = __internal_trig_reduction_kerneld(x, &q);
    z = __internal_sin_cos_kerneld(z, q);

    return z;
}

#pragma acc routine seq
double cos(double x)
{
    double z;
    int q;

    // TODO: support infinite x

    z = __internal_trig_reduction_kerneld(x, &q);
    ++q;
    z = __internal_sin_cos_kerneld(z, q);

    return z;
}

#pragma acc routine seq
double tan(double x)
{
    double z, inf = std::numeric_limits<double>::infinity();
    int i;

    if (x == inf || x == -inf) {
        x = x * 0.; // Gives NaN
    }
    z = __internal_trig_reduction_kerneld(x, &i);
    z = __internal_tan_kernel(z, i & 1);
    return z;
}


/***********************************
 * INVERSE TRIGONOMETRIC FUNCTIONS *
 ***********************************/

#pragma acc routine seq
double __internal_asin_kernel(double x)
{
  double r;
  r = 6.259798167646803E-002;
  r = __BITREPFMA (r, x, -7.620591484676952E-002);
  r = __BITREPFMA (r, x,  6.686894879337643E-002);
  r = __BITREPFMA (r, x, -1.787828218369301E-002); 
  r = __BITREPFMA (r, x,  1.745227928732326E-002);
  r = __BITREPFMA (r, x,  1.000422754245580E-002);
  r = __BITREPFMA (r, x,  1.418108777515123E-002);
  r = __BITREPFMA (r, x,  1.733194598980628E-002);
  r = __BITREPFMA (r, x,  2.237350511593569E-002);
  r = __BITREPFMA (r, x,  3.038188875134962E-002);
  r = __BITREPFMA (r, x,  4.464285849810986E-002);
  r = __BITREPFMA (r, x,  7.499999998342270E-002);
  r = __BITREPFMA (r, x,  1.666666666667375E-001);
  r = r * x;
  return r;
}

#pragma acc routine seq
double __internal_atan_kernel(double x)
{
  double t, x2;
  x2 = x * x;
  t = -2.0258553044438358E-005 ;
  t = __BITREPFMA (t, x2,  2.2302240345758510E-004);
  t = __BITREPFMA (t, x2, -1.1640717779930576E-003);
  t = __BITREPFMA (t, x2,  3.8559749383629918E-003);
  t = __BITREPFMA (t, x2, -9.1845592187165485E-003);
  t = __BITREPFMA (t, x2,  1.6978035834597331E-002);
  t = __BITREPFMA (t, x2, -2.5826796814495994E-002);
  t = __BITREPFMA (t, x2,  3.4067811082715123E-002);
  t = __BITREPFMA (t, x2, -4.0926382420509971E-002);
  t = __BITREPFMA (t, x2,  4.6739496199157994E-002);
  t = __BITREPFMA (t, x2, -5.2392330054601317E-002);
  t = __BITREPFMA (t, x2,  5.8773077721790849E-002);
  t = __BITREPFMA (t, x2, -6.6658603633512573E-002);
  t = __BITREPFMA (t, x2,  7.6922129305867837E-002);
  t = __BITREPFMA (t, x2, -9.0909012354005225E-002);
  t = __BITREPFMA (t, x2,  1.1111110678749424E-001);
  t = __BITREPFMA (t, x2, -1.4285714271334815E-001);
  t = __BITREPFMA (t, x2,  1.9999999999755019E-001);
  t = __BITREPFMA (t, x2, -3.3333333333331860E-001);
  t = t * x2;
  t = __BITREPFMA (t, x, x);
  return t;
}


#pragma acc routine seq
double asin(double x)
{
  double fx, t0, t1;
  double xhi, ihi;

  union {
      int32_t i[2];
      double d;
  } xx, fxx;

  fx = std::abs(x);
  xx.d = x;
  xhi = xx.i[1];
  fxx.d = fx;
  ihi = fxx.i[1];

  if (ihi < 0x3fe26666) {
    t1 = fx * fx;
    t1 = __internal_asin_kernel (t1);
    t1 = __BITREPFMA (t1, fx, fx);
    t1 = __internal_copysign_pos(t1, x);
  } else {
    t1 = __BITREPFMA (-0.5, fx, 0.5);
    t0 = std::sqrt (t1);
    t1 = __internal_asin_kernel (t1);
    t0 = -2.0 * t0;
    t1 = __BITREPFMA (t0, t1, 6.1232339957367660e-17);
    t0 = t0 + 7.8539816339744828e-1;
    t1 = t0 + t1;
    t1 = t1 + 7.8539816339744828e-1;
    if (xhi < 0x3ff00000) {
      t1 = __internal_copysign_pos(t1, x);
    }
  }
  return t1;
}

#pragma acc routine seq
double acos(double x)
{
    double t0, t1;

    union {
        int32_t i[2];
        double d;
    } xx, fxx;
    xx.d = x;
    fxx.d = (t0 = std::abs(x));

    const int32_t& xhi = xx.i[1];
    const int32_t& ihi = fxx.i[1];

    if (ihi < 0x3fe26666) {  
        t1 = t0 * t0;
        t1 = __internal_asin_kernel (t1);
        t0 = __BITREPFMA (t1, t0, t0);
        if (xhi < 0) {
            t0 = t0 + 6.1232339957367660e-17;
            t0 = 1.5707963267948966e+0 + t0;
        } else {
            t0 = t0 - 6.1232339957367660e-17;
            t0 = 1.5707963267948966e+0 - t0;
        }
    } else {
        /* acos(x) = [y + y^2 * p(y)] * rsqrt(y/2), y = 1 - x */
        double p, r, y;
        y = 1.0 - t0;
        r = 1. / std::sqrt(y / 2.);
        p = 2.7519189493111718E-006;
        p = __BITREPFMA (p, y, -1.5951212865388395E-006);
        p = __BITREPFMA (p, y,  6.1185294127269731E-006);
        p = __BITREPFMA (p, y,  6.9283438595562408E-006);
        p = __BITREPFMA (p, y,  1.9480663162164715E-005);
        p = __BITREPFMA (p, y,  4.5031965455307141E-005);
        p = __BITREPFMA (p, y,  1.0911426300865435E-004);
        p = __BITREPFMA (p, y,  2.7113554445344455E-004);
        p = __BITREPFMA (p, y,  6.9913006155254860E-004);
        p = __BITREPFMA (p, y,  1.8988715243469585E-003);
        p = __BITREPFMA (p, y,  5.5803571429249681E-003);
        p = __BITREPFMA (p, y,  1.8749999999999475E-002);
        p = __BITREPFMA (p, y,  8.3333333333333329E-002);
        p = p * y * y * r;
        fxx.d = y;
        if (ihi <= 0) {
            t0 = t0 * 0.;
        } else {
            t0 = __BITREPFMA (r, y, p);
        }
        if (ihi < 0) {
            t0 = t0 * std::numeric_limits<double>::infinity();
        }
        if (xhi < 0) {    
            t0 = t0 - 1.2246467991473532e-16;
            t0 = 3.1415926535897931e+0 - t0;
        }
    } 
    return t0;
}

#pragma acc routine seq
double atan(double x)
{
    double t0, t1;
    /* reduce argument to first octant */
    t0 = std::abs(x);
    t1 = t0;
    if (t0 > 1.0) {
        t1 = 1. / t1;
        if (t0 == std::numeric_limits<double>::infinity()) t1 = 0.0;
    }

    /* approximate atan(r) in first octant */
    t1 = __internal_atan_kernel(t1);

    /* map result according to octant. */
    if (t0 > 1.0) {
        t1 = 1.5707963267948966e+0 - t1;
    }
    return __internal_copysign_pos(t1, x);
}


/************************
 * HYPERBOLIC FUNCTIONS *
 ************************/

#pragma acc routine seq
double __internal_expm1_kernel (double x)
{
  double t;
  t = 2.0900320002536536E-009;
  t = __BITREPFMA (t, x, 2.5118162590908232E-008);
  t = __BITREPFMA (t, x, 2.7557338697780046E-007);
  t = __BITREPFMA (t, x, 2.7557224226875048E-006);
  t = __BITREPFMA (t, x, 2.4801587233770713E-005);
  t = __BITREPFMA (t, x, 1.9841269897009385E-004);
  t = __BITREPFMA (t, x, 1.3888888888929842E-003);
  t = __BITREPFMA (t, x, 8.3333333333218910E-003);
  t = __BITREPFMA (t, x, 4.1666666666666609E-002);
  t = __BITREPFMA (t, x, 1.6666666666666671E-001);
  t = __BITREPFMA (t, x, 5.0000000000000000E-001);
  t = t * x;
  t = __BITREPFMA (t, x, x);
  return t;
}

#pragma acc routine seq
double __internal_exp2i_kernel(int32_t b)
{
    union {
        int32_t i[2];
        double d;
    } xx;

    xx.i[0] = 0;
    xx.i[1] = (b + 1023) << 20;

    return xx.d;
}

#pragma acc routine seq
double __internal_expm1_scaled(double x, int scale)
{ 
  double t, z, u;
  int i, j;

  union {
      uint32_t i[2];
      double d;
  } xx;
  xx.d = x;
  uint32_t& k = xx.i[1];

  t = std::floor (__BITREPFMA(x, 1.4426950408889634e+0, 4.99999999999999945e-1));
  i = (int)t + scale;
  z = __BITREPFMA (t, -6.9314718055994529e-1, x);
  z = __BITREPFMA (t, -2.3190468138462996e-17, z);
  k = k + k;
  if ((unsigned)k < (unsigned)0x7fb3e647) {
    z = x;
    i = 0;
  }
  t = __internal_expm1_kernel(z);
  j = i;
  if (i == 1024) j--;
  u = __internal_exp2i_kernel(j);

  xx.i[0] = 0;
  xx.i[1] = 0x3ff00000 + (scale << 20);
  x = xx.d;

  x = u - x;
  t = __BITREPFMA (t, u, x);
  if (i == 1024) t = t + t;
  if (k == 0) t = z;              /* preserve -0 */
  return t;
}   

#pragma acc routine seq
double sinh(double x)
{
    double z;

    union {
        int32_t i[2];
        double d;
    } xx;
    xx.d = x;
    xx.i[1] = xx.i[1] & 0x7fffffff;

    int32_t& thi = xx.i[1];
    double& t = xx.d;

    if (thi < 0x3ff00000) {
        double t2 = t*t;
        z = 7.7587488021505296E-013;
        z = __BITREPFMA (z, t2, 1.6057259768605444E-010);
        z = __BITREPFMA (z, t2, 2.5052123136725876E-008);
        z = __BITREPFMA (z, t2, 2.7557319157071848E-006);
        z = __BITREPFMA (z, t2, 1.9841269841431873E-004);
        z = __BITREPFMA (z, t2, 8.3333333333331476E-003);
        z = __BITREPFMA (z, t2, 1.6666666666666669E-001);
        z = z * t2;
        z = __BITREPFMA (z, t, t);
    } else {
        z = __internal_expm1_scaled (t, -1);
        z = z + z / (__BITREPFMA (2.0, z, 1.0));
        if (t >= 7.1047586007394398e+2) {
            z = std::numeric_limits<double>::infinity();
        }
    }

    z = __internal_copysign_pos(z, x);
    return z;
}

#pragma acc routine seq
double cosh(double x)
{
    double t, z;
    z = std::abs(x);

    union {
        int32_t i[2];
        double d;
    } xx;
    xx.d = z;

    int32_t& i = xx.i[1];

    if ((unsigned)i < (unsigned)0x408633cf) {
        z = __internal_exp_kernel(z, -2);
        t = 1. / z;
        z = __BITREPFMA(2.0, z, 0.125 * t);
    } else {
        if (z > 0.0) x = std::numeric_limits<double>::infinity();
        z = x + x;
    }

    return z;
}

#pragma acc routine seq
double tanh(double x)
{
  double t;
  t = std::abs(x);
  if (t >= 0.55) {
    double s;
    s = 1. / (__internal_old_exp_kernel (2.0 * t, 0) + 1.0);
    s = __BITREPFMA (2.0, -s, 1.0);
    if (t > 350.0) {
      s = 1.0;       /* overflow -> 1.0 */
    }
    x = __internal_copysign_pos(s, x);
  } else {
    double x2;
    x2 = x * x;
    t = 5.102147717274194E-005;
    t = __BITREPFMA (t, x2, -2.103023983278533E-004);
    t = __BITREPFMA (t, x2,  5.791370145050539E-004);
    t = __BITREPFMA (t, x2, -1.453216755611004E-003);
    t = __BITREPFMA (t, x2,  3.591719696944118E-003);
    t = __BITREPFMA (t, x2, -8.863194503940334E-003);
    t = __BITREPFMA (t, x2,  2.186948597477980E-002);
    t = __BITREPFMA (t, x2, -5.396825387607743E-002);
    t = __BITREPFMA (t, x2,  1.333333333316870E-001);
    t = __BITREPFMA (t, x2, -3.333333333333232E-001);
    t = t * x2;
    t = __BITREPFMA (t, x, x);
    x = __internal_copysign_pos(t, x);
  }
  return x;
}


/********************************
 * INVERSE HIPERBOLIC FUNCTIONS *
 ********************************/

#pragma acc routine seq
double __internal_atanh_kernel (double a_1, double a_2)
{
    double a, a2, t;

    a = a_1 + a_2;
    a2 = a * a;
    t = 7.597322383488143E-002/65536.0;
    t = __BITREPFMA (t, a2, 6.457518383364042E-002/16384.0);          
    t = __BITREPFMA (t, a2, 7.705685707267146E-002/4096.0);
    t = __BITREPFMA (t, a2, 9.090417561104036E-002/1024.0);
    t = __BITREPFMA (t, a2, 1.111112158368149E-001/256.0);
    t = __BITREPFMA (t, a2, 1.428571416261528E-001/64.0);
    t = __BITREPFMA (t, a2, 2.000000000069858E-001/16.0);
    t = __BITREPFMA (t, a2, 3.333333333333198E-001/4.0);
    t = t * a2;
    t = __BITREPFMA (t, a, a_2);
    t = t + a_1;
    return t;
}

#pragma acc routine seq
double asinh(double x)
{
  double fx, t;
  fx = std::abs(x);

  union {
      int32_t i[2];
      double d;
  } fxx;
  fxx.d = fx;

  if (fxx.i[1] >= 0x5ff00000) { /* prevent intermediate underflow */
    t = 6.9314718055994529e-1 + log(fx);
  } else {
    t = fx * fx;
    t = log1p (fx + t / (1.0 + std::sqrt(1.0 + t)));
  }
  return __internal_copysign_pos(t, x);  
}

#pragma acc routine seq
double acosh(double x)
{
  double t;
  t = x - 1.0;
  if (std::abs(t) > 4503599627370496.0) {
    /* for large a, acosh = log(2*a) */
    t = 6.9314718055994529e-1 + log(x);
  } else {
    t = t + std::sqrt(__BITREPFMA(x, t, t));
    t = log1p(t);
  }
  return t;
}

double atanh(double x)
{
  double fx, t;
  fx = std::abs(x);

  union {
      int32_t i[2];
      double d;
  } xx;
  xx.d = x;

  t = (2.0 * fx) / (1.0 - fx);
  t = 0.5 * log1p(t);
  if (xx.i[1] < 0) {
    t = -t;
  }
  return t;
}

/**************
 * LOGARITHMS *
 **************/



#pragma acc routine seq
double log(double x)
{
    double m, f, g, u, v, tmp, q, ulo, log_lo, log_hi;
    int32_t ihi, ilo;

    union {
        int32_t i[2];
        double d;
    } xx, mm;
    xx.d = x;

    ihi = xx.i[1];
    ilo = xx.i[0];

    if ((x > 0.) && (x < std::numeric_limits<double>::infinity())) {
        int32_t e = -1023;

        // Normalize denormals
        if (static_cast<uint32_t>(ihi) < static_cast<uint32_t>(0x00100000)) {
            x = x * 9007199254740992.0;
            xx.d = x;
            e -= 54;
            ihi = xx.i[1];
            ilo = xx.i[0];
        }

        e += (ihi >> 20);
        ihi = (ihi & 0x800fffff) | 0x3ff00000;
        mm.i[1] = ihi;
        mm.i[0] = ilo;
        m = mm.d;
        if (static_cast<uint32_t>(ihi) > static_cast<uint32_t>(0x3ff6a09e)) {
            m = m / 2.;
            e = e + 1;
        }

        f = m - 1.0;
        g = m + 1.0;
        u = f / g;
        u = u + u;

        v = u*u;
        q = 6.7261411553826339E-2/65536.0;
        q = __BITREPFMA(q, v, 6.6133829643643394E-2/16384.0);
        q = __BITREPFMA(q, v, 7.6940931149150890E-2/4096.0);
        q = __BITREPFMA(q, v, 9.0908745692137444E-2/1024.0);
        q = __BITREPFMA(q, v, 1.1111111499059706E-1/256.0);
        q = __BITREPFMA(q, v, 1.4285714283305975E-1/64.0);
        q = __BITREPFMA(q, v, 2.0000000000007223E-1/16.0);
        q = __BITREPFMA(q, v, 3.3333333333333326E-1/4.0);
        tmp = 2.0 * (f - u);
        tmp = __BITREPFMA(-u, f, tmp);
        ulo = g * tmp;

        q = q * v;

        log_hi = u;
        log_lo = __BITREPFMA(q, u, ulo);

        q   = __BITREPFMA( e, 6.9314718055994529e-1, log_hi);
        tmp = __BITREPFMA(-e, 6.9314718055994529e-1, q);
        tmp = tmp - log_hi;
        log_hi = q;
        log_lo = log_lo - tmp;
        log_lo = __BITREPFMA(e, 2.3190468138462996e-17, log_lo);
        q = log_hi + log_lo;
    } else if (x != x) {
        q = x + x;
    } else if (x == 0.) {
        q = -std::numeric_limits<double>::infinity();
    } else if (x == std::numeric_limits<double>::infinity()) {
        q = x;
    } else {
        q = std::numeric_limits<double>::quiet_NaN();
    }

    return q;
}


#pragma acc routine seq
double log1p(double x)
{
    double t;
    union {
        int32_t i[2];
        double d;
    } xx;
    xx.d = x;
    
    int i = xx.i[1];
    if (((unsigned)i < (unsigned)0x3fe55555) || ((int)i < (int)0xbfd99999)) {
        /* Compute log2(x+1) = 2*atanh(x/(x+2)) */
        t = x + 2.0;
        t = x / t;
        t = -x * t;
        t = __internal_atanh_kernel(x, t);
    } else {
        t = log (x + 1.);
    }
    return t;
}


#pragma acc routine seq
double __internal_exp_poly(double x)
{
  double t;

  t = 2.5052097064908941E-008;
  t = __BITREPFMA (t, x, 2.7626262793835868E-007);
  t = __BITREPFMA (t, x, 2.7557414788000726E-006);
  t = __BITREPFMA (t, x, 2.4801504602132958E-005);
  t = __BITREPFMA (t, x, 1.9841269707468915E-004);
  t = __BITREPFMA (t, x, 1.3888888932258898E-003);
  t = __BITREPFMA (t, x, 8.3333333333978320E-003);
  t = __BITREPFMA (t, x, 4.1666666666573905E-002);
  t = __BITREPFMA (t, x, 1.6666666666666563E-001);
  t = __BITREPFMA (t, x, 5.0000000000000056E-001);
  t = __BITREPFMA (t, x, 1.0000000000000000E+000);
  t = __BITREPFMA (t, x, 1.0000000000000000E+000);
  return t;
}

#pragma acc routine seq
double __internal_exp_scale(double x, int i)
{
    unsigned int j, k;

    union {
        int32_t i[2];
        double d;
    } xx;

    if (std::abs(i) < 1023) {
        k = (i << 20) + (1023 << 20);
    } else {
        k = i + 2*1023;  
        j = k / 2;
        j = j << 20;
        k = (k << 20) - j;
        xx.i[0] = 0;
        xx.i[1] = j;
        x = x * xx.d;
    }

    xx.i[0] = 0;
    xx.i[1] = k;
    x = x * xx.d;

    return x;
}

#pragma acc routine seq
double __internal_exp_kernel(double x, int scale)
{ 
  double t, z;
  int i;

#if 0
  t = std::floor (x*1.4426950408889634e+0 + 4.99999999999999945e-1);
#else
  t = x*1.4426950408889634e+0 + 4.99999999999999945e-1;
  if (t<0) {t=t-1.;}
  t=(int)t;
#endif
  i = (int)t;
  z = __BITREPFMA(t, -6.9314718055994529e-1, x);
  z = __BITREPFMA(t, -2.3190468138462996e-17, z);
  t = __internal_exp_poly (z);
  z = __internal_exp_scale (t, i + scale); 
  return z;
}   

#pragma acc routine seq
double exp(double x)
{
  double t;
  int i;

  union {
      int32_t i[2];
      double d;
  } xx;
  xx.d = x;

  i = xx.i[1];

  if (((unsigned)i < 0x40862e43) || (i < (int)0xC0874911)) {
    t = __internal_exp_kernel(x, 0);
  } else {
    t = (i < 0) ? 0 : std::numeric_limits<double>::infinity();
    if (!(x == x)) {
      t = x + x;
    }
  }
  return t;
}


#pragma acc routine seq
double erf(double x)
{

  if(std::abs(x) < 1.0) {
    return __erfseries(x);
  }
  else {
    return 1.0 - erfc(x);
  }

}

#pragma acc routine seq
double __cheb_eval(const double cs[],int order,double x)
{


  int j;
  double d  = 0.0;
  double dd = 0.0;

  double y  = x ;
  double y2 = 2.0 * y;


  for(j = order; j>=1; j--) {
    double temp = d;
    d = __BITREPFMA(y2,d,-dd) + cs[j];
    dd = temp;
  }

  d= __BITREPFMA(y,d,-dd);
  d= __BITREPFMA(0.5,cs[0],d); 
//  d = y*d - dd + 0.5 * cs[0];
  
  return d;
}

#pragma acc routine seq
double erfc(double x)
{

const double erfc_xlt1_cs[20] = {
  1.06073416421769980345174155056,
 -0.42582445804381043569204735291,
  0.04955262679620434040357683080,
  0.00449293488768382749558001242,
 -0.00129194104658496953494224761,
 -0.00001836389292149396270416979,
  0.00002211114704099526291538556,
 -5.23337485234257134673693179020e-7,
 -2.78184788833537885382530989578e-7,
  1.41158092748813114560316684249e-8,
  2.72571296330561699984539141865e-9,
 -2.06343904872070629406401492476e-10,
 -2.14273991996785367924201401812e-11,
  2.22990255539358204580285098119e-12,
  1.36250074650698280575807934155e-13,
 -1.95144010922293091898995913038e-14,
 -6.85627169231704599442806370690e-16,
  1.44506492869699938239521607493e-16,
  2.45935306460536488037576200030e-18,
 -9.29599561220523396007359328540e-19
};

const double erfc_x15_cs[25] = {
  0.44045832024338111077637466616,
 -0.143958836762168335790826895326,
  0.044786499817939267247056666937,
 -0.013343124200271211203618353102,
  0.003824682739750469767692372556,
 -0.001058699227195126547306482530,
  0.000283859419210073742736310108,
 -0.000073906170662206760483959432,
  0.000018725312521489179015872934,
 -4.62530981164919445131297264430e-6,
  1.11558657244432857487884006422e-6,
 -2.63098662650834130067808832725e-7,
  6.07462122724551777372119408710e-8,
 -1.37460865539865444777251011793e-8,
  3.05157051905475145520096717210e-9,
 -6.65174789720310713757307724790e-10,
  1.42483346273207784489792999706e-10,
 -3.00141127395323902092018744545e-11,
  6.22171792645348091472914001250e-12,
 -1.26994639225668496876152836555e-12,
  2.55385883033257575402681845385e-13,
 -5.06258237507038698392265499770e-14,
  9.89705409478327321641264227110e-15,
 -1.90685978789192181051961024995e-15,
  3.50826648032737849245113757340e-16
};

const double erfc_x510_cs[20] = {
  1.11684990123545698684297865808,
  0.003736240359381998520654927536,
 -0.000916623948045470238763619870,
  0.000199094325044940833965078819,
 -0.000040276384918650072591781859,
  7.76515264697061049477127605790e-6,
 -1.44464794206689070402099225301e-6,
  2.61311930343463958393485241947e-7,
 -4.61833026634844152345304095560e-8,
  8.00253111512943601598732144340e-9,
 -1.36291114862793031395712122089e-9,
  2.28570483090160869607683087722e-10,
 -3.78022521563251805044056974560e-11,
  6.17253683874528285729910462130e-12,
 -9.96019290955316888445830597430e-13,
  1.58953143706980770269506726000e-13,
 -2.51045971047162509999527428316e-14,
  3.92607828989125810013581287560e-15,
 -6.07970619384160374392535453420e-16,
  9.12600607264794717315507477670e-17
};


  double ax = std::abs(x);
  double e_val;

  /* CHECK_POINTER(result) */

  if(ax <= 1.0) {
    double t = __BITREPFMA(2.0,ax,-1.0);
    e_val= __cheb_eval(erfc_xlt1_cs,19 ,t);
  }
  else if(ax <= 5.0) {
    double ex2 = exp(-x*x);
    double t = 0.5*(ax-3.0);
    e_val= ex2* __cheb_eval(erfc_x15_cs,24 ,t);
  }
 else {
   double exterm = exp(-x*x) / ax;
   double t = (__BITREPFMA(2.0,ax,-15.0))/5.0;
   e_val= exterm*(__cheb_eval(erfc_x510_cs,19 ,t));
 }

  if(x < 0.0) {
    return  2.0 - e_val;
  }
  else {
    return  e_val;
  }

}


#pragma acc routine seq
double __erfseries(double x)
{
  double coef = x;
  double e    = coef;
  double del;
  int k;
  for (k=1; k<30; ++k) {
    coef *= -x*x/k;
    del   = coef/(__BITREPFMA(2.0,k,1.0));
    e += del;
  }
  return 2.0 / squareRootOfPi  * e;

}


#pragma acc routine seq
double gamma
(
    double x    // We require x > 0
)
{
	if (x <= 0.0)
	{
         return std::numeric_limits<double>::quiet_NaN();
	}

    // Split the function domain into three intervals:
    // (0, 0.001), [0.001, 12), and (12, infinity)

    ///////////////////////////////////////////////////////////////////////////
    // First interval: (0, 0.001)
	//
	// For small x, 1/Gamma(x) has power series x + gamma x^2  - ...
	// So in this range, 1/Gamma(x) = x + gamma x^2 with error on the order of x^3.
	// The relative error over this interval is less than 6e-7.

	const double gamma_const = 0.577215664901532860606512090; // Euler's gamma constant

//original version
//    if (x < 0.001)
//        return 1.0/(x*(1.0 + gamma_const*x));
    if (x < 0.001)
    {
        double interm= __BITREPFMA(gamma_const,x,1.0);
        return 1.0/(x*interm);
    }

    ///////////////////////////////////////////////////////////////////////////
    // Second interval: [0.001, 12)
    
	if (x < 12.0)
    {
        // The algorithm directly approximates gamma over (1,2) and uses
        // reduction identities to reduce other arguments to this interval.
		
		double y = x;
        int n = 0;
        bool arg_was_less_than_one = (y < 1.0);

        // Add or subtract integers as necessary to bring y into (1,2)
        // Will correct for this below
        if (arg_was_less_than_one)
        {
            y += 1.0;
        }
        else
        {
            n = static_cast<int> (floor(y)) - 1;  // will use n later
            y -= n;
        }

        // numerator coefficients for approximation over the interval (1,2)
        const double p[] =
        {
            -1.71618513886549492533811E+0,
             2.47656508055759199108314E+1,
            -3.79804256470945635097577E+2,
             6.29331155312818442661052E+2,
             8.66966202790413211295064E+2,
            -3.14512729688483675254357E+4,
            -3.61444134186911729807069E+4,
             6.64561438202405440627855E+4
        };

        // denominator coefficients for approximation over the interval (1,2)
        const double q[] =
        {
            -3.08402300119738975254353E+1,
             3.15350626979604161529144E+2,
            -1.01515636749021914166146E+3,
            -3.10777167157231109440444E+3,
             2.25381184209801510330112E+4,
             4.75584627752788110767815E+3,
            -1.34659959864969306392456E+5,
            -1.15132259675553483497211E+5
        };

        double num = 0.0;
        double den = 1.0;
        int i;

        double z = y - 1;
        for (i = 0; i < 8; i++)
        {
            num = (num + p[i])*z;
//            den = den*z + q[i];
            den = __BITREPFMA(den,z,q[i]);
        }
        double result = num/den + 1.0;

        // Apply correction if argument was not initially in (1,2)
        if (arg_was_less_than_one)
        {
            // Use identity gamma(z) = gamma(z+1)/z
            // The variable "result" now holds gamma of the original y + 1
            // Thus we use y-1 to get back the orginal y.
            result /= (y-1.0);
        }
        else
        {
            // Use the identity gamma(z+n) = z*(z+1)* ... *(z+n-1)*gamma(z)
            for (i = 0; i < n; i++)
                result *= y++;
        }

		return result;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Third interval: [12, infinity)

    if (x > 171.624)
    {
		// Correct answer too large to display. Force +infinity.
		double temp = std::numeric_limits<double>::max();
		return temp*2.0;
    }

    return exp(log_gamma(x));
}

#pragma acc routine seq
double log_gamma
(
    double x    // x must be positive
)
{
	if (x <= 0.0)
	{
          return std::numeric_limits<double>::quiet_NaN();
	}

    if (x < 12.0)
    {
        return log(std::abs(gamma(x)));
    }

	// Abramowitz and Stegun 6.1.41
    // Asymptotic series should be good to at least 11 or 12 figures
    // For error analysis, see Whittiker and Watson
    // A Course in Modern Analysis (1927), page 252

    const double c[8] =
    {
		 1.0/12.0,
		-1.0/360.0,
		1.0/1260.0,
		-1.0/1680.0,
		1.0/1188.0,
		-691.0/360360.0,
		1.0/156.0,
		-3617.0/122400.0
    };
    double z = 1.0/(x*x);
    double sum = c[7];
    for (int i=6; i >= 0; i--)
    {
//        sum *= z;
//        sum += c[i];
        sum = __BITREPFMA(sum,z,c[i]);
    }
    double series = sum/x;

    double loggamma ;
        loggamma = (x - 0.5)*log(x) ;
        loggamma = loggamma - x ;
        loggamma = loggamma + halfLogTwoPi ;
        loggamma = loggamma + series;    
	return loggamma;
}


} // End of namespace fxtran_acdc_br

// Implement C interface
extern "C"
{
#pragma acc routine seq
double fxtran_acdc_br_sin  (double x) { return fxtran_acdc_br::sin  (x); }
#pragma acc routine seq
double fxtran_acdc_br_cos  (double x) { return fxtran_acdc_br::cos  (x); }
#pragma acc routine seq
double fxtran_acdc_br_tan  (double x) { return fxtran_acdc_br::tan  (x); }
#pragma acc routine seq
double fxtran_acdc_br_asin (double x) { return fxtran_acdc_br::asin (x); }
#pragma acc routine seq
double fxtran_acdc_br_acos (double x) { return fxtran_acdc_br::acos (x); }
#pragma acc routine seq
double fxtran_acdc_br_atan (double x) { return fxtran_acdc_br::atan (x); }
#pragma acc routine seq
double fxtran_acdc_br_sinh (double x) { return fxtran_acdc_br::sinh (x); }
#pragma acc routine seq
double fxtran_acdc_br_cosh (double x) { return fxtran_acdc_br::cosh (x); }
#pragma acc routine seq
double fxtran_acdc_br_tanh (double x) { return fxtran_acdc_br::tanh (x); }
#pragma acc routine seq
double fxtran_acdc_br_asinh(double x) { return fxtran_acdc_br::asinh(x); }
#pragma acc routine seq
double fxtran_acdc_br_acosh(double x) { return fxtran_acdc_br::acosh(x); }
#pragma acc routine seq
double fxtran_acdc_br_atanh(double x) { return fxtran_acdc_br::atanh(x); }
#pragma acc routine seq
double fxtran_acdc_br_log  (double x) { return fxtran_acdc_br::log  (x); }
#pragma acc routine seq
double fxtran_acdc_br_log1p(double x) { return fxtran_acdc_br::log1p(x); }
#pragma acc routine seq
double fxtran_acdc_br_exp  (double x) { return fxtran_acdc_br::exp  (x); }
#pragma acc routine seq
double fxtran_acdc_br_erf  (double x) { return fxtran_acdc_br::erf  (x); }
#pragma acc routine seq
double fxtran_acdc_br_erfc  (double x) { return fxtran_acdc_br::erfc  (x); }
#pragma acc routine seq
double fxtran_acdc_br_gamma  (double x) { return fxtran_acdc_br::gamma  (x); }
#pragma acc routine seq
double fxtran_acdc_br_log_gamma  (double x) { return fxtran_acdc_br::log_gamma  (x); }
}
