#include <cmath>
#include <limits>
#include <cstdio>
#ifdef BITREPCPP11
# include <cstdint>
#else
# include <stdint.h>
#endif


#if (defined BITREPFMA && defined BITREPCPP11)
# define __BITREPFMA(a,b,c) std::fma(a,b,c)
#else
# define __BITREPFMA(a,b,c) (a*b + c)
#endif

namespace bitrep
{

/*************
 * CONSTANTS *
 *************/
 
static const double const_2_over_pi = 6.3661977236758138e-1;



/*****************************************
 * FORWARD DECLARATION OF SOME FUNCTIONS *
 *****************************************/

double __internal_exp_kernel(double x, int scale);
double __internal_expm1_kernel(double x);
double log1p(double);
double log(double);

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


} // End of namespace bitrep

// Implement C interface
extern "C"
{
#pragma acc routine seq
double br_sin  (double x) { return bitrep::sin  (x); }
#pragma acc routine seq
double br_cos  (double x) { return bitrep::cos  (x); }
#pragma acc routine seq
double br_tan  (double x) { return bitrep::tan  (x); }
#pragma acc routine seq
double br_asin (double x) { return bitrep::asin (x); }
#pragma acc routine seq
double br_acos (double x) { return bitrep::acos (x); }
#pragma acc routine seq
double br_atan (double x) { return bitrep::atan (x); }
#pragma acc routine seq
double br_sinh (double x) { return bitrep::sinh (x); }
#pragma acc routine seq
double br_cosh (double x) { return bitrep::cosh (x); }
#pragma acc routine seq
double br_tanh (double x) { return bitrep::tanh (x); }
#pragma acc routine seq
double br_asinh(double x) { return bitrep::asinh(x); }
#pragma acc routine seq
double br_acosh(double x) { return bitrep::acosh(x); }
#pragma acc routine seq
double br_atanh(double x) { return bitrep::atanh(x); }
#pragma acc routine seq
double br_log  (double x) { return bitrep::log  (x); }
#pragma acc routine seq
double br_log1p(double x) { return bitrep::log1p(x); }
#pragma acc routine seq
double br_exp  (double x) { return bitrep::exp  (x); }
}
