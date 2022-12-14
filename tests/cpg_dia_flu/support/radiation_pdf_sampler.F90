! radiation_pdf_sampler.f90 - Get samples from a lognormal distribution for McICA
!
! Copyright (C) 2015 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_pdf_sampler

  use parkind1, only : jprb

  implicit none

  !---------------------------------------------------------------------
  ! Derived type for sampling from a lognormal distribution, used to
  ! generate water content or optical depth scalings for use in the
  ! Monte Carlo Independent Column Approximation (McICA)
  type pdf_sampler_type
    ! Number of points in look-up table for cumulative distribution
    ! function (CDF) and fractional standard deviation (FSD)
    ! dimensions
    integer :: ncdf, nfsd

    ! First value of FSD and the reciprocal of the interval between
    ! FSD values (which are assumed to be uniformly distributed)
    real(jprb) :: fsd1, inv_fsd_interval

    ! Value of the distribution for each CDF and FSD bin
    real(jprb), allocatable, dimension(:,:) :: val

  contains

    
    
    
    

  end type pdf_sampler_type

contains

  !---------------------------------------------------------------------
  ! Load look-up table from a file 
  

  !---------------------------------------------------------------------
  ! Deallocate data in pdf_sampler_type derived type
  


  !---------------------------------------------------------------------
  ! Extract the value of a lognormal distribution with fractional
  ! standard deviation "fsd" corresponding to the cumulative
  ! distribution function value "cdf", and return it in val. Since this
  ! is an elemental subroutine, fsd, cdf and val may be arrays.
  


  !---------------------------------------------------------------------
  ! For true elements of mask, extract the values of a lognormal
  ! distribution with fractional standard deviation "fsd"
  ! corresponding to the cumulative distribution function values
  ! "cdf", and return in val. For false elements of mask, return zero
  ! in val.
  

end module radiation_pdf_sampler
