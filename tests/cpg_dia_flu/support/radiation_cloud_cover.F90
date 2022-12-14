! radiation_cloud_cover.F90 - Compute cumulative cloud cover for McICA
!
! Copyright (C) 2016 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Generate profiles of the cumulative cloud cover as seen from TOA,
! used in the McICA cloud generator.

module radiation_cloud_cover

  use parkind1, only           : jprb

  ! Three overlap schemes.  Note that "Exponential" means that
  ! clear-sky regions have no special significance for computing the
  ! cumulative cloud cover: non-contiguous clouds are exponentially
  ! rather than randomly overlapped. This is the situaition in the
  ! McRad radiation scheme at ECMWF.
  enum, bind(c)
    enumerator IOverlapMaximumRandom, IOverlapExponentialRandom, &
         &     IOverlapExponential
  end enum
  character(len=*), parameter :: OverlapName(0:2) = (/ 'Max-Ran', &
       &                                               'Exp-Ran', &
       &                                               'Exp-Exp' /)

  ! Maximum cloud fraction distinguishable from 1
  real(jprb), parameter :: MaxCloudFrac = 1.0_jprb-epsilon(1.0_jprb)*10.0_jprb


contains

  !---------------------------------------------------------------------
  ! Convert "beta" overlap parameter of Shonk et al. (2010) to "alpha"
  ! overlap parameter of Hogan and Illingworth (2000)
  


  !---------------------------------------------------------------------
  ! Compute total cloud cover according to the specified overlap
  ! rule. This can be used to compute the high, mid and low cloud
  ! cover by passing in subsets of the cloud fraction array
  


  !---------------------------------------------------------------------
  ! Maximum-random overlap: Geleyn & Hollingsworth formula
  
  

  !---------------------------------------------------------------------
  ! Exponential-random overlap: exponential overlap for contiguous
  ! clouds, random overlap for non-contiguous clouds
  



  !---------------------------------------------------------------------
  ! Exponential-exponential overlap: exponential overlap for both
  ! contiguous and non-contiguous clouds. This is the result of the
  ! simple Raisanen cloud generator, but unfortunately it has no
  ! (known) analytic formula for the total cloud cover, or the
  ! cumulative cloud cover.  In partially cloudy columns, The McICA
  ! scheme needs this info in order to devote all the cloudy g-points
  ! to columns containing cloud, which reduces McICA noise. The
  ! following routine provides an approximate estimate of cumulative
  ! cloud cover consistent with the exponential-exponential scheme.
  

end module radiation_cloud_cover
