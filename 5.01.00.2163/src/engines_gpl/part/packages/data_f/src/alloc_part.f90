module alloc_part_mod
!
!  module declarations
!
use global_pointers  ! global data pointers
!
!  data definition module(s)
!
use modeldim         ! model(array) dimensions
use precision        ! single and double precision
      use timers

use alloc_mod        ! generic module for memory allocation, including error handling
!
implicit none        ! force explicit typing
!
contains
      subroutine alloc_part()
!
!
!     ALLOCATES ALL GLOBAL ARRAYS
!           (initially)
!
!     created               : july 2008, by antoon koster
!
!

      implicit none

!     pointer declarations
!
      integer(ip),   parameter  :: nfracmx=4
!
      type(model_dimensions_), pointer :: dim
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "alloc_part", ithndl )
      dim => model_dimensions
!
      call alloc(abuoy     ,dim%no_particles                         )
      call alloc(acf       ,dim%no_cont_releases                     )
      call alloc(aconc     ,dim%no_total_releases ,                  &
                            dim%no_substances                        )
      call alloc(aconud    ,dim%no_substances ,                      &
                            dim%no_ud_releases                       )
      call alloc(adepth    ,dim%no_substances ,                      &
                            dim%no_layers                            )
      call alloc(amap      ,dim%no_substances ,                      &
                            dim%no_layers     ,                      &
                            dim%nmap          ,                      &
                            dim%mmap                                 )
      call alloc(amapsett  ,dim%no_substances ,                      &
                            dim%no_layers     ,                      &
                            dim%nmap          ,                      &
                            dim%mmap                                 )
      call alloc(amassc    ,dim%no_cont_releases ,                   &
                            dim%no_substances    ,                   &
                            dim%max_brkpts_continuous_releases       )
      call alloc(amassd    ,dim%no_substances ,                      &
                            dim%no_dye_releases                      )
      call alloc(angle     ,dim%no_total_segments                    )
      call alloc(apeak     ,dim%no_substances ,                      &
                            dim%no_layers                            )
      call alloc(area      ,dim%no_total_segments                    )
      call alloc(atotal    ,dim%no_layers ,                          &
                            dim%no_substances                        )
      call alloc(atrack    ,dim%no_layers   ,                        &
                            dim%nmap        ,                        &
                            dim%mmap                                 )
      call alloc(cbuff     ,dim%no_particles                         )
      call alloc(cdelv     ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(chismp    ,dim%no_substances ,                      &
                            dim%no_layers     ,                      &
                            dim%no_monitoring_stations               )
      call alloc(chispl    ,dim%no_substances ,                      &
                            dim%no_layers     ,                      &
                            dim%no_monitoring_stations               )
      call alloc(conc      ,dim%no_substances ,                      &
                            dim%no_total_segments                    )
      call alloc(const     ,dim%no_constants                         )
      call alloc(constev   ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(c2        ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(decay     ,dim%no_substances    ,                   &
                            dim%max_brkpts_decay                     )
      call alloc(decays    ,dim%no_substances                        )
      call alloc(depth     ,dim%no_total_segments                    )
      call alloc(dfact     ,dim%no_substances                        )
      call alloc(dps       ,dim%no_hor_segments                      )
      call alloc(drand     ,dim%no_random_parameters                 )
      call alloc(dx        ,dim%no_hor_segments                      )
      call alloc(dy        ,dim%no_hor_segments                      )
      call alloc(elt_names ,dim%no_nefis_elements                    )
      call alloc(elt_types ,dim%no_nefis_elements                    )
      call alloc(elt_dims  ,6,dim%no_nefis_elements                  )
      call alloc(elt_bytes ,dim%no_nefis_elements                    )
      call alloc(finud     ,dim%no_ud_releases                       )
      call alloc(floil     ,dim%no_particles                         )
      call alloc(flow      ,dim%exchanges                            )
      call alloc(flow1     ,dim%noq                                  )
      call alloc(cellpnt   ,dim%noseg                                )
      call alloc(flowpnt   ,dim%noq                                  )
      call alloc(flres     ,dim%no_substances ,                      &
                            dim%no_total_segments                    )
      call alloc(fractd    ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(fracte    ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(fstick    ,dim%no_substances                        )
      call alloc(ftime     ,dim%no_cont_releases  ,                  &
                            dim%max_brkpts_continuous_releases       )
      call alloc(fwatoil   ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(ibuff     ,3,dim%no_particles                       )
      call alloc(ictime    ,dim%no_cont_releases ,                   &
                            dim%max_brkpts_continuous_releases       )
      call alloc(ictmax    ,dim%no_cont_releases                     )
      call alloc(idtime    ,dim%max_brkpts_decay                     )
      call alloc(ifopt     ,dim%no_ud_releases                       )
      call alloc(iftime    ,dim%no_ud_releases                       )
      call alloc(ihplot    ,dim%no_monitoring_stations               )
      call alloc(imap      ,dim%no_particles,3                       )
      call alloc(imask     ,dim%nmap   ,                             &
                            dim%mmap                                 )
      call alloc(ioptrad   ,dim%no_total_releases                    )
      call alloc(ipnt      ,dim%no_total_segments                    )
      call alloc(ipset     ,dim%max_brkpts_plottimes_zoomgrid        )
      call alloc(iptime    ,dim%no_particles                         )
      call alloc(isfile    ,dim%no_substances                        )
      call alloc(isfud     ,dim%no_substances                        )
      call alloc(isub      ,dim%no_ud_releases                       )
      call alloc(iutime    ,dim%no_ud_releases                       )
      call alloc(ivtime    ,dim%max_brkpts_settling_velocities       )
      call alloc(iwndtm    ,dim%max_brkpts_wind                      )
      call alloc(iwtime    ,dim%no_dye_releases                      )
      call alloc(kpart     ,dim%no_particles                         )
      call alloc(kwaste    ,dim%no_total_releases                    )
      call alloc(lgrid     ,dim%nmax  ,                              &
                            dim%mmax                                 )
      call alloc(lgrid2    ,dim%nmax  ,                              &
                            dim%mmax                                 )
      call alloc(lgrid3    ,dim%nmax  ,                              &
                            dim%mmax                                 )
      call alloc(linear    ,dim%no_cont_releases                     )
      call alloc(locdep    ,dim%no_hor_segments ,                    &
                            dim%no_layers                            )
      call alloc(mapsub    ,dim%no_substances                        )
      call alloc(mcell     ,dim%nmap ,                               &
                            dim%mmap                                 )
      call alloc(mpart0    ,dim%no_particles                         )
      call alloc(mpart     ,dim%no_particles                         )
      call alloc(mplsta    ,dim%no_monitoring_stations               )
      call alloc(mstat     ,dim%no_monitoring_stations               )
      call alloc(mstick    ,dim%no_substances                        )
      call alloc(mwaste    ,dim%no_total_releases                    )
      call alloc(nbin      ,dim%no_layers ,                          &
                            dim%nmap      ,                          &
                            dim%mmap                                 )
      call alloc(ncell     ,dim%nmap ,                               &
                            dim%mmap                                 )
      call alloc(ncheck    ,dim%no_cont_releases                     )
      call alloc(ndprt     ,dim%no_total_releases                    )
      call alloc(nmconr    ,dim%no_cont_releases                     )
      call alloc(nmdyer    ,dim%no_dye_releases                      )
      call alloc(nmstat    ,dim%no_monitoring_stations               )
      call alloc(nosud     ,dim%no_ud_releases                       )
      call alloc(nosys     ,dim%no_ud_releases                       )
      call alloc(npart0    ,dim%no_particles                         )
      call alloc(npart     ,dim%no_particles                         )
      call alloc(nplay     ,dim%no_layers                            )
      call alloc(nplot     ,dim%no_particle_tracks                   )
      call alloc(nplsta    ,dim%no_monitoring_stations               )
      call alloc(nstat     ,dim%no_monitoring_stations               )
      call alloc(nwaste    ,dim%no_total_releases                    )
      call alloc(radius    ,dim%no_total_releases                    )
      call alloc(rbuff     ,3,dim%no_particles                       )
      call alloc(rbuffr    ,dim%buffer_size                          )
      call alloc(recovr    ,dim%max_brkpts_plottimes_zoomgrid        )
      call alloc(rem       ,dim%no_cont_releases                     )
      call alloc(rhooilv   ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(stoch     ,dim%no_substances  ,                     &
                            dim%no_cont_releases                     )
      call alloc(stoil     ,dim%no_particles                         )
      call alloc(subst     ,dim%no_layers *                          &
                            dim%no_substances                        )
      call alloc(substi    ,dim%no_substances                        )
      call alloc(subst2    ,dim%no_substances                        )
      call alloc(subsud    ,dim%no_userdef_substances*2              )
      call alloc(t0buoy    ,dim%no_particles                         )
      call alloc(t0cf      ,dim%no_cont_releases                     )
      call alloc(tcktot    ,dim%no_layers                            )
      call alloc(title     ,4                                        )
      call alloc(tmass     ,dim%no_substances                        )
      call alloc(tmassc    ,dim%no_cont_releases ,                   &
                            dim%no_substances                        )
      call alloc(tmassu    ,dim%no_cont_releases                     )
      call alloc(tmasud    ,dim%no_substances ,                      &
                            dim%no_ud_releases                       )
      call alloc(totfe     ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(track     ,8,dim%no_particles                       )
      call alloc(qentr     ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(uscal     ,dim%no_ud_releases                       )
      call alloc(vdiff     ,dim%no_total_segments                    )
      call alloc(velo      ,dim%no_total_segments                    )
      call alloc(viso      ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(visowat   ,dim%no_oil_fractions ,                   &
                            dim%no_particles                         )
      call alloc(volfracw  ,dim%no_oil_fractions *                   &
                            dim%no_particles                         )
      call alloc(vol1      ,dim%noseg                                )
      call alloc(vol2      ,dim%noseg                                )
      call alloc(volume    ,dim%no_total_segments                    )
      call alloc(vrtdsp    ,7,dim%no_particles                       )
      call alloc(vsfact    ,6,dim%no_substances                      )
      call alloc(vsfour    ,6,dim%no_substances ,                    &
                            dim%max_brkpts_settling_velocities       )
      call alloc(wdira     ,dim%max_brkpts_wind                      )
      call alloc(wevap     ,dim%no_particles*2                       )
      call alloc(window    ,4                                        )
      call alloc(wparm     ,dim%no_total_releases                    )
      call alloc(wpart     ,dim%no_substances  ,                     &
                            dim%no_particles                         )
      call alloc(wsettl    ,dim%no_particles                         )
      call alloc(wveloa    ,dim%max_brkpts_wind                      )
      call alloc(xa0       ,dim%no_particles                         )
      call alloc(xa        ,dim%no_particles                         )
      call alloc(xb        ,dim%no_hor_segments                      )
      call alloc(xpart0    ,dim%no_particles                         )
      call alloc(xpart     ,dim%no_particles                         )
      call alloc(xpol      ,dim%max_brkpts_ini_polygone              )
      call alloc(xstat     ,dim%no_monitoring_stations               )
      call alloc(xwaste    ,dim%no_total_releases                    )
      call alloc(xyztrk    ,3,dim%no_particles                       )
      call alloc(ya0       ,dim%no_particles                         )
      call alloc(ya        ,dim%no_particles                         )
      call alloc(yb        ,dim%no_hor_segments                      )
      call alloc(ypart0    ,dim%no_particles                         )
      call alloc(ypart     ,dim%no_particles                         )
      call alloc(ypol      ,dim%max_brkpts_ini_polygone              )
      call alloc(ystat     ,dim%no_monitoring_stations               )
      call alloc(ywaste    ,dim%no_total_releases                    )
      call alloc(za        ,dim%no_particles                         )
      call alloc(zlevel    ,dim%no_hor_segments                      )
      call alloc(zpart     ,dim%no_particles                         )
      call alloc(zwaste    ,dim%no_total_releases                    )

!
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine alloc_part
end module alloc_part_mod
