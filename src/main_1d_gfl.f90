program GFL_1d_driver

   use modGate
   use module_cu_gf_monan, only: cu_gf_monan_driver       , initModConvParGF &
                               , modConvParGF_initialized , readGFConvParNML !, OUTPUT_SOUND
   use modConstants, only : c_cp, c_alvl
   implicit none

   integer,parameter :: mynum=1
   integer,parameter :: ids=1,ide=2,jds=1,jde=2,kds=1,kde=p_klev & 
                       ,ims=1,ime=1,jms=1,jme=1,kms=1,kme=p_klev & 
                       ,its=1,ite=1,jts=1,jte=1,kts=1,kte=p_klev

   integer :: itime1
   logical :: land
   !- here are the place for data related with the gate soundings
   !- soundings arrays
   integer ::jk, nruns, version, klon_local,klev_local,mzp

   !- this for the namelist gf.inp
   namelist /run/ runname, runlabel, rundata,version, land , klev_sound 

   !- for grads output
   integer :: nrec,nvx,nvar,nvartotal,klevgrads(0:300),int_byte_size,n1,n2,n3
   real    :: real_byte_size
   logical :: init_stat
   real    :: time
  
   integer                                :: itimestep
   integer, dimension(ims:ime,jms:jme)    :: kpbl

   real                                   :: dt, confrq
   real, dimension(ims:ime,jms:jme)       :: areaCell,dx_p,lats,lons
   real, dimension(ims:ime,jms:jme)       :: sflux_r,sflux_t,topt,xland,temp2m
   real, dimension(ims:ime,jms:jme)       :: mpas_cape,mpas_cin

   real, dimension(ims:ime,kms:kme,jms:jme):: u,v,w,press,pi,rvap,rho,temp,tke_pbl  
   real, dimension(ims:ime,kms:kme,jms:jme):: dz8w,p8w,turb_len_scale
   real, dimension(ims:ime,kms:kme,jms:jme):: buoyx, cnvcf

   real, dimension(ims:ime,kms:kme,jms:jme):: rqvblten,rthblten,rthratenlw,rthratensw &
                                             ,rthdyten,rqvdyten
   !-- intent in,out arguments
   real,dimension(ims:ime,jms:jme)         :: raincv,conprr,wlpool,lightn_dens,sigma_deep
   real,dimension(ims:ime,jms:jme)         :: dp_dens,sh_dens,cg_dens
   real,dimension(ims:ime,jms:jme)         :: dp_ierr,sh_ierr,cg_ierr

   real,dimension(ims:ime,kms:kme,jms:jme) :: rthcuten,rqvcuten,rqccuten,rqicuten &
                                             ,rucuten,rvcuten,rbuoyxcuten,rcnvcfcuten

   real,dimension(ims:ime,kms:kme,jms:jme) :: sub3d_rthcuten        &
                                             ,sub3d_rqvcuten        &
                                             ,sub3d_rucuten         &
                                             ,sub3d_rvcuten
   !---intent out arguments
   real,dimension(ims:ime,jms:jme)         :: rmfxdpcu,rmfxmdcu,rmfxshcu &
                                             ,rtopdpcu,rtopmdcu,rtopshcu &
                                             ,rbotdpcu,rmfxdncu
   real,dimension(ims:ime,jms:jme)         :: var2d1,var2d2

   real,dimension(ims:ime,kms:kme,jms:jme) :: rupmfxcu,rdnmfxcu
   real,dimension(ims:ime,kms:kme,jms:jme) :: var3d1


   p_use_gate = .true.

   !------------------- simulation begins  ------------------
   !
   !- reads namelists
   open(15,file='gfl.inp',status='old',form='formatted')    
    read(15,nml=run)
   close(15)

   modConvParGF_initialized = .false.
   init_stat = initModConvParGF()
   !-- read the GF namelist
   call readGFConvParNML(mynum)

   klon_local=p_klon
   klev_local=p_klev  
   itime1=0000
   
   print *, "ims, ime, jms, jme, kms, kme: ", ims,ime, jms,jme, kms,kme
   print *, "its, ite, jts, jte, kts, kte: ", its,ite, jts,jte, kts,kte

   !--- allocation      
   allocate(cupout(0:p_nvar_grads))
   do nvar=0,p_nvar_grads
        allocate(cupout(nvar)%varp(klon_LOCAL,KLEV_LOCAL))
        allocate(cupout(nvar)%varn(3))
        cupout(nvar)%varp(:,:)=0.0
        cupout(nvar)%varn(:)  ="xxxx"
   enddo
   if(.not. p_use_gate) then
       !print*,"====================================================================="
       !print*, "use_gate logical flag must be true to run in 1-d, model will stop"
       !print*,"====================================================================="
       stop "use_gate flag"
   endif


   !  
   !- reads gate soundings                
   IF(trim(RUNDATA) == "GATE.dat") THEN
   open(7,file="GATE.dat",form="formatted",STATUS="OLD")
     read(7,*)
     do jl=1,p_klon
     	read(7,*)
     	!z(m)  p(hpa) t(c) q(g/kg) u  v (m/s) w(pa/s) q1 q2 !!!qr (k/d) advt(k/d) advq(1/s)
     	do jk=p_klev,1,-1
     	read(7,*)pgeo(jl,jk),ppres(jl,jk),ptemp(jl,jk),pq(jl,jk),pu(jl,jk),pv(jl,jk),pvervel(jl,jk), &
     		       zq1(jl,jk),zq2(jl,jk),zqr(jl,jk),zadvt(jl,jk),zadvq(jl,jk)			    
     	print*,"GATE=",jl,jk,pgeo(jl,jk),ppres(jl,jk),ptemp(jl,jk)
     	end do
     enddo
   close(7)
   ENDIF

   !- general  initialization ---------------------------------------
   dx_p           = 22000. !meters
   areaCell       = dx_p*dx_p
   dt             = 450.   !seconds
   time           = 0.
   lons           = 0.
   lats           = 0.
   temp2m   (:,:) = 303.   ! Kelvin
   sflux_r  (:,:) = 700./(1.15*c_alvl) !(kg/kg/s)
   sflux_t  (:,:) = 100./(1.15*c_cp) !(K/s)
   CONPRR   (:,:) = 0.
   topt     (:,:) = 0.
   kpbl     (:,:) = 5
   wlpool   (:,:) = 5.
   tke_pbl        = 1.e-3 
   turb_len_scale = 100.
   buoyx          = 20000.
   mpas_cape      = 1000.      
   mpas_cin       = -100. 
   cnvcf          = 0.1
   if(land) then
     xland  (:,:) = 0. !land
   else
     xland  (:,:) = 1. !ocean
   endif
!- end of  initialization ---------------------------------------


!- big loop on the gate soundings
   do jl=1,klon_LOCAL !klon=number of soundings
     !do jl=10,10 !klon=number of soundings

    time=time+dt
    !if(time/86400. > 2.) cycle
    !write(0,*) "############ sounding:",jl!,time/86400.
    !grid_length= float(jl)*1000.
    print*," ====================================================================="
    print*,"Sounding =",jl,"Processando GFL"
  
                 call cu_gf_monan_driver(    &
                       dt                    &
                      ,confrq                &
                      ,dx_p                  &
                      ,areaCell              &
                      ,lats                  &
                      ,lons                  &
                      ,u                     &
                      ,v                     &
                      ,w                     &
                      ,temp                  &
                      ,rvap                  &
                      ,rho                   &
                      ,press                 &
                      ,pi                    &
                      ,p8w                   &
                      ,dz8w                  &
                      ,topt                  &
                      ,xland                 &
                      ,sflux_t               &
                      ,sflux_r               &
                      ,temp2m                &
                      ,wlpool                &
                      ,mpas_cape             & ! check if it is updated before entering GF
                      ,mpas_cin              & ! check if it is updated before entering GF
                      ,kpbl                  &
                      ,tke_pbl               &
                      ,turb_len_scale        &
                      ,buoyx                 &
                      ,cnvcf                 &
                      ,rthblten              & ! tendency of potential temperature due to pbl processes
                      ,rqvblten              & ! tendency of water vapor mixing ratio due to pbl processes
                      ,rthratenlw            & ! tendency of potential temperature due to long-wave radiation
                      ,rthratensw            & ! tendency of potential temperature due to short-wave radiation
                      ,rthdyten              & ! tendency of potential temperature due to dynamics plus filters
                      ,rqvdyten              & ! tendency of water vapor mixing ration due to dynamics plus filters
                      !---- output ----      
                      ,raincv                &
                      ,conprr                &
                      ,lightn_dens           &
                      ,sigma_deep            &
                      ,rthcuten              &
                      ,rqvcuten              &
                      ,rqccuten              &
                      ,rqicuten              &
                      ,rucuten               &
                      ,rvcuten               &
                      ,rbuoyxcuten           &
                      ,rcnvcfcuten           &

                      ,sub3d_rthcuten        &
                      ,sub3d_rqvcuten        &
                      ,sub3d_rucuten         &
                      ,sub3d_rvcuten         &

                      ,rupmfxcu              &
                      ,rdnmfxcu              &
                      !
                      ,rmfxdpcu              & 
                      ,rmfxdncu              & 
                      ,rmfxmdcu              &
                      ,rmfxshcu              &
                      ,rtopdpcu              &
                      ,rtopmdcu              &
                      ,rtopshcu              &
                      ,rbotdpcu              &
                      ,var2d1                &
                      ,var2d2                &
                      ,var3d1                &
                      !
                      ,ids, ide, jds, jde, kds, kde   &
                      ,ims, ime, jms, jme, kms, kme   &
                      ,its, ite, jts, jte, kts, kte   &
                      ,itimestep,mynum                &
                      ,dp_dens, sh_dens, cg_dens      &
                      ,dp_ierr, sh_ierr, cg_ierr      )

        ! --- vars need to be implemented
                                   ! ,TRACER                &
                                   ! ,rnlcuten              &
                                   ! ,rnicuten              &
                                   ! ,rchemcuten            &
        !-----------------------------                   
   enddo ! loop over gate soundings				     
   !
   !-- output
   print*,"writing grads control file:',trim(runname)//'.ctl"
   !
   !number of variables to be written
   nvartotal=0
   do nvar=0,p_nvar_grads
     if(cupout(nvar)%varn(1) .ne. "xxxx") nvartotal=nvartotal+1
     if(cupout(nvar)%varn(3)  ==  "3d"  ) klevgrads(nvar)=KLEV_LOCAL-1
     if(cupout(nvar)%varn(3)  ==  "2d"  ) klevgrads(nvar)=1
   enddo
  !- binary file 
   inquire (iolength=int_byte_size) real_byte_size  ! inquire by output list
   print*, 'opening grads file:',trim(runname)//'.gra'
   open(19,file=trim(runname)//'.gra',form='unformatted',&
           access='direct',status='replace', recl=int_byte_size*(klon_LOCAL))
   nrec=0
   do nvar=0,p_nvar_grads
       if(cupout(nvar)%varn(1) .ne. "xxxx") then
        do jk=1,klevgrads(nvar)
          nrec=nrec+1
          write(19,REC=nrec) real((cupout(nvar)%varp(:,jk)),4)
        enddo
       endif
   enddo

   close (19)

   !-setting vertical dimension '0' for 2d var
   where(klevgrads==1)klevgrads=0
   !- ctl file
   open(20,file=trim(runname)//'.ctl',status='unknown')
   write(20,2001) '^'//trim(runname)//'.gra'
   write(20,2002) 'undef -9.99e33'
   write(20,2002) 'options'!byteswapped' ! zrev'
   write(20,2002) 'title '//trim(runlabel)
   write(20,2003) 1,0.,1. ! units m/km
   write(20,2004) klon_LOCAL,1.,1.

   IF(trim(RUNDATA) == "GATE.dat") THEN
     write(20,2005) KLEV_LOCAL-1,(ppres(1,jk),jk=1,KLEV_LOCAL-1)
   ELSE
    n1 = KLEV_LOCAL/3
    write(20,2005) KLEV_LOCAL-1,(cupout(0)%varp(1,jk),jk=1,n1)
    n2 = n1 + KLEV_LOCAL/3
    write(20,2009)            (cupout(0)%varp(1,jk),jk=n1+1,n2)
    write(20,2009)            (cupout(0)%varp(1,jk),jk=n2+1,KLEV_LOCAL-1)
   ENDIF
   
   write(20,2006) 1,'00:00Z01JAN2000','1mn'
   write(20,2007) nvartotal
   do nvar=0,p_nvar_grads
    if(cupout(nvar)%varn(1) .ne. "xxxx") then
     write(20,2008) cupout(nvar)%varn(1)(1:len_trim(cupout(nvar)%varn(1)))&
                   ,klevgrads(nvar),cupout(nvar)%varn(2)(1:len_trim(cupout(nvar)%varn(2)))
    endif
   enddo
  
   write(20,2002) 'endvars'
   close(20)
 
  2001 format('dset ',a)
  2002 format(a)
  2003 format('xdef ',i4,' linear ',2f15.3)
  2004 format('ydef ',i4,' linear ',2f15.3)

  2005 format('zdef ',i4,' levels ',200f10.2)
  2009 format(200f10.2)

  2006 format('tdef ',i4,' linear ',2a15)
  2007 format('vars ',i4)
  2008 format(a10,i4,' 99 ',a40)!'[',a8,']')
  2055 format(60f7.0)
   133 format (1x,F7.0)

END PROGRAM GFL_1d_driver

