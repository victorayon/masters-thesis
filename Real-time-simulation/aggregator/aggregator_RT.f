      program TCL_ctrl
!
! this code reads the status of the fleet of TCLs from a file
! and the reuired power level from a controller, then it calculates
! a number between zero and one for broadcast
!
      implicit none
      real power,hw_power,current_pwr,cop,max_pos,max_neg,max_ramp,
     >     hw_error,gain,more_on,dctrl,delta_t,maxramp,current_soc,
     >     current_cap,unfiltered_pwr1,unfiltered_pwr2,unfiltered_pwr3,
     >     unfiltered_signal1,unfiltered_signal2,unfiltered_signal3
      integer i,k,ntcl,allbewell,second
      integer idum1,idum2
      integer,dimension(:),allocatable :: hw_on,hw_switchable
      real,dimension(:),allocatable :: soc,ldb,udb,unfiltered_pwr,
     >                                 unfiltered_signal
      logical tcl_in_exists,iexist,amiopen
      integer my_iostat
      character (256) my_iomsg
      integer date_time(8)
      character*10 r(3)
      integer yearstamp,monthstamp,daystamp,
     >        hourstamp,minutestamp,secondstamp
      character(19) timestamp
      real pi,signal
      real a,b,c0,c1,c2,d1,d2
      real,dimension(3) :: filtered,filtered_signal

      allocate(unfiltered_pwr(86400))
      allocate(unfiltered_signal(86400))
! some constants for filtering
      a=0.0005 ! lower cutoff frequency
      b=0.01 ! higher cutoff frequency
! set filter coefficients
      c0=-b/((1.+a)*(1.+b))
      c1=0.
      c2=b/((1.+a)*(1.+b))
      d1=((1.+a)*(1.-b)+(1.-a)*(1.+b))/((1.+a)*(1.+b))
      d2=-((1.-a)*(1.-b))/((1.+a)*(1.+b))
!
! initialize filtered array
      filtered=0.
      filtered_signal=0.
!
! some constants
      cop=2.5
      hw_power=10000./cop
      maxramp=10000.
      gain=0.
      delta_t=1.0
      pi=4.*atan(1.)
      unfiltered_pwr=0.
      unfiltered_signal=0.
      second=0.
      signal=0.
! set number of devices and allocate status arrays
      ntcl=200
      allocate(hw_on(ntcl))
      allocate(hw_switchable(ntcl))
      allocate(soc(ntcl))
      allocate(ldb(ntcl))
      allocate(udb(ntcl))
      open(unit=32,file='TCL_initstat.csv',form='formatted')
      do i=1,ntcl
       read(32,*) idum1,hw_on(i),hw_switchable(i),
     >                             soc(i),ldb(i),udb(i)
      ! write(6,*) idum1,hw_on(i),hw_switchable(i),soc(i),ldb(i),udb(i)
      end do
      close(32)
!
! if power level not -999999999. then keep going
      power=0.
      tcl_in_exists=.false.
      open(unit=54,file='states.csv',form='formatted')
      open(unit=19,file='power_setpoint.csv',form='formatted')
      do while (power.ne.-999999999.)
       do while (.not.tcl_in_exists)
        inquire(file='TCL_ctrl_input.csv',iostat=allbewell,err=1000,
     >          exist=tcl_in_exists,opened=amiopen)
       end do
!       write(6,*) 'open?',amiopen
       !open(unit=12,file='TCL_ctrl_input.csv',form='formatted')
       !read(12,*) power
       !close(12)
! wait until status file is available
       iexist=.false.
       do while (.not.iexist)
        inquire(file='donewriting.txt',iostat=allbewell,err=1000,
     >          exist=iexist,opened=amiopen)
       end do
       write(6,*) 'TCL status file exists',amiopen
       go to 2000
!
 1000  stop
 2000  continue

       if (second.eq.86400.) second=0.
       second=second+1

! read TCL status file
       open(unit=11,file='donewriting.txt',form='formatted')
       close(11,status='delete')
       write(6,*) ntcl,'devices'
       read(19,*,iostat=my_iostat) signal
       if(my_iostat.ne.0) then
        write(6,*) 'Reading setpoint failed, using previous setpoint'
       end if
       open(unit=11,file='TCL_status.csv',form='formatted')
       do i=1,ntcl
        read(11,*,iostat=my_iostat) idum1,hw_on(i),hw_switchable(i),
     >                              soc(i),ldb(i),udb(i)
        if(my_iostat.ne.0) then
         write(6,*) 'Reading TCL_status failed, using previous status'
        end if
        !write(6,*) idum1,hw_on(i),hw_switchable(i),soc(i),ldb(i),udb(i)
       end do
       write(6,*) 'done reading status'
       close(11)
      ! write(6,*) 'deleted TCL status file'
! now calculate aggregated TCL power currently and SOC
       current_soc=0.
       current_cap=0.
       current_pwr=0.
       !power=50000*sin((pi*signal)/600)
!       write(6,*) 'power =',power
       do i=1,ntcl
        if (hw_on(i).eq.1) then
         current_pwr=current_pwr+hw_power
        end if
!!!        current_soc=current_soc+(soc(i)-ldb(i))
        current_soc=current_soc+soc(i)
        current_cap=current_cap+(udb(i)-ldb(i))
       end do
       current_soc=current_soc/ntcl
       current_cap=current_cap*720000 !this gives cap in joules

!!!!!!!!Move the filtering code below write(54) when live MPC value is ready!!!!!!!!!!

! put current_pwr value into array for filtering
       unfiltered_pwr(second)=current_pwr
! bump off last point from filtered signal and update
       do i=1,2
        filtered(i)=filtered(i+1)
       end do
       if (second.gt.2) then
        unfiltered_pwr3=0.
        unfiltered_pwr2=0.
        unfiltered_pwr1=0.

        unfiltered_pwr3=unfiltered_pwr3+unfiltered_pwr(second)
        unfiltered_pwr2=unfiltered_pwr2+unfiltered_pwr(second-1)
        unfiltered_pwr1=unfiltered_pwr1+unfiltered_pwr(second-2)

        filtered(3)=c0*unfiltered_pwr3+
     >              c1*unfiltered_pwr2+
     >              c2*unfiltered_pwr1+
     >              d1*filtered(2)+
     >              d2*filtered(1)
       end if

! put power signal value into array for filtering
       unfiltered_signal(second)=signal
! bump off last point from filtered signal and update
       do i=1,2
        filtered_signal(i)=filtered_signal(i+1)
       end do
       if (second.gt.2) then
        unfiltered_signal3=0.
        unfiltered_signal2=0.
        unfiltered_signal1=0.

        unfiltered_signal3=unfiltered_signal3+
     >                     unfiltered_signal(second)
        unfiltered_signal2=unfiltered_signal2+
     >                     unfiltered_signal(second-1)
        unfiltered_signal1=unfiltered_signal1+
     >                     unfiltered_signal(second-2)

        filtered_signal(3)=c0*unfiltered_signal3+
     >                     c1*unfiltered_signal2+
     >                     c2*unfiltered_signal1+
     >                     d1*filtered_signal(2)+
     >                     d2*filtered_signal(1)
       end if
       power=-filtered_signal(3)*1000

       call date_and_time(r(1), r(2), r(3), date_time)
       yearstamp = date_time(1)
       monthstamp = date_time(2)
       daystamp = date_time(3)
       hourstamp = date_time(5)
       minutestamp = date_time(6)
       secondstamp = date_time(7)
       write(timestamp,'(I4,A,I2.2,A,I2.2,A,I2.2,
     >          A,I2.2,A,I2.2)') yearstamp,'-',monthstamp,'-',daystamp,
     >          ' ',hourstamp,':',minutestamp,':',secondstamp
!       write(54,*) timestamp,second,current_pwr,current_soc,current_cap,
!     >             power,-filtered(3)
       write(6,*) timestamp
       call flush()
!       write(6,*) 'current pwr soc cap',current_pwr,current_soc,
!     >            current_cap
! then calculate what we have available
       max_pos=0.
       max_neg=0.
       do i=1,ntcl
        if (hw_on(i).eq.1) then
         if (hw_switchable(i).eq.1) then
          max_neg=max_neg+hw_power
         end if
        else
         if (hw_switchable(i).eq.1) then
          max_pos=max_pos+hw_power
         end if
        end if
       end do
!       write(6,*) 'max pos and neg',max_pos,max_neg
       hw_error=power+filtered(3)
!       write(6,*) current_pwr,power,hw_error,max_pos,max_neg
! finally determine what the signal is that should be broadcast
       if (hw_error > 0.) then
        dctrl=min(maxramp,hw_error)/(max_pos+10000.) ! make sure denominator does not go to zero
       else
        dctrl=max(-maxramp,hw_error)/(max_neg+10000.) ! make sure denominator does not go to zero
       end if
       if (abs(hw_error) < 10000.) dctrl=0.
       more_on=dctrl*delta_t*gain
       if (more_on > 1.) more_on=1.
       write(6,*) 'TCL ctrl signal',more_on
       write(54,*) timestamp,second,current_pwr,current_soc,current_cap,
     >             power,-filtered(3),signal*1000,more_on
! write broadcast signal
       open(unit=11,file='broadcast_ctrl.csv',form='formatted')
       write(11,*) more_on
       close(11)
       open(unit=21,file='donewritingtoo.txt',form='formatted')
       write(21,*) 'puke'
       close(21)
!       write(6,*) 'donewritingtoo.txt was created!'
      end do
      close(54)
      close(19)
!
      end
