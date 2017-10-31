      program TCL_ctrl
!
! this code reads the status of the fleet of TCLs from a file
! and the reuired power level from a controller, then it calculates
! a number between zero and one for broadcast
!
      implicit none
      real power,hw_power,current_pwr,cop,max_pos,max_neg,max_ramp,
     >     hw_error,gain,more_on,dctrl,delta_t,maxramp
      integer i,ntcl,allbewell
      integer idum1,idum2
      integer,dimension(:),allocatable :: hw_on,hw_switchable
      logical tcl_in_exists,iexist,amiopen
!
! some constants
      cop=2.2
      hw_power=4000./cop
      maxramp=10000.
      gain=0.7
      delta_t=1.0
!
! if power level not -999999999. then keep going
      power=0.
      tcl_in_exists=.false.
      do while (power.ne.-999999999.)
       do while (.not.tcl_in_exists)
        inquire(file='TCL_ctrl_input.csv',iostat=allbewell,err=1000,
     >          exist=tcl_in_exists,opened=amiopen)
       end do
       write(6,*) 'open?',amiopen
       open(unit=12,file='TCL_ctrl_input.csv',form='formatted')
       read(12,*) power
       close(12)
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
! read TCL status file
       open(unit=11,file='donewriting.txt',form='formatted')
       close(11)
       open(unit=11,file='TCL_status.csv',form='formatted')
       read(11,*) ntcl,power
       write(6,*) ntcl,'devices'
       allocate(hw_on(ntcl))
       allocate(hw_switchable(ntcl))
       do i=1,ntcl
        read(11,*) idum1,hw_on(i),hw_switchable(i)
!        write(6,*) hw_on(i),hw_switchable(i)
       end do
       write(6,*) 'done reading status'
! now calculate aggregated TCL power currently
       current_pwr=0.
       do i=1,ntcl
        if (hw_on(i).eq.1) then
         current_pwr=current_pwr+hw_power
        end if
       end do
       write(6,*) 'current power consumption',current_pwr
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
       write(6,*) 'max pos and neg',max_pos,max_neg
       hw_error=power-current_pwr
       write(6,*) current_pwr,power,hw_error,max_pos,max_neg
! finally determine what the signal is that should be broadcast
       if (hw_error > 0.) then
        dctrl=min(maxramp,hw_error)/(max_pos+10000.) ! make sure denominator does not go to zero
       else
        dctrl=max(-maxramp,hw_error)/(max_neg+10000.) ! make sure denominator does not go to zero
       end if
       if (abs(hw_error) < 10000.) dctrl=0.
       more_on=dctrl*delta_t*gain
       if (more_on > 1.) more_on=1.
       write(6,*) 'broadcast control signal',more_on
! deallocate variables
       deallocate(hw_on)
       deallocate(hw_switchable)
! then delete the file
       close(unit=11)
       write(6,*) 'deleted TCL status file'
       open(unit=11,file='broadcast_ctrl.csv',form='formatted')
       write(11,*) more_on
       close(11)
       open(unit=11,file='donewriting.txt',form='formatted')
       close(11,status='delete')
!       open(unit=118,file='wait.txt',form='formatted')   !Added by victor - wait file
!       close(unit=118,status='delete')   !Added by victor - delete the wait file
      end do
!
      end
