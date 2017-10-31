      program aggro
!
! testbed for simulating residential loads and their aggregation
!
!23456789112345678921234567893123456789412345678951234567896123456789712
!
      implicit none
! for RT output
      logical iexist,amiopen
      integer allbewell

      integer ndays,day,hour,minute,second,seconds,dayseconds
      integer meters,nhloads,seed,ncluster,npoints,halfmeters,
     >        quartermeters,nweeks,dayofweek
      integer load,meter,whichmeter,metercluster,meterclusters
      integer lightcount,maxcount,counter,sgl_counter
      integer hw_avl_for_on,hw_avl_for_off,ac_avl_for_on,ac_avl_for_off
      integer n_events,points,lpoints,thiscluster,totalseconds
      integer i,j,k,l,dummy1,dummy2
      real imax,imin,jmax,jmin
      real pi
      real dummy
      integer secm,secp,timeinsecs,ntemps
      integer secs(10000)
      integer meterlist(100)
      integer meterinlist,nlist
      real meterlistpower
      real degrees(10000)
      real ran2,randy,t_amb,ldb,udb,Q_loss,hw_ldb,hw_udb
      real start_time,duration,hrs,overrun
      real ctrl,dctrl,freq,delta_t,denmtr,numrtr,ac_gain,linslope,
     >     more_on,internal_rand,switch_time,avg_hw_soc,hw_gain,
     >     hw_switch_time,hw_error,hw_power,max_pos,max_neg,
     >     maxramp,totalpowerall1,totalpowerall2,totalpowerall3,
     >     ac_power,ac_error,hw_ldbn,hw_udbn,totaltotal,hp_mode
      real Q_AC,COP,k1,k2,k3,ch_rate,HWCOP,R_X,R_loss
      real a,b,c0,c1,c2,d1,d2
      real pdf(200,2),lpdf(200,2)
      real,dimension(50) :: average
      real,dimension(3) :: filtered
      real,allocatable :: power(:),t_space(:),A_HX(:),
     >                    R_ins(:),t_mass(:),t_set(:),hw_soc(:),
     >                    t_fridge(:),f_mass(:),avgpwr(:),time_on(:),
     >                    time_off(:),hw_time_on(:),hw_time_off(:),
     >                    t_avg(:),clusterpower(:),ac_soc(:)
      real,allocatable :: hload(:,:,:)
      real,allocatable :: hload_or(:,:),hload_or_old(:,:),
     >                    totalpower(:,:)
      real,allocatable :: e_pdf(:,:,:,:),s_pdf(:,:,:,:),d_pdf(:,:,:,:)
      integer,allocatable :: e_pdfn(:,:),s_pdfn(:,:),d_pdfn(:,:)
      integer,allocatable :: mycluster(:),db_on(:),acs_on(:),cluster(:)
      logical check,interference,currentstate,hw_currentstate,weekend
      logical,allocatable :: havehload(:,:),neednewschedule(:,:)
      logical,allocatable :: ac_on(:),heater_on(:),cpr_on(:),
     >                       burner1on(:),burner2on(:),burner3on(:)
      integer date_time(8),monthdays(12)
      integer start_hour,start_minute,start_second
      integer this_month,this_day,this_week,today
      real dayofyear,total_load
      character*10 r(3)
      integer yearstamp,monthstamp,daystamp,
     >        hourstamp,minutestamp,secondstamp
      character(19) timestamp
      integer my_iostat
      character (256) my_iomsg
!
      !write(6,*) 'how many days?'
      !read(5,*) ndays
      ndays=30
      !write(6,*) 'which meter for output?'
      !read(5,*) whichmeter
      whichmeter=42
!
! set up sublist of meters for power output
! first element in array is number of meters, then meter IDs
!
      meterlist(1)=5
      meterlist(2)=20
      meterlist(3)=25
      meterlist(4)=55
      meterlist(5)=110
      meterlist(6)=165
!
! some constants for filtering
      a=0.00005 ! lower cutoff frequency
      b=0.02 ! higher cutoff frequency
!
! some control constants
      delta_t=1.
      hw_power=500000.
      ac_power=50000.
      maxramp=b*hw_power ! assume characteristic system power
      ac_gain=0.0
      hw_gain=0.0
      more_on=0.0 ! start TCL broadcast signal to 0
      t_amb=25.
      sgl_counter=0
!
! set filter coefficients
      c0=-b/((1.+a)*(1.+b))
      c1=0.
      c2=b/((1.+a)*(1.+b))
      d1=((1.+a)*(1.-b)+(1.-a)*(1.+b))/((1.+a)*(1.+b))
      d2=-((1.-a)*(1.-b))/((1.+a)*(1.+b))
!
! initialize filtered array
      filtered=0.
!
! set some variables, hard code now but read from file later
      meters=200
      quartermeters=meters/4
      halfmeters=meters/2
      seed=22
      maxcount=5000
      totalseconds=86400
      switch_time=300.
      hw_switch_time=300.
      allocate(mycluster(meters))
!
! read temperature data file
!      open(unit=11,file='phx_temps.csv',form='formatted')
!      read(11,*) ntemps
!      do i=1,ntemps
!       read(11,*) secs(i),degrees(i)
!!       write(6,*) secs(i),degrees(i)
!      end do
!      close(11)
!
! allocate space temperature related variables
      allocate(t_space(meters)) ! this is the current space temperature of a house
      allocate(R_ins(meters)) ! this is the thermal resistance of the structure
      allocate(A_HX(meters)) ! this is the combined area of walls and roof
      allocate(t_mass(meters)) ! this is the thermal capacity of the structure + air in it
      allocate(t_set(meters)) ! this is the temperature set point
      allocate(ac_on(meters)) ! this is the on/off status of the AC unit
      allocate(hw_soc(meters)) ! this is the state of charge of the water heater
      allocate(ac_soc(meters)) ! this is the state of charge of the AC units
      allocate(heater_on(meters)) ! this is the on/off status of the AC unit
      allocate(t_fridge(meters)) ! this is the on/off status of the AC unit
      allocate(f_mass(meters)) ! this is the on/off status of the AC unit
      allocate(cpr_on(meters)) ! this is the on/off status of the AC unit
      allocate(burner1on(meters)) ! this is the on/off status of the small burner on electric range
      allocate(burner2on(meters)) ! this is the on/off status of the medium burner on electric range
      allocate(burner3on(meters)) ! this is the on/off status of the large burner on electric range
      allocate(time_on(meters)) ! this is how long AC was on
      allocate(time_off(meters)) ! this is how long AC was off
      allocate(hw_time_on(meters)) ! this is how long heater was on
      allocate(hw_time_off(meters)) ! this is how long heater was off
!
! initialize space temperatures & related variables
      pi=4.*atan(1.)
      freq=0.0005
      do i=1,meters
       t_space(i)=23.-2.0+4.0*ran2(seed) ! some basic randomness
       t_fridge(i)=4.-0.25+0.5*ran2(seed) ! some basic randomness
       hw_soc(i)=0.95-0.05+0.1*ran2(seed) ! some basic randomness
       R_ins(i)=2.0-0.5+1.0*ran2(seed) ! thermal resistance of structure in m2 K / W
       if (R_ins(i) < 1.) R_ins(i)=1. ! set minimum thermal resistance of structure in m2 K / W
       A_HX(i)=369.7-100.+200.*ran2(seed) ! heat exchange area of structure in m2
       t_mass(i)=1.0e6-2.0e5+4.0e5*ran2(seed) ! mass x heat capacity incl. walls
       f_mass(i)=20000.-5000+10000.*ran2(seed) ! about 10 kg of stuff with c_p = 2000
       t_set(i)=22.-1.+2.*ran2(seed) ! same for all, for now
      end do
      ac_on(1:halfmeters)=.false. ! half ACs turned off
      ac_on(halfmeters+1:meters)=.false. ! the other half ACs turned off too
      heater_on=.false. ! all hot water heaters turned off
      cpr_on=.false. ! all refrigerator compressors off
      burner1on=.false. ! all small burners off
      burner2on=.false. ! all medium burners off
      burner3on=.false. ! all large burners off
      time_on=0.
      time_off=0.
      hw_time_on=0.
      hw_time_off=0.
      currentstate=.false.
      hw_currentstate=.false.
!
! set up list of meter clusters (not to be confused with demographic
! cluster
      allocate(cluster(meters))
      open(unit=11,file='clusters.csv',form='formatted')
      read(11,*) meterclusters
      do i=1,meters
       read(11,*) dummy1,dummy2
       cluster(dummy1)=dummy2
      end do
      close(11)
      allocate(clusterpower(meterclusters))
!
!
! read cdfs for number of events of given load type for cluster
      open(unit=11,file='eventpdfC.csv',form='formatted')
      read(11,*) ncluster,nhloads ! NOTE: the number of loads is set HERE
! we just read the total number of clusters, and the possible human-driven loads
      allocate(power(nhloads))
      allocate(db_on(totalseconds))
      allocate(acs_on(totalseconds))
      allocate(t_avg(totalseconds))
      allocate(totalpower(nhloads,totalseconds))
      allocate(avgpwr(totalseconds))
      allocate(havehload(meters,nhloads))
      allocate(neednewschedule(meters,nhloads))
      allocate(e_pdf(ncluster,nhloads,200,3))
! note that we limit the number of data points for the cdf to 100
      allocate(e_pdfn(ncluster,nhloads)) ! this variable stores the actual number of points that define the cvf
      allocate(hload(nhloads,meters,301)) ! up to 150 starts
      allocate(hload_or(nhloads,meters))
      allocate(hload_or_old(nhloads,meters))
      average=0.
      avgpwr=0.
      hload_or=0.0
!      do i=1,ncluster
       do j=1,nhloads
        read(11,*) npoints
        !write(6,*) 'for load',j,npoints,'points'
        e_pdfn(1:ncluster,j)=npoints
        do k=1,npoints
!         read(11,*) e_pdf(i,j,k,1),e_pdf(i,j,k,2) ! xy coords of points
         read(11,*) ((e_pdf(i,j,k,l),l=1,3),i=1,ncluster) ! l=2 is weekday, l=3 is weekend
!         read(11,*) (e_pdf(i,j,k,1:3),i=1,ncluster) ! l=2 is weekday, l=3 is weekend
        end do
       end do
!      end do
      close(11)
!
!23456789112345678921234567893123456789412345678951234567896123456789712
! read pdfs for start time distribution of given load type for cluster
      open(unit=11,file='startpdfC.csv',form='formatted')
      allocate(s_pdf(ncluster,nhloads,100,3))
      allocate(s_pdfn(ncluster,nhloads))
!      do i=1,ncluster
       do j=1,nhloads
        read(11,*) npoints
        s_pdfn(1:ncluster,j)=npoints
        do k=1,npoints
!         read(11,*) s_pdf(i,j,k,1),s_pdf(i,j,k,2) ! xy coords of points
         read(11,*) ((s_pdf(i,j,k,l),l=1,3),i=1,ncluster) ! xy coords of points
!         read(11,*) (s_pdf(i,j,k,1:3),i=1,ncluster) ! xy coords of points
        end do
       end do
!      end do
      close(11)
!
! read pdfs for duration time distribution of given load type for cluster
      open(unit=11,file='durnpdfC.csv',form='formatted')
      allocate(d_pdf(ncluster,nhloads,100,3))
      allocate(d_pdfn(ncluster,nhloads))
!      do i=1,ncluster
       do j=1,nhloads
        read(11,*) npoints
        d_pdfn(1:ncluster,j)=npoints
        do k=1,npoints
!         read(11,*) d_pdf(i,j,k,1),d_pdf(i,j,k,2) ! xy coords of points
         read(11,*) ((d_pdf(i,j,k,l),l=1,3),i=1,ncluster) ! xy coords of points
!         read(11,*) (d_pdf(i,j,k,1:3),i=1,ncluster) ! xy coords of points
        end do
       end do
!      end do
      close(11)
      !write(6,*) 'read all the cdfs'
!
! now print the cdfs just to make sure they are read correctly
      !write(6,*) 'events'
      !do i=1,nhloads
       !write(6,*) 'load number ',i
       !do j=1,e_pdfn(1,i)
        !write(6,*) ((e_pdf(k,i,j,l),l=1,3),k=1,ncluster)
       !end do
      !end do
      !write(6,*) 'starts'
      !do i=1,nhloads
       !write(6,*) 'load number ',i
       !do j=1,s_pdfn(1,i)
        !write(6,*) ((s_pdf(k,i,j,l),l=1,3),k=1,ncluster)
       !end do
      !end do
      !write(6,*) 'durations'
      !do i=1,nhloads
       !write(6,*) 'load number ',i
       !do j=1,d_pdfn(1,i)
        !write(6,*) ((d_pdf(k,i,j,l),l=1,3),k=1,ncluster)
       !end do
      !end do
!
! for each meter, decide when human-driven loads turn on
! and how long they stay on
! these loads are appliances such as washer, dryer, dishwasher
! probability density functions belong to specific clusters
!
!23456789112345678921234567893123456789412345678951234567896123456789712
! determine which meters have which loads
! later read from file, now set by hand
      havehload(1:meters,1:6)=.true.
! initialize request for new schedules - modify later according to persistence
      neednewschedule(1:meters,1:6)=.true.
! this is where the meter is assigned to a cluster
! for now all the same, later diversify
!      mycluster(1:meters)=1
      mycluster(1:quartermeters)=1
      mycluster(quartermeters+1:halfmeters)=2
      mycluster(halfmeters+1:halfmeters+quartermeters)=3
      mycluster(halfmeters+quartermeters+1:meters)=4
!
      !open(unit=12,file='hloads.csv',form='formatted')
      !open(unit=13,file='totals.csv',form='formatted')
      !open(unit=14,file='temps.csv',form='formatted')
      !open(unit=15,file='hwsoc.csv',form='formatted')
      !open(unit=16,file='fridge.csv',form='formatted')
      !open(unit=17,file='range.csv',form='formatted')
      !open(unit=18,file='lights.csv',form='formatted')
      !open(unit=19,file='meterloads.csv',form='formatted')
      !open(unit=33,file='listloads.csv',form='formatted')
      totalpower=0.0

! create array with number of days per month
      monthdays(1) = 31
      monthdays(2) = 28
      monthdays(3) = 31
      monthdays(4) = 30
      monthdays(5) = 31
      monthdays(6) = 30
      monthdays(7) = 31
      monthdays(8) = 31
      monthdays(9) = 30
      monthdays(10) = 31
      monthdays(11) = 30
      monthdays(12) = 31
! get current date and time
      call date_and_time(r(1), r(2), r(3), date_time)
      write(6,*)'month_of_year=',date_time(2)
      write(6,*)'day_of_month=',date_time(3)
      write(6,*)'hour of day=',date_time(5)
      write(6,*)'minutes of hour=',date_time(6)
      write(6,*)'seconds of minute=',date_time(7)
      start_hour = date_time(5)
      start_minute = date_time(6)
      start_second = date_time(7)
      this_month = date_time(2)
      this_day = date_time(3)
!  figure out which day of the week today is
      dayofyear = 0
      do i = 1,this_month-1
       dayofyear = dayofyear + monthdays(i)
      end do
      dayofyear = dayofyear + this_day
      this_week = floor((dayofyear/7))
      today = dayofyear - 7*this_week 
      write(6,*)'today is',today

      do day=today,today+ndays
       weekend=.false.
! figure out if day is a weekend (WE = day N*7+1, N*7+7)
       nweeks=int((day-1)/7)
       dayofweek=day-7*nweeks
       if (dayofweek.eq.1) weekend=.true.
       if (dayofweek.eq.7) weekend=.true.
       hload=0.
       do meter=1,meters
        thiscluster=mycluster(meter)
! clean out daily variables
        do load=1,nhloads
         !write(6,*) 'load type',load
! does the customer have this load or not
         if (havehload(meter,load)) then
! using pdf from appropriate cluster, determine:
! - how many events for this load today
! - start time and duration for each event
          randy=ran2(seed)
!          write(6,*) randy,seed
          n_events=0 ! this is the default
          points=e_pdfn(thiscluster,load)
          do i=1,points
           if (weekend) then
            pdf(i,1)=e_pdf(thiscluster,load,i,1)
            pdf(i,2)=e_pdf(thiscluster,load,i,3)
           else
            pdf(i,1)=e_pdf(thiscluster,load,i,1)
            pdf(i,2)=e_pdf(thiscluster,load,i,2)
           end if
          end do
          call howmany(pdf,points,randy,n_events)
          !write(6,*) 'on day',day,'load',load,'on meter',meter,
!     >                'starts',n_events,'times'
          hload(load,meter,1)=1.*n_events
! the above variable contains, for load type "load" and for meter number "meter",
! the number of events (up to 150), the start time of all of the i events in location i*2,
! and the duration of all of the i events, in location 2*i+1
!
! now transfer cdf for start time
          points=s_pdfn(thiscluster,load)
          do i=1,points
           if (weekend) then
            pdf(i,1)=s_pdf(thiscluster,load,i,1)
            pdf(i,2)=s_pdf(thiscluster,load,i,3)
           else
            pdf(i,1)=s_pdf(thiscluster,load,i,1)
            pdf(i,2)=s_pdf(thiscluster,load,i,2)
           end if
          end do
! and pdf for duration
          points=d_pdfn(thiscluster,load)
          do i=1,points
           if (weekend) then
            lpdf(i,1)=d_pdf(thiscluster,load,i,1)
            lpdf(i,2)=d_pdf(thiscluster,load,i,3)
           else
            lpdf(i,1)=d_pdf(thiscluster,load,i,1)
            lpdf(i,2)=d_pdf(thiscluster,load,i,2)
           end if
          end do
          !write(6,*) 'loaded cdfs for start & duration'
!
!23456789112345678921234567893123456789412345678951234567896123456789712
          do i=1,n_events
           interference=.true. ! make the code get into the loop
           counter=0
           do while((interference).and.(counter.lt.maxcount))
            counter=counter+1
            randy=ran2(seed)
            call whentostart(pdf,points,randy,start_time)
            randy=ran2(seed)
            call howlong(lpdf,lpoints,randy,duration)
            if (load.eq.5) then ! this is a range
             if (start_time.lt.11) then ! breakfast
              duration=duration/3.
             else if (start_time.lt.16.) then ! lunch
              duration=duration/2.
             else ! dinner
              duration=duration/1.
             end if
            end if
! comment lines below for many meters
!            write(6,*) 'checking start time',start_time,
!     >                 'with duration',duration
! check for interference with previously set operation periods
            interference=.false.
            if (load.ne.6) then ! lighting loads (6) are allowed to interfere
             do j=1,i-1
              overrun=hload_or(load,meter)
              imin=start_time
              imax=start_time+duration
              jmin=hload(load,meter,j*2)
              jmax=hload(load,meter,j*2)+hload(load,meter,j*2+1)
!              write(6,*) i,j,imin,imax,jmin,jmax
              if (
     >            (imin.lt.overrun).or.
     >            ((imax.gt.jmin).and.(imax.lt.jmax)).or.
     >            ((imin.gt.jmin).and.(imin.lt.jmax)).or.
     >            ((imax.gt.jmax).and.(imin.lt.jmin))
     >           ) interference=.true.
             end do
            end if
           end do
! comment lines below for many meters
!           write(6,*) 'event number',i,'assigned'
           if (counter.ge.maxcount) start_time=0.
           if (counter.ge.maxcount) duration=0.
           hload(load,meter,i*2)=start_time
           hload(load,meter,i*2+1)=duration
!           if ((load.eq.6).and.(meter.eq.42)) then
!            write(6,*) 'lights42',i,start_time,start_time+duration
!           end if
          end do
! now erase old overrun and set new one - this is going to be used for following day schedules
! note that overrun can be set to zero if there isn't one
          hload_or_old(load,meter)=hload_or(load,meter)
          hload_or(load,meter)=0.0
          if (load.ne.6) then ! lighting loads don't have to worry about overrun
           do i=1,int(hload(load,meter,1))
            if ((hload(load,meter,2*i)+
     >           hload(load,meter,2*i+1)).gt.24.) then
             hload_or(load,meter)=hload(load,meter,2*i)+
     >                            hload(load,meter,2*i+1)-24.
            end if
           end do
          end if
         end if
        end do
!        if (meter.eq.42) then ! write AC sechedule for meter 42
!         do i=1,int(hload(2,meter,1))
!          write(6,*) 'event42',i,hload(2,meter,2*i),
!     >               hload(2,meter,2*i)+hload(2,meter,2*i+1)
!         end do
!         write(6,*),'event42',hload_or(2,meter)
!        end if
       end do ! first meter loop to set schedules
       open(unit=44,file='ac_load.csv',form='formatted') !TEST FOR AC TCL CTRL
! now calculate things and output results over course of day
       totalpower=0.
       do hour=start_hour+1,24
        !write(6,*) 'hour =',hour
        do minute=start_minute+1,60
         !write(6,*) 'minute =',minute
         do second=start_second+1,60

! wait here until control file exists
          iexist=.false.
          do while (.not.iexist)
           !inquire(file='iamtoo.txt',iostat=allbewell,err=1000,
           inquire(file='donewritingtoo.txt',iostat=allbewell,err=1000,
     >             exist=iexist,opened=amiopen)
          end do
!          write(6,*) 'found donewritingtoo.txt'
          go to 2000

 1000  stop
 2000  continue
!!!! delete control file
!!!          open(unit=21,file='iamtoo.txt')
!!!          close(unit=21,status='delete')

          sgl_counter=sgl_counter+1
          open(unit=87,file='donewritingtoo.txt')
          close(87,status='delete')
          open(unit=22,file='TCL_status.csv',form='formatted')
          write(22,*) sgl_counter

          !write(6,*) 'second =',second
          meterlistpower=0.
!          seconds=second-1+60*(minute-1)+3600*(hour-1)+86400*(day-1)
          dayseconds=second-1+60*(minute-1)+3600*(hour-1)
          seconds=dayseconds ! resets to zero every day
          hrs=dayseconds/3600.
          if (hrs.lt.4.0) then
!           hw_ldb=0.4
           hw_ldb=0.9
           hw_udb=1.0
          else if (hrs.lt.7.0) then
!           hw_ldb=0.4
           hw_ldb=0.9
           hw_udb=1.0
          else if (hrs.lt.11.0) then
!           hw_ldb=0.4
           hw_ldb=0.9
           hw_udb=1.0
          else if (hrs.lt.14.0) then
!           hw_ldb=0.4 ! this leaves room for charge control
           hw_ldb=0.9 ! this gives a big uncontrolled peak at 11
           hw_udb=1.0
          else if (hrs.lt.16.0) then
!           hw_ldb=0.4
           hw_ldb=0.9
           hw_udb=1.0
          else if (hrs.lt.20.0) then
!           hw_ldb=0.2
           hw_ldb=0.9
           hw_udb=1.0
          else
!           hw_ldb=0.4
           hw_ldb=0.9
           hw_udb=1.0
          end if
!          write(6,*) 'hot water ctrl',hrs,hw_ldb,hw_udb
          avg_hw_soc=0.
          hw_avl_for_off=0
          hw_avl_for_on=0
          ac_avl_for_off=0
          ac_avl_for_on=0
          db_on(seconds+1)=0
          acs_on(seconds+1)=0
          t_avg(seconds+1)=0.
          clusterpower=0.
          ac_soc=0.
!          call whatsthetemp((day-1)*86400+seconds,ntemps,secs,degrees,
!     >                      t_amb,secm,secp)
!          t_amb=t_amb+20.
          open(unit=90,file='outside_temp.csv',form='formatted')
          read(90,*,iostat=my_iostat) t_amb
          if(my_iostat.ne.0) then
           write(6,*) 'Reading temp failed, using previous value'
          end if
          close(90)
          open(unit=11,file='broadcast_ctrl.csv',form='formatted') 
          read(11,*,iostat=my_iostat) more_on
          if(my_iostat.ne.0) then
           write(6,*) 'Reading TCL_ctrl failed, using previous value'
          end if
          close(11)
          !if ((hour.lt.11).or.(hour.gt.18)) more_on=0.0
          do meter=1,meters
           power=0.0 ! initialize power for all loads to zero
!           t_amb=28.-8.*sin(hrs*pi/12.) ! hot day in ABQ
!           t_amb=28 ! constant temperature
! control for ACs
!           ctrl=sin(2.*pi*freq*seconds)
! control for DHW
            if (hrs < 11.) then
             ctrl=0.0
             hw_gain=0.0
            else if (hrs < 11.5) then
             ctrl=(hrs-11.)/0.5
             hw_gain=0.0
            else if (hrs < 13.5) then
             ctrl=1.
             hw_gain=0.0
            else if (hrs < 14.) then
             ctrl=1.-(hrs-13.5)/0.5
             hw_gain=0.0
            else if (hrs < 15.75) then
             ctrl=0.
             hw_gain=0.0
            else if (hrs < 16.) then
!             ctrl=-(hrs-15.75)/0.5
             ctrl=0.0
             hw_gain=0.0
            else if (hrs < 18.) then
!             ctrl=-0.5
             ctrl=0.0
             hw_gain=0.0
            else if (hrs < 18.25) then
!             ctrl=-0.5+(hrs-18.)/0.5
             ctrl=0.0
             hw_gain=0.0
            else
             ctrl=0.
             hw_gain=0.0
            end if
            ctrl=ctrl+0.0 ! try to add some average background load
! set artificial T_amb for constant AC
!           T_amb=35.
           Q_loss=A_HX(meter)*(T_amb-T_space(meter))/R_ins(meter)
           if (Q_loss.gt.0.) then
            hp_mode=1. ! heat pump in cooling mode
           else
            hp_mode=-1. ! heat pump in heating mode
           end if
           do load=1,nhloads
!            write(6,*) 'load',load,'of',nhloads,' on meter',meter,
!     >                 'at time ',seconds+1
            check=.false.
            lightcount=0
! see if load is active
            if (hrs.lt.(hload_or_old(load,meter))) check=.true.
            n_events=int(hload(load,meter,1))
! comment line below for many meters
!            write(6,*) 'checking number of events',n_events,load,meter
            do j=1,n_events
!             write(6,*) hload(load,meter,2*j),hload(load,meter,2*j+1)+
!     >                  hload(load,meter,2*j)
             if ((hrs.gt.hload(load,meter,2*j)).and.
     >           (hrs.lt.(hload(load,meter,2*j+1)+
     >            hload(load,meter,2*j)))) then
              check=.true.
              if (load.eq.6) then
               lightcount=lightcount+1
              end if
             end if
            end do
!            if (load.eq.6) then
!             if (meter.eq.42) write(6,*) 'lightcount',hrs,lightcount
!            end if
! now we do things based on what the load is
            if (load.eq.1) then ! this is a dryer
             if (check) then
!              write(6,*) 'dryer active on meter ',meter
              power(load)=3000.0 ! for now 3 kW later assign power to load/meter combo from distribution
              totalpower(load,seconds+1)=totalpower(load,seconds+1)
     >                                 +power(load)
             end if
            end if ! end of dryer
!23456789112345678921234567893123456789412345678951234567896123456789712
            if (load.eq.2) then ! this is an AC
! count AC units available for switching on and off
             if (time_on(meter) > switch_time)
     >          ac_avl_for_off=ac_avl_for_off+1
             if (time_off(meter) > switch_time)
     >          ac_avl_for_on=ac_avl_for_on+1
! now some control
!!!             if (ac_error > 0.) then
!!!              dctrl=min(maxramp,ac_error)/(max_pos+10000.) ! make sure denominator does not go to zero
!!!             else
!!!              dctrl=max(-maxramp,ac_error)/(max_neg+10000.) ! make sure denominator does not go to zero
!!!             end if
!!!             if (abs(ac_error) < 10000.) dctrl=0.
!             more_on=dctrl*delta_t*ac_gain
!             if (more_on > 1.) more_on=1.
!             if (more_on < -1.) more_on=-1.
! if check is true then we are in occupied setpoint or deadband
             currentstate=ac_on(meter)
             if (check) then
              db_on(seconds+1)=db_on(seconds+1)+1
              ldb=t_set(meter)-1.
              udb=t_set(meter)+1.
             else ! let the space temp do what it wants
              ldb=16. ! not too cold min temp when away
              udb=26. ! pretty high max temp when away
             end if
             t_avg(seconds+1)=t_avg(seconds+1)+t_space(meter)
! set artificial ldb and udb to have ACs on all the time
!             ldb=22.
!             udb=24.
!             if (meter.eq.42) then
!              write(6,*) 'wtf',seconds,t_space(meter),ldb,udb
!             end if
!
             if (hp_mode.gt.0.) then
              if (t_space(meter).lt.ldb) then
               ac_on(meter)=.false.
              end if
              if (t_space(meter).gt.udb) then
               ac_on(meter)=.true.
!               acs_on(seconds+1)=acs_on(seconds+1)+1
              end if
             else
              if (t_space(meter).lt.ldb) then
               ac_on(meter)=.true.
              end if
              if (t_space(meter).gt.udb) then
               ac_on(meter)=.false.
!               acs_on(seconds+1)=acs_on(seconds+1)+1
              end if
             end if
! now switch based on external probability control
             internal_rand=ran2(seed)
             if (more_on > 0.) then ! want to turn some stats on
              if (internal_rand < more_on) then ! switch tstat to on if available
               if ((time_off(meter) > switch_time).and.
     >             (t_space(meter) > ldb)) then ! tstat available to turn on
                ac_on(meter) = .true.
               !!! time_on(meter)=0.
               !!! time_off(meter)=0.
               end if
              end if
             else ! want to turn some stats off
              if (internal_rand < (-more_on)) then ! switch tstat to off if available
               if ((time_on(meter) > switch_time).and.
     >             (t_space(meter) < udb)) then ! tstat available to turn off
                ac_on(meter) = .false.
               !!! time_on(meter)=0.
               !!! time_off(meter)=0.
               end if
              end if
             end if
             if (ac_on(meter)) then
              acs_on(seconds+1)=acs_on(seconds+1)+1
!              write(6,*) 'AC active on meter ',meter
              Q_AC=10000.
             else
              Q_AC=0.
             end if
! check if switch has occurred
             if (currentstate.eqv.ac_on(meter)) then
! switching did not occur
              if (ac_on(meter)) then
               time_on(meter)=time_on(meter)+1.
              else
               time_off(meter)=time_off(meter)+1.
              end if
             else
! switching occurred
              time_on(meter)=0.
              time_off(meter)=0.
             end if
!             write(6,*) 'time_on_off ',seconds,meter,time_on(meter),
!     >                  time_off(meter)
             COP=2.5
             power(load)=Q_AC/COP
             totalpower(load,seconds+1)=totalpower(load,seconds+1)
     >                                +power(load)
             t_space(meter)=t_space(meter)+(Q_loss-Q_AC)/t_mass(meter)
! calculate the soc of the ac's
             ac_soc(meter)=(t_space(meter)-ldb)/(udb-ldb)
! now output to status file
             if (time_on(meter).gt.0) then ! ac is on
              if (time_on(meter).gt.switch_time) then
               write(22,*) meter,1,1,ac_soc(meter),ldb,udb
              else
               write(22,*) meter,1,0,ac_soc(meter),ldb,udb
              end if
             else ! ac is off
              if (time_off(meter).gt.switch_time) then
               write(22,*) meter,0,1,ac_soc(meter),ldb,udb
              else
               write(22,*) meter,0,0,ac_soc(meter),ldb,udb
              end if
             end if
! comment below for many meters
!             write(14,*) meter,seconds,t_space(meter),t_amb,ldb,udb,
!     >                   power(load)
            end if ! end of space cooling
            if (load.eq.3) then ! this is a water heater
! count heaters available for switching on and off
             if (hw_time_on(meter) > hw_switch_time)
     >          hw_avl_for_off=hw_avl_for_off+1
             if (hw_time_off(meter) > hw_switch_time)
     >          hw_avl_for_on=hw_avl_for_on+1
             hw_currentstate=heater_on(meter)
             HWCOP=2.2 ! from NREL study for heat pump heaters
             k1=1./2400. ! discharge rate when showering or dishwashing, fixed for now
!             k1=0./1800. ! turn off to see effect of self-discharge only
             k2=0.3/86400. ! self discharge, pretty slow
             k3=1./10450. ! charge assuming 250 liters and 4 kW thermal power and 40 deg. C delta T
! set normal lbd and udb
             hw_ldbn=(hw_ldb+hw_udb)/2.-(hw_udb-hw_ldb)/5.
             hw_udbn=(hw_ldb+hw_udb)/2.+(hw_udb-hw_ldb)/5.
             if ((hw_soc(meter).lt.hw_ldb).and.
     >           (hw_time_off(meter) > hw_switch_time)) then
                 heater_on(meter)=.true.
                 hw_time_off(meter)=0.
                 hw_time_on(meter)=0.
             end if
             if ((hw_soc(meter).gt.hw_udb).and.
     >           (hw_time_on(meter) > hw_switch_time)) then
                 heater_on(meter)=.false.
                 hw_time_off(meter)=0.
                 hw_time_on(meter)=0.
             end if
! now some control
!             if (hw_error > 0.) then
!              dctrl=min(maxramp,hw_error)/(max_pos+10000.) ! make sure denominator does not go to zero
!             else
!              dctrl=max(-maxramp,hw_error)/(max_neg+10000.) ! make sure denominator does not go to zero
!             end if
!             if (abs(hw_error) < 10000.) dctrl=0.
!             more_on=dctrl*delta_t*hw_gain
!             if (more_on > 1.) more_on=1.
!             if (more_on < -1.) more_on=-1.
! now switch heaters as needed by controller
!
!
!             if (meter.eq.42) write(6,*) 'soc42',hrs,hw_soc(meter),
!     >                                    hw_ldb,hw_udb
!             if (meter.eq.11) write(6,*) 'soc11',hrs,hw_soc(meter),
!     >                                    hw_ldb,hw_udb
!
!!!             if (more_on > 0.) then ! we want to turn some heaters on
!!!              if ((hw_time_off(meter) > hw_switch_time).and.
!!!     >            (hw_soc(meter) < hw_udb)) then ! heater available to turn on
!!!               internal_rand=ran2(seed)
!!!               if (internal_rand < more_on) then ! turn heater on
!!!                heater_on(meter) = .true.
!!!                hw_time_off(meter)=0.
!!!                hw_time_on(meter)=0.
!!!               end if
!!!              end if
!!!             else ! we want to turn some heaters off
!!!              if ((hw_time_on(meter) > hw_switch_time).and.
!!!     >            (hw_soc(meter) > hw_ldb)) then ! heater available to turn off
!!!               internal_rand=ran2(seed)
!!!               if (internal_rand < (-more_on)) then ! turn heater off
!!!                heater_on(meter) = .false.
!!!                hw_time_off(meter)=0.
!!!                hw_time_on(meter)=0.
!!!               end if
!!!              end if
!!!             end if
!
             if (heater_on(meter)) then
              ch_rate=k3
              power(load)=4000./HWCOP
             else
              ch_rate=0.
              power(load)=0.
             end if
! check if switch has occurred
             if (hw_currentstate.eqv.heater_on(meter)) then
! switching did not occur
              if (heater_on(meter)) then
               hw_time_on(meter)=hw_time_on(meter)+1.
              else
               hw_time_off(meter)=hw_time_off(meter)+1.
              end if
             else
! switching occurred
              hw_time_on(meter)=0.
              hw_time_off(meter)=0.
             end if
             if (check) then ! someone is taking a shower or washing dishes
              hw_soc(meter)=hw_soc(meter)-k1-k2*hw_soc(meter)+ch_rate
!              write(6,*) 'WH active on meter ',meter
              if (hw_soc(meter).lt.0.) hw_soc(meter)=0.
             else ! no showers or washing going on
              hw_soc(meter)=hw_soc(meter)-k2*hw_soc(meter)+ch_rate
              if (hw_soc(meter).lt.0.) hw_soc(meter)=0.
             end if
             totalpower(load,seconds+1)=totalpower(load,seconds+1)
     >                                +power(load)
             avg_hw_soc=avg_hw_soc+hw_soc(meter)
!
!!!! now output to status file
!!!             if (hw_time_on(meter).gt.0) then ! heater is on
!!!              if (hw_time_on(meter).gt.hw_switch_time) then
!!!               write(22,*) meter,1,1,hw_soc(meter),hw_ldb,hw_udb
!!!              else
!!!               write(22,*) meter,1,0,hw_soc(meter),hw_ldb,hw_udb
!!!              end if
!!!             else ! heater is off
!!!              if (hw_time_off(meter).gt.hw_switch_time) then
!!!               write(22,*) meter,0,1,hw_soc(meter),hw_ldb,hw_udb
!!!              else
!!!               write(22,*) meter,0,0,hw_soc(meter),hw_ldb,hw_udb
!!!              end if
!!!             end if
! comment below for many meters
!             write(15,*) meter,seconds,hw_soc(meter),power(load)
            end if ! end of water heating
            if (load.eq.4) then ! this is a refrigerator
             ldb=3.
             udb=5.
             if (check) then ! the door is open, higher losses
              R_loss=1800. ! h=45 W/m2/K, DeltaT=20, A=2m2
             else
              R_loss=200.
             end if
             if (t_fridge(meter).lt.ldb) cpr_on(meter)=.false.
             if (t_fridge(meter).gt.udb) cpr_on(meter)=.true.
             if (cpr_on(meter)) then
!              write(6,*) 'fridge active on meter ',meter
              R_X=675.
             else
              R_X=0.
             end if
             COP=2.7
             power(load)=R_X/COP
             totalpower(load,seconds+1)=totalpower(load,seconds+1)
     >                                +power(load)
             t_fridge(meter)=t_fridge(meter)+(R_loss-R_X)/f_mass(meter)
! comment below for many meters
!             write(16,*) meter,seconds,t_fridge(meter),power(load)
            end if
            if (load.eq.5) then ! this is a cooking range
             if (check) then
! switch small burner (burner 1)
              randy=ran2(seed)
              if (burner1on(meter)) then
               if (randy.lt.0.08) burner1on(meter)=.false.
              else
               if (randy.lt.0.08) burner1on(meter)=.true.
              end if
! switch medium burner (burner 2)
              randy=ran2(seed)
              if (burner2on(meter)) then
               if (randy.lt.0.04) burner2on(meter)=.false.
              else
               if (randy.lt.0.01) burner2on(meter)=.true.
              end if
! switch large burner (burner 3)
              randy=ran2(seed)
              if (burner3on(meter)) then
               if (randy.lt.0.01) burner3on(meter)=.false.
              else
               if (randy.lt.0.03) burner3on(meter)=.true.
              end if
              power(load)=0.
              if (burner1on(meter)) power(load)=power(load)+1000.
              if (burner2on(meter)) power(load)=power(load)+2000.
              if (burner3on(meter)) power(load)=power(load)+3000.
              totalpower(load,seconds+1)=totalpower(load,seconds+1)
     >                                 +power(load)
              if (meter.eq.42) then ! output to file
               !write(17,*) meter,seconds,power(load)
              end if
             end if
            end if
            if (load.eq.6) then ! light bulbs (LED ones)
             power(load)=0.
             if (check) then ! light is on
              if (lightcount.le.5) then
               power(load)=power(load)+
     >         lightcount*8.
              else if (lightcount.le.10) then
               power(load)=power(load)+
     >         5*8.+
     >         (lightcount-5)*16.
              else if (lightcount.le.20) then
               power(load)=power(load)+
     >         5*8.+5*16.+
     >         (lightcount-10)*24.
              else
               power(load)=power(load)+
     >         5*8.+5*16.+10*24.+
     >         (lightcount-20)*16.
              end if
              totalpower(load,seconds+1)=totalpower(load,seconds+1)
     >                                 +power(load)
             end if
!             if (meter.eq.42) then ! output to file
!              write(18,*) meter,seconds,power(load)
!             end if
            end if
           end do ! load loop
           if (meter.eq.whichmeter) then ! output to file
!            write(19,*) meter,seconds,(power(j),j=1,nhloads)
           end if
! comment below for many meters
!           write(12,*) meter,seconds,(power(j),j=1,nhloads)
!
! now add power for meters on list
           metercluster=cluster(meter)
           do j=1,nhloads
            clusterpower(metercluster)=clusterpower(metercluster)
     >                                 +power(j)/1000.
           end do
!          do nlist=1,meterlist(1)
!           meterinlist=meterlist(nlist+1)
!           if (meter.eq.meterinlist) then !
!            do j=1,nhloads
!             meterlistpower=meterlistpower+power(j)
!            end do
!           end if
!          end do
          end do ! meter loop
          close(22)
!          write(33,*) (day-1)*86400+seconds,meterlistpower
! bump off last point from filtered signal and update
! note - using hw heaters for load
          do i=1,2
           filtered(i)=filtered(i+1)
          end do
          if (seconds.gt.2) then
           totalpowerall3=0.
           totalpowerall2=0.
           totalpowerall1=0.
           do k=1,6
            totalpowerall3=totalpowerall3+totalpower(k,seconds+1)
            totalpowerall2=totalpowerall2+totalpower(k,seconds)
            totalpowerall1=totalpowerall1+totalpower(k,seconds-1)
           end do
!
           filtered(3)=c0*totalpowerall3+
     >                 c1*totalpowerall2+
     >                 c2*totalpowerall1+
     >                 d1*filtered(2)+
     >                 d2*filtered(1)
!
! below for heaters alone as contollable aggregation
!           filtered(3)=c0*totalpower(3,seconds+1)+
!     >                 c1*totalpower(3,seconds)+
!     >                 c2*totalpower(3,seconds-1)+
!     >                 d1*filtered(2)+
!     >                 d2*filtered(1)
          end if
!
          avg_hw_soc=avg_hw_soc/meters
!          max_pos=10000./COP*ac_avl_for_on ! electric power of compressor
!          max_neg=10000./COP*ac_avl_for_off ! electric power of compressor
          max_pos=4000./HWCOP*hw_avl_for_on
          max_neg=4000./HWCOP*hw_avl_for_off
!          write(6,*) 'avg_hw_soc',hrs,avg_hw_soc
!          write(6,*) 'heaters available',hrs,hw_avl_for_on,
!     >                 hw_avl_for_off
          hw_error=hw_power*ctrl+filtered(3) ! the constant defines magnitude of control in W
!          ac_error=ac_power*ctrl+filtered(3) ! the constant defines magnitude of control in W
!          write(6,*) 'smoothed total heater power',hrs,-filtered(3),
!     >               hw_error
!          write(6,*) 'smoothed total power',hrs,-filtered(3),
!     >                ac_avl_for_on*3000.,
!     >                -ac_avl_for_off*3000.,
!     >                ctrl*ac_power
!          write(6,*) 'smoothed total power',hrs,-filtered(3),
!     >                hw_avl_for_on*1818.,
!     >                -hw_avl_for_off*1818.,
!     >                ctrl*hw_power,more_on
!
! move current average array one step back
! note: 50 is the most current value
          do i=1,49
           average(i)=average(i+1)
          end do
          average(50)=0.
!          do i=1,nhloads
          do i=2,2
           average(50)=average(50)+totalpower(i,seconds+1)
          end do
          do i=1,50
           avgpwr(seconds+1)=avgpwr(seconds+1)+average(i)
          end do
          avgpwr(seconds+1)=avgpwr(seconds+1)/50.
! now calculate some control parameters
!          delta_t=1. ! timestep in seconds
!          gain=0.
!          numrtr=0.
!          denmtr=0.
!          do i=1,10
!           numrtr=numrtr+(1.*i-5.5)*(average(i)-avgpwr(seconds+1))
!           denmtr=denmtr+(1.*i-5.5)**2
!          end do
!          linslope=numrtr/denmtr
!          dctrl=2.*pi*freq*cos(2.*pi*freq*seconds)-linslope/200000. ! this is the probability of turning on / off available devices
!          if (dctrl > 1.) dctrl=1. ! max positive ramp rate
!          if (dctrl < -1.) dctrl=-1. ! min negative ramp rate
!          more_on=dctrl*delta_t*gain ! this is the probability of switching
!          more_on=0.
! output timestamp, aggregated load, and clusterpower to file
          total_load=0.
          total_load = sum(clusterpower)
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
          write(6,*) timestamp,hrs
          open(unit=54,file='transformers.csv',form='formatted')
          write(54,'(I2.2,A,A,A,F13.3)') 2,',',timestamp,',',total_load
          do i=1,meterclusters
           write(54,'(I2.2,A,A,A,F13.3)') i+2,',',timestamp,
     >                                    ',',clusterpower(i)
          end do
          close(54)
          open(unit=22,file='donewriting.txt',form='formatted')
          write(22,*) 'puke'
          close(22)
          write(44,*) timestamp,totalpower(2,seconds+1),t_amb 
          write(6,*) 'second rt',secondstamp,'second sim',second-1
          call flush()
          if (second.eq.60) then
           start_second = 0
          end if
         end do ! seconds
         if (minute.eq.60) then
          start_minute = 0
         end if
        end do ! minutes
        if (hour.eq.24) then
         start_hour = 0
        end if
       end do ! hours
!        write(14,*) 'meter',meter ! blank line to separate meters
!        write(15,*) 'meter',meter ! blank line to separate meters
!        write(16,*) 'meter',meter ! blank line to separate meters
!        write(12,*) 'meter',meter ! blank line to separate meters
       do i=1,86400
       hrs=i/3600.
!       call whatsthetemp((day-1)*86400+i,ntemps,secs,degrees,
!     >                   t_amb,secm,secp)
!        t_amb=t_amb+20.
!       t_amb=28.-8.*sin(hrs*pi/12.) ! hot day in ABQ
!        ctrl=sin(2.*pi*freq*i)
       open(unit=90,file='outside_temp.csv',form='formatted')
       read(90,*,iostat=my_iostat) t_amb
       if(my_iostat.ne.0) then
        write(6,*) 'Reading temp failed, using previous value'
       end if
       close(90)
        if (hrs < 11.) then
         ctrl=0.
        else if (hrs < 11.5) then
         ctrl=(hrs-11.)/0.5
        else if (hrs < 13.5) then
         ctrl=1.
        else if (hrs < 14.) then
         ctrl=1.-(hrs-13.5)/0.5
        else
         ctrl=0.
        end if
        totaltotal=0.
        do j=1,6
         totaltotal=totaltotal+totalpower(j,i)
        end do
!        write(13,*) (day-1)*86400+i,(totalpower(j,i),j=1,nhloads),
!     >              totaltotal,t_amb,avgpwr(i),ctrl,db_on(i),acs_on(i),
!     >              t_avg(i)/meters
       end do
      end do ! day loop
      !close(12)
      !close(13)
      !close(14)
      !close(15)
      !close(17)
      !close(18)
      !close(19)
      !close(33)
      close(44)
!
      end
!
!23456789112345678921234567893123456789412345678951234567896123456789712
!
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software =$j!]--1,).
!
!23456789112345678921234567893123456789412345678951234567896123456789712
!
      subroutine howmany(pdf,npoints,random,number)
!
! from an input pdf and random number, returns integer number of events
!
      implicit none
      integer i,npoints,number
      real pdf(200,2)
      real random
!
      i=1
      do while(random.ge.pdf(i,2))
       i=i+1
      end do
      i=i-1
      number=int(pdf(i,1))
!
      return
      end
!
!23456789112345678921234567893123456789412345678951234567896123456789712
!
      subroutine whentostart(pdf,npoints,random,start_time)
!
! from an input pdf and random number, returns integer number of events
!
      implicit none
      integer i,npoints
      real start_time
      real pdf(200,2)
      real random
!
      i=1
      do while(random.ge.pdf(i,2))
       i=i+1
      end do
      start_time=pdf(i-1,1)+(pdf(i,1)-pdf(i-1,1))*
     >           (random-pdf(i-1,2))/(pdf(i,2)-pdf(i-1,2))
!
      return
      end
!
!23456789112345678921234567893123456789412345678951234567896123456789712
!
      subroutine howlong(pdf,npoints,random,duration)
!
! from an input pdf and random number, returns integer number of events
!
      implicit none
      integer i,npoints
      real duration
      real pdf(200,2)
      real random
!
      i=1
      do while(random.ge.pdf(i,2))
       i=i+1
      end do
      duration=pdf(i-1,1)+(pdf(i,1)-pdf(i-1,1))*
     >           (random-pdf(i-1,2))/(pdf(i,2)-pdf(i-1,2))
!
      return
      end
!
!23456789112345678921234567893123456789412345678951234567896123456789712
!
      subroutine whatsthetemp(now,ndata,times,degrees,temp,secm,secp)
!
      implicit none
      integer times(10000)
      real degrees(10000)
      real temp
      integer secm,secp,now,ndata,min,max,mid
!
      min=1
      max=ndata
      do while(max.gt.(min+1))
       mid=(min+max)/2
       if (times(mid).le.now) then
        min=mid
       else
        max=mid
       end if
!       write(6,*) min,max,times(min),times(max)
      end do
      if (min.eq.(ndata-1)) write(6,*) 'warning - end of temp data'
! now interpolate temperature
      temp=degrees(min)+(now-times(min))/(1.*(times(max)-times(min)))*
     >     (degrees(max)-degrees(min))
      secm=times(min)
      secp=times(max)
!
      return
      end
