! Version 0.3
! 04-12-2011 11:59PM

!#define use_unformatted 1
!#define debug 0

!#######################################################################
      Subroutine setHydroFiles(dataID_in, dataFN_in,
     &                           ctlID_in, ctlFN_in)
!     Set the file with filename to be the data file (record its name and id)
      Implicit None
      Character (len=*) :: dataFN_in, ctlFN_in
      Integer:: dataID_in, ctlID_in

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer write_id
      Common /write_id/ write_id

      dataFN = dataFN_in
      ctlFN = ctlFN_in
      dataID = dataID_in
      ctlID = ctlID_in

      write_id = 1

      End Subroutine
!-----------------------------------------------------------------------




!#######################################################################
      Subroutine writeHydroCtl(XL,XH,DX,YL,YH,DY,Tau0,dTau)
!     Write the header
!      -- fileId: id number for the file to be written
!      -- XL, XH, DX: x varies in [XL,XH]*DX; XL and XH are integers
!      -- YL, YH, DY: y varies in [YL,YH]*DY; YL and YH are integers
      Implicit None
      Integer:: XL, XH, YL, YH
      Double Precision:: DX, DY, Tau0, dTau

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: XLS,XHS,YLS,YHS
      Double Precision:: dXS,dYS,Tau0S,dTauS
      Common /hydroInfo/ XLS,XHS,dXS,YLS,YHS,dYS,Tau0S,dTauS

      Integer:: write_id
      Common /write_id/ write_id

      ! initialize write id (current writting position)
      write_id = 1

      ! write hydroInfo block
      XLS=XL
      XHS=XH
      dXS=DX
      YLS=YL
      YHS=YH
      dYS=DY
      Tau0S=Tau0
      dTauS=dTau

      ! write data file
      Open(ctlID, FILE=ctlFN,STATUS='REPLACE')
      Write(ctlID,'(I8,I8,E20.8,I8,I8,E20.8,E20.8,E20.8)')
     &              XL,XH,DX,YL,YH,DY,Tau0,dTau
      Close(ctlID)

      Open(dataID, FILE=dataFN,STATUS='REPLACE',FORM='UNFORMATTED')
      Close(dataID)

      End Subroutine
!------------------------------------------------------------------------



!########################################################################
      Subroutine readHydroCtl(XL,XH,DX,YL,YH,DY,Tau0,dTau)
!     Read the header
!      -- fileId: id number for the file to be written
!      -- XL, XH, DX: x varies in [XL,XH]*DX; XL and XH are integers
!      -- YL, YH, DY: y varies in [YL,YH]*DY; YL and YH are integers
      Implicit None
      Integer:: XL, XH, YL, YH
      Double Precision:: DX, DY, Tau0, dTau

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: XLS,XHS,YLS,YHS
      Double Precision:: dXS,dYS,Tau0S,dTauS
      Common /hydroInfo/ XLS,XHS,dXS,YLS,YHS,dYS,Tau0S,dTauS

      ! read data file
      Open(ctlID, FILE=ctlFN,STATUS='OLD')
      Read(ctlID,'(I8,I8,E20.8,I8,I8,E20.8,E20.8,E20.8)')
     &              XL,XH,DX,YL,YH,DY,Tau0,dTau
      Close(ctlID)

      ! set hydroInfo block
      XLS=XL
      XHS=XH
      dXS=DX
      YLS=YL
      YHS=YH
      dYS=DY
      Tau0S=Tau0
      dTauS=dTau

      End Subroutine
!-----------------------------------------------------------------------



!########################################################################
      Subroutine writeHydroBlock(Time,Ed,Sd,Temp,VxB,VyB)
!     Write a block to the file
      Implicit None

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) :: Ed,Sd,Temp,VxB,VyB
      Double Precision :: Time

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: write_id
      Common /write_id/ write_id

      Inquire(IOLENGTH=length) write_id,Time,Ed,Sd,Temp,VxB,VyB ! how large the data block is
      Open(dataID,FILE=dataFN,
     &  STATUS='OLD',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=length)

      Write(dataID, REC=write_id) write_id,Time,Ed,Sd,Temp,VxB,VyB ! write
      write_id = write_id + 1

      Close(dataID)

      End Subroutine
!------------------------------------------------------------------------



!########################################################################
      Subroutine readHydroBlock(idx,Time,Ed,Sd,Temp,VxB,VyB)
!     Read a block from the data file, check if it exists in the buffer first
      Implicit None

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) :: Ed,Sd,Temp,VxB,VyB
      Double Precision :: Time

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: idx_check

      Inquire(IOLENGTH=length) idx_check,Time,Ed,Sd,Temp,VxB,VyB ! how large the data block is
      Open(dataID,FILE=dataFN,
     &  STATUS='OLD',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=length)

      Read(dataID, REC=idx) idx_check,Time,Ed,Sd,Temp,VxB,VyB ! write

      Close(dataID)

      If (idx/=idx_check) Then
        Print*, "readHydroBlock: record index does not match"
        Stop
      EndIf

      End Subroutine
!------------------------------------------------------------------------




!########################################################################
      Subroutine readHydroInfo(tau,x,y,e,s,T,vx,vy)
!     Return infos from hydro data file
!     -- tau,x,y: coordinates (in)
!     -- e,s,T,vx,vy: infos (out)
      Implicit None

      Double Precision:: tau,x,y,e,s,T,vx,vy

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) ::
     &      Ed1,Sd1,Temp1,VxB1,VyB1
      Double Precision, Dimension(XL:XH,YL:YH,1:1) ::
     &      Ed2,Sd2,Temp2,VxB2,VyB2
      Double Precision :: Time1
      Double Precision :: Time2
      Integer :: T1
      Integer :: T2

      Integer:: tauI ! tau should lie between Tau0+dTau*tauI and Tau0+dTau*(tauI+1)
      Double Precision:: tauInc ! tau=Tau0+tauI1*dTau+Tau_inc*dTau, inc for increament (normalized to 0~1)

      Integer:: xi, yi ! similar to tau
      Double Precision:: xInc, yInc ! x=xi*DX+xInc*DX

      Double Precision:: var1 ! temporary variables

      ! first deal with tau
      var1 = (tau-Tau0)/dTau
      tauI = Floor(var1)
      tauInc = var1-tauI


      ! then x and y
      var1 = (x)/DX
      xi = Floor(var1)
      xInc = var1-xi
      var1 = (y)/DY
      yi = Floor(var1)
      yInc = var1-yi

!     For debug:
!      Print*, "tau, Tau0, dTau, tauI, tauInc=",
!     &        tau, Tau0, dTau, tauI, tauInc
!      Print*, "x,xi,xInc,y,yi,yInc=",
!     &        x,xi,xInc,y,yi,yInc

      Call readHydroBlock(tauI+1,Time1,Ed1,Sd1,Temp1,VxB1,VyB1) ! tauI+1: tauI=0 <-> 1st block
      Call readHydroBlock(tauI+2,Time2,Ed2,Sd2,Temp2,VxB2,VyB2)

      Call cubeInterp(xInc,yInc,tauInc,e,
     &  Ed1(xi,yi,1),Ed1(xi+1,yi,1),Ed1(xi,yi+1,1),Ed1(xi+1,yi+1,1),
     &  Ed2(xi,yi,1),Ed2(xi+1,yi,1),Ed2(xi,yi+1,1),Ed2(xi+1,yi+1,1))

      Call cubeInterp(xInc,yInc,tauInc,s,
     &  Sd1(xi,yi,1),Sd1(xi+1,yi,1),Sd1(xi,yi+1,1),Sd1(xi+1,yi+1,1),
     &  Sd2(xi,yi,1),Sd2(xi+1,yi,1),Sd2(xi,yi+1,1),Sd2(xi+1,yi+1,1))

      Call cubeInterp(xInc,yInc,tauInc,T,
     &  Temp1(xi,yi,1),Temp1(xi+1,yi,1),
     &  Temp1(xi,yi+1,1),Temp1(xi+1,yi+1,1),
     &  Temp2(xi,yi,1),Temp2(xi+1,yi,1),
     &  Temp2(xi,yi+1,1),Temp2(xi+1,yi+1,1))

      Call cubeInterp(xInc,yInc,tauInc,vx,
     &  VxB1(xi,yi,1),VxB1(xi+1,yi,1),VxB1(xi,yi+1,1),VxB1(xi+1,yi+1,1),
     &  VxB2(xi,yi,1),VxB2(xi+1,yi,1),VxB2(xi,yi+1,1),VxB2(xi+1,yi+1,1))

      Call cubeInterp(xInc,yInc,tauInc,vy,
     &  VyB1(xi,yi,1),VyB1(xi+1,yi,1),VyB1(xi,yi+1,1),VyB1(xi+1,yi+1,1),
     &  VyB2(xi,yi,1),VyB2(xi+1,yi,1),VyB2(xi,yi+1,1),VyB2(xi+1,yi+1,1))

      End Subroutine
!-----------------------------------------------------------------------




!########################################################################
      Subroutine cubeInterp(x,y,z,Axyz,
     &                      A000,A100,A010,A110,A001,A101,A011,A111)
! Perform a 3d interpolation. The known data are A### located at the 8 corners,
! labels using the xyz order. Therefore A000 is value at the origin and A010
! is the value at (x=0,y=1,z=0). Note that the coordinate (x,y,z) must be
! constrained to the unit cube. Axyz is the return value.

      Implicit None
      Double Precision :: x,y,z,Axyz
      Double Precision :: A000,A100,A010,A110,A001,A101,A011,A111

      Axyz = A000*(1-x)*(1-y)*(1-z) + A100*x*(1-y)*(1-z) +
     &    A010*(1-x)*y*(1-z) + A001*(1-x)*(1-y)*z +
     &    A101*x*(1-y)*z + A011*(1-x)*y*z +
     &    A110*x*y*(1-z) + A111*x*y*z

      !If (debug == 1) Then
      !  Print *, "cubeInterp"
      !  Print *, "x,y,z=",x,y,z
      !  Print *, "A000,A100,A010,A110=",A000,A100,A010,A110
      !  Print *, "A001,A101,A011,A111=",A001,A101,A011,A111
      !  Print *, "Axyz=", Axyz
      !End If

      RETURN
      END
!------------------------------------------------------------------------




!=======================================================================
!==============    For buffered file access    =========================
!=======================================================================


!#######################################################################
      Subroutine readHydroCtlBuffered(XL,XH,DX,YL,YH,DY,Tau0,dTau,
     &  bufferSize_in)
!     Read the header and create buffers
!      -- fileId: id number for the file to be written
!      -- XL, XH, DX: x varies in [XL,XH]*DX; XL and XH are integers
!      -- YL, YH, DY: y varies in [YL,YH]*DY; YL and YH are integers
      Implicit None
      Integer:: XL, XH, YL, YH, bufferSize_in
      Double Precision:: DX, DY, Tau0, dTau

      Integer:: bufferSize
      Integer, Pointer:: recent(:), iRecHist(:) ! recent(1)=index of the last used buffer, iRecHist records the integer index of the time of each "layer"
      Double Precision, Pointer::
     &  eM(:,:,:), sM(:,:,:), TM(:,:,:), vxM(:,:,:), vyM(:,:,:) ! the last index is the buffer "layer" index that goes from 1 to bufferSize
      Common /buffer/ bufferSize, recent, iRecHist,
     &  eM, sM, TM, vxM, vyM

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: XLS,XHS,YLS,YHS
      Double Precision:: dXS,dYS,Tau0S,dTauS
      Common /hydroInfo/ XLS,XHS,dXS,YLS,YHS,dYS,Tau0S,dTauS

      Integer:: numberMissed, numberCalled
      Common /bufferEfficiency/ numberMissed, numberCalled

      Integer:: I

      ! initialize numberMissed and numberCalled
      numberMissed = 0
      numberCalled = 0

      ! read data file
      Open(ctlID, FILE=ctlFN,STATUS='OLD')
      Read(ctlID,'(I8,I8,E20.8,I8,I8,E20.8,E20.8,E20.8)')
     &              XL,XH,DX,YL,YH,DY,Tau0,dTau
      Close(ctlID)

      ! write hydroInfo block
      XLS=XL
      XHS=XH
      dXS=DX
      YLS=YL
      YHS=YH
      dYS=DY
      Tau0S=Tau0
      dTauS=dTau

      ! create buffer
      bufferSize = bufferSize_in
      Allocate(recent(bufferSize))
      Allocate(iRecHist(bufferSize))
      recent = (/(I,I=1,bufferSize)/) ! 1 doesn't mean anything here; anything between 1 and bufferSize is a good start
      iRecHist = -1 ! -1 means empty
      Allocate(eM(XLS:XHS,YLS:YHS,1:bufferSize))
      Allocate(sM(XLS:XHS,YLS:YHS,1:bufferSize))
      Allocate(TM(XLS:XHS,YLS:YHS,1:bufferSize))
      Allocate(vxM(XLS:XHS,YLS:YHS,1:bufferSize))
      Allocate(vyM(XLS:XHS,YLS:YHS,1:bufferSize))

      End Subroutine
!------------------------------------------------------------------------




!########################################################################
      Subroutine readHydroBlockBufferedDirect
     &    (idxTau,idxX,idxY,Time,Ed22,Sd22,Temp22,VxB22,VyB22)
!     Read only the 2x2 block from the buffer or file at
!     tauI=idxTau, index_x=idxX, and index_y=idxY
      Implicit None

      Integer :: idxTau, idxX, idxY

      Integer:: XL,XH,YL,YH,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) :: Ed,Sd,Temp,VxB,VyB

      Double Precision, Dimension(1:2,1:2) ::
     &    Ed22,Sd22,Temp22,VxB22,VyB22
      Double Precision :: Time

      Integer:: bufferSize
      Integer, Pointer:: recent(:), iRecHist(:) ! recent(1)=index of the last used buffer, iRecHist records the integer index (in the file) of each buffer "layer"
      Double Precision, Pointer::
     &  eM(:,:,:), sM(:,:,:), TM(:,:,:), vxM(:,:,:), vyM(:,:,:) ! the last index is the buffer "layer" index that goes from 1 to bufferSize
      Common /buffer/ bufferSize, recent, iRecHist,
     &  eM, sM, TM, vxM, vyM

      Integer:: numberMissed, numberCalled
      Common /bufferEfficiency/ numberMissed, numberCalled

      ! for efficiency report:
      numberCalled = numberCalled + 1
!      Print*, "Buffer efficiency:",
!     &  (numberCalled-numberMissed)*100D0/numberCalled, "%"

      ! try buffer first

      J = recent(1) ! index of the last used layer
      If (iRecHist(J)==idxTAU) Then ! most recent one is the one wanted
        ! read it from buffer:
        Ed22(:,:) = eM(idxX:idxX+1,idxY:idxY+1,J)
        Sd22(:,:) = sM(idxX:idxX+1,idxY:idxY+1,J)
        Temp22(:,:) = TM(idxX:idxX+1,idxY:idxY+1,J)
        VxB22(:,:) = vxM(idxX:idxX+1,idxY:idxY+1,J)
        VyB22(:,:) = vyM(idxX:idxX+1,idxY:idxY+1,J)
        Return
      EndIf

      Do I=2,bufferSize
        J = recent(I)
        If (iRecHist(J)==idxTau) Then ! found in buffer
          ! reorder "recent used" list
          recent(2:I) = recent(1:I-1)
          recent(1) = J
          ! read it from buffer:
          Ed22(:,:) = eM(idxX:idxX+1,idxY:idxY+1,J)
          Sd22(:,:) = sM(idxX:idxX+1,idxY:idxY+1,J)
          Temp22(:,:) = TM(idxX:idxX+1,idxY:idxY+1,J)
          VxB22(:,:) = vxM(idxX:idxX+1,idxY:idxY+1,J)
          VyB22(:,:) = vyM(idxX:idxX+1,idxY:idxY+1,J)
          Return
        EndIf
      EndDo

      ! for efficiency calculation:
      numberMissed = numberMissed + 1

      ! if nothing found in buffer:
      ! first read the data from file:
      Call readHydroBlock(idxTau,Time,Ed,Sd,Temp,VxB,VyB)
      ! then organize buffer:
      J = recent(bufferSize) ! the oldest "layer"
      recent(2:bufferSize) = recent(1:bufferSize-1) ! the oldest one is discarded
      ! write to the buffer:
      eM(:,:,J) = Ed(:,:,1)
      sM(:,:,J) = Sd(:,:,1)
      TM(:,:,J) = Temp(:,:,1)
      vxM(:,:,J) = VxB(:,:,1)
      vyM(:,:,J) = VyB(:,:,1)
      ! then read it from buffer:
      Ed22(:,:) = eM(idxX:idxX+1,idxY:idxY+1,J)
      Sd22(:,:) = sM(idxX:idxX+1,idxY:idxY+1,J)
      Temp22(:,:) = TM(idxX:idxX+1,idxY:idxY+1,J)
      VxB22(:,:) = vxM(idxX:idxX+1,idxY:idxY+1,J)
      VyB22(:,:) = vyM(idxX:idxX+1,idxY:idxY+1,J)
      iRecHist(J) = idxTau ! record index in file
      recent(1) = J ! make this layer the "last used one"
      Return

      End Subroutine
!------------------------------------------------------------------------




!########################################################################
      Subroutine readHydroBlockBufferedOrdered
     &    (idxTau,idxX,idxY,Time,Ed22,Sd22,Temp22,VxB22,VyB22)
!     Read only the 2x2 block from the buffer or file at
!     tauI=idxTau, index_x=idxX, and index_y=idxY
!     This version assumes all the required hydro info are in the buffer
      Implicit None

      Integer :: idxTau, idxX, idxY

      Integer:: XL,XH,YL,YH,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) :: Ed,Sd,Temp,VxB,VyB

      Double Precision, Dimension(1:2,1:2) ::
     &    Ed22,Sd22,Temp22,VxB22,VyB22
      Double Precision :: Time

      Integer:: bufferSize
      Integer, Pointer:: recent(:), iRecHist(:) ! recent(1)=index of the last used buffer, iRecHist records the integer index (in the file) of each buffer "layer"
      Double Precision, Pointer::
     &  eM(:,:,:), sM(:,:,:), TM(:,:,:), vxM(:,:,:), vyM(:,:,:) ! the last index is the buffer "layer" index that goes from 1 to bufferSize
      Common /buffer/ bufferSize, recent, iRecHist,
     &  eM, sM, TM, vxM, vyM

      Integer:: numberMissed, numberCalled
      Common /bufferEfficiency/ numberMissed, numberCalled

      If (idxTau<0 .OR. idxTau>bufferSize) Then
        Ed22(:,:) = 0D0
        Sd22(:,:) = 0D0
        Temp22(:,:) = 0D0
        VxB22(:,:) = 0D0
        VyB22(:,:) = 0D0
        Return
      End If

      If (idxX<XL .OR. idxX>XH) Then
        Ed22(:,:) = 0D0
        Sd22(:,:) = 0D0
        Temp22(:,:) = 0D0
        VxB22(:,:) = 0D0
        VyB22(:,:) = 0D0
        Return
      End If

      If (idxY<YL .OR. idxY>YH) Then
        Ed22(:,:) = 0D0
        Sd22(:,:) = 0D0
        Temp22(:,:) = 0D0
        VxB22(:,:) = 0D0
        VyB22(:,:) = 0D0
        Return
      End If


      ! read it from buffer:
      Ed22(:,:) = eM(idxX:idxX+1,idxY:idxY+1,idxTau)
      Sd22(:,:) = sM(idxX:idxX+1,idxY:idxY+1,idxTau)
      Temp22(:,:) = TM(idxX:idxX+1,idxY:idxY+1,idxTau)
      VxB22(:,:) = vxM(idxX:idxX+1,idxY:idxY+1,idxTau)
      VyB22(:,:) = vyM(idxX:idxX+1,idxY:idxY+1,idxTau)

      End Subroutine
!------------------------------------------------------------------------





!########################################################################
      Subroutine readAllBuffers(readSize)
!     Read readSize number of slices of data into the buffer
      Implicit None
      Integer :: readSize

      Integer:: XL,XH,YL,YH,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) :: Ed,Sd,Temp,VxB,VyB

      Double Precision, Dimension(1:2,1:2) ::
     &    Ed22,Sd22,Temp22,VxB22,VyB22
      Double Precision :: Time

      Integer:: bufferSize
      Integer, Pointer:: recent(:), iRecHist(:) ! recent(1)=index of the last used buffer, iRecHist records the integer index (in the file) of each buffer "layer"
      Double Precision, Pointer::
     &  eM(:,:,:), sM(:,:,:), TM(:,:,:), vxM(:,:,:), vyM(:,:,:) ! the last index is the buffer "layer" index that goes from 1 to bufferSize
      Common /buffer/ bufferSize, recent, iRecHist,
     &  eM, sM, TM, vxM, vyM

      Integer:: numberMissed, numberCalled
      Common /bufferEfficiency/ numberMissed, numberCalled

      Do J=1,readSize
        Call readHydroBlock(J,Time,Ed,Sd,Temp,VxB,VyB)
        ! write to the buffer:
        eM(:,:,J) = Ed(:,:,1)
        sM(:,:,J) = Sd(:,:,1)
        TM(:,:,J) = Temp(:,:,1)
        vxM(:,:,J) = VxB(:,:,1)
        vyM(:,:,J) = VyB(:,:,1)
      EndDo

      End Subroutine
!------------------------------------------------------------------------





!########################################################################
      Subroutine readHydroInfoBuffered(tau,x,y,e,s,T,vx,vy)
! The only difference between this function and readHydroInfo is that all
! calls to readHydroBlock are replaced by readHydroBlockBuffered/Ordered.
! The readHydroBufferedOrdered function is significantly faster than the
! without ordered but require a relatively large amount of memory.
!     Return infos from hydro data file
!     -- tau,x,y: coordinates (in)
!     -- e,s,T,vx,vy: infos (out)
      Implicit None

      Double Precision:: tau,x,y,e,s,T,vx,vy

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(1:2,1:2) ::
     &      Ed1,Sd1,Temp1,VxB1,VyB1
      Double Precision, Dimension(1:2,1:2) ::
     &      Ed2,Sd2,Temp2,VxB2,VyB2
      Double Precision :: Time1
      Double Precision :: Time2
      Integer :: T1
      Integer :: T2

      Integer:: tauI ! tau should lie between Tau0+dTau*tauI and Tau0+dTau*(tauI+1)
      Double Precision:: tauInc ! tau=Tau0+tauI1*dTau+Tau_inc*dTau, inc for increament (normalized to 0~1)

      Integer:: xi, yi ! similar to tau
      Double Precision:: xInc, yInc ! x=xi*DX+xInc*DX

      Double Precision:: var1 ! temporary variables

      ! first deal with tau
      if (tau<Tau0 .AND. tau>Tau0-1D-15) tau=Tau0
      var1 = (tau-Tau0)/dTau
      tauI = Floor(var1)
      tauInc = var1-tauI

      ! then x and y
      var1 = (x)/DX
      xi = Floor(var1)
      xInc = var1-xi
      var1 = (y)/DY
      yi = Floor(var1)
      yInc = var1-yi

!     For debug:
!      Print*, "tau, Tau0, dTau, tauI, tauInc=",
!     &        tau, Tau0, dTau, tauI, tauInc
!      Print*, "x,xi,xInc,y,yi,yInc=",
!     &        x,xi,xInc,y,yi,yInc

      Call readHydroBlockBufferedOrdered
     &    (tauI+1,xi,yi,Time1,Ed1,Sd1,Temp1,VxB1,VyB1) ! tauI+1: tauI=0 <-> 1st block
      Call readHydroBlockBufferedOrdered
     &    (tauI+2,xi,yi,Time2,Ed2,Sd2,Temp2,VxB2,VyB2)

      Call cubeInterp(xInc,yInc,tauInc,e,
     &  Ed1(1,1),Ed1(1+1,1),Ed1(1,1+1),Ed1(1+1,1+1),
     &  Ed2(1,1),Ed2(1+1,1),Ed2(1,1+1),Ed2(1+1,1+1))

      Call cubeInterp(xInc,yInc,tauInc,s,
     &  Sd1(1,1),Sd1(1+1,1),Sd1(1,1+1),Sd1(1+1,1+1),
     &  Sd2(1,1),Sd2(1+1,1),Sd2(1,1+1),Sd2(1+1,1+1))

      Call cubeInterp(xInc,yInc,tauInc,T,
     &  Temp1(1,1),Temp1(1+1,1),
     &  Temp1(1,1+1),Temp1(1+1,1+1),
     &  Temp2(1,1),Temp2(1+1,1),
     &  Temp2(1,1+1),Temp2(1+1,1+1))

      Call cubeInterp(xInc,yInc,tauInc,vx,
     &  VxB1(1,1),VxB1(1+1,1),VxB1(1,1+1),VxB1(1+1,1+1),
     &  VxB2(1,1),VxB2(1+1,1),VxB2(1,1+1),VxB2(1+1,1+1))

      Call cubeInterp(xInc,yInc,tauInc,vy,
     &  VyB1(1,1),VyB1(1+1,1),VyB1(1,1+1),VyB1(1+1,1+1),
     &  VyB2(1,1),VyB2(1+1,1),VyB2(1,1+1),VyB2(1+1,1+1))

      End Subroutine
!------------------------------------------------------------------------

!########################################################################
      Subroutine readHydroInfoShanShan(t,x,y,z,e,s,Temp,vx,vy,vz,ctl)
! For ShanShan; to mimic 3d hydro behavior.
! -- t,x,y,z: inputs
! -- e,s,Temp,(vx,vy,vz): outputs
! -- ctl: control information:
!      0: read sucessfully
!      1: x or y out-of-boundary
!      2: tau larger than maxTau
!      3: tau smaller than tau0
      Implicit None

      Double Precision:: t,x,y,z,e,s,Temp,vx,vy,vz
      Integer:: ctl

      Integer:: XL,XH,YL,YH
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau

      Double Precision:: maxTau
      Common /maxTau/ maxTau

      Double Precision:: tau, gamma, vx2d, vy2d, max_x, max_y

      e = 0D0
      s = 0D0
      Temp = 0D0
      vx = 0D0
      vy = 0D0
      vz = 0D0

      ctl = 0
      ! check tau first
      tau = sqrt(t*t-z*z)
      If (tau < Tau0) Then
        ctl = 3
        Return
      EndIf

      If (tau > maxTau) Then
        ctl = 2
        Return
      EndIf

      max_x = XH*DX ! assume symmetryic lattice
      If (abs(x) > max_x) Then
        ctl = 1
        Return
      EndIf

      max_y = YH*DY
      If (abs(y) > max_y) Then
        ctl = 1
        Return
      EndIf


      ! Get hydro info
      call readHydroInfoBuffered(tau,x,y,e,s,Temp,vx2d,vy2d)
      vz = z/(t+1D-30)
      gamma = 1D0/(sqrt(1D0-vz*vz)+1D-30)
      vx = vx2d/gamma
      vy = vy2d/gamma

      End Subroutine
!------------------------------------------------------------------------

!=======================================================================
!==============     For sparse file output     =========================
!=======================================================================
! Sparse means that not hydro info on all lattice points are outputed; only
! those lattice points that have spacing LSX,LSY,LST will be outputted.
! Also the lattice (and the sparse one) is assumed to be symmetric w.r.t
! to (0,0)


!#######################################################################
      Subroutine writeHydroCtlSparse(XL,XH,DX,LSX_in,YL,YH,DY,LSY_in,
     &  Tau0,dTau,LST_in)
!     Write the header
!      -- XL, XH, DX: x varies in [XL,XH]*DX; XL and XH are integers
!      -- YL, YH, DY: y varies in [YL,YH]*DY; YL and YH are integers
!      -- LSX_in, LSY_in: lattice spacing for output; only info on lattice
!         with spacing LSX_in and LSY_in (centered at 0) will be outputted
      Implicit None
      Integer:: XL, XH, LSX_in, YL, YH, LSY_in, LST_in
      Double Precision:: DX, DY, Tau0, dTau

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: XLS,XHS,YLS,YHS
      Double Precision:: dXS,dYS,Tau0S,dTauS
      Common /hydroInfo/ XLS,XHS,dXS,YLS,YHS,dYS,Tau0S,dTauS

      Integer:: XShift,LSX,YShift,LSY,LST,LST_cur
      Common /sparse/ XShift,LSX,YShift,LSY,LST,LST_cur ! LST_cur is used to control when to write to file

      Integer:: write_id
      Common /write_id/ write_id

      ! initialize write id (current writting position)
      write_id = 1

      ! initialize sparse data:
      XShift = Abs(Mod(XL,LSX_in))
      LSX = LSX_in
      YShift = Abs(Mod(YL,LSY_in))
      LSY = LSY_in
      LST = LST_in
      LST_cur = 0 ! 0 means to write the 1st block

      ! write hydroInfo block
      XLS=XL
      XHS=XH
      dXS=DX
      YLS=YL
      YHS=YH
      dYS=DY
      Tau0S=Tau0
      dTauS=dTau

      ! write data file
      Open(ctlID, FILE=ctlFN,STATUS='REPLACE')
      Write(ctlID,'(I8,I8,E20.8,I8,I8,E20.8,E20.8,E20.8)')
     &  (XL+XShift)/LSX,(XH-XShift)/LSX,DX*LSX,
     &  (YL+YShift)/LSY,(YH-YShift)/LSY,DY*LSY,
     &  Tau0,dTau*LST
      Close(ctlID)

      Open(dataID, FILE=dataFN,STATUS='REPLACE',FORM='UNFORMATTED')
      Close(dataID)

      End Subroutine
!------------------------------------------------------------------------



!########################################################################
      Subroutine writeHydroBlockSparse(Time,Ed,Sd,Temp,VxB,VyB)
!     Write a block to the file
      Implicit None

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) :: Ed,Sd,Temp,VxB,VyB
      Double Precision :: Time

      Integer:: XShift,LSX,YShift,LSY,LST,LST_cur
      Common /sparse/ XShift,LSX,YShift,LSY,LST,LST_cur

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: write_id
      Common /write_id/ write_id

      If (LST_cur/=0) Then
        ! no writing action
        LST_cur = LST_cur-1
        Return
      Else
        ! write to file (later)
        LST_cur = LST-1
      EndIf

      !Print*, "HERE!!!"

      Inquire(IOLENGTH=length) write_id,Time,
     &  Ed(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  Sd(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  Temp(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  VxB(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  VyB(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1) ! how large the data block is
      Open(dataID,FILE=dataFN,
     &  STATUS='OLD',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=length)

      Write(dataID, REC=write_id) write_id,Time,
     &  Ed(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  Sd(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  Temp(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  VxB(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1),
     &  VyB(XL+XShift:XH-XShift:LSX,YL+YShift:YH-YShift:LSY,1:1) ! write


      write_id = write_id + 1

      Close(dataID)

      End Subroutine
!------------------------------------------------------------------------



!=======================================================================
!============     For hydro data size control    =======================
!=======================================================================

!#######################################################################
      Subroutine getHydroDataRecLength(numberOfRecs)
!     Return number of records
      Implicit None

      Integer:: numberOfRecs

      Integer:: maxRec
      Common /maxRec/ maxRec

      Double Precision:: maxTau
      Common /maxTau/ maxTau

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau
      Double Precision, Dimension(XL:XH,YL:YH,1:1) :: Ed,Sd,Temp,VxB,VyB
      Double Precision :: Time

      Character*80:: dataFN, ctlFN
      Integer:: dataID, ctlID
      Common /dataFile/ dataFN, ctlFN
      Common /fileId/ dataID, ctlID

      Integer:: idx_check

      Inquire(IOLENGTH=length) idx_check,Time,Ed,Sd,Temp,VxB,VyB ! how large the data block is

      Open(dataID,FILE=dataFN,
     &  STATUS='OLD',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=length)

      J = 1
      Do
        Read(dataID, REC=J, IOSTAT=I)
     &    idx_check,Time,Ed,Sd,Temp,VxB,VyB ! write
        If (I/=0) Exit
        J = J+1
      End Do

      Close(dataID)

      maxRec = J-1
      numberOfRecs = maxRec

      maxTau = Tau0 + maxRec*dTau

      End Subroutine
!-----------------------------------------------------------------------


!=======================================================================
!==============   For Jet info inside hydro profile   ==================
!=======================================================================

!#######################################################################
      Subroutine getJetDeltaTauMax(x0,y0,dirX,dirY,cutT,step,deltaTau)
!     Return the max possible deltaTau that determines the length of the
!     path of a jet positioned at (x0,y0) with direction (dirX,dirY). The
!     jet is assumed to travel at speed of light and the deltaTau value
!     is determined by the time the jet leaves the hydro data x-y-tau cube.
!     deltaTau is measured relative to tau0.
!     Must be called after getHydroDataRecLength.
!     After the crude maximum is determined, it is reduced by checking,
!     from the maximum possible value deltaTau, "step" by "step", if the
!     temperature is larger than cutT. It keeps shrinking if the readed
!     temperature is less than cutT (GeV)
!     Note that this method is slower than getJetDeltaTauMaxOld.

      Implicit None

      Double Precision:: x0,y0,dirX,dirY,deltaTau

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau

      Integer:: maxRec
      Common /maxRec/ maxRec

      Double Precision:: rXL,rXH,rYL,rYH
      Double Precision:: dirNorm
      Double Precision:: cutT,step,jetLength,e,s,T,vx,vy

      ! first, calculate boundaries
      rXL = XL*DX
      rXH = XH*DX
      rYL = YL*DY
      rYH = YH*DY

      ! next, find intersections and length
      dirNorm = Sqrt(dirX*dirX+dirY*dirY)

      If (Abs(dirX)>=Abs(dirY)) Then
        ! intersects the left or right sides
        If (dirX>=0D0) Then
          ! intersects the right side
          deltaTau = (rXH-x0)/dirX*dirNorm
        Else
          ! intersects the left side
          deltaTau = -(x0-rXL)/dirX*dirNorm
        EndIf
      Else
        ! intersects the top or bottem
        If (dirY>=0D0) Then
          ! intersects the top
          deltaTau = (rYH-y0)/dirY*dirNorm
        Else
          ! intersects with the bottom
          deltaTau = -(y0-rYL)/dirY*dirNorm
        EndIf
      EndIf

      deltaTau = Min(deltaTau, dTau*(maxRec-2)) ! constrain from tau direction)

      ! try to shrink this length
      jetLength = deltaTau
      Do
        If (jetLength<0D0) Then
          deltaTau=0D0
          Exit
        EndIf
        Call readHydroInfoBuffered(jetLength+Tau0,
     &    x0+dirX/dirNorm*jetLength/deltaTau,
     &    y0+dirY/dirNorm*jetLength/deltaTau,
     &    e,s,T,vx,vy)
        If (T>cutT) Then
          deltaTau=jetLength
          Exit
        EndIf
        jetLength=jetLength-step
      End Do

      End Subroutine
!------------------------------------------------------------------------




!#######################################################################
      Subroutine getJetDeltaTauMaxOld
     &           (x0,y0,dirX,dirY,deltaTau)
!     Return the max possible deltaTau that determines the length of the
!     path of a jet positioned at (x0,y0) with direction (dirX,dirY). The
!     jet is assumed to travel at speed of light and the deltaTau value
!     is determined by the time the jet leaves the hydro data x-y-tau cube.
!     deltaTau is measured relative to tau0.
!     Must be called after getHydroDataRecLength.


      Implicit None

      Double Precision:: x0,y0,dirX,dirY,deltaTau

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau

      Integer:: maxRec
      Common /maxRec/ maxRec

      Double Precision:: rXL,rXH,rYL,rYH
      Double Precision:: dirNorm
      Double Precision:: e,s,T,vx,vy

      ! first, calculate boundaries
      rXL = XL*DX
      rXH = XH*DX
      rYL = YL*DY
      rYH = YH*DY

      ! next, find intersections and length
      dirNorm = Sqrt(dirX*dirX+dirY*dirY)

      If (Abs(dirX)>=Abs(dirY)) Then
        ! intersects the left or right sides
        If (dirX>=0D0) Then
          ! intersects the right side
          deltaTau = (rXH-x0)/dirX*dirNorm
        Else
          ! intersects the left side
          deltaTau = -(x0-rXL)/dirX*dirNorm
        EndIf
      Else
        ! intersects the top or bottem
        If (dirY>=0D0) Then
          ! intersects the top
          deltaTau = (rYH-y0)/dirY*dirNorm
        Else
          ! intersects with the bottom
          deltaTau = -(y0-rYL)/dirY*dirNorm
        EndIf
      EndIf

      deltaTau = Min(deltaTau, dTau*(maxRec-2)) ! constrain from tau direction)

      End Subroutine
!------------------------------------------------------------------------




!=======================================================================
!==============     For easier read data setup =========================
!=======================================================================


!#######################################################################
      Subroutine setHydroFilesEZ(dataID_in, dataFN_in,
     &                           ctlID_in, ctlFN_in, bufferSize)

      Implicit None
      Character (len=*) :: dataFN_in, ctlFN_in
      Integer:: dataID_in, ctlID_in

      Integer:: XL, XH, YL, YH
      Double Precision:: DX, DY, Tau0, dTau

      Integer:: bufferSize
      Integer:: numberOfRecs
      Integer:: sizeNeeded

      Call setHydroFiles(dataID_in, dataFN_in,
     &                   ctlID_in, ctlFN_in)

      Call readHydroCtlBuffered(XL,XH,DX,YL,YH,DY,Tau0,dTau,bufferSize)

      Call getHydroDataRecLength(numberOfRecs)

      sizeNeeded = min(numberOfRecs, bufferSize)
      Call readAllBuffers(sizeNeeded)

      End Subroutine
!-----------------------------------------------------------------------



!***************  For 3-column 2d data interpolation  ******************
!***********************************************************************
      Subroutine read3Col2DArray(filename,fileID)
      Implicit None
      Character(len=*):: filename
      Integer:: fileID,IO
      Double Precision, Parameter:: EPS=1D-15

      Integer:: XL,XH,YL,YH ! integer boundaries
      Double Precision:: DX,DY,dXL,dYL ! dXL and dYL are the physical lower bounds, XL and YL are the integer ones
      Double Precision, Pointer:: P(:,:)
      Common /set3Col2DArrayData/ XL,XH,DX,dXL,YL,YH,DY,dYL,P

      Double Precision:: dXL1,dXH,dYL1,dYH ! used to determine the size of the array, "d" for double
      Double Precision:: x,y,value
      Integer:: ix,iy

      Print*, "Use table:",filename

      ! find lattice size:
      dXL=0D0
      dXL1=dXL+1D0 ! XL1 is the x directly to the right of XL, used to determine dx
      dXH=dXL-1D0
      dYL=0D0
      dYL1=dYL+1D0
      dYH=dYL-1D0
      Open(fileID,FILE=filename)
      Do
        Read(fileID,*,IOSTAT=IO) x,y,value
        If(IO/=0) Exit

        If (x<dXL+EPS) Then
          dXL=x
        ElseIf(x<dXL1+EPS) Then
          dXL1=x
        EndIf

        If (x>dXH-EPS) dXH=x

        If (y<dYL+EPS) Then
          dYL=y
        ElseIf(y<dYL1+EPS) Then
          dYL1=y
        EndIf

        If (y>dYH-EPS) dYH=y
      End Do
      Close(fileID)

      XL = 0 ! current implementation
      DX = dXL1-dXL
      XH = floor((dXH-dXL)/DX+0.5D0)

      YL = 0
      DY = dYL1-dYL
      YH = floor((dYH-dYL)/DY+0.5D0)

      Allocate(P(XL:XH,XL:XH))
      P = 0D0

      ! get values:
      Open(fileID,FILE=filename)
      Do
        Read(fileID,*,IOSTAT=IO) x,y,value
        If(IO/=0) Exit
        ix = floor((x-dXL)/DX+0.5D0)
        iy = floor((y-dYL)/DY+0.5D0)
        P(ix,iy)=value
      End Do
      Close(fileID)

c     Print*, P
      End Subroutine
!-----------------------------------------------------------------------



!***********************************************************************
      Subroutine TwoDInterp(x,y,result)
      Implicit None

      Double Precision:: x,y,result

      Integer:: XL,XH,YL,YH ! integer boundaries
      Double Precision:: DX,DY,dXL,dYL ! dXL and dYL are the physical lower bounds, XL and YL are the integer ones
      Double Precision, Pointer:: P(:,:)
      Common /set3Col2DArrayData/ XL,XH,DX,dXL,YL,YH,DY,dYL,P

      Integer:: ix,iy
      Double Precision:: x_frac, y_frac

      ix = floor((x-dXL)/DX+0.5D0)
      x_frac = (x-dXL)/DX - ix
      iy = floor((y-dYL)/DY+0.5D0)
      y_frac = (y-dYL)/DY - iy

      If (ix<XL .OR. ix>XH-1) Then
        Print*, "TwoDInterp warning: out of boundary"
        Print*, x,ix,XL,XH
        result = 0D0
        Return
      End If

      If (iy<YL .OR. iy>YH-1) Then
        Print*, "TwoDInterp warning: out of boundary"
        Print*, y,iy,YL,YH
        result = 0D0
        Return
      End If

      result = P(ix,iy)*(1-x_frac)*(1-y_frac)
     &        +P(ix+1,iy)*x_frac*(1-y_frac)
     &        +P(ix,iy+1)*(1-x_frac)*y_frac
     &        +P(ix+1,iy+1)*x_frac*y_frac

      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
!-----------------------------------------------------------------------
!
! Most recent agreed interface
!
!-----------------------------------------------------------------------
!#######################################################################


!#######################################################################
      Subroutine hydroSetFiles()
      Call setHydroFilesEZ(5379, "JetData.dat",
     &                     5380, "JetCtl.dat", 1000)
      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
      Subroutine hydroTestCoord2D(tau,x,y,ctl)
! -- ctl: control information:
!      0: no error
!      1: x or y out-of-boundary
!      2: tau larger than maxTau
!      3: tau smaller than tau0

      Implicit None
      Double Precision:: tau,x,y
      Integer:: ctl

      Integer:: XL,XH,YL,YH
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau

      Double Precision:: maxTau
      Common /maxTau/ maxTau

      Double Precision:: max_x, max_y

      ctl = 0
      ! check tau first
      If (tau < Tau0) Then
        ctl = 3
        Return
      EndIf

      If (tau > maxTau) Then
        ctl = 2
        Return
      EndIf

      max_x = XH*DX ! assume symmetryic lattice
      If (abs(x) > max_x) Then
        ctl = 1
        Return
      EndIf

      max_y = YH*DY
      If (abs(y) > max_y) Then
        ctl = 1
        Return
      EndIf

      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
      Subroutine hydroReadInfo2D(tau,x,y,e,s,Temp,Bd,vx,vy)
      ! To test the validity of (tau,x,y), call hydroTestCoord2D

      Implicit None
      Double Precision:: tau,x,y,e,s,Temp,Bd,vx,vy
      Bd = 0D0
      Call readHydroInfoBuffered(tau,x,y,e,s,Temp,vx,vy)

      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
      Subroutine hydroReadInfo2DZeroOnErr(tau,x,y,e,s,Temp,Bd,vx,vy)
      ! To test the validity of (tau,x,y), call hydroTestCoord2D

      Implicit None
      Double Precision:: tau,x,y,e,s,Temp,Bd,vx,vy
      Integer:: ctl
      Call hydroTestCoord2D(tau,x,y,ctl)
      If (ctl/=0) Then
        e = 0D0
        s = 0D0
        Temp = 0D0
        vx = 0D0
        vy = 0D0
        Return
      Else
        Call hydroReadInfo2D(tau,x,y,e,s,Temp,Bd,vx,vy)
      EndIf

      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
      Subroutine hydroTestCoord3D(t,x,y,z,ctl)
! -- ctl: control information:
!      0: no error
!      1: x or y out-of-boundary
!      2: tau larger than maxTau
!      3: tau smaller than tau0
!      4: |t| < |z|
      Implicit None
      Double Precision:: t,x,y,z
      Integer:: ctl

      Double Precision:: tau

      if (abs(t)<abs(z)) Then
        ctl = 4
        Return
      EndIf

      tau = sqrt(t*t-z*z)
      Call hydroTestCoord2D(tau,x,y,ctl)

      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
      Subroutine hydroReadInfo3D(t,x,y,z,e,s,Temp,Bd,vx,vy,vz)

      Implicit None

      Double Precision:: t,x,y,z,e,s,Temp,Bd,vx,vy,vz
      Integer:: ctl

      Double Precision:: tau, gamma, vx2d, vy2d
      Bd = 0D0

      tau = sqrt(t*t-z*z)
      Call hydroReadInfo2D(tau,x,y,e,s,Temp,Bd,vx2d,vy2d)
      vz = z/(t+1D-30)
      gamma = 1D0/(sqrt(1D0-vz*vz)+1D-30)
      vx = vx2d/gamma
      vy = vy2d/gamma

      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
      Subroutine hydroReadInfo3DZeroOnErr(t,x,y,z,e,s,Temp,Bd,vx,vy,vz)
      ! To test the validity of (t,x,y,z), call hydroTestCoord3D

      Implicit None
      Double Precision:: t,x,y,z,e,s,Temp,Bd,vx,vy,vz
      Integer:: ctl
      Call hydroTestCoord3D(t,x,y,z,ctl)
      If (ctl/=0) Then
        e = 0D0
        s = 0D0
        Temp = 0D0
        vx = 0D0
        vy = 0D0
        vz = 0D0
        Return
      Else
        Call hydroReadInfo3D(t,x,y,z,e,s,Temp,Bd,vx,vy,vz)
      EndIf

      End Subroutine
!-----------------------------------------------------------------------


!#######################################################################
      Subroutine hydroGetJetMaxTau(x0,y0,dirX,dirY,cutT,maxTau)
      Implicit None
      Double Precision:: x0,y0,dirX,dirY,cutT,maxTau
      Call getJetDeltaTauMax(x0,y0,dirX,dirY,cutT,0.05D0,maxTau)
      End Subroutine
!-----------------------------------------------------------------------



!#######################################################################
      Subroutine hydroGetLatticeInfo(vXL,vXH,vDX,vYL,vYH,vDY,
     &                               vTau0,vdTau,vmaxTau)
      Implicit None

      Integer:: vXL,vXH,vYL,vYH
      Double Precision:: vDX,vDY,vTau0,vdTau,vmaxTau

      Integer:: XL,XH,YL,YH,idx,I,J,length
      Double Precision:: DX,DY,Tau0,dTau
      Common /hydroInfo/ XL,XH,DX,YL,YH,DY,Tau0,dTau

      Double Precision:: maxTau
      Common /maxTau/ maxTau

      vXL = XL
      vXH = XH
      vYL = YL
      vYH = YH
      vDX = DX
      vDY = DY
      vTau0 = Tau0
      vdTau = dTau
      vmaxTau = maxTau

      End Subroutine
!-----------------------------------------------------------------------



!#######################################################################
      Subroutine hydroCleanUp()
      End Subroutine
!-----------------------------------------------------------------------
