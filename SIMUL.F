      PROGRAM SIMUL
***********************************************************************
*     SIMUL generates points within defined subvolumes
*     subtracts points belonging to overlapping subvolumes and
*     writes the remaining points to a file.
*......................................................................
*     NPAR1                : max no of subvolumes or "sets"
*     NPAR2                : max no of points in each set
*     TYPE(I,J)            : characteristica of set I
*     CM(I,J)              : center of mass (j=1,2,3) of set I
*     ROT(I,J)             : rotationcoordinates (j=1,2) of set I
*     NTOT(I)              : total no of points in set I
*     X(I,J),Y(I,J),Z(I,J) : coordinate J of set I
*     RAN1                 : random number (function)
***********************************************************************
      PARAMETER (NPAR1=10,NPAR2=4000)
      COMMON /ALL/ TYPE(NPAR1,4), CM(NPAR1,3), ROT(NPAR1,2),I,PI,RHO,
     !NTOT(NPAR1), X(NPAR1,NPAR2),Y(NPAR1,NPAR2),Z(NPAR1,NPAR2),R(97)
      CHARACTER*20 A
      CHARACTER*1 ANSWER
      PI=3.14159
      WRITE(6,5)
   5  FORMAT(1X,'Name of datafile     => ',$)
      READ(5,6)A
   6  FORMAT(A)
      OPEN(unit=3,NAME=A,status='UNKNOWN')
      WRITE(6,2)
   2  FORMAT(1X,'Seed in [0,1]        => ',$)
      READ(5,*)RN
      IR=-RN*2**30
      RN=RAN1(IR)
      I=1
 111  WRITE(6,112)NPAR2
 112  FORMAT(1X,'Ntot <',I5,'          => ',$)
      READ(5,*)NTOT(I)
      IF(NTOT(I).GT.NPAR2) WRITE(6,*)'Ntot > ',NPAR2, '   !!!!!!!!'
      IF(NTOT(I).GT.NPAR2) GOTO 111
      I=0
  50  I=I+1
   1  WRITE(6,*)'Sphere        (1)'
      WRITE(6,*)'Cylinder      (2)'
      WRITE(6,*)'Ellipsoid     (3)'
      WRITE(6,4)
   4  FORMAT(1X,'Shell         (4)    => ',$)
      READ(5,*)TYPE(I,1)
      IF(TYPE(I,1).EQ.1) CALL SPHERE
      IF(TYPE(I,1).EQ.2) CALL CYLINDER
      IF(TYPE(I,1).EQ.3) CALL ELLIPSOID
      IF(TYPE(I,1).EQ.4) CALL SHELL
 
      IF(NTOT(I).EQ.0) GOTO 69
      WRITE(6,61)
  61  FORMAT(1X,'Rotationangles around x and z-axis (deg) => ',$)
      READ(5,*)ROT(I,1),ROT(I,2)
      ROT(I,1)=ROT(I,1)*PI/180.
      ROT(I,2)=ROT(I,2)*PI/180.
      WRITE(6,60)
  60  FORMAT(1X,'Translation-coordinates (center of mass) => ',$)
      READ(5,*)CM(I,1),CM(I,2),CM(I,3)
 
      DO 65 J=1,NTOT(I)
      CALL DISP(X(I,J),Y(I,J),Z(I,J),ROT(I,1),ROT(I,2),
     -CM(I,1),CM(I,2),CM(I,3))
  65  CONTINUE
 
  69  IF(NTOT(I).EQ.0) I=I-1
      WRITE(6,70)
  70  FORMAT(1X,'Another volume (Y/N)                     => ',$)
      READ(5,71)ANSWER
  71  FORMAT(A)
      IF(ANSWER.EQ.'Y') GOTO 50
      N=I
 
      IF(N.LE.1) GOTO 86
      DO 80 I=2,N
      CALL TEST
  80  CONTINUE
 
  86  NTOTSUM=0
      DO 90 I=1,N
      DO 89 J=1,NTOT(I)
      IF(X(I,J).EQ.0) GOTO 89
      NTOTSUM=NTOTSUM+1
      WRITE(3,*)1.,X(I,J),Y(I,J),Z(I,J)
  89  CONTINUE
  90  CONTINUE
 
      WRITE(6,*)'Total volume consists of',NTOTSUM,'points'
 
  99  STOP
      END
 
**********************************************************************
*     Sphere
**********************************************************************
      SUBROUTINE SPHERE
      PARAMETER (NPAR1=10,NPAR2=4000)
      COMMON /ALL/ TYPE(NPAR1,4), CM(NPAR1,3), ROT(NPAR1,2),I,PI,RHO,
     -NTOT(NPAR1), X(NPAR1,NPAR2),Y(NPAR1,NPAR2),Z(NPAR1,NPAR2),R(97)
      WRITE(6,3)
   3  FORMAT(1X,'Radius of sphere     => ',$)
      READ(5,*)TYPE(I,2)
      RX=TYPE(I,2)
      VOL=4./3*PI*RX**3
      slength=rx*rx
      IF(I.GT.1) NTOT(I)=VOL*RHO
      IF(NTOT(I).GT.NPAR2) THEN
      WRITE(6,*)'Ntot > ',NPAR2, '   !!!!!!!!'
      NTOT(I)=0
      RETURN
      ENDIF
      IF(I.EQ.1) RHO=NTOT(I)/VOL
      WRITE(6,5)I,NTOT(I),VOL,RHO
   5  FORMAT(1X,'NTOT(',I2,') =',I4,'    VOL = ',E7.2,'   RHO = ',E9.4)
      J=0
  10  J=J+1 
  20  rn=ran1(ir)
      XR=(rn-.5)*2*RX
      rn=ran1(ir)
      YR=(RN-.5)*2*RX
      rn=ran1(ir)
      ZR=(RN-.5)*2*RX
      xlength=xr*xr+yr*yr+zr*zr
      IF(xlength.GT.slength) GOTO 20
      X(I,J)=XR
      Y(I,J)=YR
      Z(I,J)=ZR
      IF(J.LT.NTOT(I)) GOTO 10
      RETURN
      END
**********************************************************************
*     Cylinder
**********************************************************************
      SUBROUTINE CYLINDER
      PARAMETER (NPAR1=10,NPAR2=4000)
      COMMON /ALL/ TYPE(NPAR1,4), CM(NPAR1,3), ROT(NPAR1,2),I,PI,RHO,
     -NTOT(NPAR1), X(NPAR1,NPAR2),Y(NPAR1,NPAR2),Z(NPAR1,NPAR2),R(97)
      WRITE(6,3)
   3  FORMAT(1X,'Radius of cylinder (in XY-plane)   => ',$)
      READ(5,*)TYPE(I,2)
      RX=TYPE(I,2)
      clength=rx*rx
      WRITE(6,4)
   4  FORMAT(1X,'Length of cylinder (along Z-axis)  => ',$)
      READ(5,*)TYPE(I,3)
      A=TYPE(I,3)
      VOL=PI*RX**2*A
      IF(I.GT.1) NTOT(I)=VOL*RHO
      IF(NTOT(I).GT.NPAR2) THEN
      WRITE(6,*)'Ntot > ',NPAR2, '   !!!!!!!!'
      NTOT(I)=0
      RETURN
      ENDIF
      IF(I.EQ.1) RHO=NTOT(I)/VOL
      WRITE(6,5)I,NTOT(I),VOL,RHO
   5  FORMAT(1X,'NTOT(',I2,') =',I4,'    VOL = ',E7.2,'   RHO = ',E9.4)
      J=0
  10  J=J+1
  20  XR=(RAN1(IR)-.5)*2*RX
      YR=(RAN1(IR)-.5)*2*RX
      xlength=xr*xr+yr*yr
      IF(xlength.GT.clength) GOTO 20
      ZR=(RAN1(IR)-.5)*A
      X(I,J)=XR
      Y(I,J)=YR
      Z(I,J)=ZR
      IF(J.LT.NTOT(I)) GOTO 10
      RETURN
      END
**********************************************************************
*     Ellipsoid
**********************************************************************
      SUBROUTINE ELLIPSOID
      PARAMETER (NPAR1=10,NPAR2=4000)
      COMMON /ALL/ TYPE(NPAR1,4), CM(NPAR1,3), ROT(NPAR1,2),I,PI,RHO,
     -NTOT(NPAR1), X(NPAR1,NPAR2),Y(NPAR1,NPAR2),Z(NPAR1,NPAR2),R(97)
      WRITE(6,3)
   3  FORMAT(1X,'First (X-) axis of ellipsoid      => ',$)
      READ(5,*)A
      TYPE(I,2)=A
      WRITE(6,4)
   4  FORMAT(1X,'Second (Y-) axis of ellipsoid     => ',$)
      READ(5,*)B
      TYPE(I,3)=B
      WRITE(6,6)
   6  FORMAT(1X,'Third (Z-) axis of ellipsoid      => ',$)
      READ(5,*)C
      TYPE(I,4)=C
      VOL=4./3*PI*A*B*C
      IF(I.GT.1) NTOT(I)=VOL*RHO
      IF(NTOT(I).GT.NPAR2) THEN
      WRITE(6,*)'Ntot > ',NPAR2, '   !!!!!!!!'
      NTOT(I)=0
      RETURN
      ENDIF
      IF(I.EQ.1) RHO=NTOT(I)/VOL
      WRITE(6,5)I,NTOT(I),VOL,RHO
   5  FORMAT(1X,'NTOT(',I2,') =',I4,'    VOL = ',E7.2,'   RHO = ',E9.4)
      J=0
  10  J=J+1
  20  XR=(RAN1(IR)-.5)*A*2
      YR=(RAN1(IR)-.5)*B*2
      ZR=(RAN1(IR)-.5)*C*2
      IF(((XR/A)**2+(YR/B)**2+(ZR/C)**2).GT.1.) GOTO 20
      X(I,J)=XR
      Y(I,J)=YR
      Z(I,J)=ZR
      IF(J.LT.NTOT(I)) GOTO 10
      RETURN
      END
**********************************************************************
*     Shell
**********************************************************************
      SUBROUTINE SHELL
      PARAMETER (NPAR1=10,NPAR2=4000)
      COMMON /ALL/ TYPE(NPAR1,4), CM(NPAR1,3), ROT(NPAR1,2),I,PI,RHO,
     -NTOT(NPAR1), X(NPAR1,NPAR2),Y(NPAR1,NPAR2),Z(NPAR1,NPAR2),R(97)
      WRITE(6,3)
   3  FORMAT(1X,'Radii of shell (R1 < R2)     => ',$)
      READ(5,*)R1,R2
      TYPE(I,2)=R1
      TYPE(I,3)=R2
      VOL=4./3*PI*(R2**3-R1**3)
      IF(I.GT.1) NTOT(I)=VOL*RHO
      IF(NTOT(I).GT.NPAR2) THEN
      WRITE(6,*)'Ntot > ',NPAR2, '   !!!!!!!!'
      NTOT(I)=0
      RETURN
      ENDIF
      IF(I.EQ.1) RHO=NTOT(I)/VOL
      WRITE(6,5)I,NTOT(I),VOL,RHO
   5  FORMAT(1X,'NTOT(',I2,') =',I4,'    VOL = ',E7.2,'   RHO = ',E9.4)
      J=0
  10  J=J+1
  20  XR=(RAN1(IR)-.5)*2*R2
      YR=(RAN1(IR)-.5)*2*R2
      ZR=(RAN1(IR)-.5)*2*R2
      DIST=SQRT(XR**2+YR**2+ZR**2)
      IF((DIST.GT.R2).OR.(DIST.LT.R1)) GOTO 20
      X(I,J)=XR
      Y(I,J)=YR
      Z(I,J)=ZR
      IF(J.LT.NTOT(I)) GOTO 10
      RETURN
      END
**********************************************************************
*     Test for overlapping regions - by inverse transformation -
*     if a point satisfies the conditions of an "earlier" set
*     its x-coordinate is set eq to 0 and the point is not
*     written to the file.
**********************************************************************
      SUBROUTINE TEST
      PARAMETER (NPAR1=10,NPAR2=4000)
      COMMON /ALL/ TYPE(NPAR1,4), CM(NPAR1,3), ROT(NPAR1,2),I,PI,RHO,
     -NTOT(NPAR1), X(NPAR1,NPAR2),Y(NPAR1,NPAR2),Z(NPAR1,NPAR2),R(97) 
      DO 200 K=1,I-1
      DO 100 J=1,NTOT(I)
      XR=X(I,J)-CM(K,1)
      YR=Y(I,J)-CM(K,2)
      ZR=Z(I,J)-CM(K,3)
      CALL DISP2(XR,YR,ZR,-ROT(K,1),-ROT(K,2))
      IF(TYPE(K,1).EQ.1) GOTO 1
      IF(TYPE(K,1).EQ.2) GOTO 2
      IF(TYPE(K,1).EQ.3) GOTO 3
      IF(TYPE(K,1).EQ.4) GOTO 4
    1 IF(SQRT(XR*XR+YR*YR+ZR*ZR).LT.TYPE(K,2)) GOTO 15
      GOTO 100
    2 IF((SQRT(XR*XR+YR*YR).LT.TYPE(K,2)).AND.(ABS(ZR).LT.TYPE(K,3)/2.))
     -GOTO 15
      GOTO 100
    3 IF(((XR/TYPE(K,2))**2+(YR/TYPE(K,3))**2+(ZR/TYPE(K,4))**2).LT.1.)
     -GOTO 15
      GOTO 100
    4 DIST=SQRT(XR**2+YR**2+ZR**2)
      IF((DIST.GT.TYPE(K,2)).AND.(DIST.LT.TYPE(K,3))) GOTO 15
      GOTO 100
   15 X(I,J)=0
  100 CONTINUE
  200 CONTINUE
      RETURN
      END
**********************************************************************
*     Rotation and translation  -  usual convention of notation
*     if  l = sqrt(x**2+y**2+z**2)
*     theta = arccos(z/l)  and   phi = arccos(x/l(x,y))
**********************************************************************
      SUBROUTINE DISP(X,Y,Z,THETA,PHI,CM1,CM2,CM3)
      CT=COS(THETA)
      ST=SIN(THETA)
      CP=COS(PHI)
      SP=SIN(PHI)
      EXX=X*CP-Y*SP*CT-Z*SP*ST
      EYY=X*SP+Y*CP*CT+Z*CP*ST
      EZZ=     Y*(-ST)+Z*CT
      X=EXX+CM1
      Y=EYY+CM2
      Z=EZZ+CM3
      RETURN
      END
**********************************************************************
*     Inverse rotation
**********************************************************************
      SUBROUTINE DISP2(X,Y,Z,THETA,PHI)
      CT=COS(THETA)
      ST=SIN(THETA)
      CP=COS(PHI)
      SP=SIN(PHI)
      EXX=X*CP      +Y*(-SP)
      EYY=X*SP*CT   +Y*CP*CT   +Z*ST
      EZZ=X*(-SP*ST)+Y*(-CP*ST)+Z*CT
      X=EXX
      Y=EYY
      Z=EZZ
      RETURN
      END
***********************************************************************
*     Random number generator from "Numerical Recipes"
***********************************************************************
      FUNCTION RAN1(IDUM)
      PARAMETER (NPAR1=10,NPAR2=4000)
      COMMON /ALL/ TYPE(NPAR1,4), CM(NPAR1,3), ROT(NPAR1,2),I,PI,RHO,
     -NTOT(NPAR1), X(NPAR1,NPAR2),Y(NPAR1,NPAR2),Z(NPAR1,NPAR2),R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=1./M1)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=1./M2)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      DATA IFF /0/
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
      IFF=1
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      DO 11 J=1,97
             IX1=MOD(IA1*IX1+IC1,M1)
             IX2=MOD(IA2*IX2+IC2,M2)
             R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1 
   11 CONTINUE
      IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97.OR.J.LT.1)PAUSE
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END
***********************************************************************
