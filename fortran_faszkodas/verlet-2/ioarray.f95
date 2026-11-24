PROGRAM IOarray
        IMPLICIT NONE
  !variables
        INTEGER,PARAMETER :: wp = selected_real_kind (p=13,r=300)
        REAL :: m,Fx,Fy,Fz !mass(kg) Fxyz(kg m s-2) 
        INTEGER :: k,l,j,    nk,p  !docycle(s),    # of steps,# of particles 
        REAL :: dt !(time of one step(s)) 
        REAL ::X0,Y0,Z0,Fx0,Fy0,Fz0,Fxnext !inital position and speed
        REAL :: x,y,z,Vx,Vy,Vz !current postion of body
       
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: par
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: Fnext 
        !REAL (KIND=wp), DIMENSION (),ALLOCATABLE :: 
   
     !for testing(fixed values)
     ! m=1 ;Fx=0.1; Fy=0; Fz=0; nk=600; dt=0.2; X0=0; Y0=0; Z0=0; Fx0=0; Fy0=0; Fz0=0
     !setting 00
     x=0; y=0; z=0; Vx=0; Vy=0; Vz=0
     ! Vx=Fx0; Vy=Fy0; Vz=Fz0

        
  !input

        OPEN (unit=11, FILE="input.txt", STATUS="old", ACTION="read")
        READ (UNIT=11, FMT=*) nk, dt  
        READ (UNIT=11, FMT=*) p
     
        ALLOCATE (par(p,7))
        ALLOCATE (Fnext(p,3))

   DO l=1,p
        READ (UNIT=11, FMT=*) par(l,:)       ! = (/m,x,y,z,Vx,Vy,Vz/)
    ENDDO

  CLOSE(11)
      
   !math  
   DO j=1,p
    DO k=1,nk 
     par(j,2:4) = par(j,2:4)+(dt*par(j,5:7))+(dt**2.0)*(par(j,5:7)/(2.0*spread(par(j,1), dim=1, ncopies=3)))
      
     !Fnext(1:p,:) = par(1:p,5:7)
     par(j,5:7)=par(j,5:7)+((dt/2*spread(par(j,1), dim=1, ncopies=3))*(par(j,5:7)*2.0))
     !par(:,5:7)=par(:,5:7)+((dt/2*spread(par(:,1), dim=2, ncopies=3))*(par(:,5:7)+Fnext(:,:)))
   
   
    ENDDO 
   ENDDO  
     
  OPEN (unit=12, FILE="output.txt", STATUS="replace", ACTION="write")
    WRITE (UNIT=12,FMT=*) "x,y,z,Vx,Vy,Vz"
     DO l=1,p  
      WRITE (UNIT=12, FMT='(*(G12.4))') par (l,2:7) 
     ENDDO
  CLOSE(12)    
DEALLOCATE ( par, Fnext ) 
ENDPROGRAM
