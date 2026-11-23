PROGRAM IOarray
        IMPLICIT NONE
  !variables
        REAL :: m,Fx,Fy,Fz !mass(kg) Fxyz(kg m s-2) 
        INTEGER :: k,nk,p  !docycle, # of steps,# of particles 
        REAL :: dt !(time of one step(s)) 
        REAL ::X0,Y0,Z0,Fx0,Fy0,Fz0,Fxnext !inital position and speed
        REAL :: x,y,z,Vx,Vy,Vz !current postion of body
  !arrays
        REAL (KIND=wp), DIMENSION () ::
        REAL (KIND=wp), DIMENSION () ::
        REAL (KIND=wp), DIMENSION () ::
  !input
        OPEN (unit=11, FILE="input.txt", STATUS="old", action="read")

        READ (UNIT=11, FMT=*)  
        READ (UNIT=11, FMT=*)
        READ (UNIT=11, FMT=*)

   !for testing(fixed values)
   ! m=1 ;Fx=0.1; Fy=0; Fz=0; nk=600; dt=0.2; X0=0; Y0=0; Z0=0; Fx0=0; Fy0=0; Fz0=0
 
   !setting 00
    x=0; y=0; z=0
    Vx=Fx0; Vy=Fy0; Vz=Fz0
 


    !math  
  
   DO k=1,nk 
    x= x+(dt*Vx)+(dt**2*(Fx/2*m))
   
    
    Fxnext=Fx
    Vx=Vx+((dt/2*m)*(Fx+Fxnext))
   
     
    Print*,"pos", x,y,z
    Print*,"speed", Vx,Vy,Vz

   ENDDO 
ENDPROGRAM
