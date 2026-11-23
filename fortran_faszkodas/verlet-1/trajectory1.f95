PROGRAM Traj1
        IMPLICIT NONE
  !variables
        REAL :: m,Fx,Fy,Fz !mass(kg) Fxyz(kg m s-2) 
        INTEGER :: k,nk
        REAL :: dt !(step#,time of one step(s)) 
        REAL ::X0,Y0,Z0,Fx0,Fy0,Fz0 !inital position and speed
        REAL :: x,y,z,Vx,Vy,Vz !current postion of body


        PRINT*,"Give me the mass(Kg) and the forces x,y,z (kg m s-2)!"
        READ*, m,Fx,Fy,fz
        PRINT*,"Give me the temporal dimensions(nk,Tau)!"
        READ*, nk,dt
      !  PRINT*,"Give me the inital position (X0,Y0,Z0)!"
      !  READ*, X0,Y0,Z0
      !  PRINT*,"Give me the initial speed (Fx0,Fy0,Fz0)!"
      !  READ*, Fx0,Fy0,Fz0
       
        Print*,"The Genie will now do its thing..."  
   
   !for testing(fixed values)
    !m=1 ;Fx=0; Fy=0.1; Fz=0
    !k=600; dt=0.2 
    X0=0; Y0=0; Z0=0; Fx0=0; Fy0=0; Fz0=0
  !math
   
    x=0
    y=0
    z=0
    Vx=Fx0
    Vy=Fy0
    Vz=Fz0

   DO k=1,nk 
    x= x+(dt*Vx)+(dt**2*(Fx/2*m))
    y= y+(dt*Vy)+(dt**2*(Fy/2*m))
    z= z+(dt*Vz)+(dt**2*(Fz/2*m))
  
      !could be inproved for diffferent forces Fxnext=Fx,,,,(Fx*2)=> (Fx+Fxnext)  
    Vx=Vx+((dt/2*m)*(Fx*2))
    Vy=Vy+((dt/2*m)*(Fy*2))
    Vz=Vz+((dt/2*m)*(Fz*2))
   

    Print*,"pos", x,y,z
    Print*,"speed", Vx,Vy,Vz

   ENDDO 



END PROGRAM
