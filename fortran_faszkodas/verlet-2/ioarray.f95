PROGRAM IOarray
        IMPLICIT NONE
  !variables
        INTEGER,PARAMETER :: wp = selected_real_kind (p=13,r=300)
        !REAL (KIND=wp):: m,Fx,Fy,Fz !mass(kg) Fxyz(kg m s-2) 
        INTEGER :: k,l,j,    nk,p  !docycle(s),    # of steps,# of particles 
        REAL (KIND=wp):: dt !(time of one step(s)) 
        !REAL (KIND=wp)::X0,Y0,Z0,Fx0,Fy0,Fz0,Fxnext !inital position and speed
        !REAL (KIND=wp):: x,y,z,Vx,Vy,Vz !current postion of body
       
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: par
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: Fnext 
         
   
     !for testing(fixed values)
     ! m=1 ;Fx=0.1; Fy=0; Fz=0; nk=600; dt=0.2; X0=0; Y0=0; Z0=0;
     ! Fx0=0; Fy0=0; Fz0=0
     !setting 00
     !x=0; y=0; z=0; Vx=0; Vy=0; Vz=0
     ! Vx=Fx0; Vy=Fy0; Vz=Fz0

        
  !input

 
        OPEN (unit=11, FILE="input.txt", STATUS="old", ACTION="read")
        READ (UNIT=11, FMT=*) nk, dt  
        READ (UNIT=11, FMT=*) p
     
        ALLOCATE (par(p,10))
        ALLOCATE (Fnext(p,3))

   DO l=1,p                                        !1,2,3,4,5 ,6 ,7 ,8 ,9 ,10
        READ (UNIT=11, FMT=*) par(l,:)       !l = (/m,x,y,z,Vx,Vy,Vz,Fx,Fy,Fz/)
    ENDDO

  CLOSE(11)
      
   !math  
   DO j=1,p
    DO k=1,nk                              
     par(j,2:4) = par(j,2:4)+(dt*par(j,5:7))+((dt**2.0)*(par(j,8:10)/(2.0*par(j,1))))
      
     Fnext(j,1:3) = par(j,8:10)
     par(j,5:7)=par(j,5:7)+((dt/2*par(j,1))*(par(j,8:10)*Fnext(j,1:3)))
     
 
    ENDDO 
   ENDDO  
     
  OPEN (unit=12, FILE="output.txt", STATUS="replace", ACTION="write")
    WRITE (UNIT=12,FMT=*) "m              | x            y               z            | Vx             Vy               Vz            | Fx             Fy           Fz"
     DO l=1,p  
      WRITE (UNIT=12, FMT=*) "  "
      WRITE (UNIT=12, FMT='(*(G12.4))')par(l,1),"|",par(l,2:4),"|",par(l,5:7),"|",par(l,8:10) 
     ENDDO
  CLOSE(12)    

  DEALLOCATE ( par, Fnext ) 
ENDPROGRAM
