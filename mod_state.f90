!**************************************************
 module m_state
!**************************************************
  use m_common
  use m_step
  use m_angle
  use m_tools
  
  implicit none
  
  type t_state
     integer,private :: stateNr  ! numéro state
     integer,private, dimension(2) :: TransitionsNr
     double precision, dimension(2) ::  beta0, beta1
     double precision, dimension(3) :: TransitionLikelihoods
     double precision, dimension(3) :: SumLikelihoods
     !  TransitionLikelihoods(1) contains probabilty to go to same state
     !  TransitionLikelihoods(2) contains probabilty to go to state TransitionsNr(1)
     !  TransitionLikelihoods(3) contains probabilty to go to state  TransitionsNr(2)
     
     type(t_step) :: step
     type(t_angle) :: angle

     
   contains
   
    procedure,pass(self),public :: init => init_state
    procedure,pass(self),public :: toString => display_state 
    procedure,pass(self),public :: get_NewState
    
    
    
  end type t_state

  type(t_state),dimension(3),save :: state

CONTAINS

!****************************************************************************  
 subroutine Create_States()
      integer :: i, nbrVars, nbrStates
      nbrVars = size(prm_d_stateVars)
      nbrStates = size(state)
    !  write (*, '(I3, 1X, F13.9)') (i, prm_d_stateVars(i), i=1, nbrVars )
      do i = 1, nbrStates
        !! prm_d_stateVars come from Parameters.dat and are read in mod_parameters.f90
       call state(i)%init(i,prm_d_stateVars(i:nbrVars:nbrStates),prm_d_stateVars(14+(2*i):nbrVars))
      end do
 end subroutine Create_States
!**************************************************************************    

!===================================================================================
 subroutine init_state (self, nr, prms, prmsbeta)
     ! subroutine init_state (state, nr, mean1,sd, zero_mass,mean2, concentration, &
         ! beta0, beta1,beta2,stepPrecision,maxLength)
!--------------------------------------------------------------------------------
      class (t_state), intent(out) :: self
      integer, intent(in) :: nr
      double precision, dimension(:), intent(in) :: prms, prmsbeta
      integer :: i
      character(160) :: str
      double precision, dimension(2) :: tempvar
      double precision :: quotient
      self%stateNr = nr  ! numéro state = nr

      
      select case (nr) 
           case (1)          
             self%TransitionsNr(1) = 2
             self%TransitionsNr(2) = 3
      
           case (2)           
             self%TransitionsNr(1) = 1
             self%TransitionsNr(2) = 3 
             
           case (3)           
             self%TransitionsNr(1) = 1
             self%TransitionsNr(2) = 2               
           case default          
               WRITE(str, '(A,I2,A)')  "Dans mod_state.f90, l'état de transition ", nr, " n'a pas été programmé."
             call fatal_error(str)
         return         
      end select 
      
           
      self%beta0(1) = prmsbeta(1)
      self%beta0(2) = prmsbeta(2)
      self%beta1(1) = prmsbeta(7)
      self%beta1(2) = prmsbeta(8)
      
      tempvar = exp(self%beta0 + (self%beta1 * FAI_DEF))  ! multiplication du tableau
      quotient = 1.0d0 + sum(tempvar) 

     !  TransitionLikelihoods(1) contient la proba d'aller vers le même état
     !  TransitionLikelihoods(2) contient la proba d'aller vers l'état TransitionsNr(1)
     !  TransitionLikelihoods(3) contient la proba d'aller vers l'état TransitionsNr(2)    
      
      ! Probabilité de rester dans le même état 1->1 ou 2->2 ou 3->3
      self%TransitionLikelihoods(1)= 1.0d0/ quotient 
      
      ! Probabilité de passer (1->2 et 1->3) ou (2->1 et 2->3) ou (3->1 et 3->2)
      self%TransitionLikelihoods(2:3) = tempvar / quotient ! division de tableau
      
      self%SumLikelihoods(1) = self%TransitionLikelihoods(1)
      self%SumLikelihoods(2) = sum(self%TransitionLikelihoods(1:2))
      self%SumLikelihoods(3) = sum(self%TransitionLikelihoods) !! normalement = 1
      
      call self%step%init(nr, prms(1),prms(2),prms(3))
      call self%angle%init(nr, prms(4),prms(5))
  
 
      
      
end subroutine init_state


!===================================================================================
integer function get_NewState(self)
!--------------------------------------------------------------------------------
      class (t_state), intent(in) :: self
      
      double precision :: rndNumber
     
      call random_number(rndNumber)
      
      if(rndNumber <= self%SumLikelihoods(1)) then
         get_NewState = self%stateNr !! On reste dans le même état
      else if (rndNumber <= self%SumLikelihoods(2)) then
         get_NewState = self%TransitionsNr(1) ! 1->2 ou 2->1 ou 3->1
      else  
         get_NewState = self%TransitionsNr(2) ! 1->3 ou 2->3 ou 3->2     
      end if   
     ! Pour test
     ! write(*,'(A,I3,A,F13.9,A,I3)') "Etat:",self%stateNr," Random:", rndNumber, " New state:" ,get_NewState     
      
 end function get_NewState  

 
!===================================================================================
subroutine display_state(self)
!--------------------------------------------------------------------------------
      class (t_state), intent(in) :: self
      character(1), parameter :: beta = achar(128)
      character(2) :: indice
      integer :: i
      
      write (*,'(A,I3,A)') '*** STATE', self%stateNr,' ***'
      call self%step%toString()
      call self%angle%toString()
      write(*,'(15X,2(11X,I1,A,I1))') (self%stateNr, "->", self%TransitionsNr(i), i=1, 2)
      write (*,'(A,2(E17.9))')  "Intercept(beta0) :",(self%beta0(i),i=1,2)      
      write (*,'(A,2(E17.9))')  "FAI_DEF(beta1)   :",(self%beta1(i),i=1,2) 
      write (*,'(A)') 
      write(*,'(11X,3(11X,I1,A,I1))') self%stateNr, "->",self%stateNr,(self%stateNr, "->", self%TransitionsNr(i), i=1, 2)
      write (*,'(A,3(E17.9))')  "probabilities:",(self%TransitionLikelihoods(i),i=1,3) 
      write (*,'(A,3(E17.9))')  "sum probab.  :",(self%SumLikelihoods(i),i=1,3) 
      write (*,'(A)') '----------------'

 end subroutine display_state  
  


!******************************************************************
 end module m_state
!****************************************************************