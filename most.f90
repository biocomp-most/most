program most
  use m_common
  use m_parameters
  use m_geo 
  use m_nest
  use m_faidef
  use m_daytime
  use m_tools
  use m_point
  use m_angle
  use m_transit
  use m_state
  use m_trees
  use m_groups
  
  implicit none
  character(100) :: parameter_file  
  character(5) :: prm_month
  integer :: num_args, i,j, nbr, int_month, err
 !  type(t_path) :: path

!-------------------------------------------------------
! Parameter 1 - mandatory : name of parameter to read
! Parameter 2 - optional : month to calculate
!               if argument 2 is missing, th month to calculate is read from parameter file
!-------------------------------------------------------
  num_args = command_argument_count()
  if (num_args > 0) then
    CALL get_command_argument(1, parameter_file)
  else 
     write(*,*) 'Enter name of parameter file :'
     read(*,'(A)') parameter_file
  END IF
  
  int_month = 0
  if (num_args > 1) then
     CALL get_command_argument(2, prm_month)
     read(prm_month,*,iostat=err)  int_month
     if(err /= 0) then
        CALL fatal_error("Month must be in range 1 :: 12")   
     end if
     if(int_month < 1 .OR. int_month > 12) then
        CALL fatal_error("Month must be in range 1 :: 12")  
     end if
     write (*,'(A,I3)') "Month to calculate : ", int_month
  end if
  
  call read_parameters(parameter_file)
  if(int_month > 0) then
     prm_i_month = int_month
  end if
   

  
 !----------------------------------
 ! Read input data and allocate memory
 !----------------------------------
  call random_seed() ! init random generator
  call create_polygons() ! home-range and NoGo
  call create_nests() ! reads nets locations (sleeping sites)

  call create_faidef() 
  call create_states() ! create three states
  call create_daytimes() ! bed wakeup times
  
  call create_trees() ! reads trees locations
  call create_groups() ! create calculation groups


 !----------------------------------
 ! modules checks (debugging only)
 !----------------------------------
  if (DspParameters) then;  call print_Parameters(); end if
  if (DspStates) THEN
    do i = 1, 3
          call state(i)%toString()
    end do
  end if
  
!---------------------------------------
! Routines for debugging only
! Dsp.... variables are set in parameters.dat file 
  if (DspDaytimes) THEN;  call display_daytime(); end if
  if (DspFruits) THEN;  call print_trees(.TRUE.); END IF
  if (DspTestRoutines) THEN;  call  TestCalculsPoints(); END IF 
  if (DspSleepingSites) THEN; call print_nests(); END IF
  if (DspFaiDef) then; call print_faidef(); END IF
  
  
   
 !----------------------------------  
 ! Deallocate data
 !----------------------------------  

  call deallocate_trees()
  
  call deallocate_nests()
  call deallocate_polygons()
  call deallocate_groups()


end program most
