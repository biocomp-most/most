module m_groups
!******************************************
  use m_common
  use m_week
  use m_point
  use m_path
  use m_defecation
  
  implicit none
  
  type(t_week),dimension(:),allocatable, private :: weeks
   
   CONTAINS
 
 
!******************************************************************
  subroutine create_groups()
!-----------------------------------------------------  
   integer weekcounter,ierr,i
   character(150) :: outfile
   integer, parameter ::   generated_grps_file = 1, grp_summary_file = 2, &
           rejected_grps_file = 3, rejected_grp_summary_file = 4, DefecationPoints_File = 5, &
           SpittedPoints_File = 6
    character(20), dimension(2) :: strIdx
    

     allocate(weeks(1:prm_i_NumberOfWeeks), stat=ierr)
      if (ierr > 0) then
         call fatal_error('Insufficient memory to be allocated. Decrease the number of weeks to be generated')
      end if 
      



      ! generated_grps_file = 1
      ! grp_summary_file = 2
      ! rejected_grps_file = 3
      ! rejected_grp_summary_file = 4
      
       outfile = create_filepath(prm_c_output_dir, file_name(GeneratedWeeksXYFile))
      open(generated_grps_file,file=outfile,status='replace',action='write')
      write(generated_grps_file,'(A)') trim(display_header())
      
      outfile = create_filepath(prm_c_output_dir, file_name(Week_summary_File))
      open(grp_summary_file,file=outfile,status='replace',action='write')
      write(grp_summary_file,'(A)') trim(display_path_header())
      
      outfile = create_filepath(prm_c_output_dir, file_name(rejected_Weeks_File))
      open(rejected_grps_file,file=outfile,status='replace',action='write')
      write(rejected_grps_file,'(3A)') trim(display_header()),tab,'Error_message'
      
      outfile = create_filepath(prm_c_output_dir, file_name(rejected_Weeks_summary_File))
      open(rejected_grp_summary_file,file=outfile,status='replace',action='write')
      write(rejected_grp_summary_file,'(3A)') trim(display_path_header()),tab,'Error_message'
      
      outfile = create_filepath(prm_c_output_dir, file_name(DefecationPointsFile))
      open(DefecationPoints_File,file=outfile,status='replace',action='write')
      write(DefecationPoints_File,'(A)') trim(display_defecation_header(.TRUE.))
      
      outfile = create_filepath(prm_c_output_dir, file_name(SpittedPointsFile))
      open(SpittedPoints_File,file=outfile,status='replace',action='write')
      write(SpittedPoints_File,'(A)') trim(display_defecation_header(.FALSE.))
      
   do weekcounter = 1, prm_i_NumberOfWeeks
     call weeks(weekcounter)%init(weekcounter)

      if (MODULO(weekcounter,10) == 0) then
           write(strIdx(1),*) weekcounter
           strIdx(2) = "calculated weeks."
           write (*,'(A,X,A)') (trim(adjustl(strIdx(i))),i=1,2)
      end if
   
   end do
   
      close(rejected_grp_summary_file)
      close(rejected_grps_file)
      close(grp_summary_file)
      close(generated_grps_file)
      close(DefecationPoints_File)
      close(SpittedPoints_File)      
      
  end subroutine create_groups

!*********************************************    
 subroutine deallocate_groups()
      deallocate (weeks)
      
 end subroutine deallocate_groups  
!*********************************************  


end module m_groups
