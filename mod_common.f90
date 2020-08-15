!**************************************************
module m_common
!**************************************************
public 
!! Constant values  
  integer, parameter :: kCoord = SELECTED_INT_KIND(8) 
  integer, parameter :: kBIGINT = SELECTED_INT_KIND(16)
  integer, parameter :: kSMALLINT = SELECTED_INT_KIND(4)
  integer, parameter :: kTINYINT = SELECTED_INT_KIND(2)
  integer, parameter :: kBYTE = SELECTED_INT_KIND(1)
  
  double precision, parameter :: M_PI = 3.14159265358979323846D0
  real, parameter :: real_PI = 3.1415927
  real, parameter :: angle45 = real_PI / 4.0
  real, parameter :: angle90 = real_PI / 2.0
  real, parameter :: angle360 = real_PI * 2.0
  
  character(1), parameter :: tab = achar(9)


!! Parameter values    
  character(20),save :: prm_c_input_dir
  character(20),save :: prm_c_output_dir
  
  character(40),save :: prm_c_NoGoFile
  character(40),save :: prm_c_SleepingSitesFile
  character(40),dimension(3),save :: prm_c_FruitTreesFilePeriodFiles
  character(40),save :: prm_c_faidefFile
  character(40),save :: prm_c_BedWakeupTimeFile
  character(40),dimension(12),save :: prm_c_MCPFile  ! HR polygons 12 months
  
  double precision, dimension(27),save :: prm_d_stateVars
  
  integer, save :: prm_i_month, prm_i_NumberOfweeks, prm_i_maxweekRejected, prm_i_maxStepsTrials, &  
  prm_i_StateAtStart, prm_i_NumberOfDaysPerweek, prm_i_stepDuration, prm_i_feedingDuration, &
                 prm_i_NestRadius, prm_i_TreeRadius, prm_i_MaxNumberOfFeedingsADay, &
                 prm_i_MinutesBeforeEndOfDay, prm_i_gutTransitMean, prm_i_gutTransitSD, &
                 prm_i_CheekRetentionMean, prm_i_CheekRetentionSD
  
  double precision, save :: prm_d_gutTransitSDlimit, prm_d_CheekRetentionSDlimit
  real, save :: FAI_DEF  ! read in FAI_DEF.csv for the month of prm_i_month value


  logical,save :: DspParameters, DspFruits, DspStates, DspDaytimes, &
       DspSleepingSites, DspFaiDef, DspTestRoutines, &
       prm_l_dspDefecationDetails
  
  character(40),save :: DefecationPointsFile,SpittedPointsFile
  character(40),save :: GeneratedWeeksXYFile
  character(40),save :: week_summary_File
  character(40),save :: rejected_weeks_File
  character(40),save :: rejected_weeks_summary_File


        


end module m_common