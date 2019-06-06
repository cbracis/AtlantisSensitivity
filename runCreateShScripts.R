# generate run .sh files
source("utilityFuncs.R")

create_run_sh_scripts = function(startIdx, stopIdx, output_directory, file_base_name, user = "cbracis")
{
  for (id in startIdx:stopIdx)
  {
    # sh file name/path
    file_name = paste0( file_base_name, id, ".sh" )
    file_path = file.path(output_directory, file_name)
    
    # status file to write to, different than output where all redirected stdout and stderr go, not in subdirectory
    status_file = paste0(">> /home1/scratch/", user, "/output/status_sim", id, ".txt")
    
    text = NULL
    text[1] = "#!/bin/bash"
    text[2] = "cd simu"
    
    # before redirecting output, echo simulation number to correlate pbs output files with sims
    text[3] = paste0("echo \"running ", file_name, " from job $PBS_ARRAY_INDEX\"", status_file)
    
    # need to pipe Atlantis stdout and stderr to a file or else it is too big for the virtual nodes
    # make sure to create the directory first, this will not fail even if it already exists
    # careful, md is an alias on mkdir when I connect to datarmor, but doesn't work in the job
    text[4] = paste0("mkdir -p /home1/scratch/", user, "/output/sim", id)
    text[5] = paste0("exec 1>>/home1/scratch/", user, "/output/sim", id, "/AEEC_SA_sim", id, "_output.txt")
    text[6] = "exec 2>&1"
    
    # now call Atlantis
    text[7] = paste0("atlantisMerged -i AEEC35_final_ini.nc 0 -o AEEC_SA_sim", id, 
                     ".nc -r AEEC_run.prm -f AEEC_force_fish.prm -p AEEC_physics.prm -b AEEC_biol", id,
                     ".prm -s SETasGroups.csv -h AEEC_harvest_F.prm -q SETasFisheries.csv -d /home1/scratch/", user, "/output/sim", id)
    text[8] = paste0("echo \"Atlantis finished\"", status_file)
    
    # post-processing and file copying (we know it's finished when it arrives in output_done)
    text[9] = paste0("Rscript ~/runProcessOutput.R ", id)
    text[10] = paste0("echo \"R script finished\"", status_file)

    text[11] = paste0("flip -m /home1/scratch/", user, "/output_done/*.txt")
    text[12] = paste0("flip -m /home1/scratch/", user, "/output_done/*.csv")
    text[13] = paste0("echo \"flip finished\"", status_file)
    
    text[14] = paste0("cp `ls /home1/scratch/", user, "/output/sim", id, "/AEEC_SA_sim", id,
                     "*` -t /home1/scratch/", user, "/output_done/")
    text[15] = paste0("echo \"copy finished\"", status_file)
    text[16] = paste0("rm -r /home1/scratch/", user, "/output/sim", id)
    text[17] = paste0("echo \"job complete\"", status_file)
    
    writeLines(text = text, con = file_path, sep = '\n')
  }
}

create_run_sh_scripts(startIdx = 10001, stopIdx = 10273, user = "cbracis",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

create_run_sh_scripts(startIdx = 10274, stopIdx = 10546, user = "rgirardi",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

create_run_sh_scripts(startIdx = 10547, stopIdx = 12548, user = "cbracis",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

create_run_sh_scripts(startIdx = 12549, stopIdx = 14550, user = "rgirardi",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)




create_run_sh_scripts(startIdx = 10001, stopIdx = 14550, 
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

