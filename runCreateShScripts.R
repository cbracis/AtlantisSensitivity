# generate run .sh files
source("utilityFuncs.R")

create_run_sh_scripts = function(id_indices, output_directory, file_base_name, user = "cbracis")
{
  for (id in id_indices)
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

    text[11] = paste0("cp `ls /home1/scratch/", user, "/output/sim", id, "/AEEC_SA_sim", id,
                     "*` -t /home1/scratch/", user, "/output_done/")
    text[12] = paste0("echo \"copy finished\"", status_file)
    text[13] = paste0("rm -r /home1/scratch/", user, "/output/sim", id)
    text[14] = paste0("echo \"job complete\"", status_file)
    
    writeLines(text = text, con = file_path, sep = '\n')
  }
}

create_run_sh_scripts(id_indices = 10001:10273, user = "cbracis",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

create_run_sh_scripts(id_indices = 10274:10546, user = "rgirardi",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

create_run_sh_scripts(id_indices = 10547:12548, user = "cbracis",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

create_run_sh_scripts(id_indices = 12549:14550, user = "rgirardi",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)


# create scripts just for those that need to be re-run
# these indices come frm runConsolidateSimulationOutput.R
indices = c(10864, 10871, 10880, 11393, 11808, 11809, 11812, 12128, 10849, 10878, 10879, 10881, 
            10882, 10883, 10900, 11138, 11140, 11141, 11144, 11145, 11148, 11149, 11151, 11152, 
            11153, 11154, 11155, 11156, 11157, 11158, 11161, 11162, 11163, 11165, 11166, 11167, 
            11168, 11169, 11170, 11172, 11173, 11176, 11178, 11180, 11181, 11182, 11184, 11187, 
            11188, 11189, 11190, 11191, 11193, 11194, 11196, 11197, 11198, 11199, 11202, 11203, 
            11204, 11206, 11207, 11209, 11210, 11211, 11213, 11218, 11221, 11223, 11224, 11225, 
            11226, 11227, 11228, 11229, 11230, 11232, 11233, 11234, 11238, 11239, 11241, 11243, 
            11244, 11245, 11246, 11247, 11248, 11249, 11250, 11251, 11252, 11253, 11254, 11255, 
            11256, 11257, 11258, 11259, 11260, 11261, 11262, 11263, 11264, 11265, 11266, 11267, 
            11268, 11269, 11270, 11271, 11272, 11273, 11275, 11276, 11277, 11278, 11279, 11280, 
            11281, 11282, 11283, 11284, 11285, 11286, 11287, 11288, 11289, 11290, 11291, 11292, 
            11293, 11294, 11295, 11296, 11297, 11298, 11299, 11300, 11301, 11302, 11303, 11304, 
            11305, 11306, 11307, 11308, 11309, 11310, 11311, 11312, 11313, 11314, 11315, 11316, 
            11317, 11318, 11319, 11320, 11321, 11322, 11323, 11324, 11325, 11326, 11327, 11328, 
            11329, 11330, 11331, 11332, 11333, 11334, 11335, 11336, 11337, 11338, 11339, 11340, 
            11341, 11342, 11343, 11344, 11345, 11346, 11347, 11348, 11349, 11350, 11351, 11352, 
            11353, 11354, 11355, 11356, 11357, 11358, 11359, 11360, 11361, 11362, 11363, 11364, 
            11365, 11366, 11367, 11368, 11369, 11370, 11371, 11372, 11373, 11374, 11375, 11376, 
            11377, 11378, 11379, 11380, 11381, 11382, 11383, 11384, 11385, 11386, 11387, 11388, 
            11389, 11390, 11391, 11392, 11394, 11395, 11396, 11397, 11398, 11399, 11400, 11401, 
            11402, 11403, 11404, 11405, 11406, 11407, 11408, 11409, 11410, 11411, 11412, 11413, 
            11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421, 11422, 11423, 11424, 11425, 
            11426, 11427, 11428, 11429, 11430, 11431, 11432, 11433, 11434, 11435, 11436, 11437, 
            11438, 11439, 11440, 11441, 11442, 11443, 11444, 11445, 11446, 11447, 11448, 11449, 
            11450, 11451, 11452, 11453, 11454, 11455, 11456, 11457, 11458, 11459, 11460, 11461, 
            11462, 11463, 11464, 11465, 11466)

create_run_sh_scripts(id_indices = indices, user = "cbracis",
                      output_directory = OUTPUT_DIR, file_base_name = SHELL_SCRIPT_BASE_NAME)

