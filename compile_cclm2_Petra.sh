#! /bin/bash

# Script to compile CLM with case-specific settings
# For standalone CLM or coupling with COSMO
# Domain can be EURO-CORDEX or global

set -e # failing commands will cause the shell script to exit

#==========================================
# Case settings
#==========================================

echo "*** Setting up case ***"

date=`date +'%Y%m%d-%H%M'` # get current date and time
startdate=`date +'%Y-%m-%d %H:%M:%S'`

COMPSET=I2000Clm50Sp # for CCLM2
RES=hcru_hcru # for CCLM2
DOMAIN=eur # EURO-CORDEX for CCLM2, glob otherwise
CODE=clm5.0_features # clm5.0_features for Ronny's version, CTSMdev for latest 
COMPILER=gnu # setting to gnu-oasis will: (1) use different compiler config, (2) copy oasis source code to CASEDIR
DRIVER=mct # using nuopc requires ESMF installation
EXP=cclm2_test_${date} # case name
CASENAME=$CODE.$COMPILER.$COMPSET.$RES.$DOMAIN.$EXP
MACH=daint
QUEUE=normal # USER_REQUESTED_QUEUE, overrides default JOB_QUEUE
WALLTIME="1:30:00" # USER_REQUESTED_WALLTIME, overrides default JOB_WALLCLOCK_TIME
PROJECT=sm61
NTASKS=24
NSUBMIT=0 # partition into smaller chunks, excludes the first submission
let "NCORES = $NTASKS * 12"
STARTDATE="2004-01-01"
NYEARS=1

# Set directories
export CCLM2ROOT=$PWD # code base directory on project where this script is located
export CASEDIR=$SCRATCH/CCLM2_cases/$CASENAME # case directory on scratch
export CESMDATAROOT=/project/sm61/shared # downloaded inputdata to reuse, includes preprocessed EURO-CORDEX files

# Log output (use "tee" to send output to both screen and $outfile)
logfile=$SCRATCH/CCLM2_logs/${CASENAME}_mylogfile.log
mkdir -p "$(dirname "$logfile")" && touch "$logfile" # create parent/child directories and logfile
cp $CCLM2ROOT/$BASH_SOURCE $SCRATCH/CCLM2_logs/${CASENAME}_myjobscipt.sh # copy this script to logs
print_log() {
    output="$1"
    echo "${output}" | tee -a $logfile
}

print_log "*** Case at: ${CASEDIR} ***"
print_log "*** Case settings: compset ${COMPSET}, resolution ${RES}, domain ${DOMAIN}, compiler ${COMPILER} ***"
print_log "*** Logfile at: ${logfile} ***"


#==========================================
# Load modules
#==========================================
# can also be done through $USER/.cime/config_machines.xml

print_log "*** Loading modules ***"

module load daint-gpu # use gpu although CLM will run on cpus

if [ $COMPILER = nvhpc ] ; then
    module switch PrgEnv-cray PrgEnv-nvidia
    module load cray-netcdf-hdf5parallel
    module load cray-hdf5-parallel
    module load cray-parallel-netcdf
    spack load netlib-lapack%nvhpc # provides lapack and blas, load if config_compilers has -llapack -lblas
    #module load cray-libsci # provides lapack and blas, load if config_compilers has -llibsci_gnu (does not work with nvhpc)
fi

if [ $COMPILER = gnu ] ; then
    module switch PrgEnv-cray PrgEnv-gnu
    module switch gcc gcc/9.3.0 # the default version gives an error when building gptl
    module load cray-netcdf-hdf5parallel
    module load cray-hdf5-parallel
    module load cray-parallel-netcdf
    spack load oasis%gcc
fi

#alias python=python2.7 # currently in .bashrc, but does not work for cray PATH

module list | tee -a $logfile
print_log $LD_LIBRARY_PATH


#==========================================
# Create case
#==========================================

print_log "*** Creating CASE: ${CASENAME} ***"

cd $CCLM2ROOT/cime/scripts
./create_newcase --case $CASEDIR --compset $COMPSET --res $RES --mach $MACH --compiler $COMPILER --driver $DRIVER --project $PROJECT --run-unsupported | tee -a $logfile


#==========================================
# Modify namelists
#==========================================

print_log "*** Modifying env_*.xml  ***"
cd $CASEDIR

# Set directory structure
./xmlchange RUNDIR="$CASEDIR/run" # by defaut, RUNDIR is $SCRATCH/$CASENAME/run
./xmlchange EXEROOT="$CASEDIR/bld"

# Change job settings (env_batch.xml or env_workflow.xml). Do this here to change for both case.run and case.st_archive
./xmlchange JOB_QUEUE=$QUEUE --force
./xmlchange JOB_WALLCLOCK_TIME=$WALLTIME

# Set run start/stop options and DATM forcing (env_run.xml)
./xmlchange RUN_TYPE=startup
./xmlchange RESUBMIT=$NSUBMIT
./xmlchange RUN_STARTDATE=$STARTDATE
./xmlchange STOP_OPTION=nyears,STOP_N=$NYEARS
./xmlchange NCPL_BASE_PERIOD="day",ATM_NCPL=48 # coupling freq default 30min = day,48
if [ $DRIVER = mct ] ; then
    ./xmlchange DATM_CLMNCEP_YR_START=2004,DATM_CLMNCEP_YR_END=2004,DATM_CLMNCEP_YR_ALIGN=2004 # in clm5.0 with mct
fi
if [ $DRIVER = nuopc ] ; then
    ./xmlchange DATM_YR_START=2004,DATM_YR_END=2004,DATM_YR_ALIGN=2004 # new variable names in CTSMdev; probably only with nuopc driver
fi

# Set the number of cores and nodes (env_mach_pes.xml)
./xmlchange COST_PES=$NCORES
./xmlchange NTASKS_CPL=-$NTASKS
./xmlchange NTASKS_ATM=-$NTASKS
./xmlchange NTASKS_OCN=-$NTASKS
./xmlchange NTASKS_WAV=-$NTASKS
./xmlchange NTASKS_GLC=-$NTASKS
./xmlchange NTASKS_ICE=-$NTASKS
./xmlchange NTASKS_ROF=-$NTASKS
./xmlchange NTASKS_LND=-$NTASKS 

# If parallel netcdf is used
./xmlchange PIO_VERSION="2" # 1 is default in clm5.0, 2 is default in CTSMdev

# Activate debug mode (env_build.xml)
#./xmlchange DEBUG=TRUE
#./xmlchange INFO_DBUG=2 # Change amount of output

# Additional options
#./xmlchange CLM_BLDNML_OPTS="-irrig .true." -append # switch on irrigation
#./xmlchange RTM_MODE="NULL" # switch off river routing
#./xmlchange CCSM_BGC=CO2A,CLM_CO2_TYPE=diagnostic,DATM_CO2_TSERIES=20tr # set transient CO2

#./xmlchange CLM_NAMELIST_OPTS="use_init_interp=.false. # Ronny sets interp to false, not sure about this

# Domain and mapping files for limited spatial extent (copy from $CESMDATAROOT to scratch for access at runtime?)
if [ $DOMAIN == eur ]; then
    ./xmlchange LND_DOMAIN_PATH="$CESMDATAROOT/CCLM2_EUR_inputdata/domain"
    ./xmlchange LND_DOMAIN_FILE="domain.lnd.360x720_cruncep.100429.nc"
    ./xmlchange LND2ROF_FMAPNAME="$CESMDATAROOT/CCLM2_EUR_inputdata/mapping/map_360x720_nomask_to_0.5x0.5_nomask_aave_da_c130103.nc"
    ./xmlchange ROF2LND_FMAPNAME="$CESMDATAROOT/CCLM2_EUR_inputdata/mapping/map_0.5x0.5_nomask_to_360x720_nomask_aave_da_c120830.nc"
    ./xmlchange LND2GLC_FMAPNAME="$CESMDATAROOT/CCLM2_EUR_inputdata/mapping/map_360x720_TO_gland4km_aave.170429.nc"
    ./xmlchange LND2GLC_SMAPNAME="$CESMDATAROOT/CCLM2_EUR_inputdata/mapping/map_360x720_TO_gland4km_aave.170429.nc"
    ./xmlchange GLC2LND_FMAPNAME="$CESMDATAROOT/CCLM2_EUR_inputdata/mapping/map_gland4km_TO_360x720_aave.170429.nc"
    ./xmlchange GLC2LND_SMAPNAME="$CESMDATAROOT/CCLM2_EUR_inputdata/mapping/map_gland4km_TO_360x720_aave.170429.nc"
fi

# ESMF interface and time manager (env_build.xml)
#./xmlchange -file env_build.xml -id COMP_INTERFACE -val "mct" # mct is default in clm5.0, nuopc is default in CTSMdev (requires ESMF installation); adding --driver mct to create_newcase creates the case with everything needed
#./xmlchange -file env_build.xml -id USE_ESMF_LIB -val "FALSE" # FALSE is default in clm5.0; since cesm1_2 ESMF is no longer necessary to run with calendar=gregorian
#./xmlchange -file env_build.xml -id ESMF_LIBDIR -val ".../lib/libO/Linux.pgi.64.mpiuni.default" # path to ESMF library; can be set in config_machines.xml


#==========================================
# Modify user namelists
#==========================================
print_log "*** Modifying unser_nl_*.xml  ***"

# Output frequency and averaging (example)
# hist_empty_htapes = .true. # turn off all default output on h0
# hist_fincl1 or hist_fexcl1 # include or exclude selected variables
# hist_nhtfrq # output frequency
# hist_mfilt # number of values per file
# hist_avgflag_pertape # averaging over the output interval
# hist_dov2xy # true for 2D, false for 1D vector
# hist_type1d_pertape # '' for 2D and no averaging (i.e. PFT output), 'COL' for columns, 'LAND' for land-units, 'GRID' for grid-cells

# Commented out during testing to avoid lots of output
: '
cat > user_nl_clm << EOF
hist_fincl1 = 'TG', 'QAF', 'TAF', 'UAF'
hist_fincl2 = 'QAF', 'TAF', 'UAF', 'VPD_CAN', 'TLAI', 'FCEV', 'FCTR', 'TG', 'TSOI', 'TSOI_10CM', 'TSA', 'Q2M', 'VPD'
hist_fincl3 = 'QAF', 'TAF', 'UAF', 'VPD_CAN', 'TLAI', 'FCEV', 'FCTR', 'TG', 'TSOI', 'TSOI_10CM', 'TSA', 'Q2M', 'VPD'
hist_fincl4 = 'QAF', 'TAF', 'UAF', 'VPD_CAN', 'TLAI', 'FCEV', 'FCTR', 'TG', 'TSOI', 'TSOI_10CM', 'TSA', 'Q2M', 'VPD'

hist_nhtfrq = 0, -24, -24, -6 
hist_mfilt  = 12, 365, 365, 4 
hist_avgflag_pertape = 'A','M','X','I' 
hist_dov2xy = .true.,.true.,.true.,.false. 
hist_type1d_pertape = '','','',''
EOF

print_log "*** Output frequency and averaging  ***"
print_log "h0: default + selected variables, monthly values (0), yearly file (12 vals per file), average over the output interval (A)"
print_log "h1: selected variables, daily values (-24), yearly file (365 vals per file), min over the output interval (M)"
print_log "h2: selected variables, daily values (-24), yearly file (365 vals per file), max over the output interval (X)"
print_log "h3: selected variables, 6-hourly values (-6), daily file (4 vals per file), instantaneous at the output interval (I) by PFT"
'

# EUR surfdata and params: can be exchanged for newer versions
if [ $DOMAIN == eur ]; then
cat > user_nl_clm << EOF
fsurdat = "$CESMDATAROOT/CCLM2_EUR_inputdata/surfdata/surfdata_360x720cru_16pfts_simyr2000_c170428.nc"
paramfile = "$CESMDATAROOT/CCLM2_EUR_inputdata/CLM5params/clm5_params.cpbiomass.c190103.nc"
EOF

cat > user_nl_datm << EOF
domainfile = "$CESMDATAROOT/CCLM2_EUR_inputdata/domain/domain.lnd.360x720_cruncep.100429.nc"
EOF
fi

# GLOB surfdata and params: default

# These namelist options are available in Ronny's code
if [ $CODE == clm5.0_features ]; then
cat > user_nl_clm << EOF
use_biomass_heat_storage = .true.
use_individual_pft_soil_column = .true.
zetamaxstable = 100.0d00
EOF
fi

# These namelist options are available in CTSMdev (?)
if [ $CODE == CTSMdev ]; then
cat > user_nl_clm << EOF
use_biomass_heat_storage = .true.
z0param_method = 'Meier2022'
zetamaxstable = 100.0d00
use_z0mg_2d = .true.
use_z0m_snowmelt = .true.
flanduse_timeseries=''
EOF
fi


#==========================================
# Set up the case
#==========================================

print_log "*** Running case.setup ***"
./case.setup -r | tee -a $logfile


#==========================================
# For OASIS coupling: before building, add the additional routines for OASIS interface in your CASEDIR on scratch
#==========================================

if [[ $COMPILER =~ "oasis" ]]; then
    print_log "*** Adding OASIS routines ***"
    ln -sf $CCLM2ROOT/cesm2_oas/src/oas/* SourceMods/src.drv/
    rm  SourceMods/src.drv/oas_clm_vardef.F90
    ln -sf $CCLM2ROOT/cesm2_oas/src/drv/* SourceMods/src.drv/
    ln -sf $CCLM2ROOT/cesm2_oas/src/oas/oas_clm_vardef.F90 SourceMods/src.share/
    ln -sf $CCLM2ROOT/cesm2_oas/src/datm/* SourceMods/src.datm/
fi


#==========================================
# Build
#==========================================

print_log "*** Building case ***"
./case.build --clean-all | tee -a $logfile

if [ $CODE == clm5.0_features ]; then
    ./case.build --skip-provenance-check | tee -a $logfile # needed with Ronny's old code base
else
    ./case.build | tee -a $logfile
fi

print_log "*** Finished building new case in ${CASEDIR} ***"


#==========================================
# Preview and submit job
#==========================================

print_log "*** Preview the run ***"
./preview_run | tee -a $logfile

print_log "*** Submitting job ***"

if [ $CODE == clm5.0_features ]; then
    ./case.submit -a "-C gpu" --skip-provenance-check | tee -a $logfile # needed with Ronny's old code base
else
    ./case.submit -a "-C gpu" | tee -a $logfile
fi

# fails for clm5.0_features because tasks-per-node evaluates to float (12.0) with python 3. Cannot find where the calculation is made. Can also not override it like this:
#./case.submit -a "-C gpu -p normal --ntasks-per-node 12" # should be set in env_batch.xml

squeue --user=psieber | tee -a $logfile
#less CaseStatus

enddate=`date +'%Y-%m-%d %H:%M:%S'`
duration=$SECONDS
print_log "Started at: $startdate"
print_log "Finished at: $enddate"
print_log "Duration: $(($duration / 60)) min $(($duration % 60)) sec"


#==========================================
# Copy logfiles to some permanent directory together with output
#==========================================

# Notes:
#env_case = model version, components, resolution, machine, compiler [do not modify]
#env_mach_pes = NTASKS, number of MPI tasks (or nodes if neg. values) [modify before setup]
#env_mach_specific = controls machine specific environment [modify before setup]
#env_build = component settings [modify before build]
#env_batch = batch job settings [modify any time]
#env_run = run settings incl runtype, coupling, pyhsics/sp/bgc and output [modify any time]
#env_workflow = walltime, queue, project