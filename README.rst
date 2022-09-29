====
Features (based on Ronny Meier's cesm_bmst_bgc)
====

CTSM checkout of branch release-clm5.0

Additional features:
- Separate soil columns for natural vegetation PFTs (SeSC)
Namelist switch: use_individual_pft_soil_column = .true.

- Biomass heat storage (bhs / bmhs / bmst), tested together with friction velocity
Namelist switch bhs: use_biomass_heat_storage = .true.
Namelist switch friction velocity: zetamaxstable = 100.0d00

- Prescription of surface roughness (z0_presc)
Added src/biogeophys/PrescribeZ0Mod.F90

Not included in this code:
- Modified parameterisation of surface roughness (z0_presc)
Source code changes
Namelist switch planned: z0param_method = 'Meier2022'
paramfile = '/project/s824/ronmeier/cesm_inputdata/lnd/clm2/paramdata/ctsm51_params_newz0.c210208.nc'
paramfile = '/project/sm61/ronmeier/cesm_inputdata/lnd/clm2/paramdata/ctsm51_params_newz0.c211112.nc'
paramfile ='/glade/p/cesm/lmwg_dev/oleson/Z0_RonnieMeier/ctsm51_params_newz0.c211112.nc'

====
CTSM
====

The Community Terrestrial Systems Model.

This includes the Community Land Model (CLM5.0 and CLM4.5) of the Community Earth System Model.

For documentation, quick start, diagnostics, model output and
references, see

http://www.cesm.ucar.edu/models/cesm2.0/land/

and

https://escomp.github.io/ctsm-docs/

For help with how to work with CTSM in git, see

https://github.com/ESCOMP/ctsm/wiki/Getting-started-with-CTSM-in-git

and

https://github.com/ESCOMP/ctsm/wiki/Recommended-git-setup

To get updates on CTSM tags and important notes on CTSM developments
join our low traffic email list:

https://groups.google.com/a/ucar.edu/forum/#!forum/ctsm-dev

(Send email to ctsm-software@ucar.edu if you have problems with any of this)
