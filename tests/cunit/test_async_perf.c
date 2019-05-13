/*
 * This program tests darrays with async.
 *
 * @author Ed Hartnett
 * @date 5/4/17
 */
#include <config.h>
#include <pio.h>
#include <pio_tests.h>
#include <pio_internal.h>

/* The number of tasks this test should run on. */
#define TARGET_NTASKS 4

/* The minimum number of tasks this test should run on. */
#define MIN_NTASKS 1

/* The name of this test. */
#define TEST_NAME "test_async_perf"

/* For 1-D use. */
#define NDIM1 1

/* For 2-D use. */
#define NDIM2 2

/* For 3-D use. */
#define NDIM3 3

/* For maplens of 2. */
#define MAPLEN2 2

/* Lengths of non-unlimited dimensions. */
#define LAT_LEN 2
#define LON_LEN 3

/* The length of our sample data along each dimension. */
#define X_DIM_LEN 128
#define Y_DIM_LEN 128
#define Z_DIM_LEN 16

/* The number of timesteps of data to write. */
#define NUM_TIMESTEPS 10

/* Number of vars in test file. */
#define NVAR 4

/* Number of records written for record var. */
#define NREC 4

/* Name of record test var. */
#define REC_VAR_NAME "surface_temperature"
#define REC_VAR_NAME2 "surface_temperature2"

/* Name of non-record test var. */
#define NOREC_VAR_NAME "surface_height"
#define NOREC_VAR_NAME2 "surface_height2"

char dim_name[NDIM3][PIO_MAX_NAME + 1] = {"unlim", "lat", "lon"};

/* Length of the dimension. */
#define LEN3 3

#define NUM_VAR_SETS 2

/* Create the decomposition to divide the 4-dimensional sample data
 * between the 4 tasks. For the purposes of decomposition we are only
 * concerned with 3 dimensions - we ignore the unlimited dimension.
 *
 * @param ntasks the number of available tasks
 * @param my_rank rank of this task.
 * @param iosysid the IO system ID.
 * @param dim_len an array of length 3 with the dimension sizes.
 * @param ioid a pointer that gets the ID of this decomposition.
 * @returns 0 for success, error code otherwise.
 **/
int create_decomposition_3d(int ntasks, int my_rank, int iosysid, int *ioid)
{
    PIO_Offset elements_per_pe;     /* Array elements per processing unit. */
    PIO_Offset *compdof;  /* The decomposition mapping. */
    int dim_len_3d[NDIM3] = {X_DIM_LEN, Y_DIM_LEN, Z_DIM_LEN};
    int ret;

    /* How many data elements per task? */
    elements_per_pe = X_DIM_LEN * Y_DIM_LEN * Z_DIM_LEN / ntasks;

    /* Allocate space for the decomposition array. */
    if (!(compdof = malloc(elements_per_pe * sizeof(PIO_Offset))))
        return PIO_ENOMEM;

    /* Describe the decomposition. */
    for (int i = 0; i < elements_per_pe; i++)
        compdof[i] = my_rank * elements_per_pe + i;

    /* Create the PIO decomposition for this test. */
    if ((ret = PIOc_init_decomp(iosysid, PIO_INT, NDIM3, dim_len_3d, elements_per_pe,
                                compdof, ioid, 0, NULL, NULL)))
        ERR(ret);

    /* Free the mapping. */
    free(compdof);

    return 0;
}

/* Run a simple test using darrays with async. */
int
run_darray_async_test(int iosysid, int my_rank, int ntasks, MPI_Comm test_comm,
                      MPI_Comm comp_comm, int num_flavors, int *flavor, int piotype)
{
    int ioid;
    int ioid3;
    int dim_len[NDIM3] = {NC_UNLIMITED, 2, 3};
    PIO_Offset elements_per_pe = LAT_LEN;
    PIO_Offset compdof[LAT_LEN] = {my_rank * 2 - 2, my_rank * 2 - 1};
    char decomp_filename[PIO_MAX_NAME + 1];
    int ret;

    sprintf(decomp_filename, "decomp_rdat_%s_.nc", TEST_NAME);

    /* Create the PIO decomposition for this test. */
    if ((ret = PIOc_init_decomp(iosysid, piotype, NDIM2, &dim_len[1], elements_per_pe,
                                compdof, &ioid, PIO_REARR_BOX, NULL, NULL)))
        BAIL(ret);

    /* Decompose the data over the tasks. */
    if ((ret = create_decomposition_3d(ntasks, my_rank, iosysid, &ioid3)))
        return ret;

    /* Write the decomp file (on appropriate tasks). */
    if ((ret = PIOc_write_nc_decomp(iosysid, decomp_filename, 0, ioid, NULL, NULL, 0)))
        return ret;

    int fortran_order;
    int ioid2;
    if ((ret = PIOc_read_nc_decomp(iosysid, decomp_filename, &ioid2, comp_comm,
                                   PIO_INT, NULL, NULL, &fortran_order)))
        return ret;

    /* Free the decomposition. */
    if ((ret = PIOc_freedecomp(iosysid, ioid2)))
        BAIL(ret);

    /* Test each available iotype. */
    for (int fmt = 0; fmt < num_flavors; fmt++)
    {
        int ncid;
        PIO_Offset type_size;
        int dimid[NDIM3];
        int varid[NVAR];
        char data_filename[PIO_MAX_NAME + 1];
        void *my_data;
        int my_data_int[LAT_LEN] = {my_rank * 10, my_rank * 10 + 1};

        /* Only netCDF-4 can handle extended types. */
        if (piotype > PIO_DOUBLE && flavor[fmt] != PIO_IOTYPE_NETCDF4C && flavor[fmt] != PIO_IOTYPE_NETCDF4P)
            continue;

        /* BYTE and CHAR don't work with pnetcdf. Don't know why yet. */
        if (flavor[fmt] == PIO_IOTYPE_PNETCDF && (piotype == PIO_BYTE || piotype == PIO_CHAR))
            continue;

        my_data = my_data_int;

        /* Create sample output file. */
        sprintf(data_filename, "data_%s_iotype_%d_piotype_%d.nc", TEST_NAME, flavor[fmt],
                piotype);
        if ((ret = PIOc_createfile(iosysid, &ncid, &flavor[fmt], data_filename,
                                   NC_CLOBBER)))
            BAIL(ret);

        /* Find the size of the type. */
        if ((ret = PIOc_inq_type(ncid, piotype, NULL, &type_size)))
            BAIL(ret);

        /* Define dimensions. */
        for (int d = 0; d < NDIM3; d++)
            if ((ret = PIOc_def_dim(ncid, dim_name[d], dim_len[d], &dimid[d])))
                BAIL(ret);

        /* Define variables. */
        if ((ret = PIOc_def_var(ncid, REC_VAR_NAME, piotype, NDIM3, dimid, &varid[0])))
            BAIL(ret);

        /* End define mode. */
        if ((ret = PIOc_enddef(ncid)))
            BAIL(ret);

        /* Set the record number for the record vars. */
        if ((ret = PIOc_setframe(ncid, varid[0], 0)))
            BAIL(ret);

        /* Write some data to the record vars. */
        if ((ret = PIOc_write_darray(ncid, varid[0], ioid, elements_per_pe, my_data, NULL)))
            BAIL(ret);

        /* Sync the file. */
        if ((ret = PIOc_sync(ncid)))
            BAIL(ret);

        /* Increment the record number for the record vars. */
        if ((ret = PIOc_advanceframe(ncid, varid[0])))
            BAIL(ret);

        /* Write another record. */
        if ((ret = PIOc_write_darray(ncid, varid[0], ioid, elements_per_pe, my_data, NULL)))
            BAIL(ret);

        /* Sync the file. */
        if ((ret = PIOc_sync(ncid)))
            BAIL(ret);

        /* Increment the record number for the record var. */
        if ((ret = PIOc_advanceframe(ncid, varid[0])))
            BAIL(ret);

        /* Write a third record. */
        if ((ret = PIOc_write_darray(ncid, varid[0], ioid, elements_per_pe, my_data, NULL)))
            BAIL(ret);

        /* Close the file. */
        if ((ret = PIOc_closefile(ncid)))
            BAIL(ret);
    } /* next iotype */

    /* Free the decomposition. */
    if ((ret = PIOc_freedecomp(iosysid, ioid)))
        BAIL(ret);

    /* Free the decomposition. */
    if ((ret = PIOc_freedecomp(iosysid, ioid3)))
        BAIL(ret);
exit:
    return ret;
}

/* Run Tests for pio_spmd.c functions. */
int main(int argc, char **argv)
{
    int my_rank; /* Zero-based rank of processor. */
    int ntasks;  /* Number of processors involved in current execution. */
    int num_flavors; /* Number of PIO netCDF flavors in this build. */
    int flavor[NUM_FLAVORS]; /* iotypes for the supported netCDF IO flavors. */
    MPI_Comm test_comm; /* A communicator for this test. */
    int ret;     /* Return code. */

    /* Initialize test. */
    if ((ret = pio_test_init2(argc, argv, &my_rank, &ntasks, MIN_NTASKS,
                              TARGET_NTASKS, -1, &test_comm)))
        ERR(ERR_INIT);
    if ((ret = PIOc_set_iosystem_error_handling(PIO_DEFAULT, PIO_RETURN_ERROR, NULL)))
        return ret;

    /* Figure out iotypes. */
    if ((ret = get_iotypes(&num_flavors, flavor)))
        ERR(ret);

    {
        int iosysid;

        /* Initialize with task 0 as IO task, tasks 1-3 as a
         * computation component. */
#define NUM_IO_PROCS 1
#define NUM_COMPUTATION_PROCS 3
#define COMPONENT_COUNT 1
        int num_computation_procs = NUM_COMPUTATION_PROCS;
        MPI_Comm io_comm;              /* Will get a duplicate of IO communicator. */
        MPI_Comm comp_comm[COMPONENT_COUNT]; /* Will get duplicates of computation communicators. */
        int mpierr;

        {
            if ((ret = PIOc_init_async(test_comm, NUM_IO_PROCS, NULL, COMPONENT_COUNT,
                                       &num_computation_procs, NULL, &io_comm, comp_comm,
                                       PIO_REARR_BOX, &iosysid)))
                ERR(ERR_INIT);

            /* This code runs only on computation components. */
            if (my_rank)
            {
                /* Run the simple darray async test. */
                if ((ret = run_darray_async_test(iosysid, my_rank, ntasks, test_comm,
                                                 comp_comm[0], num_flavors, flavor,
                                                 PIO_INT)))
                    return ret;

                /* Finalize PIO system. */
                if ((ret = PIOc_finalize(iosysid)))
                    return ret;

                /* Free the computation conomponent communicator. */
                if ((mpierr = MPI_Comm_free(comp_comm)))
                    MPIERR(mpierr);
            }
            else
            {
                /* Free the IO communicator. */
                if ((mpierr = MPI_Comm_free(&io_comm)))
                    MPIERR(mpierr);
            }
        } /* next type */
    } /* endif my_rank < TARGET_NTASKS */

    /* Finalize the MPI library. */
    if ((ret = pio_test_finalize(&test_comm)))
        return ret;

    printf("%d %s SUCCESS!!\n", my_rank, TEST_NAME);

    return 0;
}
