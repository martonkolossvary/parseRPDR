# Change log

## parseRPDR 1.0.0 - 2023/03/17
-   Now support all data formats!

-   Simplified parallelization to only PSOCK

-   All functions now have unit test. Test coverage
    of code is 100%
    
-   create_img_db extensions now include: ".tmp", ""

-   Suppressed start up messages in parallel loops

-   Added research_invitations variable to Con tables

-   Corrected Diagnosis_Flag to Procedure_Flag for Prc data

-   All columns are returned as is, no punctuation etc.
    is removed as previously
    
-   Changed load_all function to load_all_data, as now
    RPDR supports Allergy tables with the load_all
    function

-   In convert functions "time_type" arrgument is changed
    to "aggr_type" to support other aggregations methods
    in the future.

## parseRPDR 0.2.4 - 2023/01/29

-   Removed all example datafiles.

-   **nThreads** for every function is now
    parallel::detectCores()-1 by default.
    
-   **reticulate** and **bigmemory** packages have been
    moved to Suggests. Importing of pydicom is
    now delay_load = TRUE. It allows users to
    specify a desired location for Python before
    interacting with your package.

-   **load_notes** now has an additional argument.
    If **format_orig** is set to TRUE, then the returned
    report maintains the original formatting, as
    opposed to FALSE, when all white spaces used for
    formatting are removed.
    


## parseRPDR 0.2.3 - 2022/06/07

-   Bug-fix: file names and extensions are now
    converted to lower case for **create_img_db**
    to avoid mismatches due to capitals. Furthermore,
    the default extensions are now: ".dcm", ".dicom"
    and ".ima" to avoid matches withing other parts
    of the file name string.
    
-   Bug-fix: in **create_img_db** now **""** is
    supported for identification of DICOM files
    without any extension.
    
-   Added **load_report** attribute to **load_notes**
    to specify whether the report text should be
    returned or not. Also, this attribute now affects
    whether **load_all** loads report text in case 
    of notes.


## parseRPDR 0.2.2 - 2022/05/30

-   Added **load_dem_old** which is the 
    **load_dem** function from previous versions of the
    software. **load_dem** now supports demographics
    tables following changes done in the beginning of 
    2022. Please use appropriate 
    load_dem or load_dem_old function to load
    demographics data.

-   Added **old_dem** boolean argument to **load_all** 
    function which defaults to FALSE. If set TRUE, then
    **load_dem_old** function is used which corresponds
    to the **load_dem** function from previous versions
    of the software.

## parseRPDR 0.2.1 - 2021/09/10

-   Bug-fix: load_notes error where inappropriate
    partitioning of the file caused read errors.
    
-   Bug-fix: if data only contains 1 row, and 
    identical column value removal is requested,
    the data.table is now returned.

## parseRPDR 0.2.0 - 2021/07/24

-   Added **create_img_db** function which parses
    DICOM header information from radiological image
    series to create a database with meta-data. 
    
-   Added **lab_result_abn_flag_pretty** column to
    **convert_lab** which marks abnormality based-on
    the Abnormal_Flag column in RPDR in lab.txt.
    
-   Bug-fix: problems with **find_exam** when
    data.table is small and when sequential execution
    is requested is fixed.


## parseRPDR 0.1.2 - 2021/06/01

-   Now support **Lno** and **Phy** datasource.

-   Now support processing of **Phy** datasource
    using **convert_phy** function. **Lno** can 
    be processed using the **convert_notes**
    function.
    
-   Updated README, and removed vignette.

-   Encounter numbers and accession numbers
    have been moved to the end of
    the data.table to make accessing them easier
    using indexes.
    
-   Inconsistencies in naming have been updated. 
    Rdt and Lab tables time variable is simply:
    time_lab or time_rdt.
    Encounter data table's encounter number is now
    enc_enc_numb.
    Time is now always in from in the naming, even
    for convert functions.

-   Bug fix: **load_notes** now names the new columns, 
    based-on the type of note.
    
-   Bug fix: corrected **load_prc** in case there
    were carriage returns present in the name field.
    
-   Bug fix: added **load_all** compatibility with 
    Dia datasource if date constraints are used.

-   Corrected documentation where needed.

## parseRPDR 0.1.1 - 2021/05/23

-   Now support **Prc** datasource.

-   Now support processing of **Prc** datasource
    using **convert_prc** function.

-   Corrected documentation where needed.

-   Optimized sequential execution by not creating a
    socket when only 1 iteration is needed in convert
    functions.

## parseRPDR 0.1.0 - 2021/05/09

-   Now support **Rfv** and all note type files:
    **Car, Dis, End, Hnp, Opn, Pat, Prg, Pul, Rad and Vis**
    using a single function: **load_notes**. Therefore,
    previous note related load functions have been removed.

-   Added **convert_rfv** functionality.

-   Now all notes files may be parsed using the
    **convert_notes** function. Therefore,
    previous note related convert functions have been removed.

-   For all datasources Clinic and Hospital names are not modified 
    in any way so they are identical to the ones in RPDR, 
    only missing values are changed to NA.
    
-   In functions parsing notes, exact matches are used for efficiency.

-   Bug-fix: load_dia did not load the encounter number of the record.

-   Bug-fix: parsing of notes has changed as in case of large files
    it caused memory issues.
    
-   Bug-fix: all columns are read as character as large MRNs interpreted
    as integers may cause problems.

## parseRPDR 0.0.2 - 2021/04/16

-   Upon attaching the package the *future.globals.maxSize* is set to
    100Gb to allow for parallel processing of large datasets on windows
    machines and also when using the shared memory version of the
    **find_exam** function.

-   Upon attaching the package the data.table *setDTthreads* is set to
    1, so that there are no problems for parallel processing. Set
    *data.table::setDTthreads()* manually after the you are done with
    using parseRPRD, if you wish to use multi-thread processing of
    data.table in other tasks.
    
-   Timezone attributes are removed from POSIXct times as they may
    cause problems if working on different OS in different timezones.
    
-   In the **find_exam** function time variables are first truncated 
    to the precision of *time_unit* argument using the *trunc.POSIXt*
    function for consistency, and then time differences are calculated
    using the *difftime* function.

-   Bug-fix: In case lab values were recorded as having “\>” or “\<”
    signs and the value was equal to the normal range i.e. value:
    \<0.01, normal range: \<0.01, then these values are considered as
    *NORMAL*.

-   Technical-fix: removed *..* calls from functions so that only simple
    calls are present.
