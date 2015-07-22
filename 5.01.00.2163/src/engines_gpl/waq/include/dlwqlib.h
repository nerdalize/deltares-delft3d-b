//  Copyright(C) Stichting Deltares, 2012.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 3,
//  as published by the Free Software Foundation.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//  contact: delft3d.support@deltares.nl
//  Stichting Deltares
//  P.O. Box 177
//  2600 MH Delft, The Netherlands
//
//  All indications and logos of, and references to registered trademarks 
//  of Stichting Deltares remain the property of Stichting Deltares. All 
//  rights reserved.

//#  define DLLEXTERN extern "C" __stdcall
//DLLEXTERN int * PC2_CreateHandle();

//
int SETPROCESSDEFINITION(char * mode, char * procdef_file, int lenmode, int lenprocdef_file);
int SETPROCESSDEFINITIONX(char * mode, char * procdef_file, char * sfrac_file, int lenmode, int lenprocdef_file, int lensfrac_file);
//
int SETSIMULATIONTIMES(int * startTime, int * endTime, int * timeStep);
int GETSIMULATIONTIMES(int * startTime, int * endTime, int * timeStep);
int SETREFERENCEDATE(int * year, int * month, int * day, int * hour, int * minute, int * second);
int SETOUTPUTTIMERS(int * type, int * startTime, int * endTime, int * timeStep);
int GETTIMEHORIZON(double * startTime, double * stopTime);
void GETCURRENTTIME(double * retVal);
int GETWQCURRENTTIME(double * currentTime);
//
int GETITEMCOUNT(int * type);
int GETITEMINDEX(int * type, char * name, int lenchar);
int GETITEMNAME(int * type, int * idx, char * name, int lenchar);
//
int GETACTIVESUBSTANCESCOUNT();
int GETTOTALSUBSTANCESCOUNT();
int GETSUBSTANCEID(char * name, int lenchar);
int GETSUBSTANCENAME(int * subid, char * name, int lenchar);
//
int GETLOCATIONCOUNT(int * type);
int GETLOCATIONID(int * type, char * name, int lenchar);
int GETLOCATIONIDS(int * type, int * idsSize, int * ids);
int GETLOCATIONNAME(int * type, int * idx, char * name, int lenchar);
//
int MODELINITIALIZE();
int MODELINITIALIZE_BY_ID(char * runid, int lenrunid);
int MODELPERFORMTIMESTEP();
int MODELFINALIZE();
//
int GETLASTMESSAGE(int * level, char * text, int lentext);
//
int GETCURRENTVALUE(char * name, float * value, int lenname);
int SETCURRENTVALUESCALARINIT(char * name, float * value, int lenname);
int SETCURRENTVALUESCALARRUN(char * name, float * value, int lenname);

int SETINTEGRATIONOPTIONS(int * method, int * disp_flow_zero, int * disp_bound, int * first_order, int * forester, int * anticreep);
int SETBALANCEOUTPUTOPTIONS(int * type, int * lump_processes, int * lump_loads, int * lump_transport, int * suppress_space, int * suppress_time, int * unit_type);
int DEFINEWQSCHEMATISATION(int * number_segments, int * pointer_table, int * number_exchanges);
int DEFINEWQDISPERSION(float * dispc, float * length);
int DEFINEWQPROCESSES(char * substance, int * number_substances, int * number_transported, char * process_parameter, int * parameters, char * process, int * number_processes, int lensubstance, int lenprocess_parameter, int lenprocess);
int DEFINEWQPROCESSESX(char * substance, int * substance_mult, int * number_substances, int * number_transported,
        char * process_parameter, int * parameters, char * field_parameter, int * fields,
        char * process, int * number_processes, int lensubstance, int lenprocess_parameter, int lenfield_parameter, int lenprocess);
int DEFINEWQEXTRAOUTPUTPARAMETERS(char * extra_output, int * number_output, int lenextra_output);
int DEFINEDISCHARGELOCATIONS(int * cell, int * number_loads);
int DEFINEMONITORINGLOCATIONS(int * cell, char * name, int * number_monitoring, int lenname);

int SETINITIALVOLUME(float * volume);
int SETFLOWDATA(float * volume, float * area, float * flow);
int SETWASTELOADVALUES(int * idx, float * value);
int SETBOUNDARYCONDITIONS(int * idx, float * value);

int COUNT_VALUES(int * partype, int * parid, int * loctype, int * locid);
int SET_VALUES(int * partype, int * parid, int * loctype, int * locid, int * operation, int * number, double * values);
int GET_VALUES(int * partype, int * parid, int * loctype, int * locid, int * number, double * values);
void CHECKPARAMETERID(int * type, int * parid, int * success);
void SETNEWVALUE(float * value, float * new_value, int * operation);

void SETCOMMONVARS(int * icons, int * iparm, int * iconc, int * ibset, int * iwste, int * nosys, int * notot, int * nocons, int * nopa, int * noseg, int * nowst, int * nobnd);
void STOREOPERATION(int * index, int * number, float * new_value, int * operation);

int CREATE_INSTANCE();
int STORE_CURRENT_INSTANCE(int * storage_level);
void SELECT_INSTANCE(int * instance_id);
void SET_MAX_INSTANCES_IN_MEMORY(int * max_instances);
int GET_INSTANCE_CORE_STATE(float * corestate, int * size_corestate);
int SET_INSTANCE_CORE_STATE(float * corestate, int * size_corestate);
int GET_INSTANCE_SIZE();

//void GETDLWQD(delwaq_data * dlwqd_copy);
//void SETDLWQD(delwaq_data * dlwqd_copy);
//void TEST_APPLY_OPERATIONS(delwaq_data * dlwqd);
