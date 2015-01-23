
/** \file SystemConfiguration.h
   
 \brief Gannet Service-based SoC project - C++/SystemC System Configuration
        
        Generated from SBA.yml with create_Cxx_SystemConfiguration.rb

  (c) 2008-2012 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
    
*/

//==============================================================================
//
// System Configuration
//
// GENERATED from YAML configuration using create_Cxx_SystemConfiguration.rb
//
//==============================================================================

// 

#ifndef _SBA_SYSTEM_CONFIGURATION_H_
#define _SBA_SYSTEM_CONFIGURATION_H_

//#include <map>

using namespace std;

typedef unsigned int UINT;

namespace SBA {

const UINT M_GMCF_GMCF_run_model1 = 73729;
const UINT M_GMCF_GMCF_run_model10 = 73730;
const UINT M_GMCF_GMCF_run_model11 = 73731;
const UINT M_GMCF_GMCF_run_model12 = 73732;
const UINT M_GMCF_GMCF_run_model2 = 73733;
const UINT M_GMCF_GMCF_run_model3 = 73734;
const UINT M_GMCF_GMCF_run_model4 = 73735;
const UINT M_GMCF_GMCF_run_model5 = 73736;
const UINT M_GMCF_GMCF_run_model6 = 73737;
const UINT M_GMCF_GMCF_run_model7 = 73738;
const UINT M_GMCF_GMCF_run_model8 = 73739;
const UINT M_GMCF_GMCF_run_model9 = 73740;
const UINT M_CoreServices_LET_let = 131841;
const UINT M_CoreServices_LET_assign = 131842;
const UINT M_CoreServices_LET_read = 131843;
const UINT M_CoreServices_LET_buf = 131844;
const UINT M_CoreServices_LET_stream = 131845;
const UINT M_CoreServices_LET_eos = 131846;
const UINT M_CoreServices_LET_update = 131847;
const UINT M_CoreServices_LET_lettc = 131848;
const UINT M_CoreServices_APPLY_apply = 132353;
const UINT M_CoreServices_APPLY_applytc = 132354;
const UINT M_CoreServices_APPLY_lambda = 132355;
const UINT M_CoreServices_IF_if = 132097;
const UINT M_CoreServices_IF_iftc = 132098;
const UINT M_CoreServices_IF_return = 132099;
const UINT M_CoreServices_IF_returntc = 132100;
const UINT M_CoreServices_CTRL_run = 133889;
const UINT M_CoreServices_SEQ_seq = 131585;
const UINT M_CoreServices_SEQ_seqtc = 131586;
const UINT M_CoreServices_BEGIN_begin = 131329;
const UINT M_CoreServices_BEGIN_begintc = 131330;
const UINT M_CoreServices_LAMBDA_lambda = 132609;
const UINT M_CoreServices_IO_open = 132865;
const UINT M_CoreServices_IO_close = 132866;
const UINT M_CoreServices_IO_readline = 132867;
const UINT M_CoreServices_IO_write = 132868;
const UINT M_CoreServices_IO_eof = 132869;
const UINT M_CoreServices_IO_display = 132870;
const UINT M_CoreServices_Math_rand = 133377;
const UINT M_CoreServices_ALU_plus = 133121;
const UINT M_CoreServices_ALU_minus = 133122;
const UINT M_CoreServices_ALU_times = 133123;
const UINT M_CoreServices_ALU_over = 133124;
const UINT M_CoreServices_ALU_lt = 133125;
const UINT M_CoreServices_ALU_gt = 133126;
const UINT M_CoreServices_ALU_eq = 133127;
const UINT M_CoreServices_ALU_not = 133128;
const UINT M_CoreServices_FPU_plus = 133633;
const UINT M_CoreServices_FPU_minus = 133634;
const UINT M_CoreServices_FPU_times = 133635;
const UINT M_CoreServices_FPU_over = 133636;
const UINT M_CoreServices_FPU_lt = 133637;
const UINT M_CoreServices_FPU_gt = 133638;
const UINT M_CoreServices_FPU_eq = 133639;
const UINT M_CoreServices_FPU_not = 133640;
const UINT M_CoreServices_REG_write = 134145;
const UINT M_CoreServices_REG_read = 134146;
const UINT M_CoreServices_REG_inc = 134147;
const UINT M_CoreServices_REG_dec = 134148;
const UINT M_CoreServices_REG_add = 134149;
const UINT M_CoreServices_REG_sub = 134150;
const UINT M_CoreServices_REG_mul = 134151;
const UINT M_CoreServices_REG_div = 134152;
const UINT M_CoreServices_REG_mulacc = 134153;
const UINT S_CoreServices_BEGIN = 13;
const UINT SC_GMCF_GMCF = 288;
const UINT SC_CoreServices_BEGIN = 513;
const UINT SC_CoreServices_SEQ = 514;
const UINT SC_CoreServices_LET = 515;
const UINT SC_CoreServices_IF = 516;
const UINT SC_CoreServices_APPLY = 517;
const UINT SC_CoreServices_LAMBDA = 518;
const UINT SC_CoreServices_IO = 519;
const UINT SC_CoreServices_ALU = 520;
const UINT SC_CoreServices_Math = 521;
const UINT SC_CoreServices_FPU = 522;
const UINT SC_CoreServices_CTRL = 523;
const UINT SC_CoreServices_REG = 524;

// Not elegant, but static arrays are a lot faster than linked lists!
const UINT NSERVICES = 13;
const UINT SERVICE_ADDRESSES[13]={1,2,3,4,5,6,7,8,9,10,11,12,13};
     
} // SBA
#endif /*_SBA_SYSTEM_CONFIGURATION_H_*/
